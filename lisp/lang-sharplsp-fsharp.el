;;; lang-sharplsp-fsharp.el --- Syntax-validated SharpLsp F# presentation -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'treesit)

(defvar-local chief/sharplsp-fsharp-parser nil)
(defvar-local chief/sharplsp-signature-overlays nil)
(defvar-local chief/sharplsp-fsharp-timer nil)
(defvar-local chief/sharplsp-signature-timer nil)
(defvar-local chief/sharplsp-fsharp-eldoc-info nil)

(defun chief/sharplsp-fsharp-eldoc (callback &rest _)
  "Supply only compiler information matching the current revision and point."
  (when (and chief/sharplsp-fsharp-eldoc-info
             (= (nth 0 chief/sharplsp-fsharp-eldoc-info) (buffer-chars-modified-tick))
             (= (nth 1 chief/sharplsp-fsharp-eldoc-info) (point)))
    (funcall callback (nth 2 chief/sharplsp-fsharp-eldoc-info))
    t))

(defun chief/sharplsp-install-fsharp-grammar ()
  "Build the official F# grammar into Emacs's own ignored grammar directory."
  (interactive)
  (let ((treesit-language-source-alist
         (cons '(fsharp "https://github.com/ionide/tree-sitter-fsharp" nil "fsharp/src")
               treesit-language-source-alist)))
    (treesit-install-language-grammar 'fsharp)))

(defun chief/sharplsp-fsharp-tree ()
  "Return the live F# syntax tree without replacing the source major mode.
Prefer an installed Emacs grammar; an existing compatible user-local grammar
can be reused read-only. No parser is downloaded or built automatically."
  (unless (treesit-language-available-p 'fsharp)
    (when-let* ((file (seq-find #'file-readable-p
                               (mapcar #'expand-file-name
                                       '("~/.local/share/nvim/site/parser/fsharp.so"
                                         "~/.local/share/nvim/lazy/nvim-treesitter/parser/fsharp.so")))))
      (add-to-list 'treesit-load-name-override-list
                   (list 'fsharp (file-name-sans-extension file) "tree_sitter_fsharp"))))
  (when (treesit-language-available-p 'fsharp)
    (setq chief/sharplsp-fsharp-parser (treesit-parser-create 'fsharp))
    (treesit-parser-root-node chief/sharplsp-fsharp-parser)))

(defun chief/sharplsp-fsharp-node (root position)
  (when (and root (<= (point-min) position) (< position (point-max)))
    (treesit-node-descendant-for-range root position (1+ position) t)))

(defun chief/sharplsp-fsharp-text (node)
  (and node (treesit-node-text node t)))

(defun chief/sharplsp-fsharp-flatten-call (node)
  "Return (CALLEE . ARGUMENT-NODES), excluding guessed/infix arguments."
  (if (and (equal (treesit-node-type node) "application_expression")
           (= (treesit-node-child-count node t) 2))
      (let* ((left (chief/sharplsp-fsharp-flatten-call (treesit-node-child node 0 t)))
             (argument (treesit-node-child node 1 t))
             (arguments
              (if (equal (treesit-node-type argument) "tuple_expression")
                  (cl-loop for index below (treesit-node-child-count argument t)
                           collect (treesit-node-child argument index t))
                (list argument))))
        (append left arguments))
    (list node)))

(defun chief/sharplsp-fsharp-type-binding (root position)
  "Return a syntax-proven unannotated (IDENTIFIER . CONTEXT) at POSITION."
  (let* ((node (chief/sharplsp-fsharp-node root (1- position)))
         (identifier node) result done)
    (when (and node (equal (treesit-node-type node) "identifier")
               (= position (treesit-node-end node)))
      (while (and node (not done))
        (let ((kind (treesit-node-type node)))
          (cond
           ((member kind '("typed_pattern" "property_or_ident" "function_declaration_left"))
            (setq done t))
           ((member kind '("argument_patterns" "value_declaration_left" "rule"))
            (let* ((annotation (if (equal kind "value_declaration_left")
                                   (treesit-node-parent node) node))
                   annotated stop)
              (cl-loop for index below (treesit-node-child-count annotation)
                       for child = (treesit-node-child annotation index)
                       unless stop do
                       (cond ((equal (treesit-node-type child) "=") (setq stop t))
                             ((equal (treesit-node-type child) ":") (setq annotated t))))
              (unless annotated (setq result (cons identifier node)))
              (setq done t)))))
        (setq node (and node (treesit-node-parent node)))))
    result))

(defun chief/sharplsp-fsharp-inline-safe-p (identifier context)
  "Reject annotations that would visually invent parentheses in F# patterns."
  (let ((name (chief/sharplsp-fsharp-text identifier))
        (node (treesit-node-parent identifier)) found done)
    (if (and (equal (treesit-node-type context) "value_declaration_left")
             (equal (string-trim (chief/sharplsp-fsharp-text context)) name))
        t
      (while (and node (not done))
        (when (string-match-p (concat "\\`([ \t]*" (regexp-quote name) "[ \t]*)\\'")
                              (chief/sharplsp-fsharp-text node))
          (setq found t))
        (setq done (treesit-node-eq node context)
              node (treesit-node-parent node)))
      found)))

(defun chief/sharplsp-fsharp-function-name (root position label)
  "Find a function's declared name, never its parameters or receiver."
  (let* ((node (chief/sharplsp-fsharp-node root (1- position)))
         (name node) result done)
    (when (and node (member (treesit-node-type node) '("identifier" "op_identifier"))
               (= (treesit-node-end node) position))
      (while (and node (not done))
        (pcase (treesit-node-type node)
          ("function_declaration_left"
           (when (treesit-node-eq name (treesit-node-child node 0 t)) (setq result name))
           (setq done t))
          ((or "argument_patterns" "typed_pattern" "property_or_ident") (setq done t))
          ("value_declaration_left"
           (when (and (string-match-p "->" label)
                      (equal (string-trim (chief/sharplsp-fsharp-text node))
                             (chief/sharplsp-fsharp-text name)))
             (setq result name))
           (setq done t)))
        (setq node (and node (treesit-node-parent node)))))
    result))

(defun chief/sharplsp-fsharp-compact-type (label node)
  "Shorten common/current namespaces, keeping the full compiler text in tooltips."
  (let ((case-fold-search nil))
    (cl-labels ((strip (prefix)
                  (setq label
                        (replace-regexp-in-string
                         (concat "\\(^\\|[^[:alnum:]_.']\\)" (regexp-quote prefix))
                         "\\1" label t))))
      (dolist (prefix '("Microsoft.FSharp.Core." "Microsoft.FSharp.Collections." "System."))
        (strip prefix))
      (let ((parent node))
        (while (and parent (not (equal (treesit-node-type parent) "namespace")))
          (setq parent (treesit-node-parent parent)))
        (when-let* ((namespace (and parent (treesit-node-child parent 0 t))))
          (strip (concat (chief/sharplsp-fsharp-text namespace) "."))))))
  label)

(defun chief/sharplsp-signature-overlay (name label)
  "Show compiler LABEL above NAME, without inserting or recoloring source text."
  (when chief/sharplsp-signatures-mode
    (let* ((position (treesit-node-start name))
           (line (save-excursion (goto-char position) (line-beginning-position)))
           (text (chief/sharplsp-fsharp-text name))
           (overlay (make-overlay line line nil t t)))
      (overlay-put overlay 'chief/sharplsp-name text)
      (overlay-put overlay 'before-string
                   (concat (propertize "val " 'face 'font-lock-keyword-face)
                           (propertize text 'face 'font-lock-function-name-face)
                           (propertize (concat " " label "\n") 'face 'font-lock-type-face)))
      (push overlay chief/sharplsp-signature-overlays))))

(defun chief/sharplsp-fsharp-filter-hints (hints range)
  "Validate HINTS against syntax, never guess positions or fabricate F# syntax."
  (let ((root (chief/sharplsp-fsharp-tree))
        (groups (make-hash-table :test #'eql))
        (seen (make-hash-table :test #'equal)) result)
    (when root
      (let ((start (if range (chief/sharplsp-position-point (chief/sharplsp-field range "start")) (point-min)))
            (end (if range (chief/sharplsp-position-point (chief/sharplsp-field range "end")) (point-max))))
        (setq chief/sharplsp-signature-overlays
              (seq-filter (lambda (overlay)
                            (if (and (overlay-start overlay)
                                     (<= start (overlay-start overlay) end))
                                (progn (delete-overlay overlay) nil)
                              (overlay-start overlay)))
                          chief/sharplsp-signature-overlays)))
      (dolist (hint hints)
        (let* ((position (ignore-errors (chief/sharplsp-position-point (chief/sharplsp-field hint "position"))))
               (kind (chief/sharplsp-field hint "kind"))
               (label (chief/sharplsp-field hint "label")))
          (when (and position (stringp label))
            (cond
             ((equal kind 1)
              (let* ((name (chief/sharplsp-fsharp-function-name root position label))
                     (binding (chief/sharplsp-fsharp-type-binding root position)))
                (when name
                  (let ((key (list position label)))
                    (unless (gethash key seen)
                      (puthash key t seen)
                      (chief/sharplsp-signature-overlay
                       name (chief/sharplsp-fsharp-compact-type label name)))))
                (when (and (not name) binding
                           (not (equal (chief/sharplsp-fsharp-text (car binding)) "_"))
                           (chief/sharplsp-fsharp-inline-safe-p (car binding) (cdr binding)))
                  (let ((copy (if (hash-table-p hint) (copy-hash-table hint) (copy-tree hint))))
                    (lsp-put copy :label (chief/sharplsp-fsharp-compact-type label (car binding)))
                    (lsp-put copy :tooltip (lsp-make-markup-content :kind "plaintext" :value label))
                    (push copy result)))))
             ((equal kind 2) (puthash position (append (gethash position groups) (list hint)) groups))))))
      (maphash
       (lambda (position group)
         (let ((node (chief/sharplsp-fsharp-node root position)) arguments correct-start)
           (while node
             (when (equal (treesit-node-type node) "application_expression")
               (let* ((call (chief/sharplsp-fsharp-flatten-call node)) (callee (car call)))
                 (when (and (member (treesit-node-type callee)
                                    '("long_identifier_or_op" "dot_expression"))
                            (<= (treesit-node-start callee) position)
                            (< position (treesit-node-end callee)))
                   (setq arguments (cdr call)))
                 (when (seq-some (lambda (argument) (= position (treesit-node-start argument)))
                                 (cdr call))
                   (setq correct-start t))))
             (setq node (treesit-node-parent node)))
           (cond
            ((and arguments (= (length arguments) (length group)))
             (cl-mapc
              (lambda (hint argument)
                (unless (equal (string-remove-suffix ":" (chief/sharplsp-field hint "label"))
                               (chief/sharplsp-fsharp-text argument))
                  (let ((copy (if (hash-table-p hint) (copy-hash-table hint) (copy-tree hint))))
                    (lsp-put copy :position (chief/sharplsp-point-position (treesit-node-start argument)))
                    (push copy result))))
              group arguments))
            ((and (not arguments) correct-start
                  (cl-every (lambda (hint) (equal (chief/sharplsp-field hint "label")
                                                  (chief/sharplsp-field (car group) "label"))) group))
             (push (car group) result)))))
       groups))
    (nreverse result)))

(defun chief/sharplsp-fsharp-response-a (function method params callback &rest arguments)
  "Validate F# hints only for this server; discard stale buffer revisions."
  (if (not (and (equal method "textDocument/inlayHint")
                (derived-mode-p 'fsharp-mode 'fsharp-ts-mode)
                (chief/sharplsp-workspace)))
      (apply function method params callback arguments)
    (let ((buffer (current-buffer)) (tick (buffer-chars-modified-tick)))
      (apply function method params
             (lambda (response)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (when (= tick (buffer-chars-modified-tick))
                     (funcall callback
                              (chief/sharplsp-fsharp-filter-hints
                               response (chief/sharplsp-field params "range")))))))
             arguments))))

(defun chief/sharplsp-fsharp-last-identifier (node)
  (cond
   ((not node) nil)
   ((equal (treesit-node-type node) "identifier") node)
   ((member (treesit-node-type node) '("type_arguments" "simple_type")) nil)
   (t (cl-loop for index downfrom (1- (treesit-node-child-count node t)) to 0
               thereis (chief/sharplsp-fsharp-last-identifier (treesit-node-child node index t))))))

(defun chief/sharplsp-fsharp-callee-position ()
  "Locate the actual nested/multiline F# callee, not the first word on a line."
  (when-let* ((root (chief/sharplsp-fsharp-tree)))
    (let* ((node (chief/sharplsp-fsharp-node root (max (point-min) (1- (point)))))
           (symbol (and node (equal (treesit-node-type node) "identifier") node))
           target)
      (while (and node (not target))
        (pcase (treesit-node-type node)
          ("application_expression"
           (let ((callee (car (chief/sharplsp-fsharp-flatten-call node))))
             (when (member (treesit-node-type callee) '("long_identifier_or_op" "dot_expression"))
               (setq target (chief/sharplsp-fsharp-last-identifier callee)))))
          ("function_declaration_left"
           (setq target (chief/sharplsp-fsharp-last-identifier (treesit-node-child node 0 t)))))
        (setq node (treesit-node-parent node)))
      (when-let* ((name (or target symbol)))
        (chief/sharplsp-point-position (max (treesit-node-start name) (1- (treesit-node-end name))))))))

(defun chief/sharplsp-fsharp-signature-help (&optional silent)
  "Show normal signature help or compiler hover for curried F# applications.
Hover fallback is callable type information, not overload/active-argument data."
  (interactive)
  (let ((buffer (current-buffer)) (tick (buffer-chars-modified-tick)) (position (point))
        (params (lsp--text-document-position-params))
        (document (lsp--text-document-identifier))
        (callee (chief/sharplsp-fsharp-callee-position)))
    (cl-labels
        ((show (text)
           (when (and (buffer-live-p buffer)
                      (with-current-buffer buffer
                        (and (= tick (buffer-chars-modified-tick)) (= position (point)))))
             (with-current-buffer buffer
               (if silent
                   (progn
                     (setq chief/sharplsp-fsharp-eldoc-info
                           (list tick position
                                 (replace-regexp-in-string
                                  "^```[^\n]*\n?" ""
                                  (if (stringp text) text (or (chief/sharplsp-field text "value") "")))))
                     (eldoc-print-current-symbol-info))
                 (lsp--display-contents text))))))
      (lsp-request-async
       "textDocument/signatureHelp" params
       (lambda (response)
         (if-let* ((signatures (chief/sharplsp-field response "signatures"))
                    ((> (length signatures) 0)))
             (show (mapconcat (lambda (signature) (chief/sharplsp-field signature "label"))
                              signatures "\n"))
           (when callee
             (lsp-request-async
              "textDocument/hover" (list :textDocument document :position callee)
              (lambda (hover)
                (when-let* ((contents (chief/sharplsp-field hover "contents")))
                  (show contents))) :mode 'tick))))
       :mode 'tick))))

(defun chief/sharplsp-fsharp-auto-signature ()
  "Debounce curried-call help without interfering with the completion popup."
  (when chief/sharplsp-signature-timer (cancel-timer chief/sharplsp-signature-timer))
  (let ((buffer (current-buffer)))
    (setq chief/sharplsp-signature-timer
          (run-with-idle-timer
           0.35 nil (lambda ()
                      (when (and (buffer-live-p buffer) (eq buffer (window-buffer))
                                 (with-current-buffer buffer (chief/sharplsp-workspace)))
                        (with-current-buffer buffer (chief/sharplsp-fsharp-signature-help t))))))))

(defun chief/sharplsp-fsharp-refresh-signatures ()
  "Refresh declaration signatures independently when inline hints are disabled."
  (when (and chief/sharplsp-signatures-mode (chief/sharplsp-workspace)
             (not (bound-and-true-p lsp-inlay-hints-mode)))
    (lsp-request-async
     "textDocument/inlayHint"
     (list :textDocument (lsp--text-document-identifier)
           :range (lsp--region-to-range (point-min) (point-max)))
     #'ignore :mode 'tick)))

(defun chief/sharplsp-fsharp-changed (&rest _)
  "Retain valid annotations while typing, but remove renamed/deleted headers."
  (setq chief/sharplsp-signature-overlays
        (seq-filter
         (lambda (overlay)
           (if (and (overlay-start overlay)
                    (save-excursion
                      (goto-char (overlay-start overlay))
                      (let* ((name (overlay-get overlay 'chief/sharplsp-name))
                             (pattern (if (string-match-p "\\`[[:alnum:]_]" name)
                                          (concat "\\_<" (regexp-quote name) "\\_>")
                                        (regexp-quote name))))
                        (re-search-forward pattern (line-end-position) t))))
               t
             (delete-overlay overlay) nil))
         chief/sharplsp-signature-overlays))
  (when chief/sharplsp-fsharp-timer (cancel-timer chief/sharplsp-fsharp-timer))
  (let ((buffer (current-buffer)))
    (setq chief/sharplsp-fsharp-timer
          (run-with-idle-timer 0.5 nil
                              (lambda ()
                                (when (buffer-live-p buffer)
                                  (with-current-buffer buffer
                                    (chief/sharplsp-fsharp-refresh-signatures))))))))

(define-minor-mode chief/sharplsp-signatures-mode
  "Show compiler function types above declarations, independently of inline hints."
  :lighter nil
  (if chief/sharplsp-signatures-mode
      (when (chief/sharplsp-workspace)
        (if (bound-and-true-p lsp-inlay-hints-mode)
            (lsp-update-inlay-hints (point-min) (point-max))
          (chief/sharplsp-fsharp-refresh-signatures)))
    (mapc #'delete-overlay chief/sharplsp-signature-overlays)
    (setq chief/sharplsp-signature-overlays nil)))

(defun chief/sharplsp-fsharp-presentation-setup ()
  "Install F#-only syntax validation and curried signature presentation."
  (when (and chief/sharplsp-mode (derived-mode-p 'fsharp-mode 'fsharp-ts-mode))
    (unless (chief/sharplsp-fsharp-tree)
      (let ((key (cons 'fsharp-grammar chief/sharplsp-origin-root)))
        (unless (gethash key chief/sharplsp-warnings)
          (puthash key t chief/sharplsp-warnings)
          (display-warning
           'chief-sharplsp
           "F# syntax-aware hints need a compatible grammar. Run M-x chief/sharplsp-install-fsharp-grammar. Other LSP and editing features remain available."))))
    (chief/sharplsp-signatures-mode 1)
    (setq-local chief/lsp-signature-function #'chief/sharplsp-fsharp-signature-help)
    (add-hook 'eldoc-documentation-functions #'chief/sharplsp-fsharp-eldoc -90 t)
    (add-hook 'lsp-after-open-hook #'chief/sharplsp-fsharp-refresh-signatures nil t)
    (add-hook 'after-change-functions #'chief/sharplsp-fsharp-changed nil t)
    (add-hook 'post-self-insert-hook #'chief/sharplsp-fsharp-auto-signature nil t)))

(advice-add 'lsp-request-async :around #'chief/sharplsp-fsharp-response-a)

(provide 'lang-sharplsp-fsharp)
;;; lang-sharplsp-fsharp.el ends here
