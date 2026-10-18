;;; ob-mojo.el --- Org Babel support for project-local Mojo -*- lexical-binding: t; -*-

;;; Commentary:

;; Execute Mojo source blocks with the SDK selected by the nearest Pixi
;; workspace.  Non-session blocks are complete programs: blocks without
;; `main' are wrapped in `def main() raises', while complete programs are
;; preserved.  Session blocks execute snippets in a persistent Mojo REPL.

;;; Code:

(require 'comint)
(require 'ob)
(require 'ob-comint)
(require 'subr-x)
(require 'lang-mojo)

(declare-function chief/org-babel-execution-directory "core-org-babel" (params))
(declare-function chief/org-babel-format-result "core-org-babel" (result params))
(declare-function chief/org-babel-run-command "core-org-babel" (command params))
(declare-function chief/org-babel-write-temp-file
                  "core-org-babel" (body extension params prefix))

(defvar org-babel-default-header-args:mojo
  '((:results . "output replace")
    (:exports . "both")
    (:wrap . "auto"))
  "Default header arguments for Mojo source blocks.")

(defun chief/org-babel-mojo-false-p (value)
  "Return non-nil when Babel header VALUE means false."
  (member (downcase (format "%s" value)) '("no" "nil" "false" "0" "none")))

(defun chief/org-babel-mojo-arguments (key params)
  "Return shell-style arguments stored under KEY in PARAMS."
  (when-let* ((value (cdr (assq key params)))
              ((not (string-empty-p (format "%s" value)))))
    (split-string-and-unquote (format "%s" value))))

(defun chief/org-babel-mojo-literal (value)
  "Translate Babel VALUE to a Mojo source literal."
  (cond
   ((eq value t) "True")
   ((null value) "None")
   ((numberp value) (number-to-string value))
   ((stringp value) (json-serialize value))
   ((vectorp value)
    (chief/org-babel-mojo-literal (append value nil)))
   ((listp value)
    (format "[%s]"
            (mapconcat #'chief/org-babel-mojo-literal value ", ")))
   (t (json-serialize (format "%s" value)))))

(defun org-babel-variable-assignments:mojo (params)
  "Return Mojo variable declarations derived from Babel PARAMS."
  (mapcar
   (lambda (pair)
     (let ((name (symbol-name (car pair))))
       (unless (string-match-p "\\`[[:alpha:]_][[:alnum:]_]*\\'" name)
         (user-error "Invalid Mojo Babel variable name: %s" name))
       (format "var %s = %s" name
               (chief/org-babel-mojo-literal (cdr pair)))))
   (org-babel--get-vars params)))

(defun chief/org-babel-mojo-main-p (body)
  "Return non-nil when BODY defines a Mojo `main' function."
  (string-match-p
   "^[ \t]*\\(?:async[ \t]+\\)?\\(?:def\\|fn\\)[ \t]+main[ \t]*[([]"
   body))

(defun chief/org-babel-mojo-inject-variables (body assignments)
  "Insert ASSIGNMENTS at the start of BODY's `main' function."
  (if (null assignments)
      body
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (unless (re-search-forward
               "^[ \t]*\\(?:async[ \t]+\\)?\\(?:def\\|fn\\)[ \t]+main[ \t]*[([][^\n]*:\\s-*$"
               nil t)
        (user-error "Cannot place Babel variables in this Mojo `main' definition"))
      (let* ((declaration-indent
              (save-excursion
                (goto-char (match-beginning 0))
                (current-indentation)))
             (indent (+ declaration-indent chief/mojo-indent-offset)))
        (end-of-line)
        (insert "\n"
                (mapconcat
                 (lambda (assignment)
                   (concat (make-string indent ?\s) assignment))
                 assignments "\n")))
      (buffer-string))))

(defun org-babel-expand-body:mojo (body params)
  "Expand Mojo BODY according to Babel PARAMS.
`:wrap auto' wraps blocks without `main'; `:wrap yes' always wraps; and
`:wrap no' requires a complete program.  Babel variables are declared inside
`main' so Mojo's prohibition on top-level executable statements is respected."
  (let* ((assignments (org-babel-variable-assignments:mojo params))
         (wrap-value (downcase (format "%s" (or (cdr (assq :wrap params)) "auto"))))
         (has-main (chief/org-babel-mojo-main-p body))
         (wrap (or (member wrap-value '("yes" "t" "true" "main"))
                   (and (string= wrap-value "auto") (not has-main))))
         (prologue (cdr (assq :prologue params)))
         (epilogue (cdr (assq :epilogue params))))
    (cond
     (wrap
      (string-join
       (delq
        nil
        (list
         prologue
         (concat "def main() raises:\n"
                 (mapconcat
                  (lambda (line)
                    (concat (make-string chief/mojo-indent-offset ?\s) line))
                  (append assignments (split-string body "\n" nil))
                  "\n"))
         epilogue))
       "\n"))
     ((and (not has-main) (chief/org-babel-mojo-false-p wrap-value))
      (user-error "A Mojo Babel block with `:wrap no' must define `main'"))
     (t
      (chief/org-babel-mojo-inject-variables
       (string-join (delq nil (list prologue body epilogue)) "\n")
       assignments)))))

(defun chief/org-babel-mojo-command (file params)
  "Return the Pixi-backed Mojo command for FILE and Babel PARAMS."
  (let* ((directory (chief/org-babel-execution-directory params))
         (root (or (chief/mojo-pixi-root directory)
                   (user-error
                    "Mojo Babel requires a Pixi workspace; set `:dir' to one")))
         (environment (cdr (assq :environment params)))
         (pixi (chief/mojo-pixi-executable))
         (compiler-args
          (append (chief/org-babel-mojo-arguments :compiler-args params)
                  (chief/org-babel-mojo-arguments :mojo-args params)))
         (runtime-args (chief/org-babel-mojo-arguments :cmdline params))
         (include-args
          (cl-mapcan (lambda (path) (list "-I" path))
                     (chief/mojo-include-directories root))))
    (append
     (list pixi "run" "--manifest-path" root)
     (if environment
         (list "--environment" (format "%s" environment))
       (chief/mojo--environment-arguments root))
     (list "--no-progress" "--color" "never" "--frozen" "--executable"
           "mojo" "run")
     compiler-args include-args (list file) runtime-args)))

(defun chief/org-babel-mojo-session-p (session)
  "Return non-nil when Babel SESSION requests a persistent Mojo REPL."
  (and session
       (not (member (downcase (format "%s" session))
                    '("none" "nil" "no")))))

(defun chief/org-babel-mojo-session-buffer (session params)
  "Return a live project Mojo REPL for Babel SESSION and PARAMS."
  (let* ((directory (chief/org-babel-execution-directory params))
         (default-directory directory)
         (root (or (chief/mojo-pixi-root directory)
                   (user-error
                    "Mojo Babel requires a Pixi workspace; set `:dir' to one")))
         (requested-environment (cdr (assq :environment params)))
         (environment
          (if requested-environment
              (format "%s" requested-environment)
            (or (chief/mojo-resolved-environment root) "default")))
         (session-name (if (member (downcase (format "%s" session))
                                   '("yes" "t" "default"))
                           "default"
                         (format "%s" session)))
         (buffer-name
          (format "*mojo-babel[%s:%s:%s]*"
                  (file-name-nondirectory (directory-file-name root))
                  environment session-name))
         (buffer (get-buffer-create buffer-name))
         (command
          (append
           (list (chief/mojo-pixi-executable) "run" "--manifest-path" root
                 "--environment" environment "--no-progress" "--color"
                 "never" "--frozen" "--executable" "mojo" "repl"))))
    (unless (comint-check-proc buffer)
      (let ((default-directory root)
            (process-environment (copy-sequence process-environment)))
        (setenv "TERM" "dumb")
        (setenv "NO_COLOR" "1")
        (apply #'make-comint-in-buffer
               "chief-mojo-babel" buffer (car command) nil (cdr command)))
      (with-current-buffer buffer
        (chief/mojo-repl-mode)
        (setq-local default-directory root))
      (with-current-buffer buffer
        (let ((deadline (+ (float-time) 30)))
          (while (and (< (float-time) deadline)
                      (not (save-excursion
                             (goto-char (point-max))
                             (forward-line 0)
                             (looking-at-p comint-prompt-regexp))))
            (accept-process-output (get-buffer-process buffer) 0.05))
          (unless (save-excursion
                    (goto-char (point-max))
                    (forward-line 0)
                    (looking-at-p comint-prompt-regexp))
            (user-error "Mojo Babel REPL did not become ready")))))
    buffer))

(defun chief/org-babel-mojo-clean-session-output (raw marker)
  "Remove Mojo REPL prompts and end MARKER from RAW session output."
  (let ((without-marker
         (replace-regexp-in-string (regexp-quote marker) "" raw t t)))
    (string-trim
     (mapconcat
      (lambda (line)
        (replace-regexp-in-string
         "\\`\\(?:[0-9]+[>.] \\)*" "" line))
      (split-string without-marker "[\r\n]+" nil)
      "\n"))))

(defun chief/org-babel-mojo-execute-session (body params session)
  "Execute Mojo BODY with PARAMS in persistent Babel SESSION."
  (let* ((assignments (org-babel-variable-assignments:mojo params))
         (full-body (org-babel-expand-body:generic body params assignments))
         (buffer (chief/org-babel-mojo-session-buffer session params))
         (marker (format "__ORG_BABEL_MOJO_%s__"
                         (substring (secure-hash 'sha1
                                                 (format "%s%s%s"
                                                         (float-time) body
                                                         (random)))
                                    0 16)))
         (raw
          (org-babel-comint-with-output
              (buffer marker nil nil 'disable-prompt-filtering)
            (let ((process (get-buffer-process buffer)))
              (comint-send-string process (string-trim-right full-body))
              (comint-send-string process "\n\n")
              (comint-send-string process
                                  (format "print(\"%s\")\n\n" marker))))))
    (chief/org-babel-format-result
     (chief/org-babel-mojo-clean-session-output raw marker)
     params)))

(defun org-babel-execute:mojo (body params)
  "Execute a Mojo source block BODY according to Babel PARAMS."
  (let ((session (cdr (assq :session params))))
    (if (chief/org-babel-mojo-session-p session)
        (chief/org-babel-mojo-execute-session body params session)
      (let* ((expanded (org-babel-expand-body:mojo body params))
             (file (chief/org-babel-write-temp-file
                    expanded ".mojo" params "chief-org-mojo-")))
        (unwind-protect
            (chief/org-babel-format-result
             (chief/org-babel-run-command
              (chief/org-babel-mojo-command file params) params)
             params)
          (when (file-exists-p file)
            (delete-file file)))))))

(add-to-list 'org-babel-tangle-lang-exts '("mojo" . "mojo"))

(provide 'ob-mojo)
;;; ob-mojo.el ends here
