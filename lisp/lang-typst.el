;;; lang-typst.el --- Typst and Tinymist support -*- lexical-binding: t; -*-

(require 'compile)
(require 'core-lsp)
(require 'core-projects)
(require 'subr-x)

(declare-function lsp-format-buffer "lsp-mode" ())
(declare-function lsp-execute-code-action "lsp-mode" ())
(declare-function lsp-rename "lsp-mode" ())

(defgroup chief-typst nil
  "Typst editing and Tinymist LSP integration."
  :group 'chief)

(defcustom chief/typst-output-directory nil
  "Optional output directory for `chief/typst-compile-current-file'.
Nil means write the PDF next to the current Typst source file."
  :type '(choice (const nil) directory)
  :group 'chief-typst)

(defvar chief/typst-watch-process nil
  "Current `typst watch' process started by `chief/typst-watch-current-file'.")

(defvar chief/typst-mode-syntax-table
  (let ((table (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?\" "\"" table)
    ;; C-style // and /* */ comments, as used by Typst.
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    table)
  "Syntax table for `chief/typst-mode'.")

(defconst chief/typst-font-lock-keywords
  '(("^=+\\s-+.*" . font-lock-function-name-face)
    ("#\\(let\\|set\\|show\\|import\\|include\\|if\\|else\\|for\\|in\\|while\\|return\\|break\\|continue\\)\\_>" . font-lock-keyword-face)
    ("#\\([[:alpha:]_][[:alnum:]_-]*\\)" . font-lock-function-call-face)
    ("@\\([[:alnum:]_:.+-]+\\)" . font-lock-constant-face))
  "Conservative fallback highlighting for `chief/typst-mode'.")

(defvar chief/typst-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'chief/typst-compile-current-file)
    (define-key map (kbd "C-c C-w") #'chief/typst-watch-current-file)
    (define-key map (kbd "C-c C-k") #'chief/typst-stop-watch)
    (define-key map (kbd "C-c C-o") #'chief/typst-open-output)
    map)
  "Keymap for `chief/typst-mode'.")

(defun chief/typst-command (&rest args)
  "Return a Typst command line using ARGS."
  (unless (executable-find "typst")
    (user-error "typst is not available on PATH"))
  (cons (executable-find "typst") args))

(defun chief/typst-tinymist-command ()
  "Return the Tinymist command line when available."
  (when-let* ((tinymist (executable-find "tinymist")))
    (list tinymist)))

(defun chief/typst-current-file ()
  "Return the current Typst file path."
  (unless buffer-file-name
    (user-error "This buffer is not visiting a Typst file"))
  (expand-file-name buffer-file-name))

(defun chief/typst-project-root ()
  "Return the current Typst project root."
  (chief/project-preferred-root
   '("typst.toml")
   (chief/project-current-root)
   default-directory))

(defun chief/typst-output-file (&optional source)
  "Return the PDF output path for SOURCE or the current buffer."
  (let* ((source (expand-file-name (or source (chief/typst-current-file))))
         (base (file-name-base source))
         (directory (or chief/typst-output-directory
                        (file-name-directory source))))
    (expand-file-name (concat base ".pdf") directory)))

(defun chief/typst-compile-current-file (&optional open)
  "Compile the current Typst file.
With prefix OPEN, open the expected PDF output after starting compilation."
  (interactive "P")
  (let* ((source (chief/typst-current-file))
         (output (chief/typst-output-file source))
         (default-directory (file-name-directory source))
         (compilation-read-command nil)
         (command (append (chief/typst-command "compile" source)
                          (list output))))
    (when (buffer-modified-p)
      (save-buffer))
    (compilation-start
     (mapconcat #'shell-quote-argument command " ")
     'compilation-mode
     (lambda (_) "*typst compile*"))
    (when open
      (run-at-time 0.5 nil #'chief/typst-open-output output))))

(defun chief/typst-watch-current-file ()
  "Run `typst watch' for the current Typst file."
  (interactive)
  (let* ((source (chief/typst-current-file))
         (output (chief/typst-output-file source))
         (default-directory (file-name-directory source))
         (buffer (get-buffer-create "*typst watch*"))
         (command (append (chief/typst-command "watch" source)
                          (list output))))
    (when (buffer-modified-p)
      (save-buffer))
    (when (process-live-p chief/typst-watch-process)
      (delete-process chief/typst-watch-process))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq-local default-directory default-directory)))
    (setq chief/typst-watch-process
          (make-process
           :name "typst watch"
           :buffer buffer
           :stderr buffer
           :command command
           :noquery t))
    (set-process-query-on-exit-flag chief/typst-watch-process nil)
    (display-buffer buffer)
    (message "Started typst watch for %s" (file-name-nondirectory source))))

(defun chief/typst-stop-watch ()
  "Stop the active `typst watch' process, if any."
  (interactive)
  (if (process-live-p chief/typst-watch-process)
      (progn
        (delete-process chief/typst-watch-process)
        (setq chief/typst-watch-process nil)
        (message "Stopped typst watch"))
    (message "No active typst watch process")))

(defun chief/typst-open-output (&optional output)
  "Open OUTPUT or the expected PDF for the current Typst file."
  (interactive)
  (let ((output (or output (chief/typst-output-file))))
    (unless (file-exists-p output)
      (user-error "Typst output does not exist yet: %s" output))
    (browse-url-of-file output)))

(defun chief/typst-mode-setup ()
  "Configure Typst buffers."
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(?://+\\|/\\*+\\)\\s *")
  (setq-local compile-command
              (mapconcat #'shell-quote-argument
                         (chief/typst-command "compile" (chief/typst-current-file))
                         " "))
  (setq-local chief/lsp-root-function #'chief/typst-project-root)
  (setq-local lsp-enabled-clients '(tinymist))
  ;; This setup intentionally does not install or use `typst-ts-mode'.  Tinymist
  ;; semantic tokens are useful fallback highlighting for the local lightweight
  ;; mode below.
  (setq-local lsp-semantic-tokens-enable t)
  (when (fboundp 'chief/lsp-managed-mode-setup)
    (chief/lsp-managed-mode-setup)))

;;;###autoload
(define-derived-mode chief/typst-mode text-mode "Typst"
  "Lightweight Typst mode backed by Tinymist LSP.
This is deliberately not `typst-ts-mode' and does not use Tree-sitter."
  :syntax-table chief/typst-mode-syntax-table
  (setq-local font-lock-defaults '(chief/typst-font-lock-keywords)))

(add-to-list 'auto-mode-alist '("\\.typ\\'" . chief/typst-mode))
(add-hook 'chief/typst-mode-hook #'chief/typst-mode-setup)

(with-eval-after-load 'core-lsp
  (add-to-list 'chief/lsp-managed-major-modes 'chief/typst-mode)
  (add-to-list 'chief/lsp-manual-install-specs
               '(:modes (chief/typst-mode)
                 :label "Typst Tinymist"
                 :install-command ("brew" "install" "tinymist")
                 :install-predicate (lambda () (executable-find "brew"))
                 :doc-url "https://github.com/Myriad-Dreamin/tinymist"
                 :help "This setup uses the Tinymist language server and does not install typst-ts-mode.")))

(with-eval-after-load 'lsp-mode
  (require 'lsp-typst nil t)
  (add-to-list 'lsp-language-id-configuration '(chief/typst-mode . "typst"))
  (add-to-list 'lsp-language-id-configuration '("\\.typ\\'" . "typst"))
  (when-let* ((command (chief/typst-tinymist-command)))
    (setq lsp-typst-server-command command))
  (setq lsp-typst-export-pdf "never")
  (setq lsp-typst-formatter-mode "typstyle")
  (setq lsp-typst-lint-enabled nil)
  (setq lsp-typst-project-resolution "singleFile"))

(when (fboundp 'chief/local-leader-def)
  (chief/local-leader-def
    :keymaps 'chief/typst-mode-map
    "c" '(:ignore t :which-key "compile")
    "cc" #'chief/typst-compile-current-file
    "cw" #'chief/typst-watch-current-file
    "ck" #'chief/typst-stop-watch
    "co" #'chief/typst-open-output
    "l" '(:ignore t :which-key "lsp")
    "la" #'lsp-execute-code-action
    "lf" #'lsp-format-buffer
    "lr" #'lsp-rename))

(provide 'lang-typst)
;;; lang-typst.el ends here
