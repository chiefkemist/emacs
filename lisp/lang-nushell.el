;;; lang-nushell.el --- Nushell support -*- lexical-binding: t; -*-

(require 'compile)
(require 'comint)
(require 'core-projects)
(require 'project)
(require 'subr-x)
(require 'treesit nil t)

(defgroup chief-nushell nil
  "Nushell support in the Chiefkemist Emacs configuration."
  :group 'chief)

(defcustom chief/nushell-repl-arguments '("-n")
  "Arguments used when starting the Nushell REPL.
The default keeps startup fast and avoids loading interactive Nu config into
the Emacs comint buffer."
  :type '(repeat string)
  :group 'chief-nushell)

(defconst chief/nushell-keywords
  '("alias" "def" "def-env" "export" "extern" "for" "if" "let" "loop"
    "match" "module" "mut" "source" "source-env" "try" "use" "while")
  "Nushell keywords highlighted by the fallback font-lock rules.")

(defconst chief/nushell-constants
  '("true" "false" "null")
  "Nushell constants highlighted by the fallback font-lock rules.")

(defvar chief/nushell-mode-syntax-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?' "\"" table)
    table)
  "Syntax table used by `nushell-mode'.")

(defvar chief/nushell-font-lock-keywords
  `((,(regexp-opt chief/nushell-keywords 'symbols) . font-lock-keyword-face)
    (,(regexp-opt chief/nushell-constants 'symbols) . font-lock-constant-face)
    ("\\_<\\$[[:alnum:]_.-]+\\_>" . font-lock-variable-name-face)
    ("\\_<[[:digit:]]+\\(?:\\.[[:digit:]]+\\)?\\_>" . font-lock-number-face))
  "Fallback font-lock keywords for `nushell-mode'.")

(defvar chief/nushell-treesit-indent-rules
  `((nu
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((parent-is "block") parent-bol 2)
     ((parent-is "string") parent-bol 2)
     ((parent-is "array") parent-bol 2)
     ((parent-is "val_list") parent-bol 2)
     ((parent-is "expr_parenthesized") parent-bol 2)
     ((parent-is "parameter_bracks") parent-bol 2)
     (no-node parent-bol 0)))
  "Simple tree-sitter indentation rules for `nushell-mode'.")

(define-derived-mode nushell-mode prog-mode "Nu"
  "Major mode for editing Nushell files."
  :syntax-table chief/nushell-mode-syntax-table
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local indent-line-function #'indent-relative)
  (setq-local font-lock-defaults '(chief/nushell-font-lock-keywords))
  (when (and (featurep 'treesit)
             (fboundp 'treesit-ready-p)
             (treesit-ready-p 'nu t))
    (treesit-parser-create 'nu)
    (setq-local treesit-simple-indent-rules chief/nushell-treesit-indent-rules)
    (setq-local indent-line-function #'treesit-indent)))

(defun chief/nushell-project-root ()
  "Return the current Nushell project root, if any."
  (chief/project-preferred-root
   '(".git")
   (chief/project-current-root)
   default-directory))

(defun chief/nushell-repl-command ()
  "Return the command list used to start the Nushell REPL."
  (let ((nu (or (executable-find "nu")
                (user-error "nu is not available on PATH"))))
    (cons nu chief/nushell-repl-arguments)))

(defun chief/nushell-repl-buffer-name ()
  "Return the current project's Nushell REPL buffer name."
  (format "*nushell[%s]*"
          (file-name-nondirectory
           (directory-file-name (chief/nushell-project-root)))))

(defun chief/nushell-strip-terminal-control-sequences (output)
  "Strip OSC title-setting control sequences from Nu OUTPUT."
  (replace-regexp-in-string
   "\\(?:\x1b\\][^\x07\x1b]*\\(?:\x07\\|\x1b\\\\\\)\\)"
   ""
   output))

(defun chief/nushell-ensure-repl-buffer ()
  "Return a live Nushell REPL buffer for the current project."
  (let* ((root (chief/nushell-project-root))
         (buffer (get-buffer-create (chief/nushell-repl-buffer-name)))
         (command (chief/nushell-repl-command)))
    (unless (comint-check-proc buffer)
      (let ((default-directory root)
            (process-environment (copy-sequence process-environment)))
        (setenv "TERM" "dumb")
        (setenv "NO_COLOR" "1")
        (apply #'make-comint-in-buffer
               "chief-nushell"
               buffer
               (car command)
               nil
               (cdr command)))
      (with-current-buffer buffer
        (setq-local default-directory root)
        (setq-local comint-process-echoes nil)
        (setq-local comint-prompt-regexp "^[^>\n]*> ")
        (setq-local paragraph-start comint-prompt-regexp)
        (setq-local comint-use-prompt-regexp t)
        (add-hook 'comint-preoutput-filter-functions
                  #'chief/nushell-strip-terminal-control-sequences
                  nil
                  t)))
    buffer))

(defun chief/nushell-start-repl ()
  "Start or switch to the current project's Nushell REPL."
  (interactive)
  (pop-to-buffer (chief/nushell-ensure-repl-buffer)))

(defun chief/nushell-restart-repl ()
  "Restart the current project's Nushell REPL."
  (interactive)
  (when-let* ((buffer (get-buffer (chief/nushell-repl-buffer-name))))
    (when-let* ((process (get-buffer-process buffer)))
      (delete-process process))
    (kill-buffer buffer))
  (chief/nushell-start-repl))

(defun chief/nushell-send-string (string)
  "Send STRING to the current project's Nushell REPL."
  (let* ((buffer (chief/nushell-ensure-repl-buffer))
         (process (get-buffer-process buffer)))
    (comint-send-string process string)
    (unless (string-suffix-p "\n" string)
      (comint-send-string process "\n"))
    (display-buffer buffer)))

(defun chief/nushell-send-line ()
  "Send the current line to the Nushell REPL."
  (interactive)
  (chief/nushell-send-string
   (buffer-substring-no-properties
    (line-beginning-position)
    (line-end-position))))

(defun chief/nushell-send-region (start end)
  "Send the region from START to END to the Nushell REPL."
  (interactive "r")
  (chief/nushell-send-string
   (buffer-substring-no-properties start end)))

(defun chief/nushell-send-buffer ()
  "Send the current buffer to the Nushell REPL."
  (interactive)
  (if buffer-file-name
      (chief/nushell-load-file)
    (chief/nushell-send-region (point-min) (point-max))))

(defun chief/nushell-send-defun ()
  "Send the current top-level form to the Nushell REPL."
  (interactive)
  (save-excursion
    (mark-defun)
    (chief/nushell-send-region (region-beginning) (region-end))
    (deactivate-mark)))

(defun chief/nushell-load-file ()
  "Load the current Nushell file in the Nushell REPL."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file"))
  (save-buffer)
  (chief/nushell-send-string
   (format "source %S" (expand-file-name buffer-file-name))))

(defun chief/nushell-compile (command name &optional directory)
  "Run Nushell COMMAND in DIRECTORY using compilation buffer NAME."
  (let ((default-directory (or directory (chief/nushell-project-root)))
        (compilation-read-command nil))
    (compilation-start
     (mapconcat #'shell-quote-argument command " ")
     'compilation-mode
     (lambda (_) name))))

(defun chief/nushell-check-buffer ()
  "Parse-check the current Nushell buffer with `nu-check --debug'."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file"))
  (unless (executable-find "nu")
    (user-error "nu is not available on PATH"))
  (save-buffer)
  (chief/nushell-compile
   (list "nu" "-c" (format "nu-check --debug %s"
                           (shell-quote-argument (expand-file-name buffer-file-name))))
   "*nu check*"))

(defun chief/nushell-run-buffer ()
  "Run the current Nushell file."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file"))
  (unless (executable-find "nu")
    (user-error "nu is not available on PATH"))
  (save-buffer)
  (chief/nushell-compile
   (list "nu" (expand-file-name buffer-file-name))
   "*nu run*"))

(defun chief/nushell-mode-setup ()
  "Configure Nushell buffers."
  (setq-local compile-command
              (if buffer-file-name
                  (format "nu -c %S"
                          (format "nu-check --debug %s"
                                  (shell-quote-argument (expand-file-name buffer-file-name))))
                "nu"))
  (setq-local chief/lsp-root-function #'chief/nushell-project-root)
  (chief/repl-configure
   :start #'chief/nushell-start-repl
   :restart #'chief/nushell-restart-repl
   :send-line #'chief/nushell-send-line
   :send-region #'chief/nushell-send-region
   :send-buffer #'chief/nushell-send-buffer
   :send-defun #'chief/nushell-send-defun
   :load-file #'chief/nushell-load-file)
  (when (require 'lsp-mode nil t)
    (setq-local lsp-enabled-clients '(nushell-ls)))
  (when (fboundp 'chief/lsp-managed-mode-setup)
    (chief/lsp-managed-mode-setup)))

(add-to-list 'auto-mode-alist '("\\.nu\\'" . nushell-mode))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '("\\.nu\\'" . "nushell"))
  (add-to-list 'lsp-language-id-configuration '(nushell-mode . "nushell")))

(add-hook 'nushell-mode-hook #'chief/nushell-mode-setup)

(chief/repl-setup-standard-local-leader 'nushell-mode-map)
(chief/local-leader-def
  :keymaps 'nushell-mode-map
  "c" '(:ignore t :which-key "check/run")
  "cc" #'chief/nushell-check-buffer
  "cr" #'chief/nushell-run-buffer)

(provide 'lang-nushell)
;;; lang-nushell.el ends here
