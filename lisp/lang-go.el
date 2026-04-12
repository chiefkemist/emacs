;;; lang-go.el --- Go, gopls, and templ support -*- lexical-binding: t; -*-

(require 'compile)
(require 'core-projects)
(require 'project)
(require 'subr-x)

(defcustom chief/go-gopls-directory-filters
  ["-**/.git"
   "-**/.direnv"
   "-**/.idea"
   "-**/.vscode"
   "-**/.turbo"
   "-**/.next"
   "-**/node_modules"
   "-**/dist"
   "-**/tmp"
   "-**/scratch"
   "-**/.venv"
   "-**/.zig-cache"
   "-**/zig-out"]
  "Directory filters passed to gopls for mixed-language monorepos.

These filters avoid indexing large non-Go trees that commonly live beside Go
modules in polyglot repos."
  :type '(vector string)
  :group 'chief)

(defcustom chief/go-gopls-expand-workspace-to-module nil
  "Whether gopls should expand a workspace to the entire enclosing module.

Keeping this nil generally reduces background work in large monorepos."
  :type 'boolean
  :group 'chief)


(defun chief/go-project-root ()
  "Return the current Go project root, if any."
  (chief/project-preferred-root
   '("go.work")
   '("go.mod")
   (chief/project-current-root)
   default-directory))

(defun chief/go-default-directory ()
  "Return the preferred working directory for Go commands."
  (or (chief/go-project-root) default-directory))

(defun chief/go-buffer-directory ()
  "Return the current buffer's package directory."
  (file-name-as-directory
   (expand-file-name
    (or (and buffer-file-name (file-name-directory buffer-file-name))
        (chief/go-project-root)
        default-directory))))

(defun chief/go-compile (command name &optional directory)
  "Run Go COMMAND in DIRECTORY using compilation buffer NAME."
  (let ((default-directory (or directory (chief/go-default-directory)))
        (compilation-read-command nil))
    (compilation-start
     (mapconcat #'shell-quote-argument command " ")
     'compilation-mode
     (lambda (_) name))))

(defun chief/go-build-package ()
  "Build the current Go package."
  (interactive)
  (chief/go-compile '("go" "build" ".") "*go build*" (chief/go-buffer-directory)))

(defun chief/go-build-project ()
  "Build the current Go project."
  (interactive)
  (chief/go-compile '("go" "build" "./...") "*go build project*" (chief/go-default-directory)))

(defun chief/go-run-current-file ()
  "Run the current Go file."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file"))
  (save-buffer)
  (chief/go-compile
   (list "go" "run" (expand-file-name buffer-file-name))
   "*go run*"
   (chief/go-buffer-directory)))

(defun chief/go-test-package ()
  "Run tests for the current Go package."
  (interactive)
  (chief/go-compile '("go" "test") "*go test*" (chief/go-buffer-directory)))

(defun chief/go-test-project ()
  "Run tests for the current Go project."
  (interactive)
  (chief/go-compile '("go" "test" "./...") "*go test project*" (chief/go-default-directory)))

(defun chief/go-generate-project ()
  "Run `go generate ./...' in the current Go project."
  (interactive)
  (chief/go-compile '("go" "generate" "./...") "*go generate*" (chief/go-default-directory)))

(defun chief/go-mod-tidy ()
  "Run `go mod tidy' in the current Go project."
  (interactive)
  (chief/go-compile '("go" "mod" "tidy") "*go mod tidy*" (chief/go-default-directory)))

(defun chief/go-install-project ()
  "Install the current Go project."
  (interactive)
  (chief/go-compile '("go" "install" "./...") "*go install*" (chief/go-default-directory)))

(defun chief/go-templ-command (&rest args)
  "Return a command list that runs templ with ARGS."
  (when-let* ((templ (executable-find "templ")))
    (cons templ args)))

(defun chief/go-templ-lsp-command ()
  "Return the command list used to start the templ language server."
  (chief/go-templ-command "lsp"))

(defun chief/go-templ-generate-file ()
  "Generate Go code for the current templ file."
  (interactive)
  (unless (and buffer-file-name
               (string-match-p "\\.templ\\'" buffer-file-name))
    (user-error "This buffer is not visiting a .templ file"))
  (save-buffer)
  (if-let* ((command (chief/go-templ-command
                      "generate"
                      "-f"
                      (expand-file-name buffer-file-name)
                      "-stdout")))
      (chief/go-compile command "*templ generate file*" (chief/go-default-directory))
    (user-error "templ is not available on PATH")))

(defun chief/go-templ-generate-project ()
  "Generate templ output for the current Go project."
  (interactive)
  (if-let* ((command (chief/go-templ-command "generate" "-path" ".")))
      (chief/go-compile command "*templ generate*" (chief/go-default-directory))
    (user-error "templ is not available on PATH")))

(defun chief/go-templ-format-buffer ()
  "Format the current templ buffer with `templ fmt'."
  (interactive)
  (unless (and buffer-file-name
               (string-match-p "\\.templ\\'" buffer-file-name))
    (user-error "This buffer is not visiting a .templ file"))
  (unless (chief/go-templ-command "fmt")
    (user-error "templ is not available on PATH"))
  (save-buffer)
  (let ((stderr-file (make-temp-file "chief-templ-fmt-"))
        (coding-system-for-read 'utf-8-unix)
        (coding-system-for-write 'utf-8-unix))
    (unwind-protect
        (let ((status (call-process (car (chief/go-templ-command "fmt"))
                                    nil
                                    (list nil stderr-file)
                                    nil
                                    "fmt"
                                    (expand-file-name buffer-file-name))))
          (if (zerop status)
              (revert-buffer t t t)
            (error "%s"
                   (with-temp-buffer
                     (insert-file-contents stderr-file)
                     (string-trim (buffer-string))))))
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

(defun chief/go-mode-setup ()
  "Configure the current Go source buffer."
  (setq-local compile-command "go test ./...")
  (setq-local chief/lsp-root-function #'chief/go-project-root)
  (when (require 'lsp-go nil t)
    (setq-local lsp-enabled-clients '(gopls))
    (setq-local lsp-go-directory-filters chief/go-gopls-directory-filters)
    (setq-local lsp-go-analysis-progress-reporting nil)
    (setq-local lsp-go-verbose-output nil)
    (setq-local lsp-go-template-extensions ["templ"])
    (when-let* ((command (and (fboundp 'chief/lsp-gopls-command)
                              (chief/lsp-gopls-command))))
      (setq-local lsp-go-gopls-server-path (car command))
      (setq-local lsp-go-gopls-server-args (cdr command))))
  (when (fboundp 'chief/lsp-managed-mode-setup)
    (chief/lsp-managed-mode-setup)))

(defun chief/go-module-mode-setup ()
  "Configure the current Go module or workspace buffer."
  (setq-local compile-command "go mod tidy")
  (setq-local chief/lsp-root-function #'chief/go-project-root)
  (when (require 'lsp-go nil t)
    (setq-local lsp-enabled-clients '(gopls))
    (setq-local lsp-go-directory-filters chief/go-gopls-directory-filters)
    (setq-local lsp-go-analysis-progress-reporting nil)
    (setq-local lsp-go-verbose-output nil)
    (when-let* ((command (and (fboundp 'chief/lsp-gopls-command)
                              (chief/lsp-gopls-command))))
      (setq-local lsp-go-gopls-server-path (car command))
      (setq-local lsp-go-gopls-server-args (cdr command))))
  (when (fboundp 'chief/lsp-managed-mode-setup)
    (chief/lsp-managed-mode-setup)))

(defun chief/templ-mode-setup ()
  "Configure the current templ buffer."
  (setq-local compile-command "templ generate -path .")
  (setq-local chief/lsp-root-function #'chief/go-project-root)
  (when (require 'lsp-mode nil t)
    (setq-local lsp-enabled-clients '(templ-ls)))
  (when (fboundp 'chief/lsp-managed-mode-setup)
    (chief/lsp-managed-mode-setup)))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '("\\.templ\\'" . "templ"))
  (lsp-register-custom-settings
   '(("gopls.expandWorkspaceToModule" chief/go-gopls-expand-workspace-to-module t)))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection #'chief/go-templ-lsp-command)
    :activation-fn (lsp-activate-on "templ")
    :priority 2
    :server-id 'templ-ls)))

(chief/safe-use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :hook (go-mode . chief/go-mode-setup))

(with-eval-after-load 'go-ts-mode
  ;; `templ-ts-mode' currently depends on private `go-ts-mode' helpers that were
  ;; removed in newer Emacs releases. Provide tiny compatibility shims so the
  ;; mode can still initialize on Emacs 31.
  (unless (fboundp 'go-ts-mode--iota-query-supported-p)
    (defun go-ts-mode--iota-query-supported-p () t))
  (unless (fboundp 'go-ts-mode--method-elem-supported-p)
    (defun go-ts-mode--method-elem-supported-p () t)))

(with-eval-after-load 'js
  ;; Newer Emacs versions expose these as functions instead of vars.
  (unless (boundp 'js--treesit-indent-rules)
    (defvar js--treesit-indent-rules (js--treesit-indent-rules)))
  (unless (boundp 'js--treesit-font-lock-settings)
    (defvar js--treesit-font-lock-settings (js--treesit-font-lock-settings))))

(defun chief/templ-ts-mode--stub-eglot-require (orig feature &rest args)
  "Let `templ-ts-mode' load without pulling in Eglot.
The upstream package only uses Eglot to append to `eglot-server-programs', while
this config uses `lsp-mode' for templ buffers."
  (if (eq feature 'eglot)
      (progn
        (defvar eglot-server-programs nil)
        'eglot)
    (apply orig feature args)))

(defun chief/templ-ts--language-at-point (point)
  "Return the embedded language at POINT inside a templ buffer."
  (let ((templ-parser (treesit-parser-create 'templ))
        (js-parser (treesit-parser-create 'javascript))
        (css-parser (treesit-parser-create 'css)))
    (cond
     ((treesit-parser-range-on css-parser point) 'css)
     ((treesit-parser-range-on js-parser point) 'javascript)
     ((treesit-parser-range-on templ-parser point) 'templ)
     (t 'templ))))

(defun chief/templ-ts--setup ()
  "Compatibility wrapper for `templ-ts-mode' on newer Emacs releases."
  (unless (treesit-available-p)
    (error "Tree-sitter is not available"))
  (unless (and (treesit-language-available-p 'templ)
               (treesit-language-available-p 'javascript)
               (treesit-language-available-p 'jsdoc)
               (treesit-language-available-p 'css))
    (error "templ-ts-mode needs the templ, javascript, jsdoc, and css tree-sitter grammars"))
  (setq-local treesit-language-source-alist
              `((templ . (,templ-ts-mode-grammar))))
  (treesit-parser-create 'templ)
  (treesit-parser-create 'javascript)
  (treesit-parser-create 'css)
  (setq-local comment-start "// "
              comment-end ""
              comment-start-skip (rx "//" (* (syntax whitespace))))
  (setq-local treesit-language-at-point-function #'chief/templ-ts--language-at-point)
  (setq-local treesit-range-settings
              (treesit-range-rules
               :embed 'javascript
               :host 'templ
               '((script_block_text) @javascript)
               :embed 'css
               :host 'templ
               '((style_element_text) @css)))
  (setq-local indent-tabs-mode t
              treesit-simple-indent-rules templ-ts--indent-rules)
  (setq-local electric-indent-chars
              (append "{}()<>" electric-indent-chars))
  (setq-local treesit-font-lock-feature-list templ-ts--font-lock-feature-list)
  (setq-local treesit-font-lock-settings
              (append js--treesit-font-lock-settings
                      css--treesit-settings
                      (apply #'treesit-font-lock-rules
                             (append templ-ts--templ-font-lock-rules
                                     templ-ts--go-font-lock-rules))))
  (treesit-major-mode-setup))

(defun chief/load-templ-ts-mode ()
  "Load and patch `templ-ts-mode' from the Go/Templ module only."
  (condition-case err
      (progn
        (when (fboundp 'straight-use-package)
          (straight-use-package 'templ-ts-mode))
        (advice-add 'require :around #'chief/templ-ts-mode--stub-eglot-require)
        (unwind-protect
            (require 'templ-ts-mode)
          (advice-remove 'require #'chief/templ-ts-mode--stub-eglot-require))
        (advice-add 'templ-ts--setup :override #'chief/templ-ts--setup)
        (add-to-list 'auto-mode-alist '("\\.templ\\'" . templ-ts-mode))
        (add-hook 'templ-ts-mode-hook #'chief/templ-mode-setup))
    (error
     (display-warning 'chief
                      (format "Failed to initialize package templ-ts-mode: %s"
                              (error-message-string err))
                      :warning))))

(chief/load-templ-ts-mode)

(when (fboundp 'go-ts-mode)
  (add-hook 'go-ts-mode-hook #'chief/go-mode-setup))

(when (fboundp 'go-mod-ts-mode)
  (add-hook 'go-mod-ts-mode-hook #'chief/go-module-mode-setup))

(when (fboundp 'go-work-ts-mode)
  (add-hook 'go-work-ts-mode-hook #'chief/go-module-mode-setup))

(with-eval-after-load 'go-mode
  (chief/local-leader-def
    :keymaps 'go-mode-map
    "c" '(:ignore t :which-key "compile")
    "cb" #'chief/go-build-package
    "cB" #'chief/go-build-project
    "ct" #'chief/go-test-package
    "cT" #'chief/go-test-project
    "cr" #'chief/go-run-current-file
    "cg" #'chief/go-generate-project
    "cm" #'chief/go-mod-tidy
    "ci" #'chief/go-install-project))

(with-eval-after-load 'go-ts-mode
  (chief/local-leader-def
    :keymaps '(go-ts-mode-map go-mod-ts-mode-map go-work-ts-mode-map)
    "c" '(:ignore t :which-key "compile")
    "cb" #'chief/go-build-package
    "cB" #'chief/go-build-project
    "ct" #'chief/go-test-package
    "cT" #'chief/go-test-project
    "cr" #'chief/go-run-current-file
    "cg" #'chief/go-generate-project
    "cm" #'chief/go-mod-tidy
    "ci" #'chief/go-install-project))

(with-eval-after-load 'templ-ts-mode
  (chief/local-leader-def
    :keymaps 'templ-ts-mode-map
    "c" '(:ignore t :which-key "templ")
    "cf" #'chief/go-templ-format-buffer
    "cg" #'chief/go-templ-generate-file
    "cG" #'chief/go-templ-generate-project))

(provide 'lang-go)
;;; lang-go.el ends here
