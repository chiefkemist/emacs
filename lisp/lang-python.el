;;; lang-python.el --- Python, uv, Ruff, and ty -*- lexical-binding: t; -*-

(require 'core-projects)
(require 'json)
(require 'project)
(require 'python)
(require 'subr-x)

(defcustom chief/python-format-on-save t
  "When non-nil, format Python buffers with Ruff before saving."
  :type 'boolean
  :group 'chief)

(defcustom chief/python-dev-tools '("ruff" "ty")
  "Development tools to add to a uv-managed Python project."
  :type '(repeat string)
  :group 'chief)

(defun chief/python-project-root ()
  "Return the current Python project root, if any."
  (chief/project-preferred-root
   '("pyproject.toml" "uv.lock" "setup.py" "setup.cfg" "requirements.txt")
   '(".venv")
   (chief/project-current-root)
   default-directory))

(defun chief/python-uv-project-p (&optional root)
  "Return non-nil when ROOT looks like a uv-managed Python project."
  (let ((root (or root (chief/python-project-root))))
    (and root
         (or (file-exists-p (expand-file-name "pyproject.toml" root))
             (file-exists-p (expand-file-name "uv.lock" root))))))

(defun chief/python-venv-root (&optional root)
  "Return the path to the project-local .venv for ROOT, if it exists."
  (let* ((root (or root (chief/python-project-root)))
         (venv (and root (expand-file-name ".venv" root))))
    (when (and venv (file-directory-p venv))
      venv)))

(defun chief/python-venv-bin-directory (&optional root)
  "Return the executable directory for the project-local virtualenv at ROOT."
  (when-let* ((venv (chief/python-venv-root root)))
    (expand-file-name
     (if (eq system-type 'windows-nt) "Scripts" "bin")
     venv)))

(defun chief/python-project-executable (name &optional root)
  "Return executable NAME from the project-local .venv under ROOT."
  (when-let* ((bin-dir (chief/python-venv-bin-directory root)))
    (let ((path (expand-file-name name bin-dir)))
      (when (file-executable-p path)
        path))))

(defun chief/python-global-executable (name)
  "Return a global executable for NAME."
  (or (executable-find name)
      (and (string= name "python")
           (executable-find "python3"))))

(defun chief/python-direct-command (tool &rest args)
  "Return a command list for TOOL and ARGS using .venv or PATH only."
  (when-let* ((program (or (chief/python-project-executable tool)
                          (chief/python-global-executable tool))))
    (cons program args)))

(defun chief/python-command (tool &rest args)
  "Return a command list for TOOL and ARGS.
In uv projects, development tools are expected inside the project-local .venv."
  (let ((root (chief/python-project-root)))
    (if (and root (chief/python-uv-project-p root))
        (when-let* ((program (chief/python-project-executable tool root)))
          (cons program args))
      (apply #'chief/python-direct-command tool args))))

(defun chief/python-repl-command ()
  "Return the command list to start the current project's Python REPL."
  (let ((root (chief/python-project-root)))
    (or (when-let* ((python (chief/python-project-executable "python" root)))
          (list python "-i"))
        (when (not (chief/python-uv-project-p root))
          (when-let* ((python (chief/python-global-executable "python")))
            (list python "-i"))))))

(defun chief/python-ty-server-command ()
  "Return the command list to start ty as an LSP server."
  (chief/python-command "ty" "server"))

(defun chief/python-ruff-server-command ()
  "Return the command list to start Ruff as an LSP server."
  (chief/python-command "ruff" "server"))

(defun chief/python-lsp-available-p ()
  "Return non-nil when ty is available as the Python LSP server."
  (chief/python-ty-server-command))

(defun chief/python-library-directories (&optional _workspace)
  "Return Python library directories for the current project environment."
  (let* ((root (chief/python-project-root))
         (venv (chief/python-venv-root root))
         (lib-root (and venv
                        (expand-file-name
                         (if (eq system-type 'windows-nt) "Lib" "lib")
                         venv))))
    (delq
     nil
     (append
      (when (and lib-root (file-directory-p lib-root))
        (if (eq system-type 'windows-nt)
            (let ((site-packages (expand-file-name "site-packages" lib-root)))
              (when (file-directory-p site-packages)
                (list site-packages)))
          (mapcar
           #'file-name-as-directory
           (seq-filter
            #'file-directory-p
            (directory-files lib-root t "^python[0-9.]+$" t)))))
      (when-let* ((lib-root (and lib-root (file-directory-p lib-root) lib-root)))
        (mapcar
         #'file-name-as-directory
         (seq-filter
          #'file-directory-p
          (directory-files-recursively lib-root "^site-packages$" t))))))))

(defun chief/python--prepend-to-exec-path (directory)
  "Prepend DIRECTORY to the buffer-local execution environment."
  (setq-local exec-path (cons directory (delete directory exec-path)))
  (setq-local process-environment (copy-sequence process-environment))
  (setenv "PATH" (string-join (cons directory exec-path) path-separator)))

(defun chief/python-activate-project-environment ()
  "Activate the project-local .venv for the current buffer when present."
  (when-let* ((venv (chief/python-venv-root)))
    (setq-local process-environment (copy-sequence process-environment))
    (setenv "VIRTUAL_ENV" venv)
    (setenv "PYTHONHOME" nil)
    (setq-local python-shell-virtualenv-root venv)
    (chief/python--prepend-to-exec-path (chief/python-venv-bin-directory))))

(defun chief/python-configure-shell ()
  "Configure the Python shell for the current project."
  (when-let* ((command (chief/python-repl-command)))
    (setq-local python-shell-interpreter (car command))
    (setq-local python-shell-interpreter-args (string-join (cdr command) " "))
    (setq-local python-shell-dedicated 'project)))

(defun chief/python-shell-send-line ()
  "Send the current line to the active Python REPL."
  (interactive)
  (python-shell-send-region (line-beginning-position) (line-end-position)))

(defun chief/python-ensure-project-shell-ready ()
  "Ensure the current project has a real Python interpreter for REPL use."
  (let ((root (chief/python-project-root)))
    (cond
     ((chief/python-repl-command) t)
     ((and root
           (chief/python-uv-project-p root)
           (not noninteractive))
      (pcase
          (car
           (read-multiple-choice
            "This uv project has no local .venv yet. Choose an action: "
            '((?s "sync" "Run `uv sync` to build the project environment.")
              (?v "venv" "Run `uv venv` to create a local virtualenv.")
              (?k "skip" "Skip REPL startup for now."))))
        (?s (chief/python-uv-sync))
        (?v (chief/python-uv-venv)))
      nil)
     (t nil))))

(defun chief/python-run-project-repl ()
  "Start or switch to the current project's Python REPL."
  (interactive)
  (if (chief/python-ensure-project-shell-ready)
      (let ((default-directory (or (chief/python-project-root) default-directory)))
        (chief/python-activate-project-environment)
        (chief/python-configure-shell)
        (run-python nil 'project t))
    (user-error "Python REPL not started because this project has no usable interpreter yet")))

(defun chief/python-restart-project-repl ()
  "Restart the current project's Python REPL."
  (interactive)
  (if (chief/python-ensure-project-shell-ready)
      (let ((default-directory (or (chief/python-project-root) default-directory)))
        (chief/python-activate-project-environment)
        (chief/python-configure-shell)
        (python-shell-restart t))
    (user-error "Python REPL not restarted because this project has no usable interpreter yet")))

(defun chief/python--compile (command name)
  "Run COMMAND from the project root using compilation buffer NAME."
  (let ((default-directory (or (chief/python-project-root) default-directory))
        (compilation-read-command nil))
    (compilation-start
     (mapconcat #'shell-quote-argument command " ")
     'compilation-mode
     (lambda (_) name))))

(defun chief/python-uv-venv ()
  "Create or refresh the project-local .venv using uv."
  (interactive)
  (unless (executable-find "uv")
    (user-error "uv is not installed"))
  (chief/python--compile '("uv" "venv") "*uv venv*"))

(defun chief/python-uv-sync ()
  "Sync the current uv project."
  (interactive)
  (unless (executable-find "uv")
    (user-error "uv is not installed"))
  (chief/python--compile '("uv" "sync") "*uv sync*"))

(defun chief/python-uv-add-dev-tooling ()
  "Add Ruff and ty as uv development dependencies for the current project."
  (interactive)
  (unless (executable-find "uv")
    (user-error "uv is not installed"))
  (chief/python--compile
   (append '("uv" "add" "--dev") chief/python-dev-tools)
   "*uv add dev tools*"))

(defun chief/python-install-project-tooling ()
  "Install ty and Ruff into the current uv project."
  (interactive)
  (unless (chief/python-project-root)
    (user-error "This buffer is not in a Python project"))
  (unless (chief/python-uv-project-p)
    (user-error "Python LSP installation is configured for uv projects only"))
  (chief/python-uv-add-dev-tooling))

(defun chief/python-project-tooling-installable-p ()
  "Return non-nil when the current buffer can install Python LSP tooling."
  (and (executable-find "uv")
       (chief/python-uv-project-p)))

(defun chief/python-ruff-check-buffer ()
  "Run Ruff against the current file."
  (interactive)
  (if-let* ((command (chief/python-command "ruff" "check" (or buffer-file-name "."))))
      (chief/python--compile command "*ruff check*")
    (user-error "Ruff is not available for this project")))

(defun chief/python-ruff-check-project ()
  "Run Ruff against the current project."
  (interactive)
  (if-let* ((command (chief/python-command "ruff" "check" ".")))
      (chief/python--compile command "*ruff project check*")
    (user-error "Ruff is not available for this project")))

(defun chief/python-ty-check-project ()
  "Run ty against the current project."
  (interactive)
  (if-let* ((command (chief/python-command "ty" "check")))
      (chief/python--compile command "*ty check*")
    (user-error "ty is not available for this project")))

(defun chief/python-format-buffer ()
  "Format the current buffer with Ruff."
  (interactive)
  (let ((command (chief/python-command
                  "ruff"
                  "format"
                  "--stdin-filename"
                  (or buffer-file-name (expand-file-name "stdin.py" default-directory))
                  "-")))
    (unless command
      (user-error "Ruff is not available for this project"))
    (let ((stdout (generate-new-buffer " *chief-python-ruff-format*"))
          (stderr-file (make-temp-file "chief-python-ruff-format-"))
          (point-marker (point-marker))
          (modified (buffer-modified-p))
          (coding-system-for-read 'utf-8-unix)
          (coding-system-for-write 'utf-8-unix))
      (unwind-protect
          (let ((status (apply #'call-process-region
                               (point-min)
                               (point-max)
                               (car command)
                               nil
                               (list stdout stderr-file)
                               nil
                               (cdr command))))
            (if (zerop status)
                (progn
                  (replace-buffer-contents stdout)
                  (goto-char (marker-position point-marker))
                  (set-buffer-modified-p modified))
              (error "%s"
                     (with-temp-buffer
                       (insert-file-contents stderr-file)
                       (string-trim (buffer-string))))))
        (kill-buffer stdout)
        (when (file-exists-p stderr-file)
          (delete-file stderr-file))
        (set-marker point-marker nil)))))

(defun chief/python-enable-format-on-save ()
  "Enable Ruff formatting on save when Ruff is available."
  (when (and chief/python-format-on-save
             (chief/python-command "ruff" "format"))
    (if (fboundp 'chief/format-enable-on-save)
        (chief/format-enable-on-save #'chief/python-format-buffer)
      (add-hook 'before-save-hook #'chief/python-format-buffer nil t))))

(defun chief/python-setup-lsp ()
  "Configure LSP clients for the current Python buffer."
  (when (chief/python-lsp-available-p)
    (require 'lsp-ruff)
    (setq-local lsp-disabled-clients '(pyls pylsp ty-ls))
    (when-let* ((ruff-command (chief/python-ruff-server-command)))
      (setq-local lsp-ruff-server-command ruff-command))
    (when-let* ((python-command (or (chief/python-project-executable "python")
                                    (chief/python-global-executable "python"))))
      (setq-local lsp-ruff-python-path python-command)))
  (when (fboundp 'chief/lsp-managed-mode-setup)
    (chief/lsp-managed-mode-setup)))

(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection #'chief/python-ty-server-command)
    :activation-fn (lsp-activate-on "python")
    :priority 1
    :library-folders-fn #'chief/python-library-directories
    :server-id 'ty)))

(defun chief/python-mode-setup ()
  "Set up Python editing for uv-managed projects."
  (setq-local chief/lsp-root-function #'chief/python-project-root)
  (chief/python-activate-project-environment)
  (chief/python-configure-shell)
  (chief/repl-configure
   :start #'chief/python-run-project-repl
   :restart #'chief/python-restart-project-repl
   :send-line #'chief/python-shell-send-line
   :send-region #'python-shell-send-region
   :send-buffer #'python-shell-send-buffer
   :send-defun #'python-shell-send-defun
   :load-file #'python-shell-send-file)
  (chief/python-enable-format-on-save)
  (chief/python-setup-lsp))

(add-hook 'python-base-mode-hook #'chief/python-mode-setup)

(chief/repl-setup-standard-local-leader 'python-base-mode-map)

(chief/local-leader-def
  :keymaps 'python-base-mode-map
  "f" #'chief/python-format-buffer
  "c" '(:ignore t :which-key "check")
  "cb" #'chief/python-ruff-check-buffer
  "cp" #'chief/python-ruff-check-project
  "ct" #'chief/python-ty-check-project
  "u" '(:ignore t :which-key "uv")
  "uv" #'chief/python-uv-venv
  "us" #'chief/python-uv-sync
  "ua" #'chief/python-uv-add-dev-tooling)

(provide 'lang-python)
;;; lang-python.el ends here
