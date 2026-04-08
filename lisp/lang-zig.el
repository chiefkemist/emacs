;;; lang-zig.el --- Zig and ZLS support -*- lexical-binding: t; -*-

(require 'compile)
(require 'core-projects)
(require 'json)
(require 'project)
(require 'subr-x)

(defun chief/zig-project-root ()
  "Return the current Zig project root, if any."
  (chief/project-preferred-root
   '("zls.json" "build.zig" "build.zig.zon")
   '(".git")
   (chief/project-current-root)
   default-directory))

(defun chief/zig-default-directory ()
  "Return the preferred working directory for Zig commands."
  (or (chief/zig-project-root) default-directory))

(defun chief/zig-zls-config-file (&optional root)
  "Return the project-local `zls.json' for ROOT, if any."
  (let ((root (or root (chief/zig-project-root))))
    (when root
      (let ((file (expand-file-name "zls.json" root)))
        (when (file-exists-p file)
          file)))))

(defvar chief/zig-env-cache nil
  "Cached result of `zig env'.")

(defun chief/zig-env ()
  "Return parsed data from `zig env', caching the result."
  (or chief/zig-env-cache
      (when-let* ((zig (executable-find "zig")))
        (setq chief/zig-env-cache
              (ignore-errors
                (json-parse-string
                 (string-join (process-lines zig "env") "\n")
                 :object-type 'alist
                 :array-type 'list))))))

(defun chief/zig-library-directories (&optional _workspace)
  "Return Zig library directories for LSP library-file reuse."
  (delq
   nil
   (list
    (when-let* ((env (chief/zig-env))
                (lib-dir (alist-get 'lib_dir env)))
      (file-name-as-directory (expand-file-name lib-dir))))))

(defun chief/zig-compile (command name &optional directory)
  "Run Zig COMMAND in DIRECTORY using compilation buffer NAME."
  (let ((default-directory (or directory (chief/zig-default-directory)))
        (compilation-read-command nil))
    (compilation-start
     (mapconcat #'shell-quote-argument command " ")
     'compilation-mode
     (lambda (_) name))))

(defun chief/zig-build ()
  "Run `zig build' for the current project."
  (interactive)
  (chief/zig-compile '("zig" "build") "*zig build*" (chief/zig-default-directory)))

(defun chief/zig-build-run ()
  "Run `zig build run' for the current project."
  (interactive)
  (chief/zig-compile '("zig" "build" "run") "*zig build run*" (chief/zig-default-directory)))

(defun chief/zig-build-test ()
  "Run `zig build test' for the current project."
  (interactive)
  (chief/zig-compile '("zig" "build" "test") "*zig build test*" (chief/zig-default-directory)))

(defun chief/zig-test-buffer ()
  "Run `zig test' for the current file."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file"))
  (save-buffer)
  (chief/zig-compile
   (list "zig" "test" (expand-file-name buffer-file-name))
   "*zig test*"
   (chief/zig-default-directory)))

(defun chief/zig-format-buffer ()
  "Format the current Zig buffer with `zig fmt'."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file"))
  (save-buffer)
  (let ((stderr-file (make-temp-file "chief-zig-fmt-")))
    (unwind-protect
        (let ((status (call-process "zig"
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

(defun chief/zig-open-build-file ()
  "Open the nearest `build.zig'."
  (interactive)
  (if-let* ((root (chief/zig-project-root))
            (file (expand-file-name "build.zig" root))
            ((file-exists-p file)))
      (find-file file)
    (user-error "No build.zig found for this project")))

(defun chief/zig-open-zon-file ()
  "Open the nearest `build.zig.zon'."
  (interactive)
  (if-let* ((root (chief/zig-project-root))
            (file (expand-file-name "build.zig.zon" root))
            ((file-exists-p file)))
      (find-file file)
    (user-error "No build.zig.zon found for this project")))

(defun chief/zig-open-zls-config ()
  "Open the nearest `zls.json'."
  (interactive)
  (if-let* ((file (chief/zig-zls-config-file)))
      (find-file file)
    (user-error "No zls.json found for this project")))

(defun chief/zig-zls-command ()
  "Return the command list used to start ZLS for the current project."
  (when-let* ((command (or (and (fboundp 'chief/lsp-probed-executable-command)
                                (chief/lsp-probed-executable-command "zls" nil '("version")))
                           (when-let* ((zls (executable-find "zls")))
                             (list zls)))))
    (if-let* ((config-file (chief/zig-zls-config-file)))
        (append command (list "--config-path" config-file))
      command)))

(defun chief/zig-mode-setup ()
  "Configure the current Zig buffer."
  (setq-local compile-command "zig build")
  (setq-local chief/lsp-root-function #'chief/zig-project-root)
  (when (require 'lsp-zig nil t)
    (setq-local lsp-enabled-clients '(chief-zls))
    (setq-local lsp-disabled-clients '(zls))
    (unless (chief/zig-zls-config-file)
      (setq-local lsp-zig-enable-build-on-save nil)
      (when-let* ((zig (executable-find "zig")))
        (setq-local lsp-zig-zig-exe-path zig))))
  (when (fboundp 'chief/lsp-managed-mode-setup)
    (chief/lsp-managed-mode-setup)))

(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection #'chief/zig-zls-command)
    :activation-fn (lsp-activate-on "zig")
    :major-modes '(zig-mode)
    :library-folders-fn #'chief/zig-library-directories
    :priority 2
    :server-id 'chief-zls)))

(chief/safe-use-package zig-mode
  :mode ("\\.zig\\'" . zig-mode)
  :mode ("\\.zir\\'" . zig-mode)
  :mode ("\\.zon\\'" . zig-mode)
  :mode ("build\\.zig\\.zon\\'" . zig-mode)
  :hook (zig-mode . chief/zig-mode-setup))

(with-eval-after-load 'zig-mode
  (chief/local-leader-def
    :keymaps 'zig-mode-map
    "c" '(:ignore t :which-key "zig")
    "cb" #'chief/zig-build
    "cr" #'chief/zig-build-run
    "cT" #'chief/zig-build-test
    "ct" #'chief/zig-test-buffer
    "cf" #'chief/zig-format-buffer
    "cz" #'chief/zig-open-zls-config
    "cm" #'chief/zig-open-build-file
    "cp" #'chief/zig-open-zon-file))

(provide 'lang-zig)
;;; lang-zig.el ends here
