;;; lang-zig.el --- Zig and ZLS support -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'compile)
(require 'core-projects)
(require 'json)
(require 'project)
(require 'seq)
(require 'subr-x)

(declare-function dap-debug "dap-mode" (debug-args))

(defcustom chief/zig-test-extra-args nil
  "Default extra arguments appended to focused `zig test' commands."
  :type '(repeat string)
  :group 'chief)

(defcustom chief/zig-build-summary "all"
  "Build summary mode used by project-level Zig build commands."
  :type 'string
  :group 'chief)

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

(defun chief/zig-command (&rest args)
  "Return a Zig command list built from ARGS."
  (unless (executable-find "zig")
    (user-error "zig is not available on PATH"))
  (cons "zig" args))

(defun chief/zig-compile (command name &optional directory)
  "Run Zig COMMAND in DIRECTORY using compilation buffer NAME."
  (let ((default-directory (or directory (chief/zig-default-directory)))
        (compilation-read-command nil))
    (compilation-start
     (mapconcat #'shell-quote-argument command " ")
     'compilation-mode
     (lambda (_) name))))

(defun chief/zig-run-command-async (command directory buffer-name on-success)
  "Run COMMAND asynchronously in DIRECTORY.
Output goes to BUFFER-NAME.  ON-SUCCESS is called with the output string after a
successful exit."
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (setq buffer-read-only nil)
        (erase-buffer)
        (setq-local default-directory directory)))
    (let* ((process
            (make-process
             :name buffer-name
             :buffer buffer
             :stderr buffer
             :command command
             :noquery t
             :sentinel
             (lambda (process _event)
               (when (memq (process-status process) '(exit signal))
                 (let* ((status (process-exit-status process))
                        (success (and (eq (process-status process) 'exit)
                                      (zerop status)))
                        (output (when (buffer-live-p (process-buffer process))
                                  (with-current-buffer (process-buffer process)
                                    (buffer-string)))))
                   (if success
                       (funcall on-success output)
                     (when (buffer-live-p (process-buffer process))
                       (with-current-buffer (process-buffer process)
                         (goto-char (point-min))
                         (compilation-mode))
                       (display-buffer (process-buffer process)))
                     (message "%s failed with exit status %s" buffer-name status))))))))
      (set-process-query-on-exit-flag process nil)
      process)))

(defun chief/zig-current-file ()
  "Return the current buffer file as an absolute Zig source path."
  (unless buffer-file-name
    (user-error "This buffer is not visiting a Zig file"))
  (expand-file-name buffer-file-name))

(defun chief/zig-save-current-buffer ()
  "Save the current Zig buffer when it is modified."
  (when (and buffer-file-name (buffer-modified-p))
    (save-buffer)))

(defun chief/zig-cache-directory ()
  "Return the directory used for transient Zig build artifacts."
  (let ((directory (expand-file-name
                    "zig/"
                    (if (boundp 'chief/var-directory)
                        chief/var-directory
                      user-emacs-directory))))
    (make-directory directory t)
    directory))

(defun chief/zig-safe-name (file)
  "Return a filesystem-safe artifact name derived from FILE."
  (let ((name (replace-regexp-in-string
               "[^[:alnum:]_ .-]" "_" (file-name-base file))))
    (replace-regexp-in-string "[[:space:]]+" "_" name)))

(defun chief/zig-artifact-path (&optional suffix)
  "Return a stable transient binary path for the current Zig file."
  (let* ((file (chief/zig-current-file))
         (hash (substring (secure-hash 'sha1 (expand-file-name file)) 0 12))
         (extension (if (eq system-type 'windows-nt) ".exe" "")))
    (expand-file-name
     (format "%s-%s%s%s"
             (chief/zig-safe-name file)
             hash
             (or suffix "")
             extension)
     (chief/zig-cache-directory))))

(defun chief/zig-build-command (&rest args)
  "Return `zig build' ARGS plus configured summary flags."
  (append (chief/zig-command "build")
          args
          (when (and chief/zig-build-summary
                     (not (string-empty-p chief/zig-build-summary)))
            (list "--summary" chief/zig-build-summary))))

(defun chief/zig-build ()
  "Run `zig build' for the current project."
  (interactive)
  (chief/zig-compile (chief/zig-build-command) "*zig build*" (chief/zig-default-directory)))

(defun chief/zig-build-steps ()
  "Return custom build step names declared in the nearest build.zig."
  (when-let* ((root (chief/zig-project-root))
              (file (expand-file-name "build.zig" root))
              ((file-readable-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (let (steps)
        (goto-char (point-min))
        (while (re-search-forward "\\.step[ \t]*([ \t]*\\([\"`]\\)\\([^\"`]+\\)\\1" nil t)
          (push (match-string 2) steps))
        (delete-dups (nreverse steps))))))

(defun chief/zig-read-build-step (&optional prompt)
  "Read a Zig build step declared by build.zig."
  (let* ((steps (or (chief/zig-build-steps)
                    '("install" "run" "test" "check")))
         (default (cond
                   ((member "run" steps) "run")
                   ((member "test" steps) "test")
                   (t (car steps)))))
    (completing-read (or prompt "Zig build step: ") steps nil t nil nil default)))

(defun chief/zig-build-step (step)
  "Run `zig build STEP' for a selected build step."
  (interactive (list (chief/zig-read-build-step)))
  (chief/zig-compile (chief/zig-build-command step)
                     (format "*zig build %s*" step)
                     (chief/zig-default-directory)))

(defun chief/zig-build-run ()
  "Run `zig build run' for the current project."
  (interactive)
  (chief/zig-build-step "run"))

(defun chief/zig-build-test ()
  "Run `zig build test' for the current project."
  (interactive)
  (chief/zig-build-step "test"))

(defun chief/zig-build-check ()
  "Run `zig build check' when the project defines that step."
  (interactive)
  (chief/zig-build-step "check"))

(defun chief/zig-current-file-has-main-p ()
  "Return non-nil when the current file appears to define `pub fn main'."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^[ \t]*pub[ \t]+fn[ \t]+main[ \t]*(" nil t)))

(defun chief/zig-run-current-file ()
  "Run the current Zig file directly with `zig run'."
  (interactive)
  (chief/zig-save-current-buffer)
  (unless (chief/zig-current-file-has-main-p)
    (user-error "Current file does not appear to define `pub fn main'"))
  (chief/zig-compile (chief/zig-command "run" (chief/zig-current-file))
                     "*zig run file*"
                     (file-name-directory (chief/zig-current-file))))

(defconst chief/zig-test-declaration-regexp
  "^[ \t]*test[ \t]+\\(?:\\([\"`]\\)\\([^\"`]+\\)\\1\\|\\([[:alnum:]_.]+\\)\\)"
  "Regexp matching Zig test declarations.")

(defun chief/zig-buffer-test-items ()
  "Return plist entries for Zig tests in the current buffer."
  (let (items)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward chief/zig-test-declaration-regexp nil t)
        (unless (nth 8 (save-excursion (syntax-ppss (match-beginning 0))))
          (let ((name (or (match-string 2) (match-string 3))))
            (push (list :name name :position (match-beginning 0)) items)))))
    (nreverse items)))

(defun chief/zig-test-item-at-point ()
  "Return the Zig test item at point, or nil."
  (let ((position (point))
        (items (chief/zig-buffer-test-items)))
    (cl-loop for item in items
             for next in (append (cdr items) (list nil))
             for start = (plist-get item :position)
             for end = (or (plist-get next :position) (point-max))
             when (and (<= start position) (< position end))
             return item)))

(defun chief/zig-read-test-name ()
  "Return the Zig test name at point, or prompt for one in this file."
  (let* ((items (chief/zig-buffer-test-items))
         (at-point (chief/zig-test-item-at-point)))
    (unless items
      (user-error "No Zig test declarations found in this file"))
    (or (plist-get at-point :name)
        (completing-read "Zig test: "
                         (mapcar (lambda (item) (plist-get item :name)) items)
                         nil t))))

(defun chief/zig-test-buffer ()
  "Run `zig test' for the current file."
  (interactive)
  (chief/zig-save-current-buffer)
  (chief/zig-compile
   (append (chief/zig-command "test" (chief/zig-current-file))
           chief/zig-test-extra-args)
   "*zig test*"
   (chief/zig-default-directory)))

(defun chief/zig-test-at-point ()
  "Run the Zig test at point, prompting when point is not in a test."
  (interactive)
  (chief/zig-save-current-buffer)
  (let ((name (chief/zig-read-test-name)))
    (chief/zig-compile
     (append (chief/zig-command "test" (chief/zig-current-file)
                                "--test-filter" name)
             chief/zig-test-extra-args)
     (format "*zig test %s*" name)
     (chief/zig-default-directory))))

(defun chief/zig-ast-check-buffer ()
  "Run `zig ast-check' for the current file."
  (interactive)
  (chief/zig-save-current-buffer)
  (chief/zig-compile (chief/zig-command "ast-check" (chief/zig-current-file))
                     "*zig ast-check*"
                     (file-name-directory (chief/zig-current-file))))

(defun chief/zig-format-buffer ()
  "Format the current Zig buffer with `zig fmt'."
  (interactive)
  (chief/zig-save-current-buffer)
  (let ((stderr-file (make-temp-file "chief-zig-fmt-")))
    (unwind-protect
        (let ((status (call-process "zig"
                                    nil
                                    (list nil stderr-file)
                                    nil
                                    "fmt"
                                    (chief/zig-current-file))))
          (if (zerop status)
              (revert-buffer t t t)
            (error "%s"
                   (with-temp-buffer
                     (insert-file-contents stderr-file)
                     (string-trim (buffer-string))))))
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

(defun chief/zig-format-check-buffer ()
  "Check formatting for the current Zig buffer."
  (interactive)
  (chief/zig-save-current-buffer)
  (chief/zig-compile (chief/zig-command "fmt" "--check" (chief/zig-current-file))
                     "*zig fmt check*"
                     (file-name-directory (chief/zig-current-file))))

(defun chief/zig-dap-debug (program name cwd &optional args)
  "Start a Zig DAP session for PROGRAM named NAME in CWD with ARGS."
  (require 'dap-mode)
  (let ((configuration
         (cond
          ((featurep 'dap-lldb)
           (list :type "lldb-vscode"
                 :request "launch"
                 :name name
                 :program program
                 :cwd cwd
                 :args (or args [])
                 :sourceLanguages ["zig"]))
          ((featurep 'dap-gdb)
           (list :type "gdb"
                 :request "launch"
                 :name name
                 :program program
                 :target program
                 :cwd cwd
                 :args (or args [])
                 :sourceLanguages ["zig"]))
          (t
           (user-error "No Zig-capable DAP adapter is configured")))))
    (dap-debug configuration)))

(defun chief/zig-debug-current-file ()
  "Compile the current Zig file with debug info and launch DAP."
  (interactive)
  (chief/zig-save-current-buffer)
  (unless (chief/zig-current-file-has-main-p)
    (user-error "Current file does not appear to define `pub fn main'"))
  (let* ((file (chief/zig-current-file))
         (binary (chief/zig-artifact-path "-debug"))
         (directory (file-name-directory file)))
    (chief/zig-run-command-async
     (chief/zig-command "build-exe" file "-O" "Debug" (format "-femit-bin=%s" binary))
     directory
     "*zig build-exe debug*"
     (lambda (_output)
       (chief/zig-dap-debug binary
                            (format "Zig file :: %s" (file-name-nondirectory file))
                            directory)))))

(defun chief/zig-debug-test-at-point ()
  "Compile the current Zig test with debug info and launch DAP."
  (interactive)
  (chief/zig-save-current-buffer)
  (let* ((name (chief/zig-read-test-name))
         (file (chief/zig-current-file))
         (binary (chief/zig-artifact-path "-tests-debug"))
         (directory (file-name-directory file)))
    (chief/zig-run-command-async
     (chief/zig-command "test" file "--test-no-exec" "--test-filter" name
                        "-O" "Debug" (format "-femit-bin=%s" binary))
     directory
     "*zig test debug*"
     (lambda (_output)
       (chief/zig-dap-debug binary
                            (format "Zig test :: %s" name)
                            directory)))))

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

(defun chief/zig-completion-setup ()
  "Enable Zig popup completion and ZLS completion-side edits."
  (setq-local lsp-completion-enable t)
  (setq-local lsp-completion-enable-additional-text-edit t)
  (when (boundp 'corfu-auto)
    (setq-local corfu-auto t))
  (when (boundp 'corfu-auto-delay)
    (setq-local corfu-auto-delay 0.05))
  (when (boundp 'corfu-auto-prefix)
    ;; ZLS member and builtin completions should pop immediately after `.' and `@'.
    (setq-local corfu-auto-prefix 0))
  (when (fboundp 'corfu-mode)
    (when (bound-and-true-p corfu-mode)
      (corfu-mode -1))
    (corfu-mode 1))
  (when (fboundp 'lsp-completion-mode)
    (lsp-completion-mode 1)))

(defun chief/zig-mode-setup ()
  "Configure the current Zig buffer."
  (setq-local compile-command "zig build")
  (setq-local chief/lsp-root-function #'chief/zig-project-root)
  (chief/zig-completion-setup)
  (when (require 'lsp-zig nil t)
    (setq-local lsp-enabled-clients '(chief-zls))
    (setq-local lsp-disabled-clients '(zls))
    (setq-local lsp-zls-enable-snippets t)
    (setq-local lsp-zig-enable-argument-placeholders t)
    (setq-local lsp-zig-enable-inlay-hints t)
    (setq-local lsp-zig-inlay-hints-show-variable-type-hints t)
    (setq-local lsp-zig-inlay-hints-show-parameter-name t)
    (setq-local lsp-zig-inlay-hints-show-builtin t)
    (setq-local lsp-zig-inlay-hints-hide-redundant-param-names t)
    (setq-local lsp-zig-inlay-hints-hide-redundant-param-names-last-token t)
    (setq-local lsp-zig-completions-with-replace t)
    (setq-local lsp-zig-enable-autofix nil)
    (setq-local lsp-zig-enable-build-on-save (not (null (chief/zig-zls-config-file))))
    (setq-local lsp-zig-build-on-save-step "check")
    (when-let* ((zig (executable-find "zig")))
      (setq-local lsp-zig-zig-exe-path zig)
      (when-let* ((env (chief/zig-env))
                  (lib-dir (alist-get 'lib_dir env)))
        (setq-local lsp-zig-zig-lib-path (expand-file-name lib-dir)))))
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
    "cs" #'chief/zig-build-step
    "cc" #'chief/zig-build-check
    "cr" #'chief/zig-build-run
    "cR" #'chief/zig-run-current-file
    "cT" #'chief/zig-build-test
    "ct" #'chief/zig-test-buffer
    "ca" #'chief/zig-test-at-point
    "cv" #'chief/zig-ast-check-buffer
    "cf" #'chief/zig-format-buffer
    "cF" #'chief/zig-format-check-buffer
    "r" '(:ignore t :which-key "run")
    "rr" #'chief/zig-run-current-file
    "rb" #'chief/zig-build-run
    "rs" #'chief/zig-build-step
    "t" '(:ignore t :which-key "test")
    "tt" #'chief/zig-test-at-point
    "tf" #'chief/zig-test-buffer
    "tp" #'chief/zig-build-test
    "d" '(:ignore t :which-key "debug")
    "dd" #'chief/zig-debug-current-file
    "dt" #'chief/zig-debug-test-at-point
    "m" '(:ignore t :which-key "metadata")
    "mz" #'chief/zig-open-zls-config
    "mb" #'chief/zig-open-build-file
    "mp" #'chief/zig-open-zon-file))

(provide 'lang-zig)
;;; lang-zig.el ends here
