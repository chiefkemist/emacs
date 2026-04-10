;;; lang-dart.el --- Dart and Flutter tooling -*- lexical-binding: t; -*-

(require 'compile)
(require 'core-projects)
(require 'subr-x)

(defvar chief/dart-preferred-analysis-root nil
  "Most recently preferred project root for external Dart library buffers.")

(defun chief/dart-remember-preferred-analysis-root (root)
  "Remember ROOT as the preferred Dart analysis root."
  (when (and root (file-directory-p root))
    (setq chief/dart-preferred-analysis-root
          (chief/project-normalize-root root))))

(defun chief/dart-external-library-file-p (&optional file)
  "Return non-nil when FILE is an SDK or external package Dart file."
  (let ((file (and file (file-truename file))))
    (and file
         (or (when-let* ((flutter-sdk (and (fboundp 'chief/lsp-flutter-sdk-dir)
                                           (chief/lsp-flutter-sdk-dir))))
               (file-in-directory-p file flutter-sdk))
             (when-let* ((dart-sdk (and (fboundp 'chief/lsp-dart-sdk-dir)
                                        (chief/lsp-dart-sdk-dir))))
               (file-in-directory-p file dart-sdk))
             (file-in-directory-p file (expand-file-name "~/.pub-cache"))))))

(defun chief/dart-preferred-analysis-root ()
  "Return the preferred non-SDK root for external Dart library buffers."
  (when-let* ((root chief/dart-preferred-analysis-root)
              (root (chief/project-normalize-root root))
              ((file-directory-p root)))
    root))

(defun chief/dart-package-root ()
  "Return the nearest Dart package root."
  (chief/project-preferred-root
   '("pubspec.yaml")
   (chief/project-current-root)
   default-directory))

(defun chief/dart-melos-root ()
  "Return the nearest Melos workspace root."
  (chief/project-preferred-root
   '("melos.yaml")
   (chief/project-current-root)
   default-directory))

(defun chief/dart-sdk-library-root ()
  "Return the preferred root for the current SDK/library buffer."
  (let ((file (or buffer-file-name "")))
    (or
     (when-let* ((flutter-sdk (and (fboundp 'chief/lsp-flutter-sdk-dir)
                                   (chief/lsp-flutter-sdk-dir)))
                 ((file-in-directory-p file flutter-sdk)))
       (cond
        ((chief/project-preferred-root '("pubspec.yaml")
                                       (file-name-directory file)
                                       flutter-sdk))
        ((string-match "\\`\\(.*/pkg/sky_engine/\\)" file)
         (chief/project-normalize-root (match-string 1 file)))
        (t
         (chief/project-normalize-root flutter-sdk))))
     (when-let* ((dart-sdk (and (fboundp 'chief/lsp-dart-sdk-dir)
                                (chief/lsp-dart-sdk-dir)))
                 ((file-in-directory-p file dart-sdk)))
       (chief/project-normalize-root dart-sdk))
     (when-let* ((pub-cache (expand-file-name "~/.pub-cache"))
                 ((file-in-directory-p file pub-cache)))
       (chief/project-preferred-root '("pubspec.yaml")
                                     (file-name-directory file)
                                     pub-cache)))))

(defun chief/dart-project-root ()
  "Return the preferred Dart analysis root for the current buffer."
  (or (and (chief/dart-external-library-file-p buffer-file-name)
           (chief/dart-preferred-analysis-root))
      (chief/dart-sdk-library-root)
      (chief/dart-melos-root)
      (chief/dart-package-root)
      (chief/project-current-root)
      default-directory))

(defun chief/dart-flutter-project-p ()
  "Return non-nil when the current Dart root is a Flutter project."
  (or (and (fboundp 'lsp-dart-flutter-project-p)
           (ignore-errors (lsp-dart-flutter-project-p)))
      (when-let* ((root (chief/dart-project-root))
                  (pubspec (expand-file-name "pubspec.yaml" root))
                  ((file-readable-p pubspec)))
        (with-temp-buffer
          (insert-file-contents pubspec)
          (re-search-forward "^[[:space:]]*flutter:[[:space:]]*$" nil t)))))

(defun chief/dart-default-directory ()
  "Return the preferred working directory for Dart commands."
  (chief/project-normalize-root (chief/dart-project-root)))

(defun chief/dart-command-program ()
  "Return the preferred top-level command for the current Dart project."
  (if (and (chief/dart-flutter-project-p)
           (executable-find "flutter"))
      "flutter"
    (or (and (fboundp 'chief/lsp-dart-sdk-executable)
             (chief/lsp-dart-sdk-executable))
        (executable-find "dart")
        (user-error "Dart is not available on PATH"))))

(defun chief/dart-compile (command name &optional directory)
  "Run Dart/Flutter COMMAND in DIRECTORY using compilation buffer NAME."
  (let ((default-directory (or directory (chief/dart-default-directory)))
        (compilation-read-command nil))
    (compilation-start
     (mapconcat #'shell-quote-argument command " ")
     'compilation-mode
     (lambda (_) name))))

(defun chief/dart-analyze-project ()
  "Run static analysis for the current Dart or Flutter project."
  (interactive)
  (chief/dart-compile
   (list (chief/dart-command-program) "analyze")
   "*dart analyze*"))

(defun chief/dart-test-project ()
  "Run tests for the current Dart or Flutter project."
  (interactive)
  (chief/dart-compile
   (list (chief/dart-command-program) "test")
   "*dart test*"))

(defun chief/dart-run-project ()
  "Run the current Dart or Flutter project."
  (interactive)
  (if (and (fboundp 'lsp-dart-run)
           (bound-and-true-p lsp-managed-mode))
      (call-interactively #'lsp-dart-run)
    (chief/dart-compile
     (list (chief/dart-command-program) "run")
     "*dart run*")))

(defun chief/dart-format-buffer ()
  "Format the current Dart buffer."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file"))
  (unless (string-match-p "\\.dart\\'" buffer-file-name)
    (user-error "This buffer is not visiting a Dart source file"))
  (save-buffer)
  (let ((command (list (or (and (fboundp 'chief/lsp-dart-sdk-executable)
                                (chief/lsp-dart-sdk-executable))
                           (executable-find "dart")
                           (user-error "Dart is not available on PATH"))
                       "format"
                       (expand-file-name buffer-file-name))))
    (chief/dart-compile command "*dart format*" (chief/dart-default-directory))))

(defun chief/dart-pub-get ()
  "Fetch Dart or Flutter dependencies for the current project."
  (interactive)
  (if (fboundp 'lsp-dart-pub-get)
      (call-interactively #'lsp-dart-pub-get)
    (chief/dart-compile
     (list (chief/dart-command-program) "pub" "get")
     "*dart pub get*")))

(defun chief/dart-pub-upgrade ()
  "Upgrade Dart or Flutter dependencies for the current project."
  (interactive)
  (if (fboundp 'lsp-dart-pub-upgrade)
      (call-interactively #'lsp-dart-pub-upgrade)
    (chief/dart-compile
     (list (chief/dart-command-program) "pub" "upgrade")
     "*dart pub upgrade*")))

(defun chief/dart-pub-outdated ()
  "Show outdated dependencies for the current Dart or Flutter project."
  (interactive)
  (if (fboundp 'lsp-dart-pub-outdated)
      (call-interactively #'lsp-dart-pub-outdated)
    (chief/dart-compile
     (list (chief/dart-command-program) "pub" "outdated")
     "*dart pub outdated*")))

(defun chief/dart-debug-project ()
  "Start a DAP debug session for the current Dart or Flutter project."
  (interactive)
  (cond
   ((and (chief/dart-flutter-project-p)
         (fboundp 'lsp-dart-dap-debug-flutter))
    (call-interactively #'lsp-dart-dap-debug-flutter))
   ((fboundp 'lsp-dart-dap-debug-dart)
    (call-interactively #'lsp-dart-dap-debug-dart))
   (t
    (user-error "Dart DAP support is not available"))))

(defun chief/dart-hot-reload ()
  "Hot reload the current Flutter debug session."
  (interactive)
  (if (fboundp 'lsp-dart-dap-flutter-hot-reload)
      (call-interactively #'lsp-dart-dap-flutter-hot-reload)
    (user-error "Flutter hot reload is not available")))

(defun chief/dart-hot-restart ()
  "Hot restart the current Flutter debug session."
  (interactive)
  (if (fboundp 'lsp-dart-dap-flutter-hot-restart)
      (call-interactively #'lsp-dart-dap-flutter-hot-restart)
    (user-error "Flutter hot restart is not available")))

(defun chief/dart-extra-library-directories ()
  "Return extra library directories for Dart analysis."
  (delq nil
        (list (expand-file-name "~/.pub-cache")
              (when-let* ((flutter-sdk (and (fboundp 'chief/lsp-flutter-sdk-dir)
                                            (chief/lsp-flutter-sdk-dir))))
                (expand-file-name "packages" flutter-sdk))
              (when-let* ((flutter-sdk (and (fboundp 'chief/lsp-flutter-sdk-dir)
                                            (chief/lsp-flutter-sdk-dir))))
                (expand-file-name "bin/cache/pkg" flutter-sdk)))))

(defun chief/dart-mode-setup ()
  "Configure Dart buffers."
  (let* ((external-library-p (chief/dart-external-library-file-p buffer-file-name))
         (project-root (chief/dart-project-root)))
    (unless external-library-p
      (chief/dart-remember-preferred-analysis-root project-root))
    (setq-local eldoc-idle-delay 0.2)
    (setq-local lsp-eldoc-render-all t)
    (setq-local lsp-lens-enable nil)
    (setq-local lsp-dart-project-root-discovery-strategies '(lsp-root))
    ;; Dart code-lens/outline extras add noticeable overlay churn and are not
    ;; needed for the CLJD -> Dart navigation workflow.
    (setq-local lsp-dart-outline nil)
    (setq-local lsp-dart-flutter-outline nil)
    (setq-local lsp-dart-main-code-lens nil)
    (setq-local lsp-dart-test-code-lens nil)
    (setq-local lsp-dart-closing-labels (not external-library-p))
    (when (boundp 'lsp-dart-flutter-widget-guides)
      (setq-local lsp-dart-flutter-widget-guides nil))
    (when (boundp 'lsp-dart-flutter-fringe-colors)
      (setq-local lsp-dart-flutter-fringe-colors nil)))
  (setq-local compile-command
              (if (chief/dart-flutter-project-p)
                  "flutter test"
                "dart test"))
  (setq-local chief/lsp-root-function #'chief/dart-project-root)
  (when (require 'lsp-dart nil t)
    (setq-local lsp-enabled-clients '(dart_analysis_server))
    (setq-local lsp-dart-extra-library-directories (chief/dart-extra-library-directories))
    (setq-local lsp-dart-only-analyze-projects-with-open-files t)
    (setq-local lsp-dart-show-todos t)
    (when (boundp 'lsp-dart-main-code-lens)
      (setq-local lsp-dart-main-code-lens nil))
    (when (boundp 'lsp-dart-test-code-lens)
      (setq-local lsp-dart-test-code-lens nil))
    (when (boundp 'lsp-dart-outline)
      (setq-local lsp-dart-outline nil))
    (when (boundp 'lsp-dart-flutter-outline)
      (setq-local lsp-dart-flutter-outline nil)))
  (when (fboundp 'chief/lsp-managed-mode-setup)
    (chief/lsp-managed-mode-setup)))

(chief/safe-use-package dart-mode
  :mode ("\\.dart\\'" . dart-mode)
  :hook (dart-mode . chief/dart-mode-setup))

(with-eval-after-load 'dart-mode
  (chief/local-leader-def
    :keymaps 'dart-mode-map
    "c" '(:ignore t :which-key "dart")
    "ca" #'chief/dart-analyze-project
    "cf" #'chief/dart-format-buffer
    "ct" #'chief/dart-test-project
    "cr" #'chief/dart-run-project
    "cp" '(:ignore t :which-key "pub")
    "cpg" #'chief/dart-pub-get
    "cpu" #'chief/dart-pub-upgrade
    "cpo" #'chief/dart-pub-outdated
    "d" '(:ignore t :which-key "debug")
    "dd" #'chief/dart-debug-project
    "dr" #'chief/dart-hot-reload
    "dR" #'chief/dart-hot-restart))

(provide 'lang-dart)
;;; lang-dart.el ends here
