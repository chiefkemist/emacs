;;; lang-java.el --- Java and shared JVM tooling -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'compile)
(require 'comint)
(require 'core-projects)
(require 'core-repl)
(require 'subr-x)

(defvar chief/jvm-java-home-cache nil
  "Cached resolved JAVA_HOME path for JVM tooling.")

(defvar chief/jvm-external-root-overrides (make-hash-table :test #'equal)
  "Map extracted external JVM source files to their owning project roots.")

(defvar-local chief/jvm-project-root-override nil
  "Buffer-local preferred JVM project root override.")

(defun chief/jdtls-config-directory ()
  "Return the shared JDT LS configuration directory."
  (let ((dir (expand-file-name "jdtls/configuration/" chief/var-directory)))
    (make-directory dir t)
    dir))

(defun chief/jdtls-workspace-directory ()
  "Return the per-project JDT LS workspace directory."
  (let* ((root (or (chief/jvm-project-root)
                   default-directory))
         (id (secure-hash 'sha1 (file-name-as-directory (expand-file-name root))))
         (dir (expand-file-name (format "jdtls/workspaces/%s/" id) chief/var-directory)))
    (make-directory dir t)
    dir))

(defun chief/jvm-valid-java-home-p (path)
  "Return non-nil when PATH looks like a valid JAVA_HOME."
  (and (stringp path)
       (file-directory-p path)
       (file-executable-p (expand-file-name "bin/java" path))))

(defun chief/jvm-sdkman-java-home ()
  "Return the preferred SDKMAN Java home on this machine, if available."
  (let* ((sdkman-root (expand-file-name "~/.sdkman/candidates/java"))
         (current (expand-file-name "current" sdkman-root)))
    (or
     (when (chief/jvm-valid-java-home-p current)
       current)
     (when (file-directory-p sdkman-root)
       (car
        (sort
         (seq-filter
          #'chief/jvm-valid-java-home-p
          (directory-files sdkman-root t directory-files-no-dot-files-regexp t))
         #'string>))))))

(defun chief/jvm-darwin-java-home ()
  "Return a macOS Java home discovered via `/usr/libexec/java_home'."
  (when (and (eq system-type 'darwin)
             (file-executable-p "/usr/libexec/java_home"))
    (let ((output
           (string-trim
            (with-temp-buffer
              (if (zerop (call-process "/usr/libexec/java_home" nil (current-buffer) nil "-v" "17"))
                  (buffer-string)
                "")))))
      (when (chief/jvm-valid-java-home-p output)
        output))))

(defun chief/jvm-derived-java-home ()
  "Return a Java home derived from the `java' executable path."
  (when-let* ((java (executable-find "java"))
              (real (file-truename java)))
    (let* ((bin-dir (file-name-directory real))
           (home (expand-file-name ".." bin-dir)))
      (when (chief/jvm-valid-java-home-p home)
        home))))

(defun chief/jvm-java-home ()
  "Return the preferred JAVA_HOME for JVM language tooling."
  (or chief/jvm-java-home-cache
      (setq chief/jvm-java-home-cache
            (or (chief/jvm-sdkman-java-home)
                (and (chief/jvm-valid-java-home-p (getenv "JAVA_HOME"))
                     (getenv "JAVA_HOME"))
                (chief/jvm-darwin-java-home)
                (chief/jvm-derived-java-home)))))

(defun chief/jvm-env-vars ()
  "Return environment variable assignments for JVM commands."
  (when-let* ((home (chief/jvm-java-home)))
    (list (format "JAVA_HOME=%s" home)
          (format "JDK_HOME=%s" home))))

(defun chief/jvm-wrap-command (command)
  "Return COMMAND wrapped with JVM environment variables when available."
  (append (when-let* ((env-vars (chief/jvm-env-vars)))
            (cons "/usr/bin/env" env-vars))
          command))

(defun chief/jvm-command-string (command)
  "Return a shell-safe command string for COMMAND."
  (mapconcat #'shell-quote-argument (chief/jvm-wrap-command command) " "))

(defun chief/jvm-register-external-root (file root)
  "Remember that FILE should use JVM project ROOT."
  (when (and file root)
    (puthash (expand-file-name file)
             (file-name-as-directory (expand-file-name root))
             chief/jvm-external-root-overrides)))

(defun chief/jvm-external-root-override (&optional file)
  "Return a registered project root override for FILE or the current buffer."
  (let ((file (or file buffer-file-name)))
    (when file
      (gethash (expand-file-name file) chief/jvm-external-root-overrides))))

(defun chief/jvm-project-root ()
  "Return the current Java/Kotlin project root."
  (chief/project-preferred-root
   chief/jvm-project-root-override
   (chief/jvm-external-root-override)
   '("settings.gradle.kts" "settings.gradle")
   '("pom.xml")
   '("build.gradle.kts" "build.gradle")
   (chief/project-current-root)
   default-directory))

(defun chief/jvm-gradle-executable ()
  "Return the preferred Gradle executable for the current JVM project."
  (let* ((root (chief/jvm-project-root))
         (wrapper (expand-file-name "gradlew" root)))
    (cond
     ((file-executable-p wrapper) wrapper)
     ((executable-find "gradle") (executable-find "gradle"))
     (t nil))))

(defun chief/jvm-maven-executable ()
  "Return the preferred Maven executable for the current JVM project."
  (let* ((root (chief/jvm-project-root))
         (wrapper (expand-file-name "mvnw" root)))
    (cond
     ((file-executable-p wrapper) wrapper)
     ((executable-find "mvn") (executable-find "mvn"))
     (t nil))))

(defun chief/jvm-project-build-tool ()
  "Return the primary build tool for the current JVM project."
  (cond
   ((or (file-exists-p (expand-file-name "settings.gradle.kts" (chief/jvm-project-root)))
        (file-exists-p (expand-file-name "settings.gradle" (chief/jvm-project-root)))
        (file-exists-p (expand-file-name "build.gradle.kts" (chief/jvm-project-root)))
        (file-exists-p (expand-file-name "build.gradle" (chief/jvm-project-root))))
    'gradle)
   ((file-exists-p (expand-file-name "pom.xml" (chief/jvm-project-root)))
    'maven)
   (t nil)))

(defun chief/jvm-gradle-command (&rest tasks)
  "Return a Gradle command list built from TASKS."
  (if-let* ((gradle (chief/jvm-gradle-executable)))
      (cons gradle tasks)
    (user-error "Neither ./gradlew nor gradle is available")))

(defun chief/jvm-maven-command (&rest args)
  "Return a Maven command list built from ARGS."
  (if-let* ((maven (chief/jvm-maven-executable)))
      (cons maven args)
    (user-error "Neither ./mvnw nor mvn is available")))

(defun chief/jvm-compile (command name &optional directory)
  "Run JVM COMMAND in DIRECTORY using compilation buffer NAME."
  (let ((default-directory (or directory (chief/jvm-project-root)))
        (compilation-read-command nil))
    (compilation-start
     (chief/jvm-command-string command)
     'compilation-mode
     (lambda (_) name))))

(defun chief/java-lsp-command ()
  "Return the `jdtls' command configured with the right JAVA_HOME."
  (chief/jvm-wrap-command
   (list (or (executable-find "jdtls")
             (user-error "jdtls is not available on PATH"))
         "-configuration" (chief/jdtls-config-directory)
         "-data" (chief/jdtls-workspace-directory))))

(defun chief/java-repl-buffer-name ()
  "Return the Java REPL buffer name for the current project."
  (format "*jshell:%s*"
          (file-name-nondirectory
           (directory-file-name (chief/jvm-project-root)))))

(defun chief/java-repl-command ()
  "Return the command used to start the Java REPL."
  (chief/jvm-wrap-command
   (list (or (executable-find "jshell")
             (user-error "jshell is not available on PATH")))))

(defun chief/java-repl-buffer ()
  "Return the Java REPL buffer for the current project, creating it when needed."
  (let* ((default-directory (chief/jvm-project-root))
         (buffer-name (chief/java-repl-buffer-name))
         (buffer (get-buffer buffer-name))
         (command (chief/java-repl-command)))
    (unless (comint-check-proc buffer)
      (setq buffer
            (apply #'make-comint-in-buffer
                   "jshell"
                   buffer-name
                   (car command)
                   nil
                   (cdr command)))
      (with-current-buffer buffer
        (setq-local comint-prompt-read-only t)))
    buffer))

(defun chief/java-start-repl ()
  "Start or switch to the Java REPL."
  (interactive)
  (pop-to-buffer (chief/java-repl-buffer)))

(defun chief/java-send-string (string)
  "Send STRING to the Java REPL."
  (let ((buffer (chief/java-repl-buffer)))
    (with-current-buffer buffer
      (goto-char (point-max)))
    (comint-send-string buffer string)
    (unless (string-suffix-p "\n" string)
      (comint-send-string buffer "\n"))
    (pop-to-buffer buffer)))

(defun chief/java-send-region (start end)
  "Send the active region from START to END to the Java REPL."
  (interactive "r")
  (chief/java-send-string
   (buffer-substring-no-properties start end)))

(defun chief/java-send-buffer ()
  "Send the current Java buffer to the REPL."
  (interactive)
  (chief/java-send-region (point-min) (point-max)))

(defun chief/java-send-defun ()
  "Send the current Java defun to the REPL."
  (interactive)
  (save-excursion
    (mark-defun)
    (chief/java-send-region (region-beginning) (region-end))
    (deactivate-mark)))

(defun chief/java-build-project ()
  "Build the current Java project."
  (interactive)
  (pcase (chief/jvm-project-build-tool)
    ('gradle (chief/jvm-compile (chief/jvm-gradle-command "build") "*gradle build*"))
    ('maven (chief/jvm-compile (chief/jvm-maven-command "package") "*mvn package*"))
    (_ (user-error "No Gradle or Maven project detected"))))

(defun chief/java-test-project ()
  "Run tests for the current Java project."
  (interactive)
  (pcase (chief/jvm-project-build-tool)
    ('gradle (chief/jvm-compile (chief/jvm-gradle-command "test") "*gradle test*"))
    ('maven (chief/jvm-compile (chief/jvm-maven-command "test") "*mvn test*"))
    (_ (user-error "No Gradle or Maven project detected"))))

(defun chief/java-run-project ()
  "Run the current Java project."
  (interactive)
  (pcase (chief/jvm-project-build-tool)
    ('gradle (chief/jvm-compile (chief/jvm-gradle-command "run") "*gradle run*"))
    ('maven (chief/jvm-compile (chief/jvm-maven-command "exec:java") "*mvn exec:java*"))
    (_ (user-error "No Gradle or Maven project detected"))))

(defun chief/java-clean-project ()
  "Clean the current Java project."
  (interactive)
  (pcase (chief/jvm-project-build-tool)
    ('gradle (chief/jvm-compile (chief/jvm-gradle-command "clean") "*gradle clean*"))
    ('maven (chief/jvm-compile (chief/jvm-maven-command "clean") "*mvn clean*"))
    (_ (user-error "No Gradle or Maven project detected"))))

(defun chief/java-mode-build-command ()
  "Return the preferred compile command string for Java buffers."
  (pcase (chief/jvm-project-build-tool)
    ('gradle (chief/jvm-command-string (chief/jvm-gradle-command "build")))
    ('maven (chief/jvm-command-string (chief/jvm-maven-command "package")))
    (_ (chief/jvm-command-string
        (list (or (executable-find "javac")
                  (user-error "javac is not available on PATH"))
              (expand-file-name buffer-file-name))))))

(defun chief/java-lsp-notification-handlers ()
  "Return custom notification handlers for JDT LS."
  (let ((handlers (make-hash-table :test #'equal)))
    (puthash "language/status" (lambda (&rest _) nil) handlers)
    (puthash "language/actionableNotification" (lambda (&rest _) nil) handlers)
    handlers))

(defun chief/java-mode-setup ()
  "Configure Java buffers."
  (setq-local chief/jvm-project-root-override
              (or chief/jvm-project-root-override
                  (chief/jvm-external-root-override)))
  (setq-local compile-command (chief/java-mode-build-command))
  (setq-local chief/lsp-root-function #'chief/jvm-project-root)
  (setq-local lsp-enabled-clients '(chief-jdtls))
  (setq-local lsp-eldoc-render-all t)
  (chief/repl-configure
   :start #'chief/java-start-repl
   :restart #'chief/java-start-repl
   :send-region #'chief/java-send-region
   :send-buffer #'chief/java-send-buffer
   :send-defun #'chief/java-send-defun
   :load-file #'chief/java-send-buffer)
  (when (fboundp 'chief/lsp-managed-mode-setup)
    (chief/lsp-managed-mode-setup)))

(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection #'chief/java-lsp-command)
    :activation-fn (lsp-activate-on "java")
    :notification-handlers (chief/java-lsp-notification-handlers)
    :priority 1
    :major-modes '(java-mode java-ts-mode)
    :server-id 'chief-jdtls)))

(add-hook 'java-mode-hook #'chief/java-mode-setup)
(when (fboundp 'java-ts-mode)
  (add-hook 'java-ts-mode-hook #'chief/java-mode-setup))

(with-eval-after-load 'cc-mode
  (chief/repl-setup-standard-local-leader 'java-mode-map)
  (chief/local-leader-def
    :keymaps 'java-mode-map
    "c" '(:ignore t :which-key "java")
    "cb" #'chief/java-build-project
    "ct" #'chief/java-test-project
    "cr" #'chief/java-run-project
    "cc" #'chief/java-clean-project))

(when (fboundp 'java-ts-mode)
  (with-eval-after-load 'java-ts-mode
    (chief/repl-setup-standard-local-leader 'java-ts-mode-map)
    (chief/local-leader-def
      :keymaps 'java-ts-mode-map
      "c" '(:ignore t :which-key "java")
      "cb" #'chief/java-build-project
      "ct" #'chief/java-test-project
      "cr" #'chief/java-run-project
      "cc" #'chief/java-clean-project)))

(provide 'lang-java)
;;; lang-java.el ends here
