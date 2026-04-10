;;; lang-kotlin.el --- Kotlin tooling -*- lexical-binding: t; -*-

(require 'compile)
(require 'cl-lib)
(require 'core-projects)
(require 'core-repl)
(require 'core-lsp)
(require 'lang-java)
(require 'lsp-kotlin nil t)
(require 'seq)
(require 'subr-x)

(defvar chief/kotlin-source-jars-cache nil
  "Cached list of Kotlin/Java dependency source jars.")

(defvar chief/kotlin-jdk-source-zips-cache nil
  "Cached list of JDK source zip archives.")

(defvar chief/kotlin-external-source-cache (make-hash-table :test #'equal)
  "Cache of resolved Kotlin external-source lookups.")

(defconst chief/kotlin-default-import-packages
  '("kotlin"
    "kotlin.annotation"
    "kotlin.collections"
    "kotlin.comparisons"
    "kotlin.io"
    "kotlin.ranges"
    "kotlin.sequences"
    "kotlin.text"
    "kotlin.jvm"
    "java.lang")
  "Packages imported into Kotlin/JVM source files by default.")

(defvar chief/kotlin-declaration-entry-cache (make-hash-table :test #'equal)
  "Cache of source-archive declaration entries keyed by ZIP and FQCN.")

(defun chief/kotlin-package-name ()
  "Return the package name for the current Kotlin buffer, if any."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^[[:space:]]*package[[:space:]]+\\([[:alnum:]_.]+\\)" nil t)
      (match-string-no-properties 1))))

(defun chief/kotlin-project-package-prefix ()
  "Return a broad package prefix for the current Kotlin project."
  (when-let* ((package-name (chief/kotlin-package-name))
              (parts (split-string package-name "\\." t)))
    (string-join (seq-take parts (min 2 (length parts))) ".")))

(defun chief/kotlin-import-alist ()
  "Return an alist mapping imported Kotlin names to fully qualified imports."
  (save-excursion
    (goto-char (point-min))
    (let (imports)
      (while (re-search-forward
              "^[[:space:]]*import[[:space:]]+\\([[:alnum:]_.*]+\\)\\(?:[[:space:]]+as[[:space:]]+\\([[:alnum:]_]+\\)\\)?"
              nil t)
        (let* ((target (match-string-no-properties 1))
               (alias (match-string-no-properties 2))
               (name (or alias
                         (car (last (split-string target "\\." t))))))
          (push (cons name target) imports)))
      (nreverse imports))))

(defun chief/kotlin-external-package-p (fqcn)
  "Return non-nil when FQCN looks like an external library/JDK symbol."
  (and fqcn
       (not (string-suffix-p ".*" fqcn))
       (or (string-prefix-p "java." fqcn)
           (string-prefix-p "javax." fqcn)
           (string-prefix-p "kotlin." fqcn)
           (string-prefix-p "jdk." fqcn)
           (let ((prefix (chief/kotlin-project-package-prefix)))
             (and prefix
                  (not (string-prefix-p prefix fqcn)))))))

(defun chief/kotlin-current-symbol ()
  "Return the symbol at point as a string."
  (chief/token-at-point))

(defun chief/kotlin-default-import-fqcns (symbol)
  "Return default-import FQCN candidates for Kotlin SYMBOL."
  (when symbol
    (mapcar (lambda (package)
              (concat package "." symbol))
            chief/kotlin-default-import-packages)))

(defun chief/kotlin-import-at-point ()
  "Return the fully qualified import at point, if any."
  (save-excursion
    (beginning-of-line)
    (when (looking-at-p "[[:space:]]*import[[:space:]]+")
      (when (re-search-forward
             "import[[:space:]]+\\([[:alnum:]_.*]+\\)"
             (line-end-position)
             t)
        (match-string-no-properties 1)))))

(defun chief/kotlin-qualified-receiver-at-point ()
  "Return the receiver token before point when on a dotted Kotlin member."
  (save-excursion
    (skip-syntax-backward "w_")
    (when (eq (char-before) ?.)
      (backward-char)
      (skip-syntax-backward " ")
      (skip-syntax-backward "w_")
      (let ((start (point)))
        (skip-syntax-forward "w_")
        (buffer-substring-no-properties start (point))))))

(defun chief/kotlin-chain-root-class ()
  "Return the constructor-like class symbol at the root of a chained call."
  (save-excursion
    (beginning-of-line)
    (while (and (not (bobp))
                (looking-at "[[:space:]]*\\."))
      (forward-line -1)
      (beginning-of-line))
    (when (re-search-forward "\\_<\\([A-Z][[:alnum:]_]*\\)\\_>[[:space:]]*(" (line-end-position) t)
      (match-string-no-properties 1))))

(defun chief/kotlin-external-definition-context ()
  "Return external-source context at point, or nil.

The returned plist contains at least :fqcn and optionally :member."
  (let* ((symbol (chief/kotlin-current-symbol))
         (imports (chief/kotlin-import-alist))
         (import-at-point (chief/kotlin-import-at-point)))
    (cond
     ((and import-at-point
           (chief/kotlin-external-package-p import-at-point))
      (list :fqcn import-at-point
            :member (car (last (split-string import-at-point "\\." t)))))
     ((when-let* ((fqcn (cdr (assoc symbol imports)))
                  ((chief/kotlin-external-package-p fqcn)))
        (list :fqcn fqcn :member symbol)))
     ((when-let* ((receiver (chief/kotlin-qualified-receiver-at-point))
                  (fqcn (cdr (assoc receiver imports)))
                  ((chief/kotlin-external-package-p fqcn)))
        (list :fqcn fqcn :member symbol)))
     ((when-let* ((root-class (chief/kotlin-chain-root-class))
                  (fqcn (cdr (assoc root-class imports)))
                  ((chief/kotlin-external-package-p fqcn)))
        (list :fqcn fqcn :member symbol)))
     ((when-let* ((fqcn
                   (cl-find-if #'chief/kotlin-find-external-source-file
                               (chief/kotlin-default-import-fqcns symbol))))
        (list :fqcn fqcn :member symbol)))
     (t nil))))

(defun chief/kotlin-source-jars ()
  "Return dependency source jars available for Kotlin/Java navigation."
  (or chief/kotlin-source-jars-cache
      (setq chief/kotlin-source-jars-cache
            (append
             (when-let* ((gradle-cache (expand-file-name "~/.gradle/caches/modules-2/files-2.1")))
               (when (file-directory-p gradle-cache)
                 (directory-files-recursively gradle-cache "-sources\\.jar\\'")))
             (when-let* ((m2 (expand-file-name "~/.m2/repository")))
               (when (file-directory-p m2)
                 (directory-files-recursively m2 "-sources\\.jar\\'")))))))

(defun chief/kotlin-jdk-source-zips ()
  "Return JDK source zip archives available on this machine."
  (or chief/kotlin-jdk-source-zips-cache
      (setq chief/kotlin-jdk-source-zips-cache
            (seq-filter
             #'file-exists-p
             (append
              (when-let* ((sdkman-java (expand-file-name "~/.sdkman/candidates/java")))
                (when (file-directory-p sdkman-java)
                  (directory-files-recursively sdkman-java "src\\.zip\\'" t)))
              (when (file-directory-p "/opt/homebrew/Cellar")
                (directory-files-recursively "/opt/homebrew/Cellar" "src\\.zip\\'" t)))))))

(defun chief/kotlin-source-entry-candidates (fqcn)
  "Return likely source-archive entry paths for FQCN."
  (let* ((path (replace-regexp-in-string "\\." "/" fqcn))
         (base-java (concat path ".java"))
         (base-kt (concat path ".kt")))
    (delete-dups
     (append
      (list base-java base-kt
            (concat "java.base/" base-java)
            (concat "java.base/" base-kt))
      (when (string-prefix-p "javax." fqcn)
        (list (concat "java.base/" base-java)))
      (when (string-prefix-p "java." fqcn)
        (list (concat "java.base/" base-java)))))))

(defun chief/kotlin-fqcn-package (fqcn)
  "Return the package portion of FQCN."
  (when-let* ((parts (split-string fqcn "\\." t))
              ((> (length parts) 1)))
    (string-join (butlast parts) ".")))

(defun chief/kotlin-fqcn-class (fqcn)
  "Return the simple class name from FQCN."
  (car (last (split-string fqcn "\\." t))))

(defun chief/kotlin-package-regexp (package)
  "Return a regexp matching PACKAGE declarations."
  (format "^[[:space:]]*package[[:space:]]+%s\\_>"
          (regexp-quote package)))

(defun chief/kotlin-declaration-regexp (symbol)
  "Return a regexp matching Kotlin or Java declarations for SYMBOL."
  (format
   "\\_<\\(?:annotation[[:space:]\n]+class\\|enum[[:space:]\n]+class\\|value[[:space:]\n]+class\\|inline[[:space:]\n]+class\\|data[[:space:]\n]+class\\|sealed[[:space:]\n]+class\\|expect[[:space:]\n]+class\\|actual[[:space:]\n]+class\\|class\\|interface\\|object\\|typealias\\|enum\\)\\_>[[:space:]\n]+%s\\_>"
   (regexp-quote symbol)))

(defun chief/kotlin-find-declaration-entry-in-zip (zip fqcn)
  "Return a ZIP entry declaring FQCN, or nil."
  (or (gethash (cons zip fqcn) chief/kotlin-declaration-entry-cache)
      (when-let* ((package (chief/kotlin-fqcn-package fqcn))
                  (symbol (chief/kotlin-fqcn-class fqcn)))
        (let* ((package-path (replace-regexp-in-string "\\." "/" package))
               (package-rx (chief/kotlin-package-regexp package))
               (decl-rx (chief/kotlin-declaration-regexp symbol))
               (entries
                (seq-filter
                 (lambda (entry)
                   (and (string-match-p "\\.\\(kt\\|java\\)\\'" entry)
                        (or (string-match-p (concat "/" (regexp-quote package-path) "/") entry)
                            (string-match-p (concat "^" (regexp-quote package-path) "/") entry))))
                 (ignore-errors (process-lines "zipinfo" "-1" zip))))
               (found
                (cl-loop for entry in entries
                         thereis
                         (with-temp-buffer
                           (when (zerop (call-process "unzip" nil t nil "-p" zip entry))
                             (goto-char (point-min))
                             (and (re-search-forward package-rx nil t)
                                  (re-search-forward decl-rx nil t)
                                  entry))))))
          (when found
            (puthash (cons zip fqcn) found chief/kotlin-declaration-entry-cache))
          found))))

(defun chief/kotlin-find-entry-in-zip (zip entry-candidates)
  "Return the first matching entry from ENTRY-CANDIDATES found in ZIP."
  (let ((entries (ignore-errors (process-lines "zipinfo" "-1" zip))))
    (or (cl-find-if (lambda (entry) (member entry entry-candidates)) entries)
        (let* ((basename-rx
                (concat "/"
                        (regexp-quote
                         (file-name-nondirectory (car entry-candidates)))
                        "\\'")))
          (cl-find-if (lambda (entry) (string-match-p basename-rx entry)) entries)))))

(defun chief/kotlin-extract-zip-entry (zip entry)
  "Extract ENTRY from ZIP into a cached source file and return the path."
  (let* ((cache-root (expand-file-name "kotlin-external/" chief/var-directory))
         (zip-id (secure-hash 'sha1 zip))
         (target (expand-file-name (concat zip-id "/" entry) cache-root)))
    (unless (file-exists-p target)
      (make-directory (file-name-directory target) t)
      (with-temp-file target
        (let ((status (call-process "unzip" nil (current-buffer) nil "-p" zip entry)))
          (unless (zerop status)
            (delete-file target)
            (user-error "Failed to extract %s from %s" entry zip)))))
    target))

(defun chief/kotlin-find-external-source-file (fqcn)
  "Return a local source file path for external FQCN, or nil."
  (or (gethash fqcn chief/kotlin-external-source-cache)
      (let* ((entries (chief/kotlin-source-entry-candidates fqcn))
             (zips (if (or (string-prefix-p "java." fqcn)
                           (string-prefix-p "javax." fqcn))
                       (chief/kotlin-jdk-source-zips)
                     (chief/kotlin-source-jars)))
             (found
              (cl-loop for zip in zips
                       for entry = (or (chief/kotlin-find-entry-in-zip zip entries)
                                       (chief/kotlin-find-declaration-entry-in-zip zip fqcn))
                       when entry
                       return (chief/kotlin-extract-zip-entry zip entry))))
        (when found
          (puthash fqcn found chief/kotlin-external-source-cache))
        found)))

(defun chief/kotlin-goto-external-source ()
  "Jump to external library or JDK source for the symbol at point."
  (interactive)
  (let ((origin-root (chief/jvm-project-root)))
    (if-let* ((context (chief/kotlin-external-definition-context))
            (fqcn (plist-get context :fqcn))
            (member (plist-get context :member))
            (target (chief/kotlin-find-external-source-file fqcn)))
        (progn
          (xref-push-marker-stack)
          (chief/jvm-register-external-root target origin-root)
          (find-file target)
          (setq-local chief/jvm-project-root-override origin-root)
          (when (and (fboundp 'chief/java-mode-setup)
                     (derived-mode-p 'java-mode 'java-ts-mode))
            (chief/java-mode-setup))
          (goto-char (point-min))
          (unless (or (and member
                           (re-search-forward
                            (format "\\_<%s\\_>[[:space:]\n]*("
                                    (regexp-quote member))
                            nil
                            t))
                      (re-search-forward
                       (format "\\_<%s\\_>" (regexp-quote (car (last (split-string fqcn "\\." t)))))
                       nil
                       t))
            (goto-char (point-min)))
          t)
      nil)))

(defun chief/kotlin-goto-definition ()
  "Jump to definition, including external Kotlin/Java library sources."
  (interactive)
  (or (chief/kotlin-goto-external-source)
      (chief/lsp-goto-definition-direct)))

(put 'kotlin-mode 'chief/lsp-definition-function #'chief/kotlin-goto-definition)
(put 'kotlin-ts-mode 'chief/lsp-definition-function #'chief/kotlin-goto-definition)

(defun chief/kotlin-compile (command name &optional directory)
  "Run Kotlin COMMAND in DIRECTORY using compilation buffer NAME."
  (chief/jvm-compile command name (or directory (chief/jvm-project-root))))

(defun chief/kotlin-project-build-tool ()
  "Return the primary build tool for the current Kotlin context."
  (cond
   ((chief/jvm-project-build-tool))
   ((and buffer-file-name (string-match-p "\\.kts\\'" buffer-file-name))
    'script)
   (t 'plain)))

(defun chief/kotlin-build-project ()
  "Build the current Kotlin project."
  (interactive)
  (pcase (chief/kotlin-project-build-tool)
    ('gradle
     (chief/kotlin-compile
      (chief/jvm-gradle-command "build")
      "*gradle build*"
      (chief/jvm-project-root)))
    ('maven
     (chief/kotlin-compile
      (chief/jvm-maven-command "compile")
      "*mvn compile*"
      (chief/jvm-project-root)))
    ('script
     (chief/kotlin-compile
      (list (or (executable-find "kotlinc")
                (user-error "kotlinc is not available on PATH"))
            "-script"
            (expand-file-name buffer-file-name))
      "*kotlinc script*"
      (chief/jvm-project-root)))
    (_
     (chief/kotlin-compile
      (list (or (executable-find "kotlinc")
                (user-error "kotlinc is not available on PATH"))
            (expand-file-name buffer-file-name))
      "*kotlinc build*"
      (chief/jvm-project-root)))))

(defun chief/kotlin-test-project ()
  "Run tests for the current Kotlin project."
  (interactive)
  (pcase (chief/kotlin-project-build-tool)
    ('gradle
     (chief/kotlin-compile
      (chief/jvm-gradle-command "test")
      "*gradle test*"
      (chief/jvm-project-root)))
    ('maven
     (chief/kotlin-compile
      (chief/jvm-maven-command "test")
      "*mvn test*"
      (chief/jvm-project-root)))
    (_
     (user-error "No project test command is configured for this Kotlin buffer"))))

(defun chief/kotlin-run-project ()
  "Run the current Kotlin project."
  (interactive)
  (pcase (chief/kotlin-project-build-tool)
    ('gradle
     (chief/kotlin-compile
      (chief/jvm-gradle-command "run")
      "*gradle run*"
      (chief/jvm-project-root)))
    ('maven
     (chief/kotlin-compile
      (chief/jvm-maven-command "exec:java")
      "*mvn exec:java*"
      (chief/jvm-project-root)))
    ('script
     (chief/kotlin-compile
      (list (or (executable-find "kotlinc")
                (user-error "kotlinc is not available on PATH"))
            "-script"
            (expand-file-name buffer-file-name))
      "*kotlinc script*"
      (chief/jvm-project-root)))
    (_
     (chief/kotlin-compile
      (list (or (executable-find "kotlin")
                (user-error "kotlin is not available on PATH"))
            (expand-file-name buffer-file-name))
      "*kotlin run*"
      (chief/jvm-project-root)))))

(defun chief/kotlin-mode-build-command ()
  "Return the preferred compile command string for Kotlin buffers."
  (pcase (chief/kotlin-project-build-tool)
    ('gradle (chief/jvm-command-string (chief/jvm-gradle-command "build")))
    ('maven (chief/jvm-command-string (chief/jvm-maven-command "compile")))
    ('script
     (chief/jvm-command-string
      (list (or (executable-find "kotlinc")
                (user-error "kotlinc is not available on PATH"))
            "-script"
            (expand-file-name buffer-file-name))))
    (_
     (chief/jvm-command-string
      (list (or (executable-find "kotlinc")
                (user-error "kotlinc is not available on PATH"))
            (expand-file-name buffer-file-name))))))

(defun chief/kotlin-repl-command ()
  "Return the command and args for the Kotlin REPL."
  (chief/jvm-wrap-command
   (list (or (executable-find "kotlinc")
             (user-error "kotlinc is not available on PATH"))
         "-Xrepl")))

(defun chief/kotlin-mode-setup ()
  "Configure Kotlin buffers."
  (setq-local compile-command (chief/kotlin-mode-build-command))
  (setq-local chief/lsp-root-function #'chief/jvm-project-root)
  (setq-local chief/lsp-definition-function #'chief/kotlin-goto-definition)
  (setq-local lsp-enabled-clients '(kotlin-lsp-custom))
  (setq-local lsp-eldoc-render-all t)
  (setq-local kotlin-command (car (chief/kotlin-repl-command)))
  (setq-local kotlin-args-repl (cdr (chief/kotlin-repl-command)))
  (chief/repl-configure
   :start #'kotlin-repl
   :restart #'kotlin-repl
   :send-line #'kotlin-send-line
   :send-region #'kotlin-send-region
   :send-buffer #'kotlin-send-buffer
   :send-defun #'kotlin-send-block
   :load-file #'kotlin-send-buffer)
  (setq lsp-kotlin-inlayhints-enable-typehints t
        lsp-kotlin-inlayhints-enable-parameterhints t
        lsp-kotlin-inlayhints-enable-chainedhints t
        lsp-kotlin-completion-snippets-enabled t
        lsp-kotlin-debug-adapter-enabled t)
  (when (fboundp 'chief/lsp-managed-mode-setup)
    (chief/lsp-managed-mode-setup)))

(chief/safe-use-package kotlin-mode
  :mode ("\\.kt\\'" . kotlin-mode)
  :mode ("\\.kts\\'" . kotlin-mode)
  :hook (kotlin-mode . chief/kotlin-mode-setup))

(with-eval-after-load 'kotlin-mode
  (when (fboundp 'evil-define-key)
    (dolist (state '(normal motion))
      (evil-define-key state kotlin-mode-map (kbd "gd") #'chief/kotlin-goto-definition)))
  (chief/repl-setup-standard-local-leader 'kotlin-mode-map)
  (chief/local-leader-def
    :keymaps 'kotlin-mode-map
    "c" '(:ignore t :which-key "kotlin")
    "cb" #'chief/kotlin-build-project
    "ct" #'chief/kotlin-test-project
    "cr" #'chief/kotlin-run-project))

(when (fboundp 'kotlin-ts-mode)
  (with-eval-after-load 'kotlin-ts-mode
    (when (fboundp 'evil-define-key)
      (dolist (state '(normal motion))
        (evil-define-key state kotlin-ts-mode-map (kbd "gd") #'chief/kotlin-goto-definition)))))

(provide 'lang-kotlin)
;;; lang-kotlin.el ends here
