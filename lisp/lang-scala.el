;;; lang-scala.el --- Modern Scala and Metals tooling -*- lexical-binding: t; -*-

;;; Commentary:
;; Tree-sitter editing, Metals, project builds, debugging, and REPL support for
;; current Scala projects.

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'comint)
(require 'core-projects)
(require 'core-repl)
(require 'lang-java)
(require 'seq)
(require 'subr-x)
(require 'vc)

(declare-function dap-breakpoint-toggle "dap-mode" ())
(declare-function dap-debug "dap-mode" (debug-args))
(declare-function lsp-avy-lens "lsp-lens" ())
(declare-function lsp-execute-code-action "lsp-mode" ())
(declare-function lsp-install-server "lsp-mode" (update? &optional server-id))
(declare-function lsp-organize-imports "lsp-mode" ())
(declare-function lsp-treemacs-call-hierarchy "lsp-treemacs" ())
(declare-function lsp-treemacs-type-hierarchy "lsp-treemacs" ())
(declare-function lsp-metals-analyze-stacktrace "lsp-metals" ())
(declare-function lsp-metals-build-connect "lsp-metals" ())
(declare-function lsp-metals-build-import "lsp-metals" ())
(declare-function lsp-metals-copy-worksheet-output "lsp-metals" ())
(declare-function lsp-metals-doctor-run "lsp-metals" ())
(declare-function lsp-metals-goto-super-method "lsp-metals" ())
(declare-function lsp-metals-new-scala-file "lsp-metals" ())
(declare-function lsp-metals-open-server-log "lsp-metals" ())
(declare-function lsp-metals-reset-workspace "lsp-metals" ())
(declare-function lsp-metals-restart-build-server "lsp-metals" ())
(declare-function lsp-metals-run-scalafix "lsp-metals" ())
(declare-function lsp-metals-sources-scan "lsp-metals" ())
(declare-function lsp-metals-super-method-hierarchy "lsp-metals" ())
(declare-function lsp-metals-toggle-inlay-hints-enable-hints-in-pattern-match "lsp-metals" ())
(declare-function lsp-metals-toggle-inlay-hints-enable-implicit-arguments "lsp-metals" ())
(declare-function lsp-metals-toggle-inlay-hints-enable-implicit-conversions "lsp-metals" ())
(declare-function lsp-metals-toggle-inlay-hints-enable-inferred-types "lsp-metals" ())
(declare-function lsp-metals-toggle-inlay-hints-enable-type-parameters "lsp-metals" ())
(declare-function lsp-metals-view-javap "lsp-metals" ())
(declare-function lsp-metals-view-semanticdb-compact "lsp-metals" ())
(declare-function lsp-metals-view-semanticdb-detailed "lsp-metals" ())
(declare-function lsp-metals-view-tasty-decoded "lsp-metals" ())
(declare-function run-scala "sbt-mode" ())
(declare-function sbt-command "sbt-mode" (command &optional focus))
(declare-function sbt-start "sbt-mode" ())
(declare-function sbt:buffer-name "sbt-mode-buffer" ())
(declare-function sbt:paste-region "sbt-mode-comint" (start end &optional no-exit))
(declare-function sbt:run-sbt "sbt-mode" (&optional kill-existing-p pop-p))

(defvar chief/lsp-managed-major-modes)
(defvar lsp-completion-enable)
(defvar lsp-enabled-clients)
(defvar lsp-eldoc-render-all)
(defvar lsp-lens-enable)
(defvar lsp-metals-enable-semantic-highlighting)
(defvar lsp-metals-inlay-hints-enable-hints-in-pattern-match)
(defvar lsp-metals-inlay-hints-enable-implicit-arguments)
(defvar lsp-metals-inlay-hints-enable-implicit-conversions)
(defvar lsp-metals-inlay-hints-enable-inferred-types)
(defvar lsp-metals-inlay-hints-enable-type-parameters)
(defvar lsp-semantic-tokens-enable)
(defvar sbt:buffer-project-root)
(defvar sbt:prefer-nested-projects)
(defvar sbt:program-name)
(defvar sbt:submode)

(defgroup chief-scala nil
  "Scala editing, build, REPL, and Metals integration."
  :group 'chief)

(defcustom chief/scala-sbt-batch-options
  '("-Dsbt.supershell=false" "-Dsbt.color=false")
  "Options placed before one-shot sbt tasks."
  :type '(repeat string)
  :group 'chief-scala)

(defcustom chief/scala-mill-build-task "__.compile"
  "Mill task used to compile all modules."
  :type 'string
  :group 'chief-scala)

(defcustom chief/scala-mill-test-task "__.test"
  "Mill task used to test all modules."
  :type 'string
  :group 'chief-scala)

(defcustom chief/scala-mill-run-task "__.run"
  "Mill task used by `chief/scala-run-project'."
  :type 'string
  :group 'chief-scala)

(defcustom chief/scala-mill-repl-task "repl"
  "Mill task used to start the project REPL.
Multi-module builds can set this to a module-qualified task in directory locals."
  :type 'string
  :group 'chief-scala)

(defcustom chief/scala-metals-root-scope 'module
  "Root scope used for Metals workspaces.
`module' uses the nearest build boundary so lsp-metals can add each opened
monorepo module as a separate workspace folder.  `workspace' explicitly uses
the containing VCS root, or the outermost ancestor build boundary when no VCS
root exists."
  :type '(choice (const :tag "Monorepo workspace" workspace)
                 (const :tag "Nearest build module" module))
  :group 'chief-scala)

(defvar-local chief/scala-build-tool-override nil
  "Build tool forced for the current Scala buffer, or nil for auto detection.")

(defvar-local chief/scala-workspace-root-override nil
  "Explicit outer Scala workspace root, or nil for automatic detection.")

(put 'chief/scala-build-tool-override 'safe-local-variable
     (lambda (value)
       (memq value '(nil sbt mill gradle maven scala-cli))))
(put 'chief/scala-workspace-root-override 'safe-local-variable
     (lambda (value) (or (null value) (stringp value))))
(put 'chief/scala-mill-repl-task 'safe-local-variable #'stringp)
(put 'chief/scala-metals-root-scope 'safe-local-variable
     (lambda (value) (memq value '(workspace module))))

(defconst chief/scala-project-root-markers
  '("build.sbt"
    "project/build.properties"
    "project/plugins.sbt"
    "build.sc"
    "build.mill"
    ".mill-version"
    "settings.gradle.kts"
    "settings.gradle"
    "build.gradle.kts"
    "build.gradle"
    "pom.xml"
    "project.scala"
    "scala-cli.yaml"
    "scala-cli.yml"
    "scala-cli.conf")
  "Project markers understood by the Scala integration.")

(defconst chief/scala-workspace-scan-ignored-directories
  '(".bloop" ".bsp" ".git" ".gradle" ".metals" ".scala-build"
    "build" "node_modules" "out" "target")
  "Generated directories skipped when discovering monorepo build roots.")

(defun chief/scala-ensure-local-project (&optional directory)
  "Reject build and REPL operations for remote DIRECTORY."
  (when (file-remote-p (or directory default-directory))
    (user-error "Scala build and REPL commands are local-only")))

(defun chief/scala--start-directory (&optional start)
  "Return a normalized directory from START or the current buffer."
  (let ((path (expand-file-name
               (or start
                   (and buffer-file-name (file-name-directory buffer-file-name))
                   default-directory))))
    (file-name-as-directory
     (if (file-directory-p path) path (file-name-directory path)))))

(defun chief/scala--vc-root (&optional start)
  "Return the containing VCS root for START, if one exists."
  (let ((default-directory (chief/scala--start-directory start)))
    (when-let* ((root (or (locate-dominating-file default-directory ".git")
                          (ignore-errors (vc-root-dir)))))
      (file-name-as-directory (expand-file-name root)))))

(defun chief/scala--ancestor-directories (&optional start limit)
  "Return ancestor directories from START through LIMIT, nearest first."
  (let ((directory (chief/scala--start-directory start))
        (limit (and limit (file-name-as-directory (expand-file-name limit))))
        directories
        done)
    (while (and directory (not done))
      (push directory directories)
      (setq done (or (equal directory limit)
                     (equal directory
                            (file-name-directory (directory-file-name directory)))))
      (unless done
        (setq directory
              (file-name-directory (directory-file-name directory)))))
    (nreverse directories)))

(defun chief/scala--matching-file-p (directory regexp)
  "Return non-nil when DIRECTORY contains a basename matching REGEXP."
  (and (file-directory-p directory)
       (seq-some (lambda (name) (string-match-p regexp name))
                 (directory-files directory nil directory-files-no-dot-files-regexp t))))

(defun chief/scala-build-tools-at-root (root)
  "Return build tools whose defining markers exist directly at ROOT."
  (let ((root (file-name-as-directory (expand-file-name root)))
        tools)
    (when (or (file-exists-p (expand-file-name "build.sbt" root))
              (file-exists-p (expand-file-name "project/build.properties" root))
              (file-exists-p (expand-file-name "project/plugins.sbt" root))
              (chief/scala--matching-file-p root "\\.sbt\\'"))
      (push 'sbt tools))
    (when (or (file-exists-p (expand-file-name "build.sc" root))
              (file-exists-p (expand-file-name "build.mill" root))
              (file-exists-p (expand-file-name ".mill-version" root)))
      (push 'mill tools))
    (when (or (file-exists-p (expand-file-name "settings.gradle.kts" root))
              (file-exists-p (expand-file-name "settings.gradle" root))
              (file-exists-p (expand-file-name "build.gradle.kts" root))
              (file-exists-p (expand-file-name "build.gradle" root)))
      (push 'gradle tools))
    (when (file-exists-p (expand-file-name "pom.xml" root))
      (push 'maven tools))
    (when (or (file-exists-p (expand-file-name "project.scala" root))
              (file-exists-p (expand-file-name "scala-cli.yaml" root))
              (file-exists-p (expand-file-name "scala-cli.yml" root))
              (file-exists-p (expand-file-name "scala-cli.conf" root)))
      (push 'scala-cli tools))
    (nreverse tools)))

(defun chief/scala-ancestor-build-boundaries (&optional start)
  "Return ancestor build boundaries for START, ordered nearest first.
Each entry is a plist with `:root' and `:tools' keys.  Ancestors above the
containing VCS root are never considered."
  (let ((limit (chief/scala--vc-root start)))
    (seq-keep
     (lambda (root)
       (when-let* ((tools (chief/scala-build-tools-at-root root)))
         (list :root root :tools tools)))
     (chief/scala--ancestor-directories start limit))))

(defun chief/scala-project-root (&optional start)
  "Return the nearest Scala build boundary for START.
Unlike marker-by-marker lookup, this compares ancestor distance before build
tool precedence, so an outer build can never shadow a nearer nested build."
  (or (plist-get (car (chief/scala-ancestor-build-boundaries start)) :root)
      (chief/scala--vc-root start)
      (chief/project-current-root (chief/scala--start-directory start))
      (chief/scala--start-directory start)))

(defun chief/scala-workspace-root (&optional start)
  "Return the outer Scala monorepo workspace root containing START."
  (or (and chief/scala-workspace-root-override
           (file-name-as-directory
            (expand-file-name chief/scala-workspace-root-override)))
      (chief/scala--vc-root start)
      (plist-get (car (last (chief/scala-ancestor-build-boundaries start))) :root)
      (chief/scala-project-root start)))

(defun chief/scala-metals-root ()
  "Return the configured Metals root for the current Scala buffer."
  (pcase chief/scala-metals-root-scope
    ('module (chief/scala-project-root))
    (_ (chief/scala-workspace-root))))

(defun chief/scala-project-build-tool (&optional root)
  "Return the preferred build tool at ROOT or the nearest boundary."
  (or chief/scala-build-tool-override
      (car (chief/scala-build-tools-at-root
            (or root (chief/scala-project-root))))
      'scala-cli))

(defun chief/scala--project-wrapper (root &rest names)
  "Return the nearest executable wrapper in NAMES at or above ROOT."
  (let* ((root (file-name-as-directory
                (expand-file-name (or root (chief/scala-project-root)))))
         (limit (chief/scala--vc-root root)))
    (cl-loop for directory in (chief/scala--ancestor-directories root limit)
             thereis
             (cl-loop for name in names
                      for path = (expand-file-name name directory)
                      when (file-executable-p path)
                      return path))))

(defun chief/scala-tool-executable (tool &optional root)
  "Return the preferred executable for Scala build TOOL at ROOT."
  (let ((root (or root (chief/scala-project-root))))
    (chief/scala-ensure-local-project root)
    (or
     (pcase tool
       ('sbt (or (chief/scala--project-wrapper root "sbtw" "sbt")
                 (executable-find "sbt")))
       ('mill (or (chief/scala--project-wrapper root "mill")
                  (executable-find "mill")))
       ('gradle (or (chief/scala--project-wrapper root "gradlew")
                    (executable-find "gradle")))
       ('maven (or (chief/scala--project-wrapper root "mvnw")
                   (executable-find "mvn")))
       ('scala-cli (or (chief/scala--project-wrapper root "scala-cli")
                       (executable-find "scala-cli"))))
     (user-error "No executable is available for Scala build tool %s" tool))))

(defun chief/scala--tool-command (tool arguments &optional root)
  "Return a command list for TOOL at ROOT followed by ARGUMENTS."
  (append
   (list (chief/scala-tool-executable tool root))
   (pcase tool
     ('sbt chief/scala-sbt-batch-options)
     ('gradle '("--console=plain"))
     ('maven '("--no-transfer-progress"))
     (_ nil))
   arguments))

(defconst chief/scala-build-marker-file-regexp
  (concat "\\(?:build\\.sbt\\|build\\.sc\\|build\\.mill\\|\\.mill-version"
          "\\|settings\\.gradle\\(?:\\.kts\\)?\\|build\\.gradle\\(?:\\.kts\\)?"
          "\\|pom\\.xml\\|project\\.scala\\|scala-cli\\.ya?ml"
          "\\|scala-cli\\.conf\\|build\\.properties\\|plugins\\.sbt\\)\\'")
  "Regexp matching files that can define a Scala build boundary.")

(defun chief/scala--workspace-scan-directory-p (directory)
  "Return non-nil when monorepo discovery should descend into DIRECTORY."
  (not (member (file-name-nondirectory (directory-file-name directory))
               chief/scala-workspace-scan-ignored-directories)))

(defun chief/scala--marker-root (file)
  "Return the build root represented by marker FILE."
  (let* ((directory (file-name-directory file))
         (basename (file-name-nondirectory file)))
    (if (and (member basename '("build.properties" "plugins.sbt"))
             (equal (file-name-nondirectory (directory-file-name directory))
                    "project"))
        (file-name-directory (directory-file-name directory))
      directory)))

(defun chief/scala-workspace-build-targets (&optional workspace)
  "Discover all build root/tool pairs under WORKSPACE.
Results are cons cells of normalized root directory and tool symbol.  Generated
build directories are excluded, and discovery runs only when explicitly used."
  (let* ((workspace (file-name-as-directory
                     (expand-file-name (or workspace (chief/scala-workspace-root)))))
         (files (directory-files-recursively
                 workspace chief/scala-build-marker-file-regexp nil
                 #'chief/scala--workspace-scan-directory-p))
         (seen (make-hash-table :test #'equal))
         targets)
    (dolist (file files)
      (let ((root (file-name-as-directory
                   (expand-file-name (chief/scala--marker-root file)))))
        (dolist (tool (chief/scala-build-tools-at-root root))
          (let ((key (cons root tool)))
            (unless (gethash key seen)
              (puthash key t seen)
              (push key targets))))))
    (sort targets
          (lambda (left right)
            (string-lessp
             (format "%s:%s" (file-relative-name (car left) workspace) (cdr left))
             (format "%s:%s" (file-relative-name (car right) workspace) (cdr right)))))))

(defun chief/scala-read-build-target (&optional prompt)
  "Read a monorepo build target using PROMPT."
  (let* ((workspace (chief/scala-workspace-root))
         (current-root (chief/scala-project-root))
         (current-tool (chief/scala-project-build-tool current-root))
         (targets (chief/scala-workspace-build-targets workspace))
         (choices
          (mapcar
           (lambda (target)
             (cons (format "%-9s %s"
                           (cdr target)
                           (file-relative-name (car target) workspace))
                   target))
           targets))
         (default
          (car (rassoc (cons current-root current-tool) choices))))
    (unless choices
      (user-error "No Scala build roots found under %s" workspace))
    (cdr (assoc (completing-read (or prompt "Scala build target: ")
                                 choices nil t nil nil default)
                choices))))

(defun chief/scala--operation-arguments (tool operation)
  "Return TOOL arguments for build OPERATION."
  (pcase tool
    ('sbt
     (pcase operation
       ('build '("compile"))
       ('test '("test"))
       ('run '("run"))
       ('clean '("clean"))))
    ('mill
     (pcase operation
       ('build (list chief/scala-mill-build-task))
       ('test (list chief/scala-mill-test-task))
       ('run (list chief/scala-mill-run-task))
       ('clean '("clean"))))
    ('gradle
     (pcase operation
       ('build '("build"))
       ('test '("test"))
       ('run '("run"))
       ('clean '("clean"))))
    ('maven
     (pcase operation
       ('build '("package"))
       ('test '("test"))
       ('run '("exec:java"))
       ('clean '("clean"))))
    ('scala-cli
     (pcase operation
       ('build '("compile" "."))
       ('test '("test" "."))
       ('run '("run" "."))
       ('clean '("clean" "."))))))

(defun chief/scala-operation-command (operation &optional root tool)
  "Return a project-aware command list for OPERATION at ROOT using TOOL."
  (let* ((root (or root (chief/scala-project-root)))
         (tool (or tool (chief/scala-project-build-tool root)))
         (arguments (chief/scala--operation-arguments tool operation)))
    (unless arguments
      (user-error "Scala tool %s does not define operation %s" tool operation))
    (chief/scala--tool-command tool arguments root)))

(defun chief/scala-command-string (command)
  "Return shell-safe JVM-wrapped COMMAND text."
  (chief/jvm-command-string command))

(defun chief/scala-compile (command name &optional directory)
  "Run COMMAND at DIRECTORY in compilation buffer NAME."
  (let ((directory (or directory (chief/scala-project-root))))
    (chief/scala-ensure-local-project directory)
    (let ((default-directory directory)
          (compilation-read-command nil))
      (compilation-start
       (chief/scala-command-string command)
       'compilation-mode
       (lambda (_) name)))))

(defun chief/scala-run-operation (operation &optional target)
  "Run Scala build OPERATION at TARGET or the nearest build boundary.
TARGET is a cons cell of root directory and build tool."
  (let* ((root (or (car-safe target) (chief/scala-project-root)))
         (tool (or (cdr-safe target) (chief/scala-project-build-tool root))))
    (chief/scala-compile
     (chief/scala-operation-command operation root tool)
     (format "*scala %s %s:%s*"
             tool operation (chief/scala-project-label root))
     root)))

(defun chief/scala--operation-target (choose-target)
  "Return a selected target when CHOOSE-TARGET is non-nil."
  (when choose-target
    (chief/scala-read-build-target)))

(defun chief/scala-build-project (&optional choose-target)
  "Build the nearest Scala module.
With prefix CHOOSE-TARGET, select any build root in the monorepo."
  (interactive "P")
  (chief/scala-run-operation 'build
                             (chief/scala--operation-target choose-target)))

(defun chief/scala-test-project (&optional choose-target)
  "Test the nearest Scala module.
With prefix CHOOSE-TARGET, select any build root in the monorepo."
  (interactive "P")
  (chief/scala-run-operation 'test
                             (chief/scala--operation-target choose-target)))

(defun chief/scala-run-project (&optional choose-target)
  "Run the nearest Scala module.
With prefix CHOOSE-TARGET, select any build root in the monorepo."
  (interactive "P")
  (chief/scala-run-operation 'run
                             (chief/scala--operation-target choose-target)))

(defun chief/scala-clean-project (&optional choose-target)
  "Clean the nearest Scala module.
With prefix CHOOSE-TARGET, select any build root in the monorepo."
  (interactive "P")
  (chief/scala-run-operation 'clean
                             (chief/scala--operation-target choose-target)))

(defun chief/scala-run-workspace-operation (operation target)
  "Run selected OPERATION at monorepo build TARGET."
  (interactive
   (list (intern (completing-read "Scala operation: "
                                  '("build" "test" "run" "clean") nil t))
         (chief/scala-read-build-target "Monorepo build target: ")))
  (chief/scala-run-operation operation target))

(defun chief/scala-read-tool-command (&optional choose-target)
  "Read build-tool arguments, selecting a root when CHOOSE-TARGET is non-nil."
  (let* ((target (or (and choose-target (chief/scala-read-build-target))
                     (let ((root (chief/scala-project-root)))
                       (cons root (chief/scala-project-build-tool root)))))
         (root (car target))
         (tool (cdr target))
         (input (read-string
                 (format "%s arguments at %s: "
                         tool (file-relative-name root
                                                  (chief/scala-workspace-root))))))
    (list tool (split-string-and-unquote input) root)))

(defun chief/scala-run-tool-command (tool arguments &optional root)
  "Run TOOL with parsed ARGUMENTS at ROOT in a compilation buffer."
  (interactive (chief/scala-read-tool-command current-prefix-arg))
  (let ((root (or root (chief/scala-project-root))))
    (chief/scala-compile
     (chief/scala--tool-command tool arguments root)
     (format "*scala %s command:%s*" tool (chief/scala-project-label root))
     root)))

(defun chief/scala--compile-command-string ()
  "Return a non-throwing default `compile-command' for Scala buffers."
  (condition-case nil
      (chief/scala-command-string (chief/scala-operation-command 'build))
    (error (mapconcat #'shell-quote-argument
                      '("scala-cli" "compile" ".") " "))))

(defun chief/scala-project-label (&optional root)
  "Return a collision-resistant label for Scala project ROOT."
  (let* ((root (file-name-as-directory
                (expand-file-name (or root (chief/scala-project-root)))))
         (name (file-name-nondirectory (directory-file-name root))))
    (format "%s:%s" name (substring (secure-hash 'sha1 root) 0 8))))

(defun chief/scala-repl-buffer-name (&optional root)
  "Return the project-scoped generic Scala REPL buffer name for ROOT."
  (format "*scala-repl:%s*" (chief/scala-project-label root)))

(defun chief/scala--sbt-call (function &rest arguments)
  "Call sbt FUNCTION at the nearest build boundary with ARGUMENTS."
  (require 'sbt-mode)
  (let* ((root (chief/scala-project-root))
         (default-directory root)
         (sbt:buffer-project-root root)
         (sbt:prefer-nested-projects t)
         (sbt:program-name (chief/scala-tool-executable 'sbt root)))
    (apply function arguments)))

(defun chief/scala-start-sbt-shell ()
  "Start or display the current project's sbt shell."
  (interactive)
  (unless (eq (chief/scala-project-build-tool) 'sbt)
    (user-error "The current Scala project is not an sbt build"))
  (chief/scala--sbt-call #'sbt-start))

(defun chief/scala-sbt-command ()
  "Prompt for and run a command in the current project's sbt shell."
  (interactive)
  (unless (eq (chief/scala-project-build-tool) 'sbt)
    (user-error "The current Scala project is not an sbt build"))
  (chief/scala--sbt-call #'call-interactively #'sbt-command))

(defun chief/scala--sbt-buffer ()
  "Return the current project's sbt buffer, if it exists."
  (require 'sbt-mode)
  (let ((default-directory (chief/scala-project-root)))
    (get-buffer (sbt:buffer-name))))

(defun chief/scala-sbt-repl-ready-p ()
  "Return non-nil when the current project's sbt console is ready."
  (when-let* ((buffer (chief/scala--sbt-buffer)))
    (and (comint-check-proc buffer)
         (memq (buffer-local-value 'sbt:submode buffer)
               '(console paste-mode)))))

(defun chief/scala--generic-repl-command ()
  "Return the best non-sbt Scala REPL command for the current project."
  (let ((tool (chief/scala-project-build-tool)))
    (cond
     ((eq tool 'mill)
      (chief/scala--tool-command 'mill (list chief/scala-mill-repl-task)))
     ((executable-find "scala-cli")
      (list (executable-find "scala-cli") "repl" "."))
     ((executable-find "amm") (list (executable-find "amm")))
     ((executable-find "scala") (list (executable-find "scala")))
     (t (user-error "Install scala-cli, Ammonite, or Scala for REPL support")))))

(defun chief/scala--ensure-generic-repl-buffer ()
  "Return a live project-scoped non-sbt Scala REPL buffer."
  (chief/scala-ensure-local-project)
  (let* ((root (chief/scala-project-root))
         (name (chief/scala-repl-buffer-name root))
         (buffer (get-buffer name)))
    (unless (comint-check-proc buffer)
      (let* ((default-directory root)
             (command (chief/jvm-wrap-command
                       (chief/scala--generic-repl-command))))
        (setq buffer
              (apply #'make-comint-in-buffer
                     "scala-repl" name (car command) nil (cdr command)))
        (with-current-buffer buffer
          (setq-local comint-prompt-read-only t))))
    buffer))

(defun chief/scala-start-repl ()
  "Start or display a project-aware Scala REPL."
  (interactive)
  (chief/scala-ensure-local-project)
  (if (eq (chief/scala-project-build-tool) 'sbt)
      (chief/scala--sbt-call #'run-scala)
    (pop-to-buffer (chief/scala--ensure-generic-repl-buffer))))

(defun chief/scala-restart-repl ()
  "Restart the current project's Scala REPL."
  (interactive)
  (if (eq (chief/scala-project-build-tool) 'sbt)
      (progn
        (chief/scala--sbt-call #'sbt:run-sbt t nil)
        (chief/scala--sbt-call #'sbt-command "console" t))
    (when-let* ((buffer (get-buffer (chief/scala-repl-buffer-name))))
      (when-let* ((process (get-buffer-process buffer)))
        (delete-process process))
      (kill-buffer buffer))
    (pop-to-buffer (chief/scala--ensure-generic-repl-buffer))))

(defun chief/scala-send-region (start end)
  "Send Scala source between START and END to the project REPL."
  (interactive "r")
  (if (eq (chief/scala-project-build-tool) 'sbt)
      (progn
        (unless (chief/scala-sbt-repl-ready-p)
          (user-error "Start the sbt REPL with , s s and wait for the scala> prompt"))
        (chief/scala--sbt-call #'sbt:paste-region start end nil))
    (let ((buffer (chief/scala--ensure-generic-repl-buffer)))
      (comint-send-region buffer start end)
      (comint-send-string buffer "\n")
      (display-buffer buffer))))

(defun chief/scala-send-line ()
  "Send the current Scala line to the project REPL."
  (interactive)
  (chief/scala-send-region (line-beginning-position) (line-end-position)))

(defun chief/scala-send-buffer ()
  "Send the current Scala buffer to the project REPL."
  (interactive)
  (chief/scala-send-region (point-min) (point-max)))

(defun chief/scala-send-defun ()
  "Send the current Scala definition to the project REPL."
  (interactive)
  (save-excursion
    (mark-defun)
    (chief/scala-send-region (region-beginning) (region-end))
    (deactivate-mark)))

(defun chief/scala-load-file ()
  "Load the current Scala file into the project REPL."
  (interactive)
  (unless buffer-file-name
    (user-error "The current Scala buffer is not visiting a file"))
  (chief/scala-send-buffer))

(defun chief/scala-install-metals ()
  "Install or update Metals through lsp-mode's maintained installer."
  (interactive)
  (require 'lsp-metals)
  (lsp-install-server nil 'metals))

(defun chief/scala-toolchain-status ()
  "Display Scala project and toolchain discovery information."
  (interactive)
  (let ((root (chief/scala-project-root))
        (tool (chief/scala-project-build-tool))
        (buffer (get-buffer-create "*Scala Toolchain*")))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "Module: %s\nWorkspace: %s\nMetals root: %s\nBuild tool: %s\nJAVA_HOME: %s\n\n"
                      root
                      (chief/scala-workspace-root)
                      (chief/scala-metals-root)
                      tool
                      (or (chief/jvm-java-home) "<missing>")))
      (dolist (program '("java" "scala" "scalac" "scala-cli" "sbt"
                         "mill" "gradle" "mvn" "metals" "cs" "amm"))
        (insert (format "%-10s %s\n"
                        program (or (executable-find program) "<missing>"))))
      (special-mode))
    (pop-to-buffer buffer)))

(defun chief/scala-mode-setup ()
  "Configure tree-sitter editing, build tools, REPL, and Metals."
  (setq-local tab-width 2)
  (setq-local indent-tabs-mode nil)
  (setq-local treesit-font-lock-level 4)
  (when (and (fboundp 'treesit-font-lock-recompute-features)
             (treesit-ready-p 'scala t))
    (treesit-font-lock-recompute-features)
    (font-lock-flush))
  (setq-local compile-command (chief/scala--compile-command-string))
  (setq-local chief/lsp-root-function #'chief/scala-metals-root)
  (setq-local lsp-enabled-clients '(metals))
  (setq-local lsp-completion-enable t)
  (setq-local lsp-eldoc-render-all t)
  (setq-local lsp-lens-enable t)
  (setq-local lsp-semantic-tokens-enable t)
  (chief/repl-configure
   :start #'chief/scala-start-repl
   :restart #'chief/scala-restart-repl
   :send-line #'chief/scala-send-line
   :send-region #'chief/scala-send-region
   :send-buffer #'chief/scala-send-buffer
   :send-defun #'chief/scala-send-defun
   :load-file #'chief/scala-load-file)
  (when (fboundp 'chief/lsp-managed-mode-setup)
    (chief/lsp-managed-mode-setup)))

;; lsp-metals still declares the classic scala-mode as a package dependency,
;; although its client supports scala-ts-mode directly.  Declaring the modern
;; mode as the provider prevents straight.el from fetching the stale mode.
(use-package scala-ts-mode
  :straight (scala-ts-mode
             :type git
             :host github
             :repo "KaranAhlawat/scala-ts-mode"
             :includes scala-mode)
  :mode (("\\.scala\\'" . scala-ts-mode)
         ("\\.sc\\'" . scala-ts-mode)
         ("\\.sbt\\'" . scala-ts-mode)))

(use-package sbt-mode
  :commands (sbt-start sbt-command run-scala)
  :custom
  (sbt:program-options '("-Dsbt.supershell=false"))
  (sbt:clear-buffer-before-command nil)
  :config
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package jarchive
  :straight (jarchive
             :type git
             :host github
             :repo "emacs-straight/jarchive")
  :demand t
  :config
  (jarchive-mode 1))

(use-package lsp-metals
  :after lsp-mode
  :demand t
  :custom
  (lsp-metals-server-args
   '("-J-Dmetals.allow-multiline-string-formatting=off"
     "-J-Dmetals.icons=unicode"))
  (lsp-metals-java-home (or (chief/jvm-java-home) ""))
  (lsp-metals-scala-cli-launcher (or (executable-find "scala-cli") ""))
  (lsp-metals-enable-semantic-highlighting t)
  (lsp-metals-inlay-hints-enable-inferred-types t)
  (lsp-metals-inlay-hints-enable-implicit-conversions t)
  (lsp-metals-inlay-hints-enable-implicit-arguments t)
  (lsp-metals-inlay-hints-enable-type-parameters t)
  (lsp-metals-inlay-hints-enable-hints-in-pattern-match t)
  (lsp-metals-test-user-interface "Code Lenses"))

(dolist (mode '(scala-ts-mode))
  (add-to-list 'chief/lsp-managed-major-modes mode))

(add-hook 'scala-ts-mode-hook #'chief/scala-mode-setup)

(with-eval-after-load 'scala-ts-mode
  (chief/repl-setup-standard-local-leader 'scala-ts-mode-map)
  (chief/local-leader-def
    :keymaps 'scala-ts-mode-map
    "c" '(:ignore t :which-key "build")
    "cb" #'chief/scala-build-project
    "ct" #'chief/scala-test-project
    "cr" #'chief/scala-run-project
    "cc" #'chief/scala-clean-project
    "c!" #'chief/scala-run-tool-command
    "cW" #'chief/scala-run-workspace-operation
    "cs" #'chief/scala-start-sbt-shell
    "cS" #'chief/scala-sbt-command
    "d" '(:ignore t :which-key "debug/lens")
    "dd" #'dap-debug
    "db" #'dap-breakpoint-toggle
    "dl" #'lsp-avy-lens
    "i" #'chief/scala-toolchain-status
    "m" '(:ignore t :which-key "metals")
    "ma" #'lsp-execute-code-action
    "mo" #'lsp-organize-imports
    "mi" #'lsp-metals-build-import
    "mI" #'chief/scala-install-metals
    "mb" #'lsp-metals-build-connect
    "md" #'lsp-metals-doctor-run
    "ml" #'lsp-metals-open-server-log
    "mr" #'lsp-metals-reset-workspace
    "mR" #'lsp-metals-restart-build-server
    "mf" #'lsp-metals-run-scalafix
    "ms" #'lsp-metals-sources-scan
    "mn" #'lsp-metals-new-scala-file
    "mw" #'lsp-metals-copy-worksheet-output
    "mx" #'lsp-metals-analyze-stacktrace
    "n" '(:ignore t :which-key "navigate")
    "ns" #'lsp-metals-goto-super-method
    "nh" #'lsp-metals-super-method-hierarchy
    "nt" #'lsp-treemacs-type-hierarchy
    "nc" #'lsp-treemacs-call-hierarchy
    "t" '(:ignore t :which-key "types/implicits")
    "tt" #'lsp-metals-toggle-inlay-hints-enable-inferred-types
    "ti" #'lsp-metals-toggle-inlay-hints-enable-implicit-arguments
    "tc" #'lsp-metals-toggle-inlay-hints-enable-implicit-conversions
    "tp" #'lsp-metals-toggle-inlay-hints-enable-type-parameters
    "tm" #'lsp-metals-toggle-inlay-hints-enable-hints-in-pattern-match
    "v" '(:ignore t :which-key "decoded views")
    "vt" #'lsp-metals-view-tasty-decoded
    "vs" #'lsp-metals-view-semanticdb-compact
    "vS" #'lsp-metals-view-semanticdb-detailed
    "vj" #'lsp-metals-view-javap))

(provide 'lang-scala)
;;; lang-scala.el ends here
