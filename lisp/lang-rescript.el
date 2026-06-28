;;; lang-rescript.el --- Modern ReScript tooling -*- lexical-binding: t; -*-

(require 'ansi-color)
(require 'cl-lib)
(require 'comint)
(require 'compile)
(require 'core-format)
(require 'core-projects)
(require 'core-repl)
(require 'json)
(require 'project)
(require 'seq)
(require 'subr-x)

(declare-function chief/local-leader-def "core-evil" (&rest args))
(declare-function chief/lsp-disable-clients "core-lsp" (&rest clients))
(declare-function chief/lsp-managed-mode-setup "core-lsp" ())
(declare-function lsp-register-client "lsp-mode" (client))
(declare-function lsp-stdio-connection "lsp-mode" (command))
(declare-function make-lsp-client "lsp-mode" (&rest args))
(declare-function rescript-mode "rescript-mode" ())
(declare-function rescript-ts-mode "rescript-ts-mode" ())
(declare-function rescript-ts-install-grammar "rescript-ts-mode" (&optional out-dir))
(defvar chief/lsp-managed-major-modes)
(defvar chief/lsp-manual-install-specs)
(defvar lsp-language-id-configuration)
(defvar rescript-mode-map)
(defvar rescript-ts-mode-map)
(defvar treesit-language-source-alist)

(defgroup chief/rescript nil
  "Modern ReScript development support."
  :group 'chief)

(defcustom chief/rescript-cli nil
  "Optional absolute path to the ReScript `rescript' executable.
When nil, prefer the nearest project/monorepo `node_modules/.bin/rescript'
and then search `exec-path'."
  :type '(choice (const :tag "Resolve project-locally" nil) file)
  :group 'chief/rescript)

(defcustom chief/rescript-bsc nil
  "Optional absolute path to the ReScript `bsc' executable used by the REPL."
  :type '(choice (const :tag "Resolve project-locally" nil) file)
  :group 'chief/rescript)

(defcustom chief/rescript-language-server-command nil
  "Optional command used to start the ReScript language server.
When nil, prefer a project-local `rescript-language-server', then PATH, and
append `--stdio'."
  :type '(choice (const :tag "Resolve project-locally" nil)
                 (repeat string))
  :group 'chief/rescript)

(defcustom chief/rescript-node-command nil
  "Optional Node.js executable used for REPL evaluation and generated JS."
  :type '(choice (const :tag "Find node on PATH" nil) file)
  :group 'chief/rescript)

(defcustom chief/rescript-supported-major-version 12
  "ReScript compiler major version supported by this integration.
ReScript CLI arguments changed substantially in version 12.  Rejecting unknown
major versions is safer than silently running legacy or future commands."
  :type 'integer
  :group 'chief/rescript)

(defcustom chief/rescript-format-on-save t
  "When non-nil, format ReScript buffers before saving."
  :type 'boolean
  :group 'chief/rescript)

(defcustom chief/rescript-auto-start-watch nil
  "When non-nil, start one `rescript watch' process per project on file open."
  :type 'boolean
  :group 'chief/rescript)

(defcustom chief/rescript-lsp-prompt-for-build t
  "When non-nil, allow the language server to ask before starting a build."
  :type 'boolean
  :group 'chief/rescript)

(defvar chief/rescript-version-cache (make-hash-table :test #'equal)
  "Compiler versions keyed by resolved ReScript executable path.")

(defvar chief/rescript-watch-buffers (make-hash-table :test #'equal)
  "Live watch compilation buffers keyed by canonical ReScript project root.")

(defun chief/rescript-start-directory (&optional start)
  "Return the normalized directory from which ReScript discovery should START."
  (chief/project-normalize-root
   (or start
       (and buffer-file-name (file-name-directory buffer-file-name))
       default-directory)))

(defun chief/rescript-project-root (&optional start)
  "Return the nearest modern ReScript project root above START.
A current ReScript project is defined by `rescript.json'.  Legacy
`bsconfig.json' is deliberately not treated as modern ReScript configuration."
  (when-let* ((start (chief/rescript-start-directory start))
              (root (locate-dominating-file start "rescript.json")))
    (chief/project-normalize-root root)))

(defun chief/rescript-project-root-required (&optional start)
  "Return the ReScript project root above START or signal a user-facing error."
  (or (chief/rescript-project-root start)
      (user-error "No rescript.json found above this buffer")))

(defun chief/rescript-find-upward-executable (name &optional start)
  "Return the nearest executable NAME under an ancestor `node_modules/.bin'."
  (when-let* ((start (chief/rescript-start-directory start))
              (root
               (locate-dominating-file
                start
                (lambda (directory)
                  (file-executable-p
                   (expand-file-name (concat "node_modules/.bin/" name)
                                     directory))))))
    (expand-file-name (concat "node_modules/.bin/" name) root)))

(defun chief/rescript-resolve-executable (name &optional override noerror)
  "Resolve executable NAME, preferring OVERRIDE and project-local installs.
When NOERROR is nil, signal a user-facing error if no executable is available."
  (let ((program
         (or (and override
                  (file-executable-p (expand-file-name override))
                  (expand-file-name override))
             (chief/rescript-find-upward-executable name)
             (executable-find name))))
    (or program
        (unless noerror
          (user-error
           "%s is unavailable. Install project tooling with M-x chief/rescript-install-tooling"
           name)))))

(defun chief/rescript-executable (&optional noerror)
  "Return the selected ReScript CLI executable.
When NOERROR is non-nil, return nil instead of signaling if it is unavailable."
  (chief/rescript-resolve-executable "rescript" chief/rescript-cli noerror))

(defun chief/rescript-bsc-executable (&optional noerror)
  "Return the selected ReScript `bsc' executable.
When NOERROR is non-nil, return nil instead of signaling if it is unavailable."
  (or (chief/rescript-resolve-executable "bsc" chief/rescript-bsc t)
      (when-let* ((rescript (chief/rescript-executable t))
                  (sibling (expand-file-name "bsc" (file-name-directory rescript)))
                  ((file-executable-p sibling)))
        sibling)
      (unless noerror
        (user-error "bsc is unavailable; install the project-local `rescript' package"))))

(defun chief/rescript-node-executable (&optional noerror)
  "Return the selected Node.js executable.
When NOERROR is non-nil, return nil instead of signaling if it is unavailable."
  (let ((node (or (and chief/rescript-node-command
                       (file-executable-p (expand-file-name chief/rescript-node-command))
                       (expand-file-name chief/rescript-node-command))
                  (executable-find "node"))))
    (or node
        (unless noerror
          (user-error "Node.js is required to execute ReScript-generated JavaScript")))))

(defun chief/rescript-lsp-server-command ()
  "Return the project-aware ReScript language-server command."
  (or chief/rescript-language-server-command
      (when-let* ((server (chief/rescript-resolve-executable
                           "rescript-language-server" nil t)))
        (list server "--stdio"))
      '("rescript-language-server" "--stdio")))

(defun chief/rescript-version (&optional refresh)
  "Return the selected ReScript compiler version string.
With REFRESH non-nil, discard the cached result first."
  (let* ((program (chief/rescript-executable))
         (cached (and (not refresh)
                      (gethash program chief/rescript-version-cache))))
    (or cached
        (let* ((default-directory (chief/rescript-project-root-required))
               (output (condition-case err
                           (car (process-lines program "--version"))
                         (error
                          (user-error "Cannot run %s --version: %s"
                                      program (error-message-string err)))))
               (version (and output
                             (string-match "[0-9]+\\(?:\\.[0-9]+\\)+" output)
                             (match-string 0 output))))
          (unless version
            (user-error "Cannot parse ReScript version from: %s" (or output "<no output>")))
          (puthash program version chief/rescript-version-cache)
          version))))

(defun chief/rescript-require-supported-cli ()
  "Require and return the configured supported ReScript CLI executable."
  (let* ((program (chief/rescript-executable))
         (version (chief/rescript-version))
         (major (string-to-number (car (split-string version "\\.")))))
    (unless (= major chief/rescript-supported-major-version)
      (user-error
       "ReScript %s is installed; this configuration supports ReScript %d.x"
       version chief/rescript-supported-major-version))
    program))

(defun chief/rescript-command (&rest args)
  "Return a supported project-local ReScript command followed by ARGS."
  (cons (chief/rescript-require-supported-cli) args))

(defun chief/rescript-shell-command (command)
  "Return a shell-safe representation of COMMAND."
  (mapconcat #'shell-quote-argument command " "))

(defun chief/rescript-project-label (&optional root)
  "Return a readable label for ReScript project ROOT."
  (file-name-nondirectory
   (directory-file-name (or root (chief/rescript-project-root-required)))))

(defun chief/rescript-compilation-buffer-name (operation &optional root)
  "Return a project-scoped compilation buffer name for OPERATION and ROOT."
  (format "*rescript %s[%s]*"
          operation
          (chief/rescript-project-label root)))

(define-derived-mode chief/rescript-compilation-mode compilation-mode
  "ReScript-Compilation"
  "Compilation mode for ReScript compiler, watcher, and package-manager output."
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter nil t))

(defun chief/rescript-compile (command operation &optional root)
  "Run ReScript COMMAND for OPERATION in project ROOT using compilation-mode."
  (let* ((root (or root (chief/rescript-project-root-required)))
         (default-directory root)
         (compilation-read-command nil)
         (buffer-name (chief/rescript-compilation-buffer-name operation root)))
    (compilation-start
     (chief/rescript-shell-command command)
     'chief/rescript-compilation-mode
     (lambda (_) buffer-name))))

(defun chief/rescript-build ()
  "Build the current ReScript project with the version-12 compiler."
  (interactive)
  (chief/rescript-compile (chief/rescript-command "build") "build"))

(defun chief/rescript-clean ()
  "Remove build artifacts for the current ReScript project."
  (interactive)
  (chief/rescript-compile (chief/rescript-command "clean") "clean"))

(defun chief/rescript-watch-buffer (&optional root)
  "Return the project-scoped ReScript watch buffer for ROOT, if any."
  (gethash (file-truename (or root (chief/rescript-project-root-required)))
           chief/rescript-watch-buffers))

(defun chief/rescript-watch (&optional restart)
  "Start or display the current project's `rescript watch' process.
With RESTART non-nil, stop the existing project watcher first."
  (interactive "P")
  (let* ((root (chief/rescript-project-root-required))
         (key (file-truename root))
         (existing (gethash key chief/rescript-watch-buffers))
         (process (and (buffer-live-p existing) (get-buffer-process existing))))
    (when (and restart process (process-live-p process))
      (delete-process process)
      (setq process nil))
    (if (and process (process-live-p process))
        (pop-to-buffer existing)
      (when (buffer-live-p existing)
        (kill-buffer existing))
      (let ((buffer (chief/rescript-compile
                     (chief/rescript-command "watch") "watch" root)))
        (puthash key buffer chief/rescript-watch-buffers)
        (when-let* ((watch-process (get-buffer-process buffer)))
          (set-process-query-on-exit-flag watch-process nil))
        buffer))))

(defun chief/rescript-restart-watch ()
  "Restart the current project's ReScript watcher."
  (interactive)
  (chief/rescript-watch t))

(defun chief/rescript-stop-watch ()
  "Stop the current project's ReScript watcher without affecting other projects."
  (interactive)
  (let* ((root (chief/rescript-project-root-required))
         (key (file-truename root))
         (buffer (gethash key chief/rescript-watch-buffers))
         (process (and (buffer-live-p buffer) (get-buffer-process buffer))))
    (if (and process (process-live-p process))
        (progn
          (delete-process process)
          (remhash key chief/rescript-watch-buffers)
          (message "Stopped ReScript watcher for %s" (chief/rescript-project-label root)))
      (remhash key chief/rescript-watch-buffers)
      (message "No live ReScript watcher for %s" (chief/rescript-project-label root)))))

(defun chief/rescript-tool-output-buffer (kind)
  "Return a project-scoped ReScript tool output buffer for KIND."
  (get-buffer-create
   (chief/rescript-compilation-buffer-name kind (chief/rescript-project-root-required))))

(defun chief/rescript-display-tool-error (kind command stderr-file &optional stdout)
  "Display failed ReScript KIND output for COMMAND and STDERR-FILE.
Optional STDOUT is a buffer whose contents are appended after stderr."
  (let ((buffer (chief/rescript-tool-output-buffer kind)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "$ %s\n\n" (chief/rescript-shell-command command)))
        (when (file-readable-p stderr-file)
          (insert-file-contents stderr-file))
        (when (and stdout (buffer-live-p stdout))
          (insert (with-current-buffer stdout (buffer-string))))
        (ansi-color-apply-on-region (point-min) (point-max))
        (goto-char (point-min))
        (chief/rescript-compilation-mode)))
    (display-buffer buffer)))

(defun chief/rescript-format-buffer ()
  "Format the current `.res' or `.resi' buffer safely through stdin."
  (interactive)
  (let* ((program (chief/rescript-require-supported-cli))
         (extension (if (and buffer-file-name
                             (string-suffix-p ".resi" buffer-file-name t))
                        ".resi"
                      ".res"))
         (command (list program "format" "--stdin" extension))
         (default-directory (chief/rescript-project-root-required))
         (stdout (generate-new-buffer " *rescript-format*"))
         (stderr-file (make-temp-file "chief-rescript-format-")))
    (unwind-protect
        (let ((status (apply #'call-process-region
                             (point-min) (point-max)
                             program nil (list stdout stderr-file) nil
                             (cdr command))))
          (if (zerop status)
              (let ((point-offset (- (point) (point-min)))
                    (formatted (with-current-buffer stdout (buffer-string))))
                (replace-region-contents
                 (point-min) (point-max) (lambda () formatted))
                (goto-char (min (+ (point-min) point-offset) (point-max))))
            (chief/rescript-display-tool-error "format errors" command stderr-file stdout)
            (user-error "ReScript formatting failed")))
      (when (buffer-live-p stdout)
        (kill-buffer stdout))
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

(defun chief/rescript-format-project ()
  "Format all source files in the current ReScript project."
  (interactive)
  (chief/rescript-compile (chief/rescript-command "format") "format"))

(defun chief/rescript-check-format ()
  "Check formatting without changing files in the current ReScript project."
  (interactive)
  (chief/rescript-compile (chief/rescript-command "format" "--check")
                          "format check"))

(defun chief/rescript-package-directory ()
  "Return the nearest package directory containing package.json."
  (when-let* ((start (chief/rescript-start-directory))
              (root (locate-dominating-file start "package.json")))
    (chief/project-normalize-root root)))

(defun chief/rescript-package-manager (&optional directory)
  "Return the package-manager symbol appropriate for DIRECTORY."
  (let* ((directory (or directory
                        (chief/rescript-package-directory)
                        (chief/rescript-project-root-required)))
         (lock-root
          (locate-dominating-file
           directory
           (lambda (dir)
             (cl-some (lambda (file) (file-exists-p (expand-file-name file dir)))
                      '("pnpm-lock.yaml" "yarn.lock" "bun.lock" "bun.lockb"
                        "package-lock.json"))))))
    (cond
     ((and lock-root (file-exists-p (expand-file-name "pnpm-lock.yaml" lock-root))) 'pnpm)
     ((and lock-root (file-exists-p (expand-file-name "yarn.lock" lock-root))) 'yarn)
     ((and lock-root
           (or (file-exists-p (expand-file-name "bun.lock" lock-root))
               (file-exists-p (expand-file-name "bun.lockb" lock-root)))) 'bun)
     (t 'npm))))

(defun chief/rescript-package-manager-executable (&optional manager)
  "Return executable for package MANAGER or signal an actionable error."
  (let* ((manager (or manager (chief/rescript-package-manager)))
         (program (executable-find (symbol-name manager))))
    (or program
        (user-error "%s is not available on PATH" manager))))

(defun chief/rescript-package-manager-command (action &optional packages dev script)
  "Return a package-manager command for ACTION.
PACKAGES is a list of package names, DEV requests development dependencies, and
SCRIPT names a package.json script for the `run' action."
  (let* ((manager (chief/rescript-package-manager))
         (program (chief/rescript-package-manager-executable manager)))
    (pcase action
      ('install
       (list program "install"))
      ('add
       (append
        (list program (if (eq manager 'npm) "install" "add"))
        (when dev
          (pcase manager
            ('npm '("--save-dev"))
            ('bun '("--dev"))
            (_ '("--save-dev"))))
        packages))
      ('run
       (pcase manager
         ('yarn (list program script))
         (_ (list program "run" script))))
      (_ (error "Unknown package-manager action: %s" action)))))

(defun chief/rescript-run-package-command (command operation)
  "Run package-manager COMMAND as ReScript OPERATION."
  (let ((directory (or (chief/rescript-package-directory)
                       (chief/rescript-project-root-required))))
    (chief/rescript-compile command operation directory)))

(defun chief/rescript-install-dependencies ()
  "Install package.json dependencies with the project's package manager."
  (interactive)
  (chief/rescript-run-package-command
   (chief/rescript-package-manager-command 'install)
   "dependencies"))

(defun chief/rescript-read-package (prompt)
  "Read a non-empty npm package name using PROMPT."
  (let ((package (string-trim (read-string prompt))))
    (when (string-empty-p package)
      (user-error "No package name entered"))
    package))

(defun chief/rescript-add-js-package (package)
  "Install third-party JavaScript PACKAGE for typed `@module' interop."
  (interactive (list (chief/rescript-read-package "JavaScript/npm package: ")))
  (chief/rescript-run-package-command
   (chief/rescript-package-manager-command 'add (list package))
   (format "add %s" package)))

(defun chief/rescript-add-dev-package (package)
  "Install development PACKAGE in the current ReScript project."
  (interactive (list (chief/rescript-read-package "Development npm package: ")))
  (chief/rescript-run-package-command
   (chief/rescript-package-manager-command 'add (list package) t)
   (format "add dev %s" package)))

(defun chief/rescript-install-tooling ()
  "Install current ReScript compiler and language server project-locally."
  (interactive)
  (unless (chief/rescript-package-directory)
    (user-error "A package.json is required before installing ReScript tooling"))
  (chief/rescript-run-package-command
   (chief/rescript-package-manager-command
    'add '("rescript@^12" "@rescript/language-server") t)
   "install tooling"))

(defun chief/rescript-package-scripts ()
  "Return package.json script names for the current ReScript package."
  (when-let* ((directory (chief/rescript-package-directory))
              (file (expand-file-name "package.json" directory))
              ((file-readable-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (let* ((json-object-type 'hash-table)
             (object (json-parse-buffer :object-type 'hash-table))
             (scripts (gethash "scripts" object))
             names)
        (when (hash-table-p scripts)
          (maphash (lambda (name _) (push name names)) scripts))
        (sort names #'string-lessp)))))

(defun chief/rescript-run-package-script (script)
  "Run package.json SCRIPT with the project's selected package manager."
  (interactive
   (let ((scripts (chief/rescript-package-scripts)))
     (unless scripts
       (user-error "No package.json scripts found"))
     (list (completing-read "Package script: " scripts nil t))))
  (chief/rescript-run-package-command
   (chief/rescript-package-manager-command 'run nil nil script)
   (format "run %s" script)))

(defun chief/rescript-repl-buffer-name (&optional root)
  "Return the project-scoped ReScript/Node REPL buffer name for ROOT."
  (format "*rescript repl[%s]*"
          (chief/rescript-project-label root)))

(defun chief/rescript-node-module-directories (&optional root)
  "Return ancestor node_modules directories visible from ReScript ROOT."
  (let ((directory (file-name-as-directory
                    (expand-file-name
                     (or root (chief/rescript-project-root-required)))))
        directories)
    (while directory
      (let ((node-modules (expand-file-name "node_modules" directory)))
        (when (file-directory-p node-modules)
          (push node-modules directories)))
      (if (equal directory "/")
          (setq directory nil)
        (setq directory
              (file-name-directory (directory-file-name directory)))))
    (nreverse directories)))

(defun chief/rescript-ensure-repl-buffer ()
  "Return a live project-scoped Node runtime REPL for compiled ReScript snippets."
  (let* ((root (chief/rescript-project-root-required))
         (name (chief/rescript-repl-buffer-name root))
         (buffer (get-buffer-create name))
         (node (chief/rescript-node-executable)))
    (unless (comint-check-proc buffer)
      (let ((default-directory root)
            (process-connection-type nil)
            (process-environment (copy-sequence process-environment)))
        ;; Compiled snippets live in the system temporary directory.  NODE_PATH
        ;; keeps `@module("package")' imports resolving against project-local or
        ;; monorepo-hoisted third-party dependencies.
        (when-let* ((module-dirs (chief/rescript-node-module-directories root)))
          (setenv "NODE_PATH"
                  (string-join
                   (append module-dirs
                           (when-let* ((existing (getenv "NODE_PATH")))
                             (list existing)))
                   path-separator)))
        (apply #'make-comint-in-buffer
               (format "rescript-node-%s" (chief/rescript-project-label root))
               buffer node nil '("--interactive")))
      (with-current-buffer buffer
        (setq-local default-directory root)
        (setq-local comint-process-echoes nil)
        (setq-local comint-scroll-to-bottom-on-input t)
        (setq-local comint-scroll-to-bottom-on-output t)))
    buffer))

(defun chief/rescript-start-repl ()
  "Start or display the ReScript compile-to-Node evaluation REPL.
The official ReScript CLI has no native REPL.  Source-buffer send commands use
the official `bsc -e' compiler, then evaluate its CommonJS output in this Node
REPL at the project root.  Each submission is a separate typed module, so npm
`@module' bindings work but ReScript declarations do not persist between sends."
  (interactive)
  (pop-to-buffer (chief/rescript-ensure-repl-buffer)))

(defun chief/rescript-restart-repl ()
  "Restart the current project's ReScript/Node evaluation REPL."
  (interactive)
  (let ((buffer (get-buffer (chief/rescript-repl-buffer-name))))
    (when (buffer-live-p buffer)
      (when-let* ((process (get-buffer-process buffer)))
        (delete-process process))
      (kill-buffer buffer)))
  (chief/rescript-start-repl))

(defun chief/rescript-compile-snippet-to-js (source)
  "Compile ReScript SOURCE to CommonJS and return the generated JavaScript."
  (chief/rescript-require-supported-cli)
  (let* ((bsc (chief/rescript-bsc-executable))
         (command (list bsc "-e" source))
         (default-directory (chief/rescript-project-root-required))
         (stdout (generate-new-buffer " *rescript-bsc*"))
         (stderr-file (make-temp-file "chief-rescript-bsc-")))
    (unwind-protect
        (let ((status (apply #'process-file bsc nil (list stdout stderr-file) nil
                             (cdr command))))
          (if (zerop status)
              (with-current-buffer stdout (buffer-string))
            (chief/rescript-display-tool-error "REPL compiler errors"
                                                command stderr-file stdout)
            (user-error "ReScript snippet did not compile")))
      (when (buffer-live-p stdout)
        (kill-buffer stdout))
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

(defun chief/rescript-send-source (source)
  "Compile ReScript SOURCE and evaluate the resulting JavaScript in Node."
  (let* ((javascript (chief/rescript-compile-snippet-to-js source))
         (compiled-file (make-temp-file "chief-rescript-repl-" nil ".cjs"))
         (quoted-file (json-serialize compiled-file))
         (payload
          (format
           "(()=>{const p=%s;try{delete require.cache[require.resolve(p)];return require(p)}finally{require('node:fs').rmSync(p,{force:true})}})()\n"
           quoted-file))
         (buffer (chief/rescript-ensure-repl-buffer))
         (process (get-buffer-process buffer)))
    (with-temp-file compiled-file
      (insert javascript))
    (if (and process (process-live-p process))
        (progn
          (comint-send-string process payload)
          (display-buffer buffer))
      (when (file-exists-p compiled-file)
        (delete-file compiled-file))
      (user-error "ReScript runtime REPL is not running"))))

(defun chief/rescript-send-line ()
  "Compile and evaluate the current ReScript line as an independent module."
  (interactive)
  (chief/rescript-send-source
   (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun chief/rescript-send-region (start end)
  "Compile and evaluate ReScript source in region START..END."
  (interactive "r")
  (chief/rescript-send-source (buffer-substring-no-properties start end)))

(defun chief/rescript-send-buffer ()
  "Compile and evaluate the current ReScript buffer as an independent module."
  (interactive)
  (chief/rescript-send-region (point-min) (point-max)))

(defun chief/rescript-compiled-js-candidates (&optional file root)
  "Return plausible generated JavaScript files for ReScript FILE under ROOT."
  (let* ((file (expand-file-name
                (or file buffer-file-name
                    (user-error "Current buffer is not visiting a file"))))
         (root (or root (chief/rescript-project-root-required)))
         (relative (file-relative-name file root))
         (relative-stem (file-name-sans-extension relative))
         (absolute-stem (file-name-sans-extension file))
         (suffixes '(".js" ".mjs" ".cjs" ".bs.js" ".res.js" ".res.mjs" ".res.cjs"))
         candidates)
    (dolist (suffix suffixes)
      (push (concat absolute-stem suffix) candidates)
      (push (concat file suffix) candidates)
      (dolist (directory '("lib/js" "lib/es6" "lib/bs"))
        (push (expand-file-name (concat relative-stem suffix)
                                (expand-file-name directory root))
              candidates)))
    (seq-filter #'file-readable-p (delete-dups (nreverse candidates)))))

(defun chief/rescript-read-compiled-js (&optional file)
  "Return a generated JavaScript file corresponding to ReScript FILE.
Prefer an in-source output next to FILE.  Prompt only when the project emits
multiple plausible out-of-source module formats."
  (let* ((file (expand-file-name
                (or file buffer-file-name
                    (user-error "Current buffer is not visiting a file"))))
         (candidates (chief/rescript-compiled-js-candidates file))
         (in-source
          (cl-find-if
           (lambda (candidate)
             (equal (file-name-directory candidate)
                    (file-name-directory file)))
           candidates)))
    (or in-source
        (pcase candidates
          ('nil (user-error "No generated JavaScript found; build the project first"))
          (`(,only) only)
          (_ (completing-read "Generated JavaScript: " candidates nil t))))))

(defun chief/rescript-build-sync ()
  "Build the current ReScript project synchronously, returning non-nil on success."
  (let* ((command (chief/rescript-command "build"))
         (program (car command))
         (default-directory (chief/rescript-project-root-required))
         (stdout (generate-new-buffer " *rescript-build*"))
         (stderr-file (make-temp-file "chief-rescript-build-")))
    (unwind-protect
        (let ((status (apply #'process-file program nil (list stdout stderr-file) nil
                             (cdr command))))
          (if (zerop status)
              t
            (chief/rescript-display-tool-error "build errors" command stderr-file stdout)
            nil))
      (when (buffer-live-p stdout)
        (kill-buffer stdout))
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

(defun chief/rescript-open-compiled-js ()
  "Build, then open the JavaScript generated for the current ReScript file."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a ReScript file"))
  (when (buffer-modified-p)
    (save-buffer))
  (when (chief/rescript-build-sync)
    (find-file-other-window (chief/rescript-read-compiled-js buffer-file-name))))

(defun chief/rescript-run-current-js ()
  "Build, then execute the JavaScript generated for the current ReScript file."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a ReScript file"))
  (when (buffer-modified-p)
    (save-buffer))
  (when (chief/rescript-build-sync)
    (let* ((root (chief/rescript-project-root-required))
           (javascript (chief/rescript-read-compiled-js buffer-file-name))
           (command (list (chief/rescript-node-executable) javascript)))
      (chief/rescript-compile command "run js" root))))

(defun chief/rescript-show-versions ()
  "Display selected ReScript, compiler, language-server, and Node tools."
  (interactive)
  (let ((root (chief/rescript-project-root-required)))
    (with-current-buffer (get-buffer-create "*ReScript Tooling*")
      (let ((inhibit-read-only t)
            (default-directory root))
        (erase-buffer)
        (insert (format "Project: %s\n\n" root))
        (dolist (entry
                 `(("rescript" ,(chief/rescript-executable t) "--version")
                   ("bsc" ,(chief/rescript-bsc-executable t) "-v")
                   ("language server" ,(car (chief/rescript-lsp-server-command)) "--version")
                   ("node" ,(chief/rescript-node-executable t) "--version")))
          (pcase-let ((`(,label ,program ,argument) entry))
            (insert (format "%s: %s\n" label (or program "<missing>")))
            (when (and program (or (file-executable-p program)
                                   (executable-find program)))
              (condition-case err
                  (insert (format "  %s\n" (car (process-lines program argument))))
                (error (insert (format "  error: %s\n" (error-message-string err))))))))
        (goto-char (point-min))
        (special-mode))
      (display-buffer (current-buffer)))))

(defun chief/rescript-object-get (object key)
  "Return KEY from JSON OBJECT represented as a hash table or plist."
  (cond
   ((hash-table-p object)
    (or (gethash key object)
        (gethash (if (keywordp key)
                     (substring (symbol-name key) 1)
                   (format "%s" key))
                 object)))
   ((listp object)
    (plist-get object (if (keywordp key) key
                        (intern (concat ":" (format "%s" key))))))))

(defun chief/rescript-lsp-show-message-request (_workspace params)
  "Handle ReScript language-server show-message request PARAMS.
Return the complete selected action object so projectRootPath is preserved."
  (when (and chief/rescript-lsp-prompt-for-build
             (not noninteractive))
    (let* ((message (or (chief/rescript-object-get params :message) "ReScript"))
           (actions (chief/rescript-object-get params :actions))
           (actions (cond ((vectorp actions) (append actions nil))
                          ((listp actions) actions)
                          (t nil))))
      (if (null actions)
          (progn (message "%s" message) nil)
        (let* ((choices
                (mapcar (lambda (action)
                          (cons (or (chief/rescript-object-get action :title)
                                    "<unnamed action>")
                                action))
                        actions))
               (selected (completing-read (concat message " ") choices nil t)))
          (cdr (assoc selected choices)))))))

(defun chief/rescript-register-lsp-client ()
  "Register a dedicated modern ReScript client with lsp-mode."
  (add-to-list 'lsp-language-id-configuration '(rescript-mode . "rescript"))
  (add-to-list 'lsp-language-id-configuration '(rescript-ts-mode . "rescript"))
  (let ((requests (make-hash-table :test #'equal))
        (notifications (make-hash-table :test #'equal)))
    (puthash "window/showMessageRequest"
             #'chief/rescript-lsp-show-message-request requests)
    (puthash "rescript/compilationFinished" #'ignore notifications)
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection #'chief/rescript-lsp-server-command)
      :major-modes '(rescript-mode rescript-ts-mode)
      :priority 2
      :server-id 'chief-rescript-lsp
      :request-handlers requests
      :notification-handlers notifications))))

(defun chief/rescript-tooling-installable-p ()
  "Return non-nil when project-local ReScript tooling can be installed."
  (and (chief/rescript-package-directory)
       (executable-find (symbol-name (chief/rescript-package-manager)))))

(defun chief/rescript-mode-setup ()
  "Configure compiler, REPL, JavaScript interop, formatting, and LSP support."
  (setq-local chief/lsp-root-function #'chief/rescript-project-root)
  (setq-local lsp-enabled-clients '(chief-rescript-lsp))
  (when (fboundp 'chief/lsp-disable-clients)
    (chief/lsp-disable-clients
     'ocaml-ls 'ocaml-lsp-server 'rescript-vscode 'ts-ls 'deno-ls))
  (setq-local compile-command
              (chief/rescript-shell-command
               (list (or (chief/rescript-executable t) "rescript") "build")))
  (setq-local lsp-completion-enable t)
  (setq-local lsp-completion-enable-additional-text-edit t)
  (when (boundp 'corfu-auto)
    (setq-local corfu-auto t))
  (when (boundp 'corfu-auto-delay)
    (setq-local corfu-auto-delay 0.05))
  (when (boundp 'corfu-auto-prefix)
    (setq-local corfu-auto-prefix 0))
  (when (fboundp 'corfu-mode)
    (corfu-mode 1))
  (chief/repl-configure
   :start #'chief/rescript-start-repl
   :restart #'chief/rescript-restart-repl
   :send-line #'chief/rescript-send-line
   :send-region #'chief/rescript-send-region
   :send-buffer #'chief/rescript-send-buffer
   :load-file #'chief/rescript-send-buffer)
  (when (and chief/rescript-format-on-save
             (chief/rescript-executable t))
    (chief/format-enable-on-save #'chief/rescript-format-buffer))
  (when (and chief/rescript-auto-start-watch
             (chief/rescript-executable t))
    (chief/rescript-watch))
  (when (fboundp 'chief/lsp-managed-mode-setup)
    (chief/lsp-managed-mode-setup)))

(defun chief/rescript-major-mode ()
  "Activate tree-sitter ReScript mode when ready, otherwise classic mode."
  (interactive)
  (require 'rescript-mode)
  (if (and (fboundp 'treesit-ready-p)
           (treesit-ready-p 'rescript t)
           (require 'rescript-ts-mode nil t))
      (rescript-ts-mode)
    (rescript-mode)))

(defun chief/rescript-auto-mode-entry-p (entry)
  "Return non-nil when auto-mode ENTRY improperly owns `.res' or `.resi'."
  (and (consp entry)
       (stringp (car entry))
       (memq (cdr entry) '(reason-mode rescript-mode rescript-ts-mode
                           chief/rescript-major-mode))
       (or (ignore-errors (string-match-p (car entry) "Probe.res"))
           (ignore-errors (string-match-p (car entry) "Probe.resi")))))

(defun chief/rescript-register-auto-mode ()
  "Give modern `.res' and `.resi' files exclusively to ReScript mode."
  (setq auto-mode-alist
        (cl-remove-if #'chief/rescript-auto-mode-entry-p auto-mode-alist))
  (add-to-list 'auto-mode-alist '("\\.resi?\\'" . chief/rescript-major-mode)))

(defun chief/rescript-bind-local-leader (map)
  "Install ReScript commands into MAP."
  (chief/repl-setup-standard-local-leader map)
  (chief/local-leader-def
    :keymaps map
    "c" '(:ignore t :which-key "compile")
    "cb" #'chief/rescript-build
    "cc" #'chief/rescript-clean
    "cw" #'chief/rescript-watch
    "cW" #'chief/rescript-restart-watch
    "cs" #'chief/rescript-stop-watch
    "cf" #'chief/rescript-format-buffer
    "cF" #'chief/rescript-format-project
    "ck" #'chief/rescript-check-format
    "cr" #'chief/rescript-run-current-js
    "cx" #'chief/rescript-run-package-script
    "co" #'chief/rescript-open-compiled-js
    "cv" #'chief/rescript-show-versions
    "d" '(:ignore t :which-key "dependencies")
    "di" #'chief/rescript-install-dependencies
    "da" #'chief/rescript-add-js-package
    "dD" #'chief/rescript-add-dev-package
    "dt" #'chief/rescript-install-tooling))

(chief/safe-use-package rescript-mode
  :commands rescript-mode
  :init
  (chief/rescript-register-auto-mode)
  :config
  (chief/rescript-register-auto-mode))

(autoload 'rescript-ts-mode "rescript-ts-mode"
  "Major mode for ReScript using tree-sitter." t)
(autoload 'rescript-ts-install-grammar "rescript-ts-mode"
  "Install the ReScript tree-sitter grammar." t)

(add-hook 'rescript-mode-hook #'chief/rescript-mode-setup)
(add-hook 'rescript-ts-mode-hook #'chief/rescript-mode-setup)

(with-eval-after-load 'reason-mode
  (chief/rescript-register-auto-mode))

(with-eval-after-load 'rescript-mode
  (chief/rescript-register-auto-mode)
  (when (and (fboundp 'chief/local-leader-def)
             (boundp 'rescript-mode-map))
    (chief/rescript-bind-local-leader 'rescript-mode-map)))

(with-eval-after-load 'rescript-ts-mode
  (when (and (fboundp 'chief/local-leader-def)
             (boundp 'rescript-ts-mode-map))
    (chief/rescript-bind-local-leader 'rescript-ts-mode-map)))

(with-eval-after-load 'lsp-mode
  (chief/rescript-register-lsp-client)
  (when (boundp 'chief/lsp-managed-major-modes)
    (add-to-list 'chief/lsp-managed-major-modes 'rescript-mode)
    (add-to-list 'chief/lsp-managed-major-modes 'rescript-ts-mode))
  (when (boundp 'chief/lsp-manual-install-specs)
    (add-to-list
     'chief/lsp-manual-install-specs
     '(:modes (rescript-mode rescript-ts-mode)
       :label "ReScript"
       :install-function chief/rescript-install-tooling
       :install-predicate chief/rescript-tooling-installable-p
       :doc-url "https://github.com/rescript-lang/rescript-vscode/tree/master/server"
       :help "Install project-local `rescript' and `@rescript/language-server'; the server runs as `rescript-language-server --stdio`."))))

(when (boundp 'chief/project-root-markers)
  (add-to-list 'chief/project-root-markers "rescript.json" t))

(when (boundp 'project-vc-extra-root-markers)
  (add-to-list 'project-vc-extra-root-markers "rescript.json" t))

(when (boundp 'treesit-language-source-alist)
  (setf (alist-get 'rescript treesit-language-source-alist nil nil #'eq)
        '("https://github.com/rescript-lang/tree-sitter-rescript")))

(provide 'lang-rescript)
;;; lang-rescript.el ends here
