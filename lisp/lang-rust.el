;;; lang-rust.el --- Rust, Cargo, and rust-analyzer support -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'compile)
(require 'core-projects)
(require 'json)
(require 'seq)
(require 'subr-x)

(declare-function chief/local-leader-def "core-evil" (&rest args))
(declare-function dap-debug "dap-mode" (debug-args))
(declare-function lsp-get "lsp-mode" (hash key))
(declare-function lsp-rust-analyzer--select-runnable "lsp-rust" ())
(declare-function lsp-rust-analyzer-status "lsp-rust" ())
(declare-function lsp-rust-analyzer-expand-macro "lsp-rust" ())
(declare-function lsp-rust-analyzer-open-external-docs "lsp-rust" ())
(defvar lsp-rust-analyzer--last-runnable)

(defun chief/rust-crate-root ()
  "Return the nearest Rust crate root for the current buffer."
  (chief/project-preferred-root
   '("Cargo.toml")
   (chief/project-current-root)
   default-directory))

(defun chief/rust-manifest-path (&optional root)
  "Return the Cargo manifest path for ROOT, if it exists."
  (when-let* ((root (chief/project-normalize-root (or root (chief/rust-crate-root)))))
    (let ((manifest (expand-file-name "Cargo.toml" root)))
      (when (file-exists-p manifest)
        manifest))))

(defun chief/rust-workspace-manifest-p (manifest)
  "Return non-nil when MANIFEST defines a Cargo workspace."
  (when (and manifest (file-exists-p manifest))
    (with-temp-buffer
      (insert-file-contents manifest nil 0 4096)
      (re-search-forward "^[[:space:]]*\\[workspace\\][[:space:]]*$" nil t))))

(defun chief/rust-workspace-root ()
  "Return the outermost Cargo workspace root for the current buffer, if any."
  (let ((dir (chief/project-normalize-root
              (or (chief/rust-crate-root)
                  (chief/project-default-start-directory))))
        workspace-root)
    (while dir
      (let ((manifest (expand-file-name "Cargo.toml" dir)))
        (when (chief/rust-workspace-manifest-p manifest)
          (setq workspace-root dir)))
      (if (equal dir "/")
          (setq dir nil)
        (setq dir
              (file-name-directory
               (directory-file-name dir)))))
    workspace-root))

(defun chief/rust-project-root ()
  "Return the preferred Rust project root for the current buffer."
  (chief/project-preferred-root
   (chief/rust-workspace-root)
   (chief/rust-crate-root)
   (chief/project-current-root)
   default-directory))

(defun chief/rust-workspace-directory ()
  "Return the directory used for workspace-level Cargo commands."
  (or (chief/rust-workspace-root)
      (chief/rust-project-root)))

(defun chief/rust-default-directory ()
  "Return the directory used for current-package Cargo commands."
  (or (chief/rust-crate-root)
      (chief/rust-project-root)
      default-directory))

(defun chief/rust-command (&rest args)
  "Return a Cargo command list built from ARGS."
  (unless (executable-find "cargo")
    (user-error "cargo is not available on PATH"))
  (cons "cargo" args))

(defun chief/rust-compile (command name &optional directory)
  "Run Rust COMMAND in DIRECTORY using compilation buffer NAME."
  (let ((default-directory (or directory (chief/rust-project-root)))
        (compilation-read-command nil))
    (compilation-start
     (mapconcat #'shell-quote-argument command " ")
     'compilation-mode
     (lambda (_) name))))

(defun chief/rust-shell-command (command)
  "Return shell-safe string for COMMAND list."
  (mapconcat #'shell-quote-argument command " "))

(defun chief/rust-shell-and (&rest commands)
  "Return a shell command that runs COMMANDS sequentially."
  (mapconcat #'chief/rust-shell-command commands " && "))

(defun chief/rust-compile-shell (command name &optional directory)
  "Run shell COMMAND in DIRECTORY using compilation buffer NAME."
  (let ((default-directory (or directory (chief/rust-project-root) default-directory))
        (compilation-read-command nil))
    (compilation-start command 'compilation-mode (lambda (_) name))))

(defun chief/rust-current-file ()
  "Return the current buffer file as an absolute Rust source path."
  (unless buffer-file-name
    (user-error "This buffer is not visiting a Rust file"))
  (expand-file-name buffer-file-name))

(defun chief/rust-save-current-buffer ()
  "Save the current Rust buffer when it is modified."
  (when (and buffer-file-name (buffer-modified-p))
    (save-buffer)))

(defun chief/rust-cache-directory ()
  "Return the directory used for transient Rust build artifacts."
  (let ((directory (expand-file-name
                    "rust/"
                    (if (boundp 'chief/var-directory)
                        chief/var-directory
                      user-emacs-directory))))
    (make-directory directory t)
    directory))

(defun chief/rust-safe-crate-name (file)
  "Return a rustc-safe crate name derived from FILE."
  (let ((name (replace-regexp-in-string
               "[^[:alnum:]_]" "_" (file-name-base file))))
    (if (string-match-p "\\`[[:digit:]]" name)
        (concat "_" name)
      name)))

(defun chief/rust-standalone-binary-path (&optional suffix)
  "Return a stable transient binary path for the current Rust file.
SUFFIX distinguishes normal binaries from test harnesses."
  (let* ((file (chief/rust-current-file))
         (hash (substring (secure-hash 'sha1 (expand-file-name file)) 0 12))
         (extension (if (eq system-type 'windows-nt) ".exe" "")))
    (expand-file-name
     (format "%s-%s%s%s"
             (chief/rust-safe-crate-name file)
             hash
             (or suffix "")
             extension)
     (chief/rust-cache-directory))))

(defun chief/rust-json-get (object key)
  "Return KEY from JSON hash-table OBJECT."
  (and (hash-table-p object) (gethash key object)))

(defun chief/rust-sequence-to-list (value)
  "Return VALUE as a list when it is a list or vector."
  (cond
   ((null value) nil)
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t (list value))))

(defun chief/rust-cargo-metadata (&optional directory)
  "Return parsed `cargo metadata' for DIRECTORY, or nil outside Cargo."
  (let* ((start (file-name-as-directory
                 (expand-file-name (or directory default-directory))))
         (root (locate-dominating-file start "Cargo.toml")))
    (when (and root (executable-find "cargo"))
      (let ((default-directory root))
        (with-temp-buffer
          (let ((status (call-process "cargo" nil t nil
                                      "metadata" "--no-deps" "--format-version" "1")))
            (when (zerop status)
              (condition-case nil
                  (json-parse-string (buffer-string)
                                     :object-type 'hash-table
                                     :array-type 'list
                                     :null-object nil
                                     :false-object :json-false)
                (error nil)))))))))

(defun chief/rust-path-equal-p (left right)
  "Return non-nil when LEFT and RIGHT name the same file."
  (and left right
       (string= (file-truename left) (file-truename right))))

(defun chief/rust-file-in-directory-p (file directory)
  "Return non-nil when FILE is inside DIRECTORY, resolving symlinks."
  (and file directory
       (file-in-directory-p (file-truename file)
                            (file-name-as-directory (file-truename directory)))))

(defun chief/rust-package-root (package)
  "Return PACKAGE's root directory from Cargo metadata."
  (when-let* ((manifest (chief/rust-json-get package "manifest_path")))
    (file-name-as-directory (file-name-directory manifest))))

(defun chief/rust-current-package (&optional metadata file)
  "Return the Cargo metadata package containing FILE."
  (let* ((metadata (or metadata (chief/rust-cargo-metadata (chief/rust-default-directory))))
         (packages (chief/rust-json-get metadata "packages"))
         (file (and (or file buffer-file-name)
                    (expand-file-name (or file buffer-file-name))))
         (manifest (chief/rust-manifest-path (chief/rust-crate-root))))
    (or
     (when manifest
       (seq-find
        (lambda (package)
          (chief/rust-path-equal-p
           manifest
           (chief/rust-json-get package "manifest_path")))
        packages))
     (when file
       (caar
        (sort
         (cl-loop for package in packages
                  for root = (chief/rust-package-root package)
                  when (and root (chief/rust-file-in-directory-p file root))
                  collect (cons package root))
         (lambda (left right)
           (> (length (cdr left)) (length (cdr right))))))))))

(defun chief/rust-package-targets (&optional kind package)
  "Return Cargo targets from PACKAGE matching KIND.
KIND may be nil, a kind string, or a list of kind strings."
  (let* ((package (or package (chief/rust-current-package)))
         (targets (chief/rust-json-get package "targets")))
    (seq-filter
     (lambda (target)
       (chief/rust-target-kind-p target kind))
     targets)))

(defun chief/rust-target-kind-p (target kind)
  "Return non-nil when TARGET has Cargo KIND."
  (let ((kinds (chief/rust-json-get target "kind")))
    (cond
     ((null kind) t)
     ((listp kind) (seq-some (lambda (item) (member item kinds)) kind))
     (t (member kind kinds)))))

(defun chief/rust-target-primary-kind (target)
  "Return the primary Cargo kind for TARGET."
  (let ((kinds (chief/rust-json-get target "kind")))
    (or (seq-find (lambda (kind) (member kind kinds))
                  '("bin" "lib" "test" "example" "bench"))
        (car kinds))))

(defun chief/rust-target-selector-args (target)
  "Return Cargo selector arguments for TARGET."
  (let ((name (chief/rust-json-get target "name")))
    (pcase (chief/rust-target-primary-kind target)
      ("bin" (list "--bin" name))
      ("test" (list "--test" name))
      ("example" (list "--example" name))
      ("bench" (list "--bench" name))
      ("lib" (list "--lib"))
      (_ nil))))

(defun chief/rust-current-target (&optional kind)
  "Return the Cargo target for the current file matching KIND."
  (when-let* ((file (and buffer-file-name (expand-file-name buffer-file-name))))
    (seq-find
     (lambda (target)
       (chief/rust-path-equal-p file (chief/rust-json-get target "src_path")))
     (chief/rust-package-targets kind))))

(defun chief/rust-target-display (target package)
  "Return a completion display string for TARGET in PACKAGE."
  (let* ((name (chief/rust-json-get target "name"))
         (kind (chief/rust-target-primary-kind target))
         (src (chief/rust-json-get target "src_path"))
         (root (chief/rust-package-root package))
         (relative (if (and src root)
                       (file-relative-name src root)
                     src)))
    (format "%s <%s> %s" name kind relative)))

(defun chief/rust-read-target (kind prompt)
  "Read a Cargo target of KIND using PROMPT."
  (let* ((package (or (chief/rust-current-package)
                      (user-error "No Cargo package found for this buffer")))
         (targets (chief/rust-package-targets kind package))
         (current (chief/rust-current-target kind))
         (choices (mapcar (lambda (target)
                            (cons (chief/rust-target-display target package) target))
                          targets))
         (default (and current (chief/rust-target-display current package))))
    (cond
     ((null choices)
      (user-error "No Cargo target matching %s found" kind))
     ((and (= (length choices) 1) (not current-prefix-arg))
      (cdar choices))
     (t
      (cdr (assoc (completing-read prompt choices nil t nil nil default)
                  choices))))))

(defun chief/rust-run-cargo-target (target)
  "Run Cargo executable TARGET."
  (let ((name (chief/rust-json-get target "name"))
        (kind (chief/rust-target-primary-kind target)))
    (unless (member kind '("bin" "example"))
      (user-error "Cargo target %s is not runnable" name))
    (chief/rust-compile
     (append (chief/rust-command "run")
             (chief/rust-target-selector-args target))
     (format "*cargo run %s*" name)
     (chief/rust-default-directory))))

(defun chief/rust-run-bin ()
  "Run a selected Cargo bin target."
  (interactive)
  (chief/rust-run-cargo-target
   (chief/rust-read-target "bin" "Cargo bin: ")))

(defun chief/rust-run-executable-target ()
  "Run a selected Cargo bin or example target."
  (interactive)
  (chief/rust-run-cargo-target
   (chief/rust-read-target '("bin" "example") "Cargo executable: ")))

(defun chief/rust-current-file-has-main-p ()
  "Return non-nil when the current file appears to define `fn main'."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward
     "^[[:space:]]*\\(?:pub[[:space:]]+\\)?\\(?:async[[:space:]]+\\)?fn[[:space:]]+main[[:space:]]*("
     nil t)))

(defun chief/rust-current-edition ()
  "Return the Rust edition for the current package or a standalone default."
  (or (when-let* ((package (chief/rust-current-package)))
        (chief/rust-json-get package "edition"))
      "2021"))

(defun chief/rust-rustc-command (file output &optional test debug)
  "Return a rustc command compiling FILE to OUTPUT.
When TEST is non-nil, build a test harness.  When DEBUG is non-nil, include
symbols for debugger sessions."
  (unless (executable-find "rustc")
    (user-error "rustc is not available on PATH"))
  (append
   (list "rustc"
         (format "--edition=%s" (chief/rust-current-edition))
         "--crate-name" (chief/rust-safe-crate-name file))
   (when test (list "--test"))
   (when debug (list "-g"))
   (list file "-o" output)))

(defun chief/rust-run-standalone-file ()
  "Compile and run the current Rust file directly with rustc."
  (interactive)
  (chief/rust-save-current-buffer)
  (let* ((file (chief/rust-current-file))
         (binary (chief/rust-standalone-binary-path))
         (directory (file-name-directory file)))
    (chief/rust-compile-shell
     (chief/rust-shell-and
      (chief/rust-rustc-command file binary nil nil)
      (list binary))
     "*rust run file*"
     directory)))

(defun chief/rust-test-standalone-file ()
  "Compile and run the current Rust file as a standalone test harness."
  (interactive)
  (chief/rust-save-current-buffer)
  (let* ((file (chief/rust-current-file))
         (binary (chief/rust-standalone-binary-path "-tests"))
         (directory (file-name-directory file)))
    (chief/rust-compile-shell
     (chief/rust-shell-and
      (chief/rust-rustc-command file binary t nil)
      (list binary "--nocapture"))
     "*rust test file*"
     directory)))

(defun chief/rust-run-current-file ()
  "Run the current Rust file.
Cargo bin/example targets use Cargo; other files with `fn main' use rustc
standalone compilation."
  (interactive)
  (chief/rust-save-current-buffer)
  (cond
   ((chief/rust-current-target '("bin" "example"))
    (chief/rust-run-cargo-target (chief/rust-current-target '("bin" "example"))))
   ((chief/rust-current-file-has-main-p)
    (chief/rust-run-standalone-file))
   (t
    (user-error "Current file is not a Cargo executable target and no `fn main' was found"))))

(defun chief/rust-run-command-async (command directory buffer-name on-success)
  "Run COMMAND asynchronously in DIRECTORY.
Output goes to BUFFER-NAME.  ON-SUCCESS is called with the output string when
COMMAND exits with status 0."
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (setq buffer-read-only nil)
        (erase-buffer)
        (setq-local default-directory directory)))
    (message "Running %s" (chief/rust-shell-command command))
    (let ((process
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
                      (display-buffer (process-buffer process)))
                    (message "%s failed with exit status %s" buffer-name status))))))))
      (set-process-query-on-exit-flag process nil)
      process)))

(defun chief/rust-dap-debug (program name cwd &optional args)
  "Start a Rust DAP session for PROGRAM named NAME in CWD with ARGS."
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
                 :sourceLanguages ["rust"]))
          ((featurep 'dap-gdb)
           (list :type "gdb"
                 :request "launch"
                 :name name
                 :program program
                 :target program
                 :cwd cwd
                 :args (or args [])
                 :sourceLanguages ["rust"]))
          (t
           (user-error "No Rust-capable DAP adapter is configured")))))
    (dap-debug configuration)))

(defun chief/rust-debug-standalone-file ()
  "Compile the current Rust file with debug info and launch DAP."
  (interactive)
  (chief/rust-save-current-buffer)
  (let* ((file (chief/rust-current-file))
         (binary (chief/rust-standalone-binary-path "-debug"))
         (directory (file-name-directory file)))
    (chief/rust-run-command-async
     (chief/rust-rustc-command file binary nil t)
     directory
     "*rustc debug file*"
     (lambda (_output)
       (chief/rust-dap-debug binary
                             (format "Rust file :: %s" (file-name-nondirectory file))
                             directory)))))

(defun chief/rust-artifact-matches-target-p (artifact target)
  "Return non-nil when Cargo ARTIFACT belongs to TARGET."
  (or (null target)
      (let ((artifact-target (chief/rust-json-get artifact "target")))
        (and artifact-target
             (string= (chief/rust-json-get artifact-target "name")
                      (chief/rust-json-get target "name"))
             (chief/rust-path-equal-p
              (chief/rust-json-get artifact-target "src_path")
              (chief/rust-json-get target "src_path"))))))

(defun chief/rust-parse-artifact-executables (output &optional target)
  "Return executable paths from Cargo JSON OUTPUT, optionally matching TARGET."
  (let (executables)
    (dolist (line (split-string output "\n" t))
      (condition-case nil
          (let ((message (json-parse-string line
                                            :object-type 'hash-table
                                            :array-type 'list
                                            :null-object nil
                                            :false-object :json-false)))
            (when (and (string= (or (chief/rust-json-get message "reason") "")
                                "compiler-artifact")
                       (chief/rust-json-get message "executable")
                       (chief/rust-artifact-matches-target-p message target))
              (push (chief/rust-json-get message "executable") executables)))
        (error nil)))
    (delete-dups (nreverse executables))))

(defun chief/rust-cargo-artifact-executable-async (cargo-args directory target callback)
  "Run Cargo with CARGO-ARGS and call CALLBACK with TARGET executable."
  (chief/rust-run-command-async
   (append (chief/rust-command) cargo-args (list "--message-format=json"))
   directory
   "*cargo build debug artifact*"
   (lambda (output)
     (pcase (chief/rust-parse-artifact-executables output target)
       (`(,executable)
        (funcall callback executable))
       ('()
        (message "Cargo produced no debug executable for the selected target"))
       (executables
        (message "Cargo produced multiple executables, refusing to guess: %s"
                 (mapconcat #'identity executables ", ")))))))

(defun chief/rust-debug-cargo-target (target)
  "Build Cargo executable TARGET and launch DAP."
  (let ((name (chief/rust-json-get target "name"))
        (kind (chief/rust-target-primary-kind target)))
    (unless (member kind '("bin" "example"))
      (user-error "Cargo target %s is not a debuggable executable" name))
    (chief/rust-cargo-artifact-executable-async
     (append (list "build") (chief/rust-target-selector-args target))
     (chief/rust-default-directory)
     target
     (lambda (program)
       (chief/rust-dap-debug program
                             (format "Cargo %s :: %s" kind name)
                             (chief/rust-default-directory))))))

(defun chief/rust-debug-bin ()
  "Debug a selected Cargo bin target."
  (interactive)
  (chief/rust-debug-cargo-target
   (chief/rust-read-target "bin" "Debug Cargo bin: ")))

(defun chief/rust-debug-current-file ()
  "Debug the current Rust file's main executable.
Cargo bin/example targets use Cargo; other files with `fn main' use rustc."
  (interactive)
  (chief/rust-save-current-buffer)
  (cond
   ((chief/rust-current-target '("bin" "example"))
    (chief/rust-debug-cargo-target (chief/rust-current-target '("bin" "example"))))
   ((chief/rust-current-file-has-main-p)
    (chief/rust-debug-standalone-file))
   (t
    (user-error "Current file is not a Cargo executable target and no `fn main' was found"))))

(defun chief/rust-test-attribute-p (attribute)
  "Return non-nil when ATTRIBUTE marks a Rust test function."
  (or (string-match-p "\\(?:\\`\\|::\\)test\\(?:[[:space:]]*(\\|[[:space:]]*\\'\\)" attribute)
      (string-match-p "\\(?:\\`\\|::\\)rstest\\(?:[[:space:]]*(\\|[[:space:]]*\\'\\)" attribute)))

(defun chief/rust-buffer-test-items ()
  "Return plist entries for test functions in the current buffer."
  (let (items)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[[:space:]]*#\\[\\([^]\n]+\\)\\]" nil t)
        (when (chief/rust-test-attribute-p (match-string 1))
          (let ((attribute-position (match-beginning 0)))
            (forward-line 1)
            (while (looking-at-p "^[[:space:]]*#\\[[^]\n]+\\][[:space:]]*$")
              (forward-line 1))
            (while (looking-at-p "^[[:space:]]*$")
              (forward-line 1))
            (when (looking-at "^[[:space:]]*\\(?:pub\\(?:([^)]*)\\)?[[:space:]]+\\)?\\(?:async[[:space:]]+\\)?fn[[:space:]]+\\([[:alnum:]_]+\\)")
              (push (list :name (match-string 1)
                          :position (point)
                          :attribute-position attribute-position)
                    items))))))
    (nreverse items)))

(defun chief/rust-test-item-at-point ()
  "Return the test item at point, or nil."
  (let* ((position (point))
         (items (chief/rust-buffer-test-items)))
    (cl-loop for item in items
             for next in (append (cdr items) (list nil))
             for start = (plist-get item :attribute-position)
             for end = (or (plist-get next :attribute-position) (point-max))
             when (and (<= start position) (< position end))
             return item)))

(defun chief/rust-enclosing-inline-modules (&optional position)
  "Return inline `mod name { ... }' modules enclosing POSITION."
  (let ((position (or position (point)))
        modules)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^[[:space:]]*\\(?:pub\\(?:([^)]*)\\)?[[:space:]]+\\)?mod[[:space:]]+\\([[:alnum:]_]+\\)[[:space:]]*{"
              position t)
        (unless (nth 8 (syntax-ppss))
          (let* ((name (match-string 1))
                 (open-brace (1- (point)))
                 (end (ignore-errors (scan-sexps open-brace 1))))
            (when (and end (< position end))
              (push (cons open-brace name) modules))))))
    (mapcar #'cdr (sort modules (lambda (left right) (< (car left) (car right)))))))

(defun chief/rust-module-prefix-from-root (file root)
  "Return Rust module path for FILE relative to module ROOT."
  (when (and file root (chief/rust-file-in-directory-p file root))
    (let* ((file (file-truename file))
           (root (file-name-as-directory (file-truename root)))
           (relative (file-relative-name file root))
           (without-extension (file-name-sans-extension relative))
           (components (split-string without-extension "/" t)))
      (when (member (car (last components)) '("lib" "main" "mod"))
        (setq components (butlast components)))
      (when components
        (mapconcat #'identity components "::")))))

(defun chief/rust-file-under-src-bin-p (file package)
  "Return non-nil when FILE is under PACKAGE's src/bin directory."
  (when-let* ((root (chief/rust-package-root package)))
    (chief/rust-file-in-directory-p file (expand-file-name "src/bin/" root))))

(defun chief/rust-target-module-root (target package)
  "Return the module root directory for TARGET in PACKAGE, if any."
  (pcase (chief/rust-target-primary-kind target)
    ("lib"
     (when-let* ((root (chief/rust-package-root package)))
       (expand-file-name "src/" root)))
    (_
     (when-let* ((src (chief/rust-json-get target "src_path"))
                 ((member (file-name-nondirectory src) '("main.rs" "lib.rs" "mod.rs"))))
       (file-name-directory src)))))

(defun chief/rust-current-test-selection ()
  "Return plist describing the Cargo test target/filter for the current file."
  (when-let* ((file (and buffer-file-name (expand-file-name buffer-file-name)))
              (package (chief/rust-current-package)))
    (let* ((targets (chief/rust-package-targets '("lib" "bin" "test" "example") package))
           (direct (seq-find
                    (lambda (target)
                      (chief/rust-path-equal-p file (chief/rust-json-get target "src_path")))
                    targets)))
      (if direct
          (list :target direct
                :filter nil
                :package package
                :directory (chief/rust-package-root package)
                :root (file-name-directory (chief/rust-json-get direct "src_path")))
        (let (matches)
          (dolist (target targets)
            (let* ((kind (chief/rust-target-primary-kind target))
                   (root (chief/rust-target-module-root target package))
                   (skip-lib-src-bin (and (string= kind "lib")
                                          (chief/rust-file-under-src-bin-p file package)))
                   (filter (and root
                                (not skip-lib-src-bin)
                                (chief/rust-module-prefix-from-root file root))))
              (when filter
                (push (list :target target
                            :filter filter
                            :package package
                            :directory (chief/rust-package-root package)
                            :root root)
                      matches))))
          (car (sort matches
                     (lambda (left right)
                       (> (length (plist-get left :root))
                          (length (plist-get right :root)))))))))))

(defun chief/rust-test-path-for-item (item &optional selection)
  "Return the fully-qualified test path for ITEM and Cargo SELECTION."
  (let* ((filter (plist-get selection :filter))
         (position (plist-get item :position))
         (components (append (when filter (split-string filter "::" t))
                             (chief/rust-enclosing-inline-modules position)
                             (list (plist-get item :name)))))
    (mapconcat #'identity components "::")))

(defun chief/rust-read-test-path (&optional selection)
  "Return the test path at point, or prompt for a test in the current file."
  (let* ((items (chief/rust-buffer-test-items))
         (at-point (chief/rust-test-item-at-point)))
    (unless items
      (user-error "No Rust test functions found in this file"))
    (if at-point
        (chief/rust-test-path-for-item at-point selection)
      (let* ((choices (mapcar
                       (lambda (item)
                         (let ((path (chief/rust-test-path-for-item item selection)))
                           (cons path path)))
                       items)))
        (cdr (assoc (completing-read "Rust test: " choices nil t)
                    choices))))))

(defun chief/rust-cargo-test-args (selection &optional test-path exact no-run)
  "Return Cargo test args for SELECTION.
TEST-PATH runs a specific test.  EXACT adds test-harness --exact.  NO-RUN
builds the selected test executable without running it."
  (let* ((target (plist-get selection :target))
         (filter (or test-path (plist-get selection :filter))))
    (append
     (list "test")
     (when no-run (list "--no-run"))
     (chief/rust-target-selector-args target)
     (unless no-run
       (append
        (when filter (list filter))
        (list "--")
        (when exact (list "--exact"))
        (list "--nocapture"))))))

(defun chief/rust-run-cargo-test-selection (selection &optional test-path exact)
  "Run Cargo tests for SELECTION, optionally restricted to TEST-PATH."
  (let* ((target (plist-get selection :target))
         (filter (or test-path (plist-get selection :filter)))
         (name (chief/rust-json-get target "name")))
    (chief/rust-compile
     (append (chief/rust-command)
             (chief/rust-cargo-test-args selection test-path exact nil))
     (format "*cargo test %s%s*" name (if filter (format " %s" filter) ""))
     (plist-get selection :directory))))

(defun chief/rust-test-current-file ()
  "Run the current Rust file's test suite.
Cargo target files use the matching Cargo target.  Cargo module files use a
module-path filter.  Files outside Cargo use a standalone rustc test harness."
  (interactive)
  (chief/rust-save-current-buffer)
  (if-let* ((selection (chief/rust-current-test-selection)))
      (chief/rust-run-cargo-test-selection selection)
    (chief/rust-test-standalone-file)))

(defun chief/rust-test-at-point ()
  "Run the single Rust test at point, prompting when point is not in a test."
  (interactive)
  (chief/rust-save-current-buffer)
  (let* ((selection (chief/rust-current-test-selection))
         (test-path (chief/rust-read-test-path selection)))
    (if selection
        (chief/rust-run-cargo-test-selection selection test-path t)
      (chief/rust-test-standalone-path test-path))))

(defun chief/rust-test-standalone-path (test-path)
  "Run standalone TEST-PATH from the current Rust file."
  (chief/rust-save-current-buffer)
  (let* ((file (chief/rust-current-file))
         (binary (chief/rust-standalone-binary-path "-tests"))
         (directory (file-name-directory file)))
    (chief/rust-compile-shell
     (chief/rust-shell-and
      (chief/rust-rustc-command file binary t nil)
      (list binary test-path "--exact" "--nocapture"))
     (format "*rust test %s*" test-path)
     directory)))

(defun chief/rust-debug-standalone-tests (&optional test-path exact)
  "Compile current file as a test harness with debug info and launch DAP."
  (interactive)
  (chief/rust-save-current-buffer)
  (let* ((file (chief/rust-current-file))
         (binary (chief/rust-standalone-binary-path "-tests-debug"))
         (directory (file-name-directory file))
         (args (append (when test-path (list test-path))
                       (when exact (list "--exact"))
                       (list "--nocapture"))))
    (chief/rust-run-command-async
     (chief/rust-rustc-command file binary t t)
     directory
     "*rustc debug tests*"
     (lambda (_output)
       (chief/rust-dap-debug binary
                             (if test-path
                                 (format "Rust test :: %s" test-path)
                               (format "Rust tests :: %s" (file-name-nondirectory file)))
                             directory
                             args)))))

(defun chief/rust-debug-cargo-test-selection (selection &optional test-path exact)
  "Build Cargo test executable for SELECTION and launch DAP."
  (let* ((target (plist-get selection :target))
         (filter (or test-path (plist-get selection :filter)))
         (directory (plist-get selection :directory))
         (args (append (when filter (list filter))
                       (when exact (list "--exact"))
                       (list "--nocapture")))
         (target-name (chief/rust-json-get target "name")))
    (chief/rust-cargo-artifact-executable-async
     (chief/rust-cargo-test-args selection nil nil t)
     directory
     target
     (lambda (program)
       (chief/rust-dap-debug program
                             (format "Cargo test %s%s"
                                     target-name
                                     (if filter (format " :: %s" filter) ""))
                             directory
                             args)))))

(defun chief/rust-debug-current-file-tests ()
  "Debug the current Rust file's test suite."
  (interactive)
  (chief/rust-save-current-buffer)
  (if-let* ((selection (chief/rust-current-test-selection)))
      (chief/rust-debug-cargo-test-selection selection)
    (chief/rust-debug-standalone-tests)))

(defun chief/rust-debug-test-at-point ()
  "Debug the single Rust test at point, prompting when needed."
  (interactive)
  (chief/rust-save-current-buffer)
  (let* ((selection (chief/rust-current-test-selection))
         (test-path (chief/rust-read-test-path selection)))
    (if selection
        (chief/rust-debug-cargo-test-selection selection test-path t)
      (chief/rust-debug-standalone-tests test-path t))))

(defun chief/rust-build-project ()
  "Run `cargo build' for the current Rust crate."
  (interactive)
  (chief/rust-compile (chief/rust-command "build") "*cargo build*" (chief/rust-default-directory)))

(defun chief/rust-build-workspace ()
  "Run `cargo build --workspace' for the current Rust workspace."
  (interactive)
  (chief/rust-compile (chief/rust-command "build" "--workspace")
                      "*cargo build workspace*"
                      (chief/rust-workspace-directory)))

(defun chief/rust-check-project ()
  "Run `cargo check' for the current Rust crate."
  (interactive)
  (chief/rust-compile (chief/rust-command "check") "*cargo check*" (chief/rust-default-directory)))

(defun chief/rust-check-workspace ()
  "Run `cargo check --workspace' for the current Rust workspace."
  (interactive)
  (chief/rust-compile (chief/rust-command "check" "--workspace")
                      "*cargo check workspace*"
                      (chief/rust-workspace-directory)))

(defun chief/rust-test-project ()
  "Run `cargo test' for the current Rust crate."
  (interactive)
  (chief/rust-compile (chief/rust-command "test") "*cargo test*" (chief/rust-default-directory)))

(defun chief/rust-test-workspace ()
  "Run `cargo test --workspace' for the current Rust workspace."
  (interactive)
  (chief/rust-compile (chief/rust-command "test" "--workspace")
                      "*cargo test workspace*"
                      (chief/rust-workspace-directory)))

(defun chief/rust-run-project ()
  "Run `cargo run' for the current Rust crate."
  (interactive)
  (chief/rust-compile (chief/rust-command "run") "*cargo run*" (chief/rust-default-directory)))

(defun chief/rust-format-project ()
  "Run `cargo fmt --all' for the current Rust project."
  (interactive)
  (chief/rust-compile (chief/rust-command "fmt" "--all")
                      "*cargo fmt*"
                      (chief/rust-workspace-directory)))

(defun chief/rust-lint-project ()
  "Run `cargo clippy' for the current Rust crate."
  (interactive)
  (chief/rust-compile (chief/rust-command "clippy" "--all-targets" "--all-features")
                      "*cargo clippy*"
                      (chief/rust-default-directory)))

(defun chief/rust-lint-workspace ()
  "Run `cargo clippy --workspace' for the current Rust workspace."
  (interactive)
  (chief/rust-compile (chief/rust-command "clippy" "--workspace" "--all-targets" "--all-features")
                      "*cargo clippy workspace*"
                      (chief/rust-workspace-directory)))

(defun chief/rust-run-runnable ()
  "Run the current rust-analyzer runnable."
  (interactive)
  (if (fboundp 'lsp-rust-analyzer-run)
      (call-interactively #'lsp-rust-analyzer-run)
    (user-error "rust-analyzer runnable support is not available")))

(defun chief/rust-debug-runnable ()
  "Debug a selected rust-analyzer runnable with this setup's DAP adapter."
  (interactive)
  (unless (fboundp 'lsp-rust-analyzer--select-runnable)
    (user-error "rust-analyzer runnable support is not available"))
  (require 'lsp-mode)
  (let* ((runnable (lsp-rust-analyzer--select-runnable))
         (kind (lsp-get runnable :kind))
         (label (or (lsp-get runnable :label) "rust-analyzer runnable"))
         (args (lsp-get runnable :args))
         (cargo-args (chief/rust-sequence-to-list (lsp-get args :cargo-args)))
         (executable-args (chief/rust-sequence-to-list (lsp-get args :executable-args)))
         (workspace-root (or (lsp-get args :workspace-root?)
                             (chief/rust-default-directory))))
    (unless (string= kind "cargo")
      (user-error "Only Cargo rust-analyzer runnables can be debugged"))
    (setq cargo-args (copy-sequence cargo-args))
    (pcase (car cargo-args)
      ("run" (setcar cargo-args "build"))
      ("test" (unless (member "--no-run" cargo-args)
                (setq cargo-args (append cargo-args (list "--no-run"))))))
    (setq lsp-rust-analyzer--last-runnable runnable)
    (chief/rust-cargo-artifact-executable-async
     cargo-args
     workspace-root
     nil
     (lambda (program)
       (chief/rust-dap-debug program label workspace-root executable-args)))))

(defun chief/rust-open-cargo-toml ()
  "Open the current Rust manifest."
  (interactive)
  (if-let* ((manifest (chief/rust-manifest-path (chief/rust-crate-root))))
      (find-file manifest)
    (user-error "No Cargo.toml found for this buffer")))

(defun chief/rust-mode-setup ()
  "Configure Rust buffers."
  (setq-local compile-command "cargo test")
  (setq-local chief/lsp-root-function #'chief/rust-project-root)
  (setq-local lsp-enabled-clients '(rust-analyzer))
  (when (require 'lsp-rust nil t)
    (if-let* ((command (and (fboundp 'chief/lsp-rust-analyzer-command)
                            (chief/lsp-rust-analyzer-command))))
        (progn
          (setq-local lsp-rust-server 'rust-analyzer)
          (setq-local lsp-rust-analyzer-server-command command)
          (setq-local lsp-rust-analyzer-cargo-watch-command "check")
          (setq-local lsp-rust-analyzer-cargo-watch-enable t)
          (setq-local lsp-rust-analyzer-cargo-target-dir t)
          (setq-local lsp-rust-analyzer-check-all-targets t)
          (setq-local lsp-rust-analyzer-cargo-run-build-scripts t)
          (setq-local lsp-rust-analyzer-cargo-auto-reload t)
          (setq-local lsp-rust-analyzer-proc-macro-enable t))))
  (when (fboundp 'chief/lsp-managed-mode-setup)
    (chief/lsp-managed-mode-setup)))

(when (fboundp 'rust-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-hook 'rust-ts-mode-hook #'chief/rust-mode-setup))

(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode-hook #'chief/rust-mode-setup)
  (chief/local-leader-def
    :keymaps 'rust-mode-map
    "c" '(:ignore t :which-key "cargo")
    "cb" #'chief/rust-build-project
    "cB" #'chief/rust-build-workspace
    "cc" #'chief/rust-check-project
    "cC" #'chief/rust-check-workspace
    "ct" #'chief/rust-test-project
    "cT" #'chief/rust-test-workspace
    "cr" #'chief/rust-run-project
    "cR" #'chief/rust-run-current-file
    "ce" #'chief/rust-run-executable-target
    "cs" #'chief/rust-run-standalone-file
    "cf" #'chief/rust-format-project
    "cl" #'chief/rust-lint-project
    "cL" #'chief/rust-lint-workspace
    "r" '(:ignore t :which-key "run")
    "rr" #'chief/rust-run-current-file
    "rb" #'chief/rust-run-bin
    "re" #'chief/rust-run-executable-target
    "rs" #'chief/rust-run-standalone-file
    "ra" #'chief/rust-run-runnable
    "t" '(:ignore t :which-key "test")
    "tt" #'chief/rust-test-at-point
    "tf" #'chief/rust-test-current-file
    "ts" #'chief/rust-test-standalone-file
    "d" '(:ignore t :which-key "debug")
    "dd" #'chief/rust-debug-runnable
    "df" #'chief/rust-debug-current-file
    "db" #'chief/rust-debug-bin
    "dt" #'chief/rust-debug-test-at-point
    "dT" #'chief/rust-debug-current-file-tests
    "ds" #'chief/rust-debug-standalone-file
    "dS" #'chief/rust-debug-standalone-tests
    "dr" #'chief/rust-run-runnable
    "dm" #'chief/rust-open-cargo-toml))

(with-eval-after-load 'rust-ts-mode
  (chief/local-leader-def
    :keymaps 'rust-ts-mode-map
    "c" '(:ignore t :which-key "cargo")
    "cb" #'chief/rust-build-project
    "cB" #'chief/rust-build-workspace
    "cc" #'chief/rust-check-project
    "cC" #'chief/rust-check-workspace
    "ct" #'chief/rust-test-project
    "cT" #'chief/rust-test-workspace
    "cr" #'chief/rust-run-project
    "cR" #'chief/rust-run-current-file
    "ce" #'chief/rust-run-executable-target
    "cs" #'chief/rust-run-standalone-file
    "cf" #'chief/rust-format-project
    "cl" #'chief/rust-lint-project
    "cL" #'chief/rust-lint-workspace
    "r" '(:ignore t :which-key "run")
    "rr" #'chief/rust-run-current-file
    "rb" #'chief/rust-run-bin
    "re" #'chief/rust-run-executable-target
    "rs" #'chief/rust-run-standalone-file
    "ra" #'chief/rust-run-runnable
    "t" '(:ignore t :which-key "test")
    "tt" #'chief/rust-test-at-point
    "tf" #'chief/rust-test-current-file
    "ts" #'chief/rust-test-standalone-file
    "d" '(:ignore t :which-key "debug")
    "dd" #'chief/rust-debug-runnable
    "df" #'chief/rust-debug-current-file
    "db" #'chief/rust-debug-bin
    "dt" #'chief/rust-debug-test-at-point
    "dT" #'chief/rust-debug-current-file-tests
    "ds" #'chief/rust-debug-standalone-file
    "dS" #'chief/rust-debug-standalone-tests
    "dr" #'chief/rust-run-runnable
    "dm" #'chief/rust-open-cargo-toml)
  (with-eval-after-load 'lsp-rust
    (chief/local-leader-def
      :keymaps 'rust-ts-mode-map
      "da" #'lsp-rust-analyzer-status
      "de" #'lsp-rust-analyzer-expand-macro
      "do" #'lsp-rust-analyzer-open-external-docs)))

(provide 'lang-rust)
;;; lang-rust.el ends here
