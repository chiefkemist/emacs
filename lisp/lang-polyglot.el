;;; lang-polyglot.el --- Extra language coverage -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'compile)
(require 'comint)
(require 'core-projects)
(require 'core-repl)
(require 'json)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'xml)

(declare-function dap-debug "dap-mode" (debug-args))
(declare-function lsp-execute-code-action "lsp-mode" ())
(declare-function lsp-format-buffer "lsp-mode" ())
(declare-function lsp-organize-imports "lsp-mode" ())
(declare-function lsp-csharp-run-all-tests-in-buffer "lsp-csharp" ())
(declare-function lsp-csharp-run-last-tests "lsp-csharp" ())
(declare-function lsp-csharp-run-test-at-point "lsp-csharp" ())
(declare-function lsp-csharp-run-test-in-buffer "lsp-csharp" ())
(declare-function lsp-ocaml-find-alternate-file "lsp-ocaml" ())
(declare-function lsp-ocaml-infer-interface "lsp-ocaml" ())
(declare-function lsp-ocaml-type-enclosing "lsp-ocaml" ())

(defun chief/polyglot-project-root ()
  "Return the current project root or `default-directory'."
  (chief/project-preferred-root
   (chief/project-current-root)
   default-directory))

(defun chief/ocaml-project-root ()
  "Return the current OCaml project root."
  (chief/project-preferred-root
   '("dune-workspace")
   '("dune-project")
   (chief/polyglot-project-root)))

(defun chief/dotnet-project-root ()
  "Return the current C# or F# project root."
  (or (chief/project-nearest-regexp-root "\\.sln\\'")
      (chief/project-nearest-regexp-root "\\.[cf]sproj\\'")
      (chief/polyglot-project-root)))

(defcustom chief/dotnet-test-args nil
  "Extra arguments appended to `dotnet test' commands."
  :type '(repeat string)
  :group 'chief)

(defcustom chief/dotnet-build-configuration "Debug"
  "Configuration used by .NET build and debug helpers."
  :type 'string
  :group 'chief)

(defcustom chief/ocaml-dune-profile nil
  "Optional Dune profile passed to Dune commands."
  :type '(choice (const nil) string)
  :group 'chief)

(defun chief/polyglot-compile (command name &optional directory)
  "Run COMMAND in DIRECTORY using compilation buffer NAME."
  (let ((default-directory (or directory (chief/polyglot-project-root)))
        (compilation-read-command nil))
    (compilation-start
     (mapconcat #'shell-quote-argument command " ")
     'compilation-mode
     (lambda (_) name))))

(defun chief/polyglot-current-file (language)
  "Return the current buffer file for LANGUAGE as an absolute path."
  (unless buffer-file-name
    (user-error "This buffer is not visiting a %s file" language))
  (expand-file-name buffer-file-name))

(defun chief/polyglot-save-current-buffer ()
  "Save the current buffer when it is visiting a modified file."
  (when (and buffer-file-name (buffer-modified-p))
    (save-buffer)))

(defun chief/polyglot-command (program &rest args)
  "Return a command list starting with PROGRAM and ARGS."
  (unless (executable-find program)
    (user-error "%s is not available on PATH" program))
  (cons program args))

(defun chief/polyglot-completion-setup (&optional zero-prefix)
  "Enable Corfu/LSP completion for language buffers.
When ZERO-PREFIX is non-nil, popup completion may trigger immediately after
member-access punctuation with an empty textual prefix."
  (setq-local lsp-completion-enable t)
  (setq-local lsp-completion-enable-additional-text-edit t)
  (when (boundp 'corfu-auto)
    (setq-local corfu-auto t))
  (when (boundp 'corfu-auto-delay)
    (setq-local corfu-auto-delay 0.05))
  (when (and zero-prefix (boundp 'corfu-auto-prefix))
    (setq-local corfu-auto-prefix 0))
  (when (fboundp 'corfu-mode)
    (when (bound-and-true-p corfu-mode)
      (corfu-mode -1))
    (corfu-mode 1))
  (when (fboundp 'lsp-completion-mode)
    (lsp-completion-mode 1)))

(defun chief/polyglot-run-command-async (command directory buffer-name on-success)
  "Run COMMAND asynchronously in DIRECTORY.
Output goes to BUFFER-NAME.  ON-SUCCESS receives the buffer output after a zero
exit status."
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (setq buffer-read-only nil)
        (erase-buffer)
        (setq-local default-directory directory)))
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

(defun chief/polyglot-lldb-dap-debug (program name cwd source-language &optional args)
  "Start a DAP session for PROGRAM named NAME in CWD.
SOURCE-LANGUAGE is sent to LLDB/GDB when supported.  ARGS are program args."
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
                 :sourceLanguages (vector source-language)))
          ((featurep 'dap-gdb)
           (list :type "gdb"
                 :request "launch"
                 :name name
                 :program program
                 :target program
                 :cwd cwd
                 :args (or args [])
                 :sourceLanguages (vector source-language)))
          (t
           (user-error "No LLDB/GDB DAP adapter is configured")))))
    (dap-debug configuration)))

(defun chief/polyglot-directory-files-matching (directory regexp)
  "Return files in DIRECTORY whose basenames match REGEXP."
  (when (file-directory-p directory)
    (seq-filter
     (lambda (file)
       (string-match-p regexp (file-name-nondirectory file)))
     (directory-files directory t directory-files-no-dot-files-regexp t))))

(defun chief/polyglot-nearest-matching-file (regexp &optional start root)
  "Return the nearest file matching REGEXP between START and ROOT."
  (let ((dir (file-name-as-directory
              (expand-file-name
               (or start
                   (and buffer-file-name (file-name-directory buffer-file-name))
                   default-directory))))
        (limit (and root (file-name-as-directory (expand-file-name root)))))
    (catch 'found
      (while dir
        (when-let* ((files (chief/polyglot-directory-files-matching dir regexp)))
          (throw 'found (car files)))
        (if (or (equal dir limit)
                (equal dir "/"))
            (setq dir nil)
          (setq dir
                (file-name-directory
                 (directory-file-name dir))))))))

(defun chief/polyglot-dotnet-target ()
  "Return the best dotnet build target for the current project."
  (let ((root (chief/polyglot-project-root)))
    (or (chief/polyglot-nearest-matching-file "\\.sln\\'" nil root)
        (chief/polyglot-nearest-matching-file "\\.csproj\\'" nil root)
        (chief/polyglot-nearest-matching-file "\\.fsproj\\'" nil root))))

(defun chief/polyglot-csharp-project-file ()
  "Return the nearest C# project file."
  (chief/polyglot-nearest-matching-file "\\.csproj\\'"))

(defun chief/polyglot-fsharp-project-file ()
  "Return the nearest F# project file."
  (chief/polyglot-nearest-matching-file "\\.fsproj\\'"))

(defun chief/polyglot-dotnet-command (verb &rest args)
  "Return a dotnet command using VERB and ARGS."
  (unless (executable-find "dotnet")
    (user-error "dotnet is not available on PATH"))
  (let ((target (chief/polyglot-dotnet-target)))
    (append (list "dotnet" verb)
            (when target (list target))
            args)))

(defun chief/rust-project-root ()
  "Return the current Rust project root, if any."
  (chief/project-preferred-root
   '("Cargo.toml")
   (chief/polyglot-project-root)))

(defun chief/swift-project-root ()
  "Return the current Swift project root."
  (chief/project-preferred-root
   '("Package.swift")
   (chief/polyglot-project-root)))

(defun chief/rust-command (&rest args)
  "Return a cargo command list built from ARGS."
  (unless (executable-find "cargo")
    (user-error "cargo is not available on PATH"))
  (cons "cargo" args))

(defun chief/rust-build-project ()
  "Run `cargo build' for the current Rust project."
  (interactive)
  (chief/polyglot-compile (chief/rust-command "build") "*cargo build*" (chief/rust-project-root)))

(defun chief/rust-check-project ()
  "Run `cargo check' for the current Rust project."
  (interactive)
  (chief/polyglot-compile (chief/rust-command "check") "*cargo check*" (chief/rust-project-root)))

(defun chief/rust-test-project ()
  "Run `cargo test' for the current Rust project."
  (interactive)
  (chief/polyglot-compile (chief/rust-command "test") "*cargo test*" (chief/rust-project-root)))

(defun chief/rust-run-project ()
  "Run `cargo run' for the current Rust project."
  (interactive)
  (chief/polyglot-compile (chief/rust-command "run") "*cargo run*" (chief/rust-project-root)))

(defun chief/rust-format-project ()
  "Run `cargo fmt' for the current Rust project."
  (interactive)
  (chief/polyglot-compile (chief/rust-command "fmt") "*cargo fmt*" (chief/rust-project-root)))

(defun chief/dotnet-command (&rest args)
  "Return a dotnet command list built from ARGS."
  (apply #'chief/polyglot-command "dotnet" args))

(defun chief/dotnet-project-file (&optional extension)
  "Return the nearest .NET project file.
When EXTENSION is non-nil, prefer project files with that extension."
  (or (and extension
           (chief/polyglot-nearest-matching-file
            (format "\\.%s\\'" (regexp-quote extension))))
      (chief/polyglot-nearest-matching-file "\\.[cf]sproj\\'")))

(defun chief/dotnet-solution-file ()
  "Return the nearest .NET solution file."
  (chief/polyglot-nearest-matching-file "\\.sln\\'"))

(defun chief/dotnet-local-tool-p (tool)
  "Return non-nil when TOOL appears in the project's dotnet tools manifest."
  (let ((manifest (expand-file-name ".config/dotnet-tools.json"
                                    (chief/dotnet-project-root))))
    (and (file-readable-p manifest)
         (with-temp-buffer
           (insert-file-contents manifest)
           (search-forward tool nil t)))))

(defun chief/dotnet-build-target (&optional extension prefer-project)
  "Return a solution or project target for dotnet commands.
EXTENSION restricts project selection when non-nil.  Prefer a project over a
solution when PREFER-PROJECT is non-nil."
  (let ((project (chief/dotnet-project-file extension))
        (solution (chief/dotnet-solution-file)))
    (or (and prefer-project project)
        solution
        project)))

(defun chief/dotnet-run-project (&optional extension)
  "Return a runnable .NET project file, optionally restricted by EXTENSION."
  (or (chief/dotnet-project-file extension)
      (user-error "No .%sproj found for this buffer" (or (and extension (substring extension 0 1)) "cs/fs"))))

(defun chief/dotnet-command-for-target (verb &optional extension prefer-project &rest args)
  "Return dotnet VERB command for the selected target and ARGS."
  (append (chief/dotnet-command verb)
          (when-let* ((target (chief/dotnet-build-target extension prefer-project)))
            (list target))
          args))

(defun chief/dotnet-compile (command name &optional directory)
  "Run dotnet COMMAND using compilation buffer NAME."
  (chief/polyglot-compile command name (or directory (chief/dotnet-project-root))))

(defun chief/dotnet-xml-text (node tag)
  "Return the first text value for TAG inside XML NODE."
  (when-let* ((child (car (xml-get-children node tag)))
              (text (car (xml-node-children child))))
    (and (stringp text) (string-trim text))))

(defun chief/dotnet-project-xml (project)
  "Parse .NET PROJECT XML and return the root node."
  (with-temp-buffer
    (insert-file-contents project)
    (car (xml-parse-region (point-min) (point-max)))))

(defun chief/dotnet-project-property (project tag)
  "Return TAG property from PROJECT."
  (let ((xml (chief/dotnet-project-xml project)))
    (catch 'value
      (dolist (group (xml-get-children xml 'PropertyGroup))
        (when-let* ((value (chief/dotnet-xml-text group tag)))
          (throw 'value value))))))

(defun chief/dotnet-project-target-framework (project)
  "Return the first target framework declared by PROJECT."
  (or (chief/dotnet-project-property project 'TargetFramework)
      (when-let* ((frameworks (chief/dotnet-project-property project 'TargetFrameworks)))
        (car (split-string frameworks ";" t)))))

(defun chief/dotnet-project-assembly-name (project)
  "Return PROJECT assembly name."
  (or (chief/dotnet-project-property project 'AssemblyName)
      (file-name-base project)))

(defun chief/dotnet-project-output-dll (project)
  "Return PROJECT's expected build output dll path."
  (let* ((directory (file-name-directory project))
         (framework (or (chief/dotnet-project-target-framework project)
                        (user-error "No TargetFramework found in %s" project)))
         (assembly (chief/dotnet-project-assembly-name project)))
    (expand-file-name
     (format "bin/%s/%s/%s.dll" chief/dotnet-build-configuration framework assembly)
     directory)))

(defun chief/dotnet-test-filter (names)
  "Return a VSTest filter matching NAMES."
  (mapconcat (lambda (name)
               (format "FullyQualifiedName~%s" name))
             names
             "|"))

(defun chief/dotnet-test-command (&optional extension names)
  "Return a dotnet test command for EXTENSION and optional test NAMES."
  (append (chief/dotnet-command-for-target "test" extension t)
          chief/dotnet-test-args
          (when names
            (list "--filter" (chief/dotnet-test-filter names)))))

(defun chief/dotnet-debug-project (project name)
  "Build PROJECT and launch a CoreCLR DAP session named NAME."
  (require 'dap-mode)
  (require 'dap-netcore)
  (let ((dll (chief/dotnet-project-output-dll project))
        (directory (file-name-directory project)))
    (chief/polyglot-run-command-async
     (chief/dotnet-command "build" project "-c" chief/dotnet-build-configuration)
     directory
     "*dotnet build debug*"
     (lambda (_output)
       (unless (file-exists-p dll)
         (user-error "Build succeeded but no dll was found at %s" dll))
       (dap-debug
        (list :type "coreclr"
              :request "launch"
              :mode "launch"
              :name name
              :program dll
              :cwd directory))))))

(defun chief/dotnet-debug-test (extension names)
  "Debug .NET tests in EXTENSION project filtered by NAMES."
  (require 'dap-mode)
  (require 'dap-netcore)
  (let* ((project (chief/dotnet-run-project extension))
         (buffer (get-buffer-create "*dotnet debug test*"))
         (command (append (chief/dotnet-command "test" project "--verbosity" "Quiet")
                          (when names
                            (list "--filter" (chief/dotnet-test-filter names))))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (setq buffer-read-only nil)
        (erase-buffer)
        (setq-local default-directory (file-name-directory project))))
    (let ((process-environment (cons "VSTEST_HOST_DEBUG=1" process-environment)))
      (make-process
       :name "dotnet debug test"
       :buffer buffer
       :stderr buffer
       :command command
       :noquery t
       :filter
       (lambda (process output)
         (when (buffer-live-p (process-buffer process))
           (with-current-buffer (process-buffer process)
             (let ((moving (= (point) (process-mark process))))
               (save-excursion
                 (goto-char (process-mark process))
                 (insert output)
                 (set-marker (process-mark process) (point)))
               (when moving
                 (goto-char (process-mark process))))
             (save-excursion
               (goto-char (point-min))
               (when (search-forward "Process Id: " nil t)
                 (let ((pid (string-to-number
                             (buffer-substring-no-properties
                              (point)
                              (line-end-position)))))
                   (set-process-filter process nil)
                   (dap-debug
                    (list :type "coreclr"
                          :request "attach"
                          :mode "attach"
                          :name (format ".NET test :: %s" (or (car names) project))
                          :processId pid))))))))
       :sentinel
       (lambda (process event)
         (when (and (memq (process-status process) '(exit signal))
                    (buffer-live-p (process-buffer process)))
           (message "dotnet debug test %s" (string-trim event))
           (display-buffer (process-buffer process))))))))

(defconst chief/csharp-test-attribute-regexp
  "\\[\\(?:Fact\\|Theory\\|Test\\|TestCase\\|TestMethod\\|DataTestMethod\\|TestCaseSource\\)\\_>"
  "Regexp matching common C# test attributes.")

(defconst chief/csharp-method-regexp
  "^[ \t]*.*[ \t]+\\([[:alpha:]_][[:alnum:]_]*\\)[ \t]*(.*"
  "Regexp matching a C# method declaration line.")

(defun chief/csharp-test-attribute-before-point-p ()
  "Return non-nil when a C# test attribute appears above point."
  (save-excursion
    (let ((limit (save-excursion (forward-line -8) (point))))
      (catch 'found
        (forward-line -1)
        (while (and (> (point) limit)
                    (looking-at-p "^[ \t]*\\(?:\\[\\|//\\|$\\)"))
          (when (looking-at-p (concat "^[ \t]*" chief/csharp-test-attribute-regexp))
            (throw 'found t))
          (forward-line -1))))))

(defun chief/csharp-buffer-test-items ()
  "Return plist entries for C# test methods in the current buffer."
  (let (items)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward chief/csharp-method-regexp nil t)
        (unless (nth 8 (save-excursion (syntax-ppss (match-beginning 0))))
          (let ((name (match-string 1))
                (position (match-beginning 0)))
            (when (chief/csharp-test-attribute-before-point-p)
              (push (list :name name :position position) items))))))
    (nreverse items)))

(defun chief/dotnet-test-item-at-point (items)
  "Return the .NET test item from ITEMS at point, or nil."
  (let ((position (point)))
    (cl-loop for item in items
             for next in (append (cdr items) (list nil))
             for start = (plist-get item :position)
             for end = (or (plist-get next :position) (point-max))
             when (and (<= start position) (< position end))
             return item)))

(defun chief/dotnet-read-test-item (items prompt)
  "Return test item at point from ITEMS, or prompt with PROMPT."
  (unless items
    (user-error "No test methods found in this file"))
  (or (chief/dotnet-test-item-at-point items)
      (cdr (assoc (completing-read prompt
                                   (mapcar (lambda (item)
                                             (cons (plist-get item :name) item))
                                           items)
                                   nil t)
                  (mapcar (lambda (item)
                            (cons (plist-get item :name) item))
                          items)))))

(defun chief/csharp-build-project ()
  "Run `dotnet build' for the current C# project or solution."
  (interactive)
  (chief/dotnet-compile
   (chief/dotnet-command-for-target "build" "csproj" nil "-c" chief/dotnet-build-configuration)
   "*dotnet build csharp*"))

(defun chief/csharp-clean-project ()
  "Run `dotnet clean' for the current C# project or solution."
  (interactive)
  (chief/dotnet-compile (chief/dotnet-command-for-target "clean" "csproj") "*dotnet clean csharp*"))

(defun chief/csharp-test-project ()
  "Run `dotnet test' for the current C# project."
  (interactive)
  (chief/dotnet-compile (chief/dotnet-test-command "csproj") "*dotnet test csharp*"))

(defun chief/csharp-test-buffer ()
  "Run C# tests declared in the current buffer."
  (interactive)
  (chief/polyglot-save-current-buffer)
  (let ((names (mapcar (lambda (item) (plist-get item :name))
                       (chief/csharp-buffer-test-items))))
    (unless names
      (user-error "No C# test methods found in this buffer"))
    (chief/dotnet-compile (chief/dotnet-test-command "csproj" names)
                          "*dotnet test csharp buffer*")))

(defun chief/csharp-test-at-point ()
  "Run the C# test method at point, prompting when needed."
  (interactive)
  (chief/polyglot-save-current-buffer)
  (let* ((item (chief/dotnet-read-test-item (chief/csharp-buffer-test-items) "C# test: "))
         (name (plist-get item :name)))
    (chief/dotnet-compile (chief/dotnet-test-command "csproj" (list name))
                          (format "*dotnet test %s*" name))))

(defun chief/csharp-run-project ()
  "Run the current C# project."
  (interactive)
  (let ((project (chief/dotnet-run-project "csproj")))
    (chief/dotnet-compile (chief/dotnet-command "run" "--project" project) "*dotnet run csharp*")))

(defun chief/csharp-watch-run-project ()
  "Run `dotnet watch run' for the current C# project."
  (interactive)
  (let ((project (chief/dotnet-run-project "csproj")))
    (chief/dotnet-compile (chief/dotnet-command "watch" "--project" project "run") "*dotnet watch run csharp*")))

(defun chief/csharp-watch-test-project ()
  "Run `dotnet watch test' for the current C# project."
  (interactive)
  (let ((project (chief/dotnet-run-project "csproj")))
    (chief/dotnet-compile (chief/dotnet-command "watch" "--project" project "test") "*dotnet watch test csharp*")))

(defun chief/csharp-restore-project ()
  "Run `dotnet restore' for the current C# project or solution."
  (interactive)
  (chief/dotnet-compile (chief/dotnet-command-for-target "restore" "csproj") "*dotnet restore csharp*"))

(defun chief/csharp-format-project ()
  "Run `dotnet format' for the current C# project or solution."
  (interactive)
  (chief/dotnet-compile (chief/dotnet-command-for-target "format" "csproj") "*dotnet format csharp*"))

(defun chief/csharp-debug-project ()
  "Debug the current C# project with netcoredbg."
  (interactive)
  (chief/dotnet-debug-project (chief/dotnet-run-project "csproj") "C# project"))

(defun chief/csharp-debug-test-at-point ()
  "Debug the C# test method at point with netcoredbg attach flow."
  (interactive)
  (let* ((item (chief/dotnet-read-test-item (chief/csharp-buffer-test-items) "C# debug test: "))
         (name (plist-get item :name)))
    (chief/dotnet-debug-test "csproj" (list name))))

(defconst chief/fsharp-test-attribute-regexp
  "^[ \t]*\\[<.*\\(?:Fact\\|Theory\\|Test\\|TestCase\\|TestMethod\\).*?>\\]"
  "Regexp matching common F# test attributes.")

(defconst chief/fsharp-binding-regexp
  "^[ \t]*\\(?:let\\|member\\)[ \t]+"
  "Regexp matching the start of an F# let binding or member line.")

(defun chief/fsharp-binding-name-on-line ()
  "Return the F# binding/member name on the current line."
  (let ((line (buffer-substring-no-properties
               (line-beginning-position)
               (line-end-position))))
    (cond
     ((string-match "^[ \t]*let[ \t]+\\(?:inline[ \t]+\\)?\\(?:``\\([^`]+\\)``\\|\\([[:alpha:]_][[:alnum:]_']*\\)\\)" line)
      (or (match-string 1 line) (match-string 2 line)))
     ((string-match "^[ \t]*member[ \t]+.*\\(?:\\.\\|[ \t]\\)\\(?:``\\([^`]+\\)``\\|\\([[:alpha:]_][[:alnum:]_']*\\)\\)[ \t]*(" line)
      (or (match-string 1 line) (match-string 2 line))))))

(defun chief/fsharp-test-attribute-before-point-p ()
  "Return non-nil when an F# test attribute appears above point."
  (save-excursion
    (let ((limit (save-excursion (forward-line -8) (point))))
      (catch 'found
        (forward-line -1)
        (while (and (> (point) limit)
                    (looking-at-p "^[ \t]*\\(?:\\[<\\|//\\|$\\)"))
          (when (looking-at-p chief/fsharp-test-attribute-regexp)
            (throw 'found t))
          (forward-line -1))))))

(defun chief/fsharp-buffer-test-items ()
  "Return plist entries for F# test bindings in the current buffer."
  (let (items)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward chief/fsharp-binding-regexp nil t)
        (unless (nth 8 (save-excursion (syntax-ppss (match-beginning 0))))
          (let* ((position (match-beginning 0))
                 (name (chief/fsharp-binding-name-on-line)))
            (when (and name (chief/fsharp-test-attribute-before-point-p))
              (push (list :name name :position position) items))))))
    (nreverse items)))

(defun chief/fsharp-build-project ()
  "Run `dotnet build' for the current F# project or solution."
  (interactive)
  (chief/dotnet-compile
   (chief/dotnet-command-for-target "build" "fsproj" nil "-c" chief/dotnet-build-configuration)
   "*dotnet build fsharp*"))

(defun chief/fsharp-clean-project ()
  "Run `dotnet clean' for the current F# project or solution."
  (interactive)
  (chief/dotnet-compile (chief/dotnet-command-for-target "clean" "fsproj") "*dotnet clean fsharp*"))

(defun chief/fsharp-test-project ()
  "Run `dotnet test' for the current F# project."
  (interactive)
  (chief/dotnet-compile (chief/dotnet-test-command "fsproj") "*dotnet test fsharp*"))

(defun chief/fsharp-test-buffer ()
  "Run F# tests declared in the current buffer."
  (interactive)
  (chief/polyglot-save-current-buffer)
  (let ((names (mapcar (lambda (item) (plist-get item :name))
                       (chief/fsharp-buffer-test-items))))
    (unless names
      (user-error "No F# test bindings found in this buffer"))
    (chief/dotnet-compile (chief/dotnet-test-command "fsproj" names)
                          "*dotnet test fsharp buffer*")))

(defun chief/fsharp-test-at-point ()
  "Run the F# test binding at point, prompting when needed."
  (interactive)
  (chief/polyglot-save-current-buffer)
  (let* ((item (chief/dotnet-read-test-item (chief/fsharp-buffer-test-items) "F# test: "))
         (name (plist-get item :name)))
    (chief/dotnet-compile (chief/dotnet-test-command "fsproj" (list name))
                          (format "*dotnet test %s*" name))))

(defun chief/fsharp-run-project ()
  "Run the current F# project."
  (interactive)
  (let ((project (chief/dotnet-run-project "fsproj")))
    (chief/dotnet-compile (chief/dotnet-command "run" "--project" project) "*dotnet run fsharp*")))

(defun chief/fsharp-run-script ()
  "Run the current F# script with `dotnet fsi'."
  (interactive)
  (chief/polyglot-save-current-buffer)
  (chief/dotnet-compile (chief/dotnet-command "fsi" (chief/polyglot-current-file "F#"))
                        "*dotnet fsi*"
                        (file-name-directory (chief/polyglot-current-file "F#"))))

(defun chief/fsharp-watch-run-project ()
  "Run `dotnet watch run' for the current F# project."
  (interactive)
  (let ((project (chief/dotnet-run-project "fsproj")))
    (chief/dotnet-compile (chief/dotnet-command "watch" "--project" project "run") "*dotnet watch run fsharp*")))

(defun chief/fsharp-watch-test-project ()
  "Run `dotnet watch test' for the current F# project."
  (interactive)
  (let ((project (chief/dotnet-run-project "fsproj")))
    (chief/dotnet-compile (chief/dotnet-command "watch" "--project" project "test") "*dotnet watch test fsharp*")))

(defun chief/fsharp-restore-project ()
  "Run `dotnet restore' for the current F# project or solution."
  (interactive)
  (chief/dotnet-compile (chief/dotnet-command-for-target "restore" "fsproj") "*dotnet restore fsharp*"))

(defun chief/fsharp-format-project ()
  "Run `dotnet fantomas' if available, otherwise `dotnet format'."
  (interactive)
  (chief/dotnet-compile
   (if (file-exists-p (expand-file-name ".config/dotnet-tools.json" (chief/dotnet-project-root)))
       (chief/dotnet-command "tool" "run" "fantomas" (or (chief/dotnet-build-target "fsproj" t) "."))
     (chief/dotnet-command-for-target "format" "fsproj"))
   "*dotnet format fsharp*"))

(defun chief/fsharp-debug-project ()
  "Debug the current F# project with netcoredbg."
  (interactive)
  (chief/dotnet-debug-project (chief/dotnet-run-project "fsproj") "F# project"))

(defun chief/fsharp-debug-test-at-point ()
  "Debug the F# test binding at point with netcoredbg attach flow."
  (interactive)
  (let* ((item (chief/dotnet-read-test-item (chief/fsharp-buffer-test-items) "F# debug test: "))
         (name (plist-get item :name)))
    (chief/dotnet-debug-test "fsproj" (list name))))

(defun chief/ocaml-command (&rest args)
  "Return a Dune command list built from ARGS."
  (append (apply #'chief/polyglot-command "dune" args)
          (when chief/ocaml-dune-profile
            (list "--profile" chief/ocaml-dune-profile))))

(defun chief/ocaml-compile (command name &optional directory)
  "Run OCaml COMMAND in DIRECTORY using compilation buffer NAME."
  (chief/polyglot-compile command name (or directory (chief/ocaml-project-root))))

(defun chief/ocaml-build-project ()
  "Run `dune build' for the current OCaml project."
  (interactive)
  (chief/ocaml-compile (chief/ocaml-command "build") "*dune build*"))

(defun chief/ocaml-build-check ()
  "Run Dune's @check alias for the current OCaml project."
  (interactive)
  (chief/ocaml-compile (chief/ocaml-command "build" "@check") "*dune check*"))

(defun chief/ocaml-build-all ()
  "Run Dune's @all alias for the current OCaml project."
  (interactive)
  (chief/ocaml-compile (chief/ocaml-command "build" "@all") "*dune all*"))

(defun chief/ocaml-test-project ()
  "Run `dune runtest' for the current OCaml project."
  (interactive)
  (chief/ocaml-compile (chief/ocaml-command "runtest") "*dune runtest*"))

(defun chief/ocaml-test-current-dir ()
  "Run Dune tests for the current file's directory."
  (interactive)
  (let* ((file (chief/polyglot-current-file "OCaml"))
         (relative-dir (file-relative-name (file-name-directory file)
                                           (chief/ocaml-project-root))))
    (chief/ocaml-compile (chief/ocaml-command "runtest" relative-dir)
                         "*dune runtest dir*")))

(defun chief/ocaml-format-project ()
  "Run `dune fmt' for the current OCaml project."
  (interactive)
  (chief/ocaml-compile (chief/ocaml-command "fmt") "*dune fmt*"))

(defun chief/ocaml-format-buffer ()
  "Format the current OCaml buffer with ocamlformat when available."
  (interactive)
  (chief/polyglot-save-current-buffer)
  (let ((file (chief/polyglot-current-file "OCaml")))
    (if (executable-find "ocamlformat")
        (let ((stderr-file (make-temp-file "chief-ocamlformat-")))
          (unwind-protect
              (let ((status (call-process "ocamlformat" nil (list nil stderr-file) nil
                                          "--inplace" file)))
                (if (zerop status)
                    (revert-buffer t t t)
                  (error "%s"
                         (with-temp-buffer
                           (insert-file-contents stderr-file)
                           (string-trim (buffer-string))))))
            (when (file-exists-p stderr-file)
              (delete-file stderr-file))))
      (chief/ocaml-format-project))))

(defun chief/ocaml-clean-project ()
  "Run `dune clean' for the current OCaml project."
  (interactive)
  (chief/ocaml-compile (chief/ocaml-command "clean") "*dune clean*"))

(defun chief/ocaml-promote-project ()
  "Run `dune promote' for accepted expect-test output."
  (interactive)
  (chief/ocaml-compile (chief/ocaml-command "promote") "*dune promote*"))

(defun chief/ocaml-dune-files ()
  "Return dune files in the current OCaml project."
  (let ((root (chief/ocaml-project-root)))
    (when (file-directory-p root)
      (directory-files-recursively root "\\`dune\\'"))))

(defun chief/ocaml-dune-stanzas ()
  "Return executable/test stanza names from dune files."
  (let (items)
    (dolist (file (chief/ocaml-dune-files))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "(\\(executable\\|test\\)[[:space:]\n]+[^)]*?(name[[:space:]\n]+\\([^[:space:]\n)]+\\)" nil t)
          (push (list :kind (match-string 1)
                      :name (match-string 2)
                      :directory (file-name-directory file))
                items))
        (goto-char (point-min))
        (while (re-search-forward "(executables[[:space:]\n]+[^)]*?(names[[:space:]\n]+\\([^)]*\\))" nil t)
          (dolist (name (split-string (match-string 1) "[[:space:]\n]+" t))
            (push (list :kind "executable"
                        :name name
                        :directory (file-name-directory file))
                  items)))))
    (delete-dups (nreverse items))))

(defun chief/ocaml-read-dune-stanza (&optional kind prompt)
  "Read a Dune stanza, optionally restricted to KIND."
  (let* ((items (seq-filter (lambda (item)
                              (or (null kind)
                                  (string= (plist-get item :kind) kind)))
                            (chief/ocaml-dune-stanzas)))
         (choices (mapcar (lambda (item)
                            (cons (format "%s <%s> %s"
                                          (plist-get item :name)
                                          (plist-get item :kind)
                                          (file-relative-name (plist-get item :directory)
                                                              (chief/ocaml-project-root)))
                                  item))
                          items)))
    (unless choices
      (user-error "No Dune %s stanzas found" (or kind "executable/test")))
    (cdr (assoc (completing-read (or prompt "Dune target: ") choices nil t)
                choices))))

(defun chief/ocaml-exec-stanza (item)
  "Run Dune executable/test stanza ITEM."
  (let* ((name (plist-get item :name))
         (directory (plist-get item :directory))
         (relative-dir (string-remove-suffix
                        "/"
                        (file-relative-name directory (chief/ocaml-project-root))))
         (target (if (string= relative-dir ".")
                     (format "./%s.exe" name)
                   (format "./%s/%s.exe" relative-dir name))))
    (chief/ocaml-compile (chief/ocaml-command "exec" target)
                         (format "*dune exec %s*" name))))

(defun chief/ocaml-run-executable ()
  "Run a selected Dune executable stanza."
  (interactive)
  (chief/ocaml-exec-stanza
   (chief/ocaml-read-dune-stanza "executable" "Dune executable: ")))

(defun chief/ocaml-run-test-executable ()
  "Run a selected Dune test executable stanza."
  (interactive)
  (chief/ocaml-exec-stanza
   (chief/ocaml-read-dune-stanza "test" "Dune test executable: ")))

(defun chief/ocaml-lsp-type-enclosing ()
  "Show OCaml type enclosing information."
  (interactive)
  (if (fboundp 'lsp-ocaml-type-enclosing)
      (call-interactively #'lsp-ocaml-type-enclosing)
    (user-error "lsp-ocaml-type-enclosing is not available")))

(defun chief/ocaml-lsp-alternate-file ()
  "Switch between OCaml implementation and interface files."
  (interactive)
  (if (fboundp 'lsp-ocaml-find-alternate-file)
      (call-interactively #'lsp-ocaml-find-alternate-file)
    (user-error "lsp-ocaml-find-alternate-file is not available")))

(defun chief/ocaml-lsp-infer-interface ()
  "Infer an OCaml interface through ocamllsp."
  (interactive)
  (if (fboundp 'lsp-ocaml-infer-interface)
      (call-interactively #'lsp-ocaml-infer-interface)
    (user-error "lsp-ocaml-infer-interface is not available")))

(defun chief/swift-package-project-p ()
  "Return non-nil when the current project is a Swift package."
  (file-exists-p (expand-file-name "Package.swift" (chief/polyglot-project-root))))

(defun chief/swift-build-project ()
  "Run `swift build' for the current Swift project."
  (interactive)
  (unless (executable-find "swift")
    (user-error "swift is not available on PATH"))
  (chief/polyglot-compile '("swift" "build") "*swift build*" (chief/swift-project-root)))

(defun chief/swift-test-project ()
  "Run `swift test' for the current Swift project."
  (interactive)
  (unless (executable-find "swift")
    (user-error "swift is not available on PATH"))
  (chief/polyglot-compile '("swift" "test") "*swift test*" (chief/swift-project-root)))

(defun chief/swift-run-project ()
  "Run `swift run' for the current Swift project."
  (interactive)
  (unless (executable-find "swift")
    (user-error "swift is not available on PATH"))
  (chief/polyglot-compile '("swift" "run") "*swift run*" (chief/swift-project-root)))

(defun chief/lua-mode-setup ()
  "Configure REPL integration for Lua buffers."
  (chief/repl-configure
   :start #'lua-start-process
   :restart #'lua-restart-with-whole-file
   :send-line #'lua-send-current-line
   :send-region #'lua-send-region
   :send-buffer #'lua-send-buffer
   :send-defun #'lua-send-defun
   :load-file #'lua-send-buffer))

(defun chief/swift-mode-setup ()
  "Configure REPL integration for Swift buffers."
  (setq-local compile-command
              (if (chief/swift-package-project-p)
                  "swift test"
                "swift"))
  (setq-local chief/lsp-root-function #'chief/swift-project-root)
  (chief/repl-configure
   :start #'swift-mode:run-repl
   :restart #'swift-mode:run-repl
   :send-region #'swift-mode:send-region
   :send-buffer #'swift-mode:send-buffer
   :load-file #'swift-mode:send-buffer))

(defun chief/tuareg-send-line ()
  "Send the current line to the OCaml REPL."
  (interactive)
  (tuareg-eval-region (line-beginning-position) (line-end-position)))

(defun chief/tuareg-restart-repl ()
  "Restart the OCaml REPL."
  (interactive)
  (tuareg-kill-ocaml)
  (tuareg-run-ocaml))

(defun chief/tuareg-mode-setup ()
  "Configure REPL, LSP, and completion for OCaml-family buffers."
  (setq-local compile-command "dune build")
  (setq-local chief/lsp-root-function #'chief/ocaml-project-root)
  (chief/polyglot-completion-setup t)
  (when (require 'lsp-ocaml nil t)
    (setq-local lsp-enabled-clients '(ocaml-lsp-server))
    (setq-local lsp-disabled-clients '(ocaml-ls))
    (setq-local lsp-ocaml-markupkind 'markdown)
    (setq-local lsp-ocaml-enclosing-type-verbosity 1)
    (when-let* ((command (and (fboundp 'chief/lsp-ocamllsp-command)
                              (chief/lsp-ocamllsp-command))))
      (setq-local lsp-ocaml-lsp-server-command command)))
  (chief/repl-configure
   :start #'tuareg-run-ocaml
   :restart #'chief/tuareg-restart-repl
   :send-line #'chief/tuareg-send-line
   :send-region #'tuareg-eval-region
   :send-buffer #'tuareg-eval-buffer
   :send-defun #'tuareg-eval-phrase
   :load-file #'tuareg-eval-buffer))

(defun chief/ess-r-mode-setup ()
  "Configure REPL integration for ESS R buffers."
  (chief/repl-configure
   :start #'R
   :restart #'R
   :send-line #'ess-eval-line
   :send-region #'ess-eval-region
   :send-buffer #'ess-eval-buffer
   :load-file #'ess-load-file))

(defun chief/scheme-mode-setup ()
  "Configure REPL integration for Scheme buffers."
  (chief/repl-configure
   :start #'geiser
   :restart #'geiser-restart-repl
   :send-line #'geiser-eval-last-sexp
   :send-region #'geiser-eval-region
   :send-buffer #'geiser-eval-buffer
   :send-defun #'geiser-eval-definition))

(defun chief/racket-mode-setup ()
  "Configure REPL integration for Racket buffers."
  (chief/repl-configure
   :start #'racket-repl
   :restart #'racket-run
   :send-region #'racket-send-region
   :send-defun #'racket-send-definition
   :load-file #'racket-run))

(defun chief/ruby-repl-buffer-name ()
  "Return the current project's Ruby REPL buffer name."
  (format "*ruby[%s]*"
          (file-name-nondirectory
           (directory-file-name (chief/polyglot-project-root)))))

(defun chief/ruby-ensure-repl-buffer ()
  "Return a live Ruby REPL buffer for the current project."
  (let* ((buffer-name (chief/ruby-repl-buffer-name))
         (buffer (get-buffer-create buffer-name))
         (project-root (chief/polyglot-project-root))
         (program (or (executable-find "irb")
                      (executable-find "ruby")
                      (user-error "No Ruby REPL executable is available on PATH"))))
    (unless (comint-check-proc buffer)
      (let ((default-directory project-root))
        (make-comint-in-buffer "chief-ruby" buffer program nil))
      (with-current-buffer buffer
        (setq-local default-directory project-root)
        (setq-local comint-process-echoes nil)))
    buffer))

(defun chief/ruby-start-repl ()
  "Start or switch to the current project's Ruby REPL."
  (interactive)
  (pop-to-buffer (chief/ruby-ensure-repl-buffer)))

(defun chief/ruby-restart-repl ()
  "Restart the current project's Ruby REPL."
  (interactive)
  (when-let* ((buffer (get-buffer (chief/ruby-repl-buffer-name))))
    (when-let* ((process (get-buffer-process buffer)))
      (delete-process process))
    (kill-buffer buffer))
  (chief/ruby-start-repl))

(defun chief/ruby-send-string (string)
  "Send STRING to the current project's Ruby REPL."
  (let* ((buffer (chief/ruby-ensure-repl-buffer))
         (process (get-buffer-process buffer)))
    (comint-send-string process string)
    (unless (string-suffix-p "\n" string)
      (comint-send-string process "\n"))
    (display-buffer buffer)))

(defun chief/ruby-send-line ()
  "Send the current line to the current project's Ruby REPL."
  (interactive)
  (chief/ruby-send-string
   (buffer-substring-no-properties
    (line-beginning-position)
    (line-end-position))))

(defun chief/ruby-send-region (start end)
  "Send the region from START to END to the current project's Ruby REPL."
  (interactive "r")
  (chief/ruby-send-string (buffer-substring-no-properties start end)))

(defun chief/ruby-send-buffer ()
  "Send the current buffer to the current project's Ruby REPL."
  (interactive)
  (chief/ruby-send-region (point-min) (point-max)))

(defun chief/ruby-mode-setup ()
  "Configure REPL integration for Ruby buffers."
  (chief/repl-configure
   :start #'chief/ruby-start-repl
   :restart #'chief/ruby-restart-repl
   :send-line #'chief/ruby-send-line
   :send-region #'chief/ruby-send-region
   :send-buffer #'chief/ruby-send-buffer
   :load-file #'chief/ruby-send-buffer))

(defun chief/rust-mode-setup ()
  "Configure build integration for Rust buffers."
  (setq-local compile-command "cargo test")
  (setq-local chief/lsp-root-function #'chief/rust-project-root))

(defun chief/csharp-mode-setup ()
  "Configure C# buffers with LSP completion, tests, and dotnet commands."
  (setq-local compile-command "dotnet build")
  (setq-local chief/lsp-root-function #'chief/dotnet-project-root)
  (chief/polyglot-completion-setup t)
  (when (require 'lsp-csharp nil t)
    (setq-local lsp-csharp-omnisharp-enable-decompilation-support t)
    (setq-local lsp-csharp-csharpls-use-dotnet-tool t)
    (setq-local lsp-csharp-csharpls-use-local-tool
                (chief/dotnet-local-tool-p "csharp-ls"))
    (when-let* ((solution (chief/dotnet-solution-file)))
      (setq-local lsp-csharp-solution-file solution))
    (if (or (executable-find "csharp-ls")
            (chief/dotnet-local-tool-p "csharp-ls"))
        (setq-local lsp-enabled-clients '(csharp-ls))
      (setq-local lsp-enabled-clients '(omnisharp csharp-ls)))))

(defun chief/fsharp-mode-setup ()
  "Configure F# buffers with FSAC completion, tests, and dotnet commands."
  (setq-local compile-command "dotnet build")
  (setq-local chief/lsp-root-function #'chief/dotnet-project-root)
  (chief/polyglot-completion-setup t)
  (when (require 'lsp-fsharp nil t)
    (setq-local lsp-enabled-clients '(fsac))
    (setq-local lsp-fsharp-keywords-autocomplete t)
    (setq-local lsp-fsharp-external-autocomplete t)
    (setq-local lsp-fsharp-linter t)
    (setq-local lsp-fsharp-union-case-stub-generation t)
    (setq-local lsp-fsharp-record-stub-generation t)
    (setq-local lsp-fsharp-interface-stub-generation t)
    (setq-local lsp-fsharp-unused-opens-analyzer t)
    (setq-local lsp-fsharp-unused-declarations-analyzer t)
    (setq-local lsp-fsharp-simplify-name-analyzer t)
    (setq-local lsp-fsharp-resolve-namespaces t)
    (setq-local lsp-fsharp-enable-reference-code-lens t)
    (setq-local lsp-fsharp-auto-workspace-init t)
    (setq-local lsp-fsharp-use-dotnet-tool-for-fsac t)
    (setq-local lsp-fsharp-use-dotnet-local-tool
                (chief/dotnet-local-tool-p "fsautocomplete"))
    (setq-local lsp-fsharp-workspace-extra-exclude-dirs
                ["bin" "obj" ".git" ".ionide" ".fake"])))

(autoload 'markdown-mode "markdown-mode" "Major mode for Markdown." t)
(autoload 'gfm-mode "markdown-mode" "Major mode for GitHub Flavored Markdown." t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(chief/safe-use-package markdown-mode
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-hide-markup nil))

(chief/safe-use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))

(chief/safe-use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode)
  :hook (lua-mode . chief/lua-mode-setup))

(chief/safe-use-package fennel-mode
  :mode ("\\.fnl\\'" . fennel-mode))

(chief/safe-use-package janet-mode
  :mode ("\\.janet\\'" . janet-mode))

(chief/safe-use-package web-mode
  :mode ("\\.razor\\'" . web-mode)
  :mode ("\\.cshtml\\'" . web-mode)
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t))

(chief/safe-use-package nix-mode
  :mode ("\\.nix\\'" . nix-mode))

(chief/safe-use-package cue-mode
  :mode ("\\.cue\\'" . cue-mode))

(chief/safe-use-package fsharp-mode
  :mode ("\\.fs[iylx]?\\'" . fsharp-mode)
  :hook (fsharp-mode . chief/fsharp-mode-setup))

(chief/safe-use-package geiser
  :defer t
  :hook (scheme-mode . chief/scheme-mode-setup))

(chief/safe-use-package racket-mode
  :mode ("\\.rkt\\'" . racket-mode)
  :mode ("\\.rktd\\'" . racket-mode)
  :hook (racket-mode . chief/racket-mode-setup)
  :config
  (add-hook 'racket-mode-hook #'racket-xp-mode))

(chief/safe-use-package geiser-racket
  :after geiser
  :defer t)

(chief/safe-use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :mode ("\\.lhs\\'" . haskell-mode))

(chief/safe-use-package hy-mode
  :mode ("\\.hy\\'" . hy-mode))

(chief/safe-use-package julia-mode
  :mode ("\\.jl\\'" . julia-mode))

(chief/safe-use-package php-mode
  :mode ("\\.php\\'" . php-mode))

(chief/safe-use-package tuareg
  :mode ("\\.ml\\'" . tuareg-mode)
  :mode ("\\.mli\\'" . tuareg-mode)
  :mode ("\\.mly\\'" . tuareg-menhir-mode)
  :mode ("\\.mll\\'" . tuareg-mode)
  :hook ((tuareg-mode . chief/tuareg-mode-setup)
         (reason-mode . chief/tuareg-mode-setup)))

(chief/safe-use-package reason-mode
  :mode ("\\.re\\'" . reason-mode)
  :mode ("\\.rei\\'" . reason-mode))

(chief/safe-use-package swift-mode
  :mode ("\\.swift\\'" . swift-mode)
  :hook (swift-mode . chief/swift-mode-setup))

(chief/safe-use-package protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode))

(chief/safe-use-package graphviz-dot-mode
  :mode ("\\.dot\\'" . graphviz-dot-mode)
  :mode ("\\.gv\\'" . graphviz-dot-mode))

(chief/safe-use-package plantuml-mode
  :mode ("\\.puml\\'" . plantuml-mode)
  :mode ("\\.plantuml\\'" . plantuml-mode))

(chief/safe-use-package ess
  :mode ("\\.R\\'" . ess-r-mode)
  :mode ("\\.r\\'" . ess-r-mode)
  :hook (ess-r-mode . chief/ess-r-mode-setup))

(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.ss\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.sld\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.mojo\\'" . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\.🔥\\'" . python-ts-mode))

(add-hook 'ruby-mode-hook #'chief/ruby-mode-setup)
(when (fboundp 'rust-ts-mode)
  (add-hook 'rust-ts-mode-hook #'chief/rust-mode-setup))
(when (fboundp 'csharp-ts-mode)
  (add-hook 'csharp-ts-mode-hook #'chief/csharp-mode-setup))

(with-eval-after-load 'lua-mode
  (chief/repl-setup-standard-local-leader 'lua-mode-map))

(with-eval-after-load 'swift-mode
  (chief/repl-setup-standard-local-leader 'swift-mode-map)
  (chief/local-leader-def
    :keymaps 'swift-mode-map
    "c" '(:ignore t :which-key "compile")
    "cb" #'chief/swift-build-project
    "ct" #'chief/swift-test-project
    "cr" #'chief/swift-run-project))

(with-eval-after-load 'tuareg
  (chief/repl-setup-standard-local-leader 'tuareg-mode-map)
  (chief/local-leader-def
    :keymaps 'tuareg-mode-map
    "c" '(:ignore t :which-key "ocaml")
    "cb" #'chief/ocaml-build-project
    "cc" #'chief/ocaml-build-check
    "ca" #'chief/ocaml-build-all
    "ct" #'chief/ocaml-test-project
    "cT" #'chief/ocaml-test-current-dir
    "cf" #'chief/ocaml-format-buffer
    "cF" #'chief/ocaml-format-project
    "cl" #'chief/ocaml-clean-project
    "cp" #'chief/ocaml-promote-project
    "cr" #'chief/ocaml-run-executable
    "cR" #'chief/ocaml-run-test-executable
    "r" '(:ignore t :which-key "run")
    "rr" #'chief/ocaml-run-executable
    "rt" #'chief/ocaml-run-test-executable
    "t" '(:ignore t :which-key "test")
    "tt" #'chief/ocaml-test-project
    "td" #'chief/ocaml-test-current-dir
    "te" #'chief/ocaml-run-test-executable
    "l" '(:ignore t :which-key "lsp")
    "lt" #'chief/ocaml-lsp-type-enclosing
    "la" #'chief/ocaml-lsp-alternate-file
    "li" #'chief/ocaml-lsp-infer-interface
    "lc" #'lsp-execute-code-action))

(with-eval-after-load 'reason-mode
  (chief/repl-setup-standard-local-leader 'reason-mode-map)
  (chief/local-leader-def
    :keymaps 'reason-mode-map
    "c" '(:ignore t :which-key "ocaml")
    "cb" #'chief/ocaml-build-project
    "cc" #'chief/ocaml-build-check
    "ca" #'chief/ocaml-build-all
    "ct" #'chief/ocaml-test-project
    "cT" #'chief/ocaml-test-current-dir
    "cf" #'chief/ocaml-format-buffer
    "cF" #'chief/ocaml-format-project
    "cl" #'chief/ocaml-clean-project
    "cp" #'chief/ocaml-promote-project
    "cr" #'chief/ocaml-run-executable
    "cR" #'chief/ocaml-run-test-executable
    "r" '(:ignore t :which-key "run")
    "rr" #'chief/ocaml-run-executable
    "rt" #'chief/ocaml-run-test-executable
    "t" '(:ignore t :which-key "test")
    "tt" #'chief/ocaml-test-project
    "td" #'chief/ocaml-test-current-dir
    "te" #'chief/ocaml-run-test-executable
    "l" '(:ignore t :which-key "lsp")
    "lt" #'chief/ocaml-lsp-type-enclosing
    "la" #'chief/ocaml-lsp-alternate-file
    "li" #'chief/ocaml-lsp-infer-interface
    "lc" #'lsp-execute-code-action))

(with-eval-after-load 'scheme
  (chief/repl-setup-standard-local-leader 'scheme-mode-map))

(with-eval-after-load 'racket-mode
  (chief/repl-setup-standard-local-leader 'racket-mode-map))

(with-eval-after-load 'ess-r-mode
  (chief/repl-setup-standard-local-leader 'ess-r-mode-map))

(with-eval-after-load 'ruby-mode
  (chief/repl-setup-standard-local-leader 'ruby-mode-map))

(with-eval-after-load 'rust-ts-mode
  (chief/local-leader-def
    :keymaps 'rust-ts-mode-map
    "c" '(:ignore t :which-key "compile")
    "cb" #'chief/rust-build-project
    "cc" #'chief/rust-check-project
    "ct" #'chief/rust-test-project
    "cr" #'chief/rust-run-project
    "cf" #'chief/rust-format-project))

(with-eval-after-load 'csharp-mode
  (chief/local-leader-def
    :keymaps 'csharp-ts-mode-map
    "c" '(:ignore t :which-key "csharp")
    "cb" #'chief/csharp-build-project
    "cc" #'chief/csharp-clean-project
    "ct" #'chief/csharp-test-project
    "cT" #'chief/csharp-test-buffer
    "ca" #'chief/csharp-test-at-point
    "cr" #'chief/csharp-run-project
    "cw" #'chief/csharp-watch-run-project
    "cW" #'chief/csharp-watch-test-project
    "cR" #'chief/csharp-restore-project
    "cf" #'chief/csharp-format-project
    "r" '(:ignore t :which-key "run")
    "rr" #'chief/csharp-run-project
    "rw" #'chief/csharp-watch-run-project
    "t" '(:ignore t :which-key "test")
    "tt" #'chief/csharp-test-at-point
    "tf" #'chief/csharp-test-buffer
    "tp" #'chief/csharp-test-project
    "tw" #'chief/csharp-watch-test-project
    "tl" #'lsp-csharp-run-last-tests
    "d" '(:ignore t :which-key "debug")
    "dd" #'chief/csharp-debug-project
    "dt" #'chief/csharp-debug-test-at-point
    "l" '(:ignore t :which-key "lsp")
    "la" #'lsp-execute-code-action
    "li" #'lsp-organize-imports
    "lf" #'lsp-format-buffer))

(with-eval-after-load 'fsharp-mode
  (chief/local-leader-def
    :keymaps 'fsharp-mode-map
    "c" '(:ignore t :which-key "fsharp")
    "cb" #'chief/fsharp-build-project
    "cc" #'chief/fsharp-clean-project
    "ct" #'chief/fsharp-test-project
    "cT" #'chief/fsharp-test-buffer
    "ca" #'chief/fsharp-test-at-point
    "cr" #'chief/fsharp-run-project
    "cs" #'chief/fsharp-run-script
    "cw" #'chief/fsharp-watch-run-project
    "cW" #'chief/fsharp-watch-test-project
    "cR" #'chief/fsharp-restore-project
    "cf" #'chief/fsharp-format-project
    "r" '(:ignore t :which-key "run")
    "rr" #'chief/fsharp-run-project
    "rs" #'chief/fsharp-run-script
    "rw" #'chief/fsharp-watch-run-project
    "t" '(:ignore t :which-key "test")
    "tt" #'chief/fsharp-test-at-point
    "tf" #'chief/fsharp-test-buffer
    "tp" #'chief/fsharp-test-project
    "tw" #'chief/fsharp-watch-test-project
    "d" '(:ignore t :which-key "debug")
    "dd" #'chief/fsharp-debug-project
    "dt" #'chief/fsharp-debug-test-at-point
    "l" '(:ignore t :which-key "lsp")
    "la" #'lsp-execute-code-action
    "li" #'lsp-organize-imports
    "lf" #'lsp-format-buffer))

(chief/leader-def
  "m" '(:ignore t :which-key "major")
  "mm" #'describe-mode)

(provide 'lang-polyglot)
;;; lang-polyglot.el ends here
