;;; lang-dotnet.el --- Dedicated .NET language support -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'compile)
(require 'core-bootstrap)
(require 'core-format)
(require 'core-lsp)
(require 'core-projects)
(require 'json)
(require 'lang-polyglot)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'xml)

(declare-function dap-debug "dap-mode" (debug-args))
(declare-function lsp-execute-code-action "lsp-mode" ())
(declare-function lsp-format-buffer "lsp-mode" ())
(declare-function lsp-organize-imports "lsp-mode" ())
(declare-function lsp--uri-to-path-1 "lsp-mode" (uri))
(declare-function lsp-workspace-root "lsp-mode" ())
(declare-function lsp-csharp-run-all-tests-in-buffer "lsp-csharp" ())
(declare-function lsp-csharp-run-last-tests "lsp-csharp" ())
(declare-function lsp-csharp-run-test-at-point "lsp-csharp" ())
(declare-function lsp-csharp-run-test-in-buffer "lsp-csharp" ())

(defgroup chief-dotnet nil
  "Dedicated .NET support for Chiefkemist Emacs."
  :group 'chief)

(defcustom chief/dotnet-test-args nil
  "Extra arguments appended to `dotnet test' commands."
  :type '(repeat string)
  :group 'chief-dotnet)

(defcustom chief/dotnet-build-configuration "Debug"
  "Configuration used by .NET build and debug helpers."
  :type 'string
  :group 'chief-dotnet)

(defcustom chief/dotnet-restore-before-lsp t
  "When non-nil, run `dotnet restore' before LSP if assets are missing.

C# language servers can start without restored NuGet assets, but then they only
know about local variables and code navigation/hovers/inlay hints for framework
and package symbols return nil."
  :type 'boolean
  :group 'chief-dotnet)

(defcustom chief/dotnet-restore-timeout 180
  "Seconds to wait for the synchronous pre-LSP `dotnet restore'."
  :type 'integer
  :group 'chief-dotnet)

(defvar chief/dotnet-restore-history (make-hash-table :test #'equal)
  "Projects already considered for pre-LSP restore in this Emacs session.")

(defcustom chief/dotnet-root-directories
  (delq nil
        (list (getenv "DOTNET_ROOT")
              (expand-file-name "~/.dotnet")
              "/usr/local/share/dotnet"
              "/opt/homebrew/share/dotnet"))
  "Candidate .NET installation roots used for DOTNET_ROOT."
  :type '(repeat directory)
  :group 'chief-dotnet)

(defcustom chief/dotnet-tool-directories
  (delq nil
        (list (expand-file-name "~/.dotnet/tools")
              (when-let* ((dotnet-root (getenv "DOTNET_ROOT")))
                (expand-file-name "tools" dotnet-root))))
  "Directories that contain globally installed `dotnet tool' shims."
  :type '(repeat directory)
  :group 'chief-dotnet)

(defcustom chief/dotnet-roll-forward "Major"
  "DOTNET_ROLL_FORWARD value used for editor-launched .NET tools.

This keeps net10-targeted language servers such as csharp-ls usable when the
machine only has a newer preview SDK/runtime installed.  Set to nil to leave
DOTNET_ROLL_FORWARD unchanged."
  :type '(choice (const nil)
                 (const "Minor")
                 (const "Major")
                 (const "LatestMinor")
                 (const "LatestMajor")
                 (const "Disable")
                 string)
  :group 'chief-dotnet)

(defcustom chief/dotnet-csharp-server 'csharp-ls
  "Preferred C# language server.

`csharp-ls' is used by default because it is lightweight, installable as a
normal dotnet tool, and has built-in experimental Razor `.cshtml' support."
  :type '(choice (const :tag "csharp-ls" csharp-ls)
                 (const :tag "Roslyn" csharp-roslyn)
                 (const :tag "OmniSharp" omnisharp))
  :group 'chief-dotnet)

(defcustom chief/dotnet-csharp-ls-log-level "info"
  "Log level passed to csharp-ls."
  :type '(choice (const "trace")
                 (const "debug")
                 (const "info")
                 (const "warning")
                 (const "error"))
  :group 'chief-dotnet)

(defcustom chief/dotnet-csharp-ls-use-metadata-uris t
  "When non-nil, enable csharp-ls metadata URI support."
  :type 'boolean
  :group 'chief-dotnet)

(defcustom chief/dotnet-csharp-ls-razor-support t
  "When non-nil, enable csharp-ls Razor `.cshtml' support."
  :type 'boolean
  :group 'chief-dotnet)

(defcustom chief/dotnet-csharp-ls-analyzers-enabled nil
  "When non-nil, ask csharp-ls to run Roslyn analyzers for diagnostics."
  :type 'boolean
  :group 'chief-dotnet)

(defcustom chief/dotnet-csharp-ls-apply-formatting-options nil
  "When non-nil, let csharp-ls apply client formatting options."
  :type 'boolean
  :group 'chief-dotnet)

(defcustom chief/dotnet-csharp-ls-extra-args nil
  "Extra arguments appended to the csharp-ls command line."
  :type '(repeat string)
  :group 'chief-dotnet)

(defcustom chief/dotnet-fsautocomplete-extra-args nil
  "Extra arguments appended to the FsAutoComplete command line."
  :type '(repeat string)
  :group 'chief-dotnet)

(defcustom chief/dotnet-vbnet-ls-extra-args nil
  "Extra arguments appended to the VB.NET language server command line."
  :type '(repeat string)
  :group 'chief-dotnet)

(defun chief/dotnet-project-root ()
  "Return the current .NET project root."
  (chief/project-preferred-root
   (chief/project-nearest-regexp-root "\\.slnx?\\'")
   (chief/project-nearest-regexp-root "\\.slnf\\'")
   (chief/project-nearest-regexp-root "\\.\\(?:cs\\|fs\\|vb\\)proj\\'")
   (chief/project-nearest-marker-root
    '("global.json"
      "Directory.Build.props"
      "Directory.Build.targets"
      "Directory.Packages.props"
      "NuGet.config"
      ".config/dotnet-tools.json"))
   (chief/polyglot-project-root)))

(defun chief/dotnet-root-directory ()
  "Return the first usable .NET installation root."
  (or (when-let* ((root (getenv "DOTNET_ROOT"))
                  ((file-executable-p (expand-file-name "dotnet" root))))
        (directory-file-name (expand-file-name root)))
      (cl-loop for root in chief/dotnet-root-directories
               for expanded = (and root (directory-file-name (expand-file-name root)))
               when (and expanded
                         (file-executable-p (expand-file-name "dotnet" expanded)))
               return expanded)))

(defun chief/dotnet-ensure-runtime-environment ()
  "Set DOTNET_ROOT variables so apphost-based dotnet tools can launch."
  (when-let* ((root (chief/dotnet-root-directory)))
    (setenv "DOTNET_ROOT" root)
    (cond
     ((string-match-p "\\(?:aarch64\\|arm64\\)" system-configuration)
      (setenv "DOTNET_ROOT_ARM64" root))
     ((string-match-p "x86_64" system-configuration)
      (setenv "DOTNET_ROOT_X64" root))))
  (when chief/dotnet-roll-forward
    (setenv "DOTNET_ROLL_FORWARD" chief/dotnet-roll-forward)))

(defun chief/dotnet--path-entries ()
  "Return `PATH' entries as a list."
  (split-string (or (getenv "PATH") "") path-separator t))

(defun chief/dotnet--prepend-to-path (directory)
  "Put DIRECTORY at the front of `exec-path' and the process PATH."
  (let ((directory (directory-file-name (expand-file-name directory))))
    (when (file-directory-p directory)
      (setq exec-path (cons directory (delete directory exec-path)))
      (setenv "PATH"
              (string-join
               (cons directory (delete directory (chief/dotnet--path-entries)))
               path-separator)))))

(defun chief/dotnet-ensure-tool-paths ()
  "Ensure dotnet runtime and tool directories are visible to Emacs processes."
  (chief/dotnet-ensure-runtime-environment)
  (when-let* ((root (chief/dotnet-root-directory)))
    (chief/dotnet--prepend-to-path root))
  (dolist (directory chief/dotnet-tool-directories)
    (chief/dotnet--prepend-to-path directory)))

(defun chief/dotnet-tool-executable (program)
  "Return an executable path for dotnet tool PROGRAM, if installed."
  (chief/dotnet-ensure-tool-paths)
  (or (executable-find program)
      (and (eq system-type 'windows-nt)
           (executable-find (concat program ".exe")))
      (cl-loop for directory in chief/dotnet-tool-directories
               for path = (expand-file-name program directory)
               for exe-path = (concat path ".exe")
               when (file-executable-p path)
               return path
               when (and (eq system-type 'windows-nt)
                         (file-executable-p exe-path))
               return exe-path)))

(defun chief/dotnet-tool-manifest-file ()
  "Return the nearest dotnet tools manifest, if present."
  (when-let* ((start (chief/project-default-start-directory))
              (root (locate-dominating-file start ".config/dotnet-tools.json")))
    (expand-file-name ".config/dotnet-tools.json" root)))

(defun chief/dotnet-local-tool-p (tool)
  "Return non-nil when TOOL appears in the nearest dotnet tool manifest."
  (when-let* ((manifest (chief/dotnet-tool-manifest-file))
              ((file-readable-p manifest)))
    (with-temp-buffer
      (let ((case-fold-search t))
        (insert-file-contents manifest)
        (re-search-forward (regexp-quote tool) nil t)))))

(defun chief/dotnet-local-tool-command (tool &rest args)
  "Return a `dotnet tool run' command for local TOOL and ARGS when configured."
  (when (chief/dotnet-local-tool-p tool)
    (append (chief/dotnet-command "tool" "run" tool) args)))

(defun chief/polyglot-dotnet-target ()
  "Return the best dotnet build target for the current project."
  (let ((root (chief/polyglot-project-root)))
    (or (chief/polyglot-nearest-matching-file "\\.slnx?\\'" nil root)
        (chief/polyglot-nearest-matching-file "\\.slnf\\'" nil root)
        (chief/polyglot-nearest-matching-file "\\.csproj\\'" nil root)
        (chief/polyglot-nearest-matching-file "\\.fsproj\\'" nil root)
        (chief/polyglot-nearest-matching-file "\\.vbproj\\'" nil root))))

(defun chief/polyglot-csharp-project-file ()
  "Return the nearest C# project file."
  (chief/polyglot-nearest-matching-file "\\.csproj\\'"))

(defun chief/polyglot-fsharp-project-file ()
  "Return the nearest F# project file."
  (chief/polyglot-nearest-matching-file "\\.fsproj\\'"))

(defun chief/polyglot-vbnet-project-file ()
  "Return the nearest VB.NET project file."
  (chief/polyglot-nearest-matching-file "\\.vbproj\\'"))

(defun chief/polyglot-dotnet-command (verb &rest args)
  "Return a dotnet command using VERB and ARGS."
  (unless (executable-find "dotnet")
    (user-error "dotnet is not available on PATH"))
  (let ((target (chief/polyglot-dotnet-target)))
    (append (list "dotnet" verb)
            (when target (list target))
            args)))

(defun chief/dotnet-command (&rest args)
  "Return a dotnet command list built from ARGS."
  (chief/dotnet-ensure-tool-paths)
  (apply #'chief/polyglot-command "dotnet" args))

(defun chief/dotnet-project-assets-file (project)
  "Return the NuGet assets file path for PROJECT."
  (expand-file-name "obj/project.assets.json" (file-name-directory project)))

(defun chief/dotnet-project-assets-current-p (project)
  "Return non-nil when PROJECT has a usable NuGet assets file."
  (let ((assets (chief/dotnet-project-assets-file project)))
    (and (file-readable-p assets)
         (file-newer-than-file-p assets project))))

(defun chief/dotnet-project-file (&optional extension)
  "Return the nearest .NET project file.
When EXTENSION is non-nil, prefer project files with that extension."
  (or (and extension
           (chief/polyglot-nearest-matching-file
            (format "\\.%s\\'" (regexp-quote extension))))
      (chief/polyglot-nearest-matching-file "\\.\\(?:cs\\|fs\\|vb\\)proj\\'")))

(defun chief/dotnet-restore-project-for-lsp (&optional project force)
  "Restore PROJECT synchronously so LSP can load real references.
When FORCE is non-nil, restore even if the assets file appears current."
  (interactive (list (chief/dotnet-project-file) current-prefix-arg))
  (let* ((project (or project (chief/dotnet-project-file)))
         (project (and project (expand-file-name project)))
         (key (and project (file-truename project)))
         (buffer (get-buffer-create "*dotnet restore lsp*")))
    (unless project
      (user-error "No .NET project file found for this buffer"))
    (when (or force
              (and (not (chief/dotnet-project-assets-current-p project))
                   (not (gethash key chief/dotnet-restore-history))))
      (puthash key t chief/dotnet-restore-history)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (setq buffer-read-only nil)
          (erase-buffer)
          (setq-local default-directory (file-name-directory project))
          (insert (format "dotnet restore %s\n\n" project))))
      (let* ((default-directory (file-name-directory project))
             (command (chief/dotnet-command "restore" project))
             (status
              (with-timeout (chief/dotnet-restore-timeout :timeout)
                (apply #'process-file
                       (car command)
                       nil
                       buffer
                       nil
                       (cdr command)))))
        (cond
         ((eq status :timeout)
          (display-buffer buffer)
          (message "dotnet restore timed out before starting LSP: %s" project))
         ((and (integerp status) (zerop status))
          (message "dotnet restore completed for %s" (file-name-nondirectory project)))
         (t
          (display-buffer buffer)
          (message "dotnet restore failed before LSP for %s (exit %s)" project status)))))))

(defun chief/dotnet-restore-before-lsp-start (&optional project)
  "Run `dotnet restore' for PROJECT before LSP when assets are missing."
  (when chief/dotnet-restore-before-lsp
    (when-let* ((project (or project (chief/dotnet-project-file))))
      (unless (chief/dotnet-project-assets-current-p project)
        (chief/dotnet-restore-project-for-lsp project)))))

(defun chief/dotnet-solution-file ()
  "Return the nearest .NET solution file."
  (or (chief/polyglot-nearest-matching-file "\\.slnx?\\'")
      (chief/polyglot-nearest-matching-file "\\.slnf\\'")))

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
      (user-error "No .%s found for this buffer" (or extension "csproj/fsproj/vbproj"))))

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
   (if (chief/dotnet-local-tool-p "fantomas")
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

(defun chief/vbnet-build-project ()
  "Run `dotnet build' for the current VB.NET project or solution."
  (interactive)
  (chief/dotnet-compile
   (chief/dotnet-command-for-target "build" "vbproj" nil "-c" chief/dotnet-build-configuration)
   "*dotnet build vbnet*"))

(defun chief/vbnet-clean-project ()
  "Run `dotnet clean' for the current VB.NET project or solution."
  (interactive)
  (chief/dotnet-compile (chief/dotnet-command-for-target "clean" "vbproj") "*dotnet clean vbnet*"))

(defun chief/vbnet-test-project ()
  "Run `dotnet test' for the current VB.NET project."
  (interactive)
  (chief/dotnet-compile (chief/dotnet-test-command "vbproj") "*dotnet test vbnet*"))

(defun chief/vbnet-run-project ()
  "Run the current VB.NET project."
  (interactive)
  (let ((project (chief/dotnet-run-project "vbproj")))
    (chief/dotnet-compile (chief/dotnet-command "run" "--project" project) "*dotnet run vbnet*")))

(defun chief/vbnet-watch-run-project ()
  "Run `dotnet watch run' for the current VB.NET project."
  (interactive)
  (let ((project (chief/dotnet-run-project "vbproj")))
    (chief/dotnet-compile (chief/dotnet-command "watch" "--project" project "run") "*dotnet watch run vbnet*")))

(defun chief/vbnet-watch-test-project ()
  "Run `dotnet watch test' for the current VB.NET project."
  (interactive)
  (let ((project (chief/dotnet-run-project "vbproj")))
    (chief/dotnet-compile (chief/dotnet-command "watch" "--project" project "test") "*dotnet watch test vbnet*")))

(defun chief/vbnet-restore-project ()
  "Run `dotnet restore' for the current VB.NET project or solution."
  (interactive)
  (chief/dotnet-compile (chief/dotnet-command-for-target "restore" "vbproj") "*dotnet restore vbnet*"))

(defun chief/vbnet-format-project ()
  "Run `dotnet format' for the current VB.NET project or solution."
  (interactive)
  (chief/dotnet-compile (chief/dotnet-command-for-target "format" "vbproj") "*dotnet format vbnet*"))

(defun chief/vbnet-debug-project ()
  "Debug the current VB.NET project with netcoredbg."
  (interactive)
  (chief/dotnet-debug-project (chief/dotnet-run-project "vbproj") "VB.NET project"))

(defun chief/dotnet-csharp-ls-features ()
  "Return enabled optional csharp-ls feature names."
  (delq nil
        (list (when chief/dotnet-csharp-ls-use-metadata-uris "metadata-uris")
              (when chief/dotnet-csharp-ls-razor-support "razor-support"))))

(defun chief/dotnet-csharp-ls-base-command ()
  "Return the command used to launch csharp-ls."
  (or (chief/dotnet-local-tool-command "csharp-ls")
      (when-let* ((executable (chief/dotnet-tool-executable "csharp-ls")))
        (list executable))
      ;; Fall back to the shim name so lsp-mode's installer can still make the
      ;; command usable after `lsp-install-server'.
      (list "csharp-ls")))

(defun chief/dotnet-csharp-ls-command ()
  "Return a robust csharp-ls command line for LSP."
  (append (chief/dotnet-csharp-ls-base-command)
          (when-let* ((solution (chief/dotnet-solution-file)))
            (list "--solution" (expand-file-name solution)))
          (when-let* ((features (chief/dotnet-csharp-ls-features)))
            (list "--features" (string-join features ",")))
          (list "--loglevel" chief/dotnet-csharp-ls-log-level)
          chief/dotnet-csharp-ls-extra-args))

(defun chief/dotnet-lsp-field (object &rest names)
  "Return the first present field from OBJECT matching NAMES."
  (catch 'value
    (dolist (name names)
      (let ((value (cond
                    ((hash-table-p object) (gethash name object))
                    ((and (symbolp name) (listp object)) (plist-get object name))
                    ((and (stringp name) (listp object))
                     (plist-get object (intern (concat ":" name)))))))
        (when value
          (throw 'value value))))))

(defun chief/dotnet-csharp-ls-metadata-symbol (uri)
  "Return the fully qualified symbol encoded in csharp-ls metadata URI."
  (when (string-match "/decompiled/\\(.+\\)\\.cs\\(?:[?#].*\\)?\\'" uri)
    (url-unhex-string (match-string 1 uri))))

(defun chief/dotnet-reference-directories (&optional project)
  "Return likely reference assembly directories for PROJECT."
  (let* ((project (or project (chief/dotnet-project-file "csproj")))
         (tfm (and project (chief/dotnet-project-target-framework project)))
         (home (expand-file-name "~"))
         (patterns (and tfm
                        (list
                         (format "%s/.nuget/packages/microsoft.aspnetcore.app.ref/*/ref/%s" home tfm)
                         (format "%s/.nuget/packages/microsoft.netcore.app.ref/*/ref/%s" home tfm)
                         (format "%s/.dotnet/packs/Microsoft.AspNetCore.App.Ref/*/ref/%s" home tfm)
                         (format "%s/.dotnet/packs/Microsoft.NETCore.App.Ref/*/ref/%s" home tfm)))))
    (delete-dups
     (cl-loop for pattern in patterns
              append (file-expand-wildcards pattern t)))))

(defun chief/dotnet-reference-assembly-for-symbol (symbol &optional project)
  "Return a likely reference assembly containing fully qualified SYMBOL."
  (car
   (sort
    (cl-loop for directory in (chief/dotnet-reference-directories project)
             append
             (cl-loop for dll in (file-expand-wildcards (expand-file-name "*.dll" directory) t)
                      for assembly = (file-name-base dll)
                      when (string-prefix-p (concat assembly ".") symbol)
                      collect dll))
    (lambda (a b)
      (> (length (file-name-base a))
         (length (file-name-base b)))))))

(defun chief/dotnet-csharp-ls-ilspy-metadata-path (uri)
  "Use ilspycmd to materialize metadata URI when csharp-ls returns no source."
  (when-let* ((symbol-name (chief/dotnet-csharp-ls-metadata-symbol uri))
              (project (chief/dotnet-project-file "csproj"))
              (assembly (chief/dotnet-reference-assembly-for-symbol symbol-name project))
              (ilspy (chief/dotnet-tool-executable "ilspycmd"))
              (root (or (and (fboundp 'lsp-workspace-root) (lsp-workspace-root))
                        (chief/dotnet-project-root)))
              (file-location
               (expand-file-name
                (string-join
                 (list ".cache" "lsp-csharp" "metadata" "ilspy"
                       (file-name-base assembly) (concat symbol-name ".cs"))
                 "/")
                root)))
    (unless (file-exists-p file-location)
      (make-directory (file-name-directory file-location) t)
      (with-temp-buffer
        (let ((status (process-file ilspy nil t nil "-t" symbol-name assembly)))
          (when (and (integerp status) (zerop status) (> (buffer-size) 0))
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region (point-min) (point-max) file-location nil 'silent)
              (with-temp-file (concat file-location ".metadata-uri")
                (insert uri)))))))
    (when (file-readable-p file-location)
      file-location)))

(defun chief/dotnet-csharp-ls-metadata-uri-path (uri)
  "Materialize csharp-ls metadata URI and return the generated file path."
  (or
   (when-let* ((metadata (lsp-request "csharp/metadata"
                                      `(:textDocument (:uri ,uri))))
               (project-name (chief/dotnet-lsp-field metadata "projectName" :projectName :project-name))
               (assembly-name (chief/dotnet-lsp-field metadata "assemblyName" :assemblyName :assembly-name))
               (symbol-name (chief/dotnet-lsp-field metadata "symbolName" :symbolName :symbol-name))
               (source (chief/dotnet-lsp-field metadata "source" :source))
               (root (or (and (fboundp 'lsp-workspace-root) (lsp-workspace-root))
                         (chief/dotnet-project-root)))
               (file-location
                (expand-file-name
                 (string-join
                  (list ".cache" "lsp-csharp" "metadata" "projects" project-name
                        "assemblies" assembly-name (concat symbol-name ".cs"))
                  "/")
                 root)))
     (unless (file-exists-p file-location)
       (make-directory (file-name-directory file-location) t)
       (let ((coding-system-for-write 'utf-8-unix))
         (with-temp-file (concat file-location ".metadata-uri")
           (insert uri))
         (with-temp-file file-location
           (insert source))))
     file-location)
   (chief/dotnet-csharp-ls-ilspy-metadata-path uri)))

(defun chief/dotnet-csharp-ls-uri-to-path (uri)
  "Convert csharp-ls URI to a local path, including `csharp:' metadata URIs.

Recent csharp-ls returns metadata locations like
`csharp:/path/project.csproj/decompiled/System.TimeSpan.cs'.  `url.el' parses
that single-slash scheme poorly, so lsp-mode can otherwise turn it into an empty
path and `gd' reports \"Not found\" even though the server returned a location."
  (if (string-prefix-p "csharp:" uri)
      (or (condition-case err
              (chief/dotnet-csharp-ls-metadata-uri-path uri)
            (error
             (message "csharp-ls metadata lookup failed for %s: %s"
                      uri (error-message-string err))
             nil))
          uri)
    (lsp--uri-to-path-1 uri)))

(defun chief/dotnet-fsautocomplete-base-command ()
  "Return the command used to launch FsAutoComplete."
  (or (chief/dotnet-local-tool-command "fsautocomplete")
      (when-let* ((executable (chief/dotnet-tool-executable "fsautocomplete")))
        (list executable))
      (list "fsautocomplete")))

(defun chief/dotnet-fsautocomplete-command ()
  "Return a robust FsAutoComplete command line for LSP."
  (append (chief/dotnet-fsautocomplete-base-command)
          chief/dotnet-fsautocomplete-extra-args))

(defun chief/dotnet-vbnet-ls-base-command ()
  "Return the command used to launch vbnet-ls."
  (or (chief/dotnet-local-tool-command "vbnet-ls")
      (when (chief/dotnet-local-tool-p "DNAKode.VbNet.Lsp")
        (chief/dotnet-command "tool" "run" "vbnet-ls"))
      (when-let* ((executable (chief/dotnet-tool-executable "vbnet-ls")))
        (list executable))
      (list "vbnet-ls")))

(defun chief/dotnet-vbnet-ls-command ()
  "Return a robust VB.NET language server command line for LSP."
  (append (chief/dotnet-vbnet-ls-base-command)
          (list "--stdio")
          chief/dotnet-vbnet-ls-extra-args))

(defun chief/dotnet-lsp-client-available-p (command)
  "Return non-nil when COMMAND starts with an executable or dotnet local tool."
  (let ((program (car command)))
    (or (and program (file-name-absolute-p program) (file-executable-p program))
        (and program (executable-find program))
        (equal command (chief/dotnet-local-tool-command "csharp-ls"))
        (equal command (chief/dotnet-local-tool-command "fsautocomplete"))
        (equal command (chief/dotnet-local-tool-command "vbnet-ls")))))

(defun chief/dotnet-csharp-ls-available-p ()
  "Return non-nil when csharp-ls is available."
  (or (chief/dotnet-local-tool-p "csharp-ls")
      (chief/dotnet-tool-executable "csharp-ls")))

(defun chief/dotnet-fsautocomplete-available-p ()
  "Return non-nil when FsAutoComplete is available."
  (or (chief/dotnet-local-tool-p "fsautocomplete")
      (chief/dotnet-tool-executable "fsautocomplete")))

(defun chief/dotnet-vbnet-ls-available-p ()
  "Return non-nil when the VB.NET language server is available."
  (or (chief/dotnet-local-tool-p "vbnet-ls")
      (chief/dotnet-local-tool-p "DNAKode.VbNet.Lsp")
      (chief/dotnet-tool-executable "vbnet-ls")))

(defun chief/dotnet-apply-solution-local ()
  "Expose the nearest solution file to LSP clients."
  (when-let* ((solution (chief/dotnet-solution-file)))
    (setq-local lsp-csharp-solution-file (expand-file-name solution))))

(defun chief/csharp-mode-setup ()
  "Configure C# buffers with LSP completion, tests, and dotnet commands."
  (setq-local compile-command "dotnet build")
  (setq-local chief/lsp-root-function #'chief/dotnet-project-root)
  (chief/dotnet-ensure-tool-paths)
  (chief/dotnet-restore-before-lsp-start (chief/dotnet-project-file "csproj"))
  (chief/polyglot-completion-setup t)
  (chief/format-enable-on-save)
  (when (require 'lsp-csharp nil t)
    (setq-local lsp-csharp-omnisharp-enable-decompilation-support t)
    ;; We install and launch csharp-ls as a dotnet tool, but avoid lsp-csharp's
    ;; bare `csharp-ls' launcher so GUI Emacs sessions do not miss ~/.dotnet/tools.
    (setq-local lsp-csharp-csharpls-use-dotnet-tool nil)
    (setq-local lsp-csharp-csharpls-use-local-tool
                (chief/dotnet-local-tool-p "csharp-ls"))
    (chief/dotnet-apply-solution-local)
    (pcase chief/dotnet-csharp-server
      ('csharp-ls
       (setq-local lsp-enabled-clients '(csharp-ls))
       (setq-local lsp-disabled-clients '(omnisharp csharp-roslyn)))
      ('csharp-roslyn
       (require 'lsp-roslyn nil t)
       (setq-local lsp-enabled-clients '(csharp-roslyn))
       (setq-local lsp-disabled-clients '(omnisharp csharp-ls)))
      ('omnisharp
       (setq-local lsp-enabled-clients '(omnisharp))
       (setq-local lsp-disabled-clients '(csharp-ls csharp-roslyn))))))

(defun chief/fsharp-mode-setup ()
  "Configure F# buffers with FSAC completion, tests, and dotnet commands."
  (setq-local compile-command "dotnet build")
  (setq-local chief/lsp-root-function #'chief/dotnet-project-root)
  (chief/dotnet-ensure-tool-paths)
  (chief/dotnet-restore-before-lsp-start (chief/dotnet-project-file "fsproj"))
  (chief/polyglot-completion-setup t)
  (chief/format-enable-on-save)
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
    (setq-local lsp-fsharp-use-dotnet-tool-for-fsac nil)
    (setq-local lsp-fsharp-use-dotnet-local-tool
                (chief/dotnet-local-tool-p "fsautocomplete"))
    (setq-local lsp-fsharp-workspace-extra-exclude-dirs
                ["bin" "obj" ".git" ".ionide" ".fake"])))

(defun chief/vbnet-mode-setup ()
  "Configure VB.NET buffers with LSP completion and dotnet commands."
  (setq-local compile-command "dotnet build")
  (setq-local chief/lsp-root-function #'chief/dotnet-project-root)
  (chief/dotnet-ensure-tool-paths)
  (chief/dotnet-restore-before-lsp-start (chief/dotnet-project-file "vbproj"))
  (chief/polyglot-completion-setup t)
  (chief/format-enable-on-save)
  (when (require 'lsp-mode nil t)
    (setq-local lsp-enabled-clients '(vbnet-ls)))
  (when (fboundp 'chief/lsp-managed-mode-setup)
    (chief/lsp-managed-mode-setup)))

(defun chief/dotnet-cshtml-buffer-p ()
  "Return non-nil when the current buffer is a Razor view `.cshtml' file."
  (and buffer-file-name
       (string-match-p "\\.cshtml\\'" buffer-file-name)))

(defun chief/dotnet-razor-component-buffer-p ()
  "Return non-nil when the current buffer is a Blazor `.razor' file."
  (and buffer-file-name
       (string-match-p "\\.razor\\'" buffer-file-name)))

(defun chief/dotnet-razor-injected-symbol-position (symbol)
  "Return the buffer position of Razor injected SYMBOL, if declared."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((case-fold-search nil)
            (pattern (format "^[[:space:]]*@inject[[:space:]].*[[:space:]]\\(%s\\)\\_>"
                             (regexp-quote symbol))))
        (when (re-search-forward pattern nil t)
          (match-beginning 1))))))

(defun chief/dotnet-razor-model-directive-position ()
  "Return the position of the Razor @model directive, if present."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^[[:space:]]*\\(@model\\)\\_>" nil t)
        (match-beginning 1)))))

(defun chief/dotnet-razor-using-namespaces ()
  "Return namespaces imported with Razor @using directives in this buffer."
  (let ((namespaces nil))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward
                "^[[:space:]]*@using[[:space:]]+\\([[:alnum:]_.]+\\)" nil t)
          (push (match-string-no-properties 1) namespaces))))
    (delete-dups
     (append (nreverse namespaces)
             '("System"
               "System.Collections.Generic"
               "Microsoft.AspNetCore.Mvc"
               "Microsoft.Extensions.Localization")))))

(defun chief/dotnet-razor-workspace-symbol-item (symbol)
  "Return an exact workspace symbol xref item for SYMBOL, when available."
  (when (and (bound-and-true-p lsp-managed-mode)
             (fboundp 'lsp-request))
    (when-let* ((matches (condition-case nil
                            (lsp-request "workspace/symbol" `(:query ,symbol))
                          (error nil)))
                (match (seq-find
                        (lambda (candidate)
                          (string= symbol (gethash "name" candidate)))
                        matches))
                (location (gethash "location" match))
                (items (lsp--locations-to-xref-items location)))
      (car items))))

(defun chief/dotnet-razor-metadata-path (symbol)
  "Return a generated metadata file path for Razor type SYMBOL, if possible."
  (when (and (chief/dotnet-cshtml-buffer-p)
             (chief/lsp-ensure-active-for-navigation))
    (when-let* ((project (chief/dotnet-project-file "csproj")))
      (catch 'path
        (dolist (namespace (chief/dotnet-razor-using-namespaces))
          (let* ((qualified (if (string-match-p "\\." symbol)
                                symbol
                              (concat namespace "." symbol)))
                 (uri (format "csharp:%s/decompiled/%s.cs"
                              (expand-file-name project)
                              qualified))
                 (path (condition-case nil
                           (chief/dotnet-csharp-ls-metadata-uri-path uri)
                         (error nil))))
            (when (and path (file-readable-p path))
              (throw 'path path))))))))

(defun chief/dotnet-razor-goto-definition ()
  "Go to the Razor definition at point, with a local fallback for @inject.

csharp-ls handles C# well once restore assets are present, but its Razor support
is still experimental and can return no location for injected view symbols such
as `Localizer'.  This keeps `gd' useful for those local Razor symbols while still
trying LSP first."
  (interactive)
  (let* ((symbol (or (chief/token-at-point) ""))
         (model-pos (and (string= symbol "Model")
                         (chief/dotnet-razor-model-directive-position)))
         (inject-pos (chief/dotnet-razor-injected-symbol-position symbol))
         (lsp-item (and (not model-pos)
                        (not inject-pos)
                        (chief/dotnet-cshtml-buffer-p)
                        (chief/lsp-ensure-active-for-navigation)
                        (car (chief/lsp-location-items "textDocument/definition"))))
         (workspace-item (and (not model-pos)
                              (not inject-pos)
                              (not lsp-item)
                              (chief/dotnet-razor-workspace-symbol-item symbol)))
         (metadata-path (and (not model-pos)
                             (not inject-pos)
                             (not lsp-item)
                             (not workspace-item)
                             (chief/dotnet-razor-metadata-path symbol))))
    (cond
     (model-pos
      (xref-push-marker-stack)
      (goto-char model-pos))
     (inject-pos
      (xref-push-marker-stack)
      (goto-char inject-pos))
     (lsp-item
      (xref-push-marker-stack)
      (xref-pop-to-location lsp-item))
     (workspace-item
      (xref-push-marker-stack)
      (xref-pop-to-location workspace-item))
     (metadata-path
      (xref-push-marker-stack)
      (find-file metadata-path)
      (goto-char (point-min))
      (re-search-forward (format "\\_<%s\\_>" (regexp-quote symbol)) nil t))
     (t
      (message "No Razor definition found for: %s" symbol)))))

(defun chief/dotnet-razor-mode-setup ()
  "Configure Razor buffers.

`.cshtml' gets csharp-ls Razor support.  `.razor' gets web-mode Razor syntax
support; full Blazor `.razor' LSP requires rzls/Roslyn coordination that lsp-mode
does not currently ship as a stable client."
  (when (or (chief/dotnet-cshtml-buffer-p)
            (chief/dotnet-razor-component-buffer-p))
    (setq-local compile-command "dotnet build")
    (setq-local web-mode-engine "razor")
    (setq-local chief/lsp-root-function #'chief/dotnet-project-root)
    (setq-local chief/lsp-definition-function #'chief/dotnet-razor-goto-definition)
    (chief/dotnet-ensure-tool-paths)
    (chief/polyglot-completion-setup t))
  (when (chief/dotnet-cshtml-buffer-p)
    (chief/dotnet-restore-before-lsp-start (chief/dotnet-project-file "csproj"))
    (when (require 'lsp-csharp nil t)
      (setq-local lsp-enabled-clients '(csharp-ls))
      (setq-local lsp-disabled-clients '(omnisharp csharp-roslyn))
      (setq-local lsp-csharp-csharpls-use-dotnet-tool nil)
      (chief/dotnet-apply-solution-local)
      (when (fboundp 'chief/lsp-managed-mode-setup)
        (chief/lsp-managed-mode-setup)))))

(defun chief/dotnet-project-file-setup ()
  "Configure XML-like .NET project and MSBuild files."
  (when (and buffer-file-name
             (string-match-p "\\.\\(?:csproj\\|fsproj\\|vbproj\\|props\\|targets\\|slnx\\|resx\\)\\'"
                             buffer-file-name))
    (setq-local compile-command "dotnet build")
    (setq-local chief/lsp-root-function #'chief/dotnet-project-root)))

(defun chief/dotnet-solution-file-setup ()
  "Configure Visual Studio solution files."
  (when (and buffer-file-name
             (string-match-p "\\.slnf?\\'" buffer-file-name))
    (setq-local compile-command "dotnet build")
    (setq-local chief/lsp-root-function #'chief/dotnet-project-root)))

(chief/dotnet-ensure-tool-paths)

(chief/safe-use-package web-mode
  :mode ("\\.razor\\'" . web-mode)
  :mode ("\\.cshtml\\'" . web-mode)
  :hook (web-mode . chief/dotnet-razor-mode-setup)
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  (add-to-list 'web-mode-engines-alist '("razor" . "\\.\\(?:cshtml\\|razor\\)\\'")))

(chief/safe-use-package fsharp-mode
  :mode ("\\.fs[iylx]?\\'" . fsharp-mode)
  :hook (fsharp-mode . chief/fsharp-mode-setup))

(chief/safe-use-package vbnet-mode
  :straight (:host github :repo "emacsmirror/vbnet-mode")
  :mode ("\\.vb\\'" . vbnet-mode)
  :init
  ;; The package's legacy Flymake integration touches removed Emacs 31
  ;; internals; LSP/Flycheck provide diagnostics in this config.
  (setq vbnet-want-flymake-fixup nil)
  :hook (vbnet-mode . chief/vbnet-mode-setup))

(dolist (entry '(("\\.csx\\'" . csharp-ts-mode)
                 ("\\.cake\\'" . csharp-ts-mode)))
  (add-to-list 'auto-mode-alist entry))

(dolist (entry '(("\\.\\(?:csproj\\|fsproj\\|vbproj\\|props\\|targets\\|slnx\\|resx\\)\\'" . nxml-mode)
                 ("\\.slnf?\\'" . conf-mode)
                 ("global\\.json\\'" . json-mode)
                 ("NuGet\\.config\\'" . nxml-mode)
                 ("packages\\.config\\'" . nxml-mode)
                 ("paket\\.dependencies\\'" . conf-mode)
                 ("paket\\.references\\'" . conf-mode)))
  (add-to-list 'auto-mode-alist entry))

(add-hook 'csharp-mode-hook #'chief/csharp-mode-setup)
(when (fboundp 'csharp-ts-mode)
  (add-hook 'csharp-ts-mode-hook #'chief/csharp-mode-setup))
(add-hook 'nxml-mode-hook #'chief/dotnet-project-file-setup)
(add-hook 'conf-mode-hook #'chief/dotnet-solution-file-setup)

(with-eval-after-load 'lsp-mode
  (require 'lsp-csharp nil t)
  (require 'lsp-fsharp nil t)
  (require 'lsp-roslyn nil t)
  (add-to-list 'lsp-language-id-configuration '("\\.csx\\'" . "csharp"))
  (add-to-list 'lsp-language-id-configuration '("\\.cake\\'" . "csharp"))
  (add-to-list 'lsp-language-id-configuration '("\\.cshtml\\'" . "razor"))
  (add-to-list 'lsp-language-id-configuration '("\\.vb\\'" . "vb"))
  (lsp-register-custom-settings
   '(("csharp.razorSupport" chief/dotnet-csharp-ls-razor-support t)
     ("csharp.useMetadataUris" chief/dotnet-csharp-ls-use-metadata-uris t)
     ("csharp.analyzersEnabled" chief/dotnet-csharp-ls-analyzers-enabled t)
     ("csharp.applyFormattingOptions" chief/dotnet-csharp-ls-apply-formatting-options t)
     ("csharp.logLevel" chief/dotnet-csharp-ls-log-level)
     ("vbnet.output.language" "en-US")))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection #'chief/dotnet-vbnet-ls-command
                                          #'chief/dotnet-vbnet-ls-available-p)
    :activation-fn (lsp-activate-on "vb")
    :priority 0
    :server-id 'vbnet-ls)))

(with-eval-after-load 'lsp-csharp
  (advice-add 'lsp-csharp--cls-make-launch-cmd
              :override #'chief/dotnet-csharp-ls-command)
  (when-let* ((client (gethash 'csharp-ls lsp-clients)))
    (setf (lsp--client-activation-fn client)
          (lsp-activate-on "csharp" "razor"))
    (setf (lsp--client-uri->path-fn client)
          #'chief/dotnet-csharp-ls-uri-to-path)))

(with-eval-after-load 'lsp-fsharp
  (advice-add 'lsp-fsharp--make-launch-cmd
              :override #'chief/dotnet-fsautocomplete-command))

(with-eval-after-load 'csharp-mode
  (chief/local-leader-def
    :keymaps '(csharp-mode-map csharp-ts-mode-map)
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

(with-eval-after-load 'vbnet-mode
  (chief/local-leader-def
    :keymaps 'vbnet-mode-map
    "c" '(:ignore t :which-key "vbnet")
    "cb" #'chief/vbnet-build-project
    "cc" #'chief/vbnet-clean-project
    "ct" #'chief/vbnet-test-project
    "cr" #'chief/vbnet-run-project
    "cw" #'chief/vbnet-watch-run-project
    "cW" #'chief/vbnet-watch-test-project
    "cR" #'chief/vbnet-restore-project
    "cf" #'chief/vbnet-format-project
    "r" '(:ignore t :which-key "run")
    "rr" #'chief/vbnet-run-project
    "rw" #'chief/vbnet-watch-run-project
    "t" '(:ignore t :which-key "test")
    "tp" #'chief/vbnet-test-project
    "tw" #'chief/vbnet-watch-test-project
    "d" '(:ignore t :which-key "debug")
    "dd" #'chief/vbnet-debug-project
    "l" '(:ignore t :which-key "lsp")
    "la" #'lsp-execute-code-action
    "lf" #'lsp-format-buffer))

(provide 'lang-dotnet)
;;; lang-dotnet.el ends here
