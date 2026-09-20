;;; lang-dotnet.el --- Dedicated .NET language support -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'comint)
(require 'compile)
(require 'csharp-mode)
(require 'core-bootstrap)
(require 'core-format)
(require 'core-lsp)
(require 'core-projects)
(require 'core-repl)
(require 'json)
(require 'lang-polyglot)
(require 'project)
(require 'seq)
(require 'shr)
(require 'subr-x)
(require 'url-util)
(require 'xml)

(declare-function dap-debug "dap-mode" (debug-args))
(defvar dap-netcore-install-dir)
(declare-function lsp-execute-code-action "lsp-mode" ())
(declare-function lsp-format-buffer "lsp-mode" ())
(declare-function lsp-organize-imports "lsp-mode" ())
(declare-function lsp--path-to-uri "lsp-mode" (path))
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

(defcustom chief/dotnet-restore-before-lsp nil
  "When non-nil, run `dotnet restore' before LSP if assets are missing.

This is disabled by default because opening a source buffer must not block on a
network restore.  Roslyn and FsAutoComplete can start immediately; use the
explicit restore commands when a project needs restored assets."
  :type 'boolean
  :group 'chief-dotnet)

(defcustom chief/dotnet-restore-timeout 180
  "Seconds to wait for the synchronous pre-LSP `dotnet restore'."
  :type 'integer
  :group 'chief-dotnet)

(defcustom chief/fsharp-metadata-cache-directory
  (locate-user-emacs-file "var/fsharp-metadata/")
  "Directory for external F# definitions decompiled with ILSpy."
  :type 'directory
  :group 'chief-dotnet)

(defvar-local chief/dotnet-metadata-source-root nil
  "Original project root for a generated .NET metadata buffer.")
(put 'chief/dotnet-metadata-source-root 'permanent-local t)

(defvar-local chief/dotnet-metadata-source-solution nil
  "Original solution file for a generated .NET metadata buffer.")
(put 'chief/dotnet-metadata-source-solution 'permanent-local t)

(defvar-local chief/dotnet-metadata-workspace-root nil
  "Synthetic C# workspace root for a generated .NET metadata buffer.")
(put 'chief/dotnet-metadata-workspace-root 'permanent-local t)

(defvar-local chief/fsharp-metadata-lsp-offered nil
  "Non-nil after offering to provision LSP for an F# metadata buffer.")

(defvar chief/dotnet-restore-history (make-hash-table :test #'equal)
  "Projects already considered for pre-LSP restore in this Emacs session.")

(defvar chief/dotnet-roslyn-metadata-roots (make-hash-table :test #'equal)
  "Map Roslyn metadata-as-source files back to their workspace roots.")

(defvar chief/dotnet-roslyn-ready-workspaces
  (make-hash-table :test #'eq :weakness 'key)
  "Roslyn workspaces that completed project initialization.")

(defvar chief/dotnet-roslyn-initialization-timers
  (make-hash-table :test #'eq :weakness 'key)
  "Pending project-initialization grace timers keyed by Roslyn workspace.")

(defvar chief/dotnet-tool-installations (make-hash-table :test #'equal)
  "Active global dotnet tool installation buffers keyed by package.")

(defvar chief/dotnet-tool-install-callbacks (make-hash-table :test #'equal)
  "Queued completion callbacks for global dotnet tool installations.")

(defvar chief/dotnet-repl-reload-processes (make-hash-table :test #'equal)
  "Active project rebuilds requested by .NET REPL load commands.")

(defvar chief/dotnet-nuget-packages-cache (make-hash-table :test #'equal)
  "Effective NuGet global-packages directories keyed by project root.")

(defvar chief/dotnet-namespace-types-cache (make-hash-table :test #'equal)
  "External namespace type indexes keyed by project state and namespace.")

(defvar chief/dotnet-type-assembly-cache (make-hash-table :test #'equal)
  "Assemblies known to define fully qualified .NET type names.")

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

(defcustom chief/dotnet-netcoredbg-version "3.2.0-1092"
  "Maintained Samsung netcoredbg release installed for .NET DAP sessions."
  :type 'string
  :group 'chief-dotnet)

(defcustom chief/dotnet-netcoredbg-install-directory
  (locate-user-emacs-file ".cache/lsp/netcoredbg/")
  "Directory containing the installed netcoredbg release."
  :type 'directory
  :group 'chief-dotnet)

(defcustom chief/dotnet-roll-forward "Major"
  "DOTNET_ROLL_FORWARD value used for editor-launched .NET tools.

This lets maintained editor tools target an older supported .NET runtime while
running on a newer installed runtime.  Set to nil to leave the variable
unchanged."
  :type '(choice (const nil)
                 (const "Minor")
                 (const "Major")
                 (const "LatestMinor")
                 (const "LatestMajor")
                 (const "Disable")
                 string)
  :group 'chief-dotnet)

(defcustom chief/dotnet-roll-forward-to-prerelease t
  "When non-nil, let editor tools run on an installed prerelease runtime.

This only affects processes launched by Emacs and is useful when the installed
SDK is a preview or release candidate newer than a tool's target runtime."
  :type 'boolean
  :group 'chief-dotnet)

(defcustom chief/dotnet-csharp-server 'roslyn
  "Preferred maintained C# language server.

`roslyn' is Microsoft's official `roslyn-language-server' dotnet tool and is
the default.  `csharp-ls' remains available as a maintained lightweight
alternative and for its experimental Razor support."
  :type '(choice (const :tag "Official Microsoft Roslyn" roslyn)
                 (const :tag "csharp-ls" csharp-ls))
  :group 'chief-dotnet)

(defcustom chief/dotnet-roslyn-log-level "Information"
  "Log level passed to the official Roslyn language server."
  :type '(choice (const "Trace")
                 (const "Debug")
                 (const "Information")
                 (const "Warning")
                 (const "Error")
                 (const "None"))
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

(defconst chief/dotnet-msbuild-project-regexp
  "\\.\\(?:[[:alnum:]_-]+proj\\|proj\\)\\'"
  "Regexp matching SDK and legacy MSBuild project file names.")

(defconst chief/dotnet-xml-file-regexp
  (concat
   "\\(?:"
   "\\.\\(?:[[:alnum:]_-]+proj\\|proj\\)\\(?:\\.user\\)?"
   "\\|\\.\\(?:projitems\\|props\\|targets\\|tasks\\|slnx\\|resx"
   "\\|ruleset\\|testsettings\\|runsettings\\|pubxml\\|nuspec"
   "\\|vsixmanifest\\|manifest\\|appxmanifest\\|appinstaller\\|trx"
   "\\|items\\|filters\\|natvis\\|xaml\\|axaml\\|xoml"
   "\\|[Dd]ot[Ss]ettings\\)\\(?:\\.user\\)?"
   "\\)\\'")
  "Regexp matching XML-based .NET workspace and configuration files.")

(defconst chief/dotnet-json-file-regexp
  (concat
   "\\(?:\\.slnf"
   "\\|\\.\\(?:deps\\|runtimeconfig\\|runtimeconfig\\.dev"
   "\\|nuget\\.dgspec\\)\\.json"
   "\\)\\'")
  "Regexp matching suffix-based JSON .NET workspace files.")

(defconst chief/dotnet-plain-file-regexp
  "\\.\\(?:sln\\|rsp\\|editorconfig\\|globalconfig\\)\\'"
  "Regexp matching plain-text .NET workspace files.")

(defun chief/dotnet-project-root ()
  "Return the current .NET project root."
  (chief/project-preferred-root
   (chief/project-nearest-regexp-root "\\.slnx?\\'")
   (chief/project-nearest-regexp-root "\\.slnf\\'")
   (chief/project-nearest-regexp-root chief/dotnet-msbuild-project-regexp)
   (chief/project-nearest-marker-root
    '("global.json"
      "Directory.Build.props"
      "Directory.Build.targets"
      "Directory.Build.rsp"
      "Directory.Packages.props"
      "NuGet.config"
      "paket.dependencies"
      "dotnet-tools.json"
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
    (setenv "DOTNET_ROLL_FORWARD" chief/dotnet-roll-forward))
  (when chief/dotnet-roll-forward-to-prerelease
    (setenv "DOTNET_ROLL_FORWARD_TO_PRERELEASE" "1")))

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
  "Return the nearest dotnet tools manifest, if present.

Recognize both the conventional `.config/dotnet-tools.json' path and the
root-level `dotnet-tools.json' accepted by current .NET SDKs."
  (when-let* ((start (chief/project-default-start-directory))
              (root
               (locate-dominating-file
                start
                (lambda (directory)
                  (or (file-readable-p
                       (expand-file-name ".config/dotnet-tools.json" directory))
                      (file-readable-p
                       (expand-file-name "dotnet-tools.json" directory)))))))
    (let ((configured (expand-file-name ".config/dotnet-tools.json" root))
          (root-level (expand-file-name "dotnet-tools.json" root)))
      (if (file-readable-p configured) configured root-level))))

(defun chief/dotnet-local-tool-p (tool)
  "Return non-nil when TOOL appears in the nearest dotnet tool manifest."
  (when-let* ((manifest (chief/dotnet-tool-manifest-file))
              ((file-readable-p manifest)))
    (with-temp-buffer
      (let ((case-fold-search t))
        (insert-file-contents manifest)
        (re-search-forward (regexp-quote tool) nil t)))))

(defun chief/dotnet-local-tool-entry (tool)
  "Return the manifest package and version providing local TOOL."
  (when-let* ((manifest (chief/dotnet-tool-manifest-file))
              ((file-readable-p manifest)))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents manifest)
          (let* ((document
                  (json-parse-buffer
                   :object-type 'hash-table
                   :array-type 'list
                   :null-object nil
                   :false-object nil))
                 (tools (gethash "tools" document))
                 found)
            (when (hash-table-p tools)
              (maphash
               (lambda (package details)
                 (when (and (not found)
                            (or (string-equal
                                 (downcase package) (downcase tool))
                                (member tool (gethash "commands" details))))
                   (setq found
                         (cons package (gethash "version" details)))))
               tools))
            found))
      (error nil))))

(defun chief/dotnet-nuget-global-packages-directory ()
  "Return the effective NuGet global-packages directory for this project."
  (or (when-let* ((configured (getenv "NUGET_PACKAGES")))
        (file-name-as-directory (expand-file-name configured)))
      (let* ((root (or (and (chief/dotnet-tool-manifest-file)
                            (file-name-directory
                             (chief/dotnet-tool-manifest-file)))
                       (chief/dotnet-project-root)))
             (key (expand-file-name root))
             (cached (gethash key chief/dotnet-nuget-packages-cache :missing)))
        (if (not (eq cached :missing))
            cached
          (let ((default-directory root)
                (dotnet (chief/dotnet-tool-executable "dotnet"))
                directory)
            (when dotnet
              (with-temp-buffer
                (when (zerop
                       (process-file
                        dotnet nil t nil
                        "nuget" "locals" "global-packages" "--list"))
                  (goto-char (point-min))
                  (when (re-search-forward
                         "^[^:\n]+:[[:space:]]*\\(.+\\)$" nil t)
                    (setq directory
                          (file-name-as-directory
                           (expand-file-name
                            (string-trim
                             (match-string-no-properties 1)))))))))
            (setq directory
                  (or directory
                      (file-name-as-directory
                       (expand-file-name "~/.nuget/packages"))))
            (puthash key directory chief/dotnet-nuget-packages-cache)
            directory)))))

(defun chief/dotnet-local-tool-ready-p (tool)
  "Return non-nil when local TOOL is configured and restored."
  (when-let* ((entry (chief/dotnet-local-tool-entry tool))
              (package (car entry))
              (version (cdr entry))
              (packages-root
               (chief/dotnet-nuget-global-packages-directory)))
    (let ((package-directory
           (expand-file-name
            (format "%s/%s" (downcase package) (downcase version))
            packages-root)))
      (and (file-directory-p package-directory)
           (file-readable-p
            (expand-file-name ".nupkg.metadata" package-directory))))))

(defun chief/dotnet-local-tool-command (tool &rest args)
  "Return a `dotnet tool run' command for restored local TOOL and ARGS."
  (when (chief/dotnet-local-tool-ready-p tool)
    (append (chief/dotnet-command "tool" "run" tool) args)))

(defun chief/dotnet-tool-installable-p ()
  "Return non-nil when Emacs can install a dotnet tool."
  (chief/dotnet-ensure-tool-paths)
  (executable-find "dotnet"))

(defun chief/dotnet-install-global-tool (package &optional prerelease on-success)
  "Install dotnet tool PACKAGE globally in a compilation buffer.
Include prerelease versions when PRERELEASE is non-nil.  Run outside the
current project so an unrelated `global.json' cannot prevent provisioning.
Call every queued ON-SUCCESS callback after one shared installation succeeds."
  (if-let* ((active (gethash package chief/dotnet-tool-installations))
            ((buffer-live-p active))
            (process (get-buffer-process active))
            ((process-live-p process)))
      (progn
        (when on-success
          (puthash package
                   (append
                    (gethash package chief/dotnet-tool-install-callbacks)
                    (list on-success))
                   chief/dotnet-tool-install-callbacks))
        active)
    (remhash package chief/dotnet-tool-installations)
    (puthash package (and on-success (list on-success))
             chief/dotnet-tool-install-callbacks)
    (let* ((default-directory (file-name-as-directory (expand-file-name "~")))
           (command
            (append (chief/dotnet-command
                     "tool" "install" "--global" package
                     "--allow-roll-forward")
                    (when prerelease (list "--prerelease"))))
           (compilation-read-command nil)
           (buffer
            (compilation-start
             (mapconcat #'shell-quote-argument command " ")
             'compilation-mode
             (lambda (_) (format "*install %s*" package)))))
      (puthash package buffer chief/dotnet-tool-installations)
      (with-current-buffer buffer
        (add-hook
         'compilation-finish-functions
         (lambda (_buffer status)
           (remhash package chief/dotnet-tool-installations)
           (let ((callbacks
                  (prog1
                      (gethash package chief/dotnet-tool-install-callbacks)
                    (remhash package chief/dotnet-tool-install-callbacks))))
             (when (string-prefix-p "finished" status)
               (dolist (callback callbacks)
                 (condition-case err
                     (funcall callback)
                   (error
                    (message "%s post-install action failed: %s"
                             package (error-message-string err))))))))
         nil t))
      buffer)))

(defun chief/dotnet-install-roslyn-language-server (&optional on-success)
  "Install Microsoft's current official Roslyn language-server tool.
Call ON-SUCCESS after installation when it is non-nil."
  (interactive)
  (chief/dotnet-install-global-tool
   "roslyn-language-server" t on-success))

(defun chief/dotnet-install-fsautocomplete (&optional on-success)
  "Install the current FsAutoComplete language-server tool.
Call ON-SUCCESS after installation when it is non-nil."
  (interactive)
  (chief/dotnet-install-global-tool "fsautocomplete" nil on-success))

(defun chief/dotnet-install-csharp-ls (&optional on-success)
  "Install the maintained csharp-ls dotnet tool.
Call ON-SUCCESS after installation when it is non-nil."
  (interactive)
  (chief/dotnet-install-global-tool "csharp-ls" nil on-success))

(defun chief/dotnet-install-csharp-repl (&optional on-success)
  "Install the maintained CSharpRepl dotnet tool.
Call ON-SUCCESS after installation when it is non-nil."
  (interactive)
  (chief/dotnet-install-global-tool "csharprepl" nil on-success))

(defun chief/dotnet-install-interactive (&optional on-success)
  "Install Microsoft's maintained .NET Interactive tool.
Call ON-SUCCESS after installation when it is non-nil."
  (interactive)
  (chief/dotnet-install-global-tool
   "microsoft.dotnet-interactive" nil on-success))

(defun chief/dotnet-install-fantomas (&optional on-success)
  "Install the maintained Fantomas F# formatter dotnet tool.
Call ON-SUCCESS after installation when it is non-nil."
  (interactive)
  (chief/dotnet-install-global-tool "fantomas" nil on-success))

(defun chief/dotnet-install-ilspycmd (&optional on-success)
  "Install the maintained ILSpy command-line decompiler tool.
Call ON-SUCCESS after installation when it is non-nil."
  (interactive)
  (chief/dotnet-install-global-tool "ilspycmd" nil on-success))

(defun chief/dotnet-netcoredbg-executable ()
  "Return the maintained netcoredbg executable, when installed."
  (or (executable-find "netcoredbg")
      (let ((candidate
             (expand-file-name
              (if (eq system-type 'windows-nt)
                  "netcoredbg/netcoredbg.exe"
                "netcoredbg/netcoredbg")
              chief/dotnet-netcoredbg-install-directory)))
        (and (file-executable-p candidate) candidate))))

(defun chief/dotnet-netcoredbg-release ()
  "Return the netcoredbg release asset name and archive kind for this host."
  (let ((arm-p (string-match-p "\\(?:aarch64\\|arm64\\)"
                               system-configuration)))
    (pcase system-type
      ('darwin
       (unless arm-p
         (user-error "Current netcoredbg releases support macOS ARM64 only"))
       '("netcoredbg-osx-arm64.zip" . zip))
      ('gnu/linux
       (cons (if arm-p
                 "netcoredbg-linux-arm64.tar.gz"
               "netcoredbg-linux-amd64.tar.gz")
             'tar))
      ('windows-nt '("netcoredbg-win64.zip" . zip))
      (_ (user-error "Unsupported netcoredbg platform: %s" system-type)))))

(defun chief/dotnet-netcoredbg-install-command ()
  "Return a shell command that installs the configured netcoredbg release."
  (pcase-let* ((`(,asset . ,kind) (chief/dotnet-netcoredbg-release))
               (version chief/dotnet-netcoredbg-version)
               (url
                (format
                 "https://github.com/Samsung/netcoredbg/releases/download/%s/%s"
                 version asset))
               (destination
                (directory-file-name
                 (expand-file-name
                  chief/dotnet-netcoredbg-install-directory)))
               (archive (expand-file-name asset temporary-file-directory)))
    (if (eq system-type 'windows-nt)
        (cl-labels ((quote-powershell
                     (value)
                     (concat "'"
                             (replace-regexp-in-string "'" "''" value t t)
                             "'")))
          (let ((script
                 (string-join
                  (list
                   "$ErrorActionPreference = 'Stop'"
                   (format
                    (concat "if (Test-Path -LiteralPath %s) "
                            "{ Remove-Item -LiteralPath %s -Recurse -Force }")
                    (quote-powershell destination)
                    (quote-powershell destination))
                   (format
                    (concat "New-Item -ItemType Directory -Force -Path %s "
                            "| Out-Null")
                    (quote-powershell destination))
                   (format "Invoke-WebRequest -Uri %s -OutFile %s"
                           (quote-powershell url)
                           (quote-powershell archive))
                   (format
                    (concat "Expand-Archive -LiteralPath %s "
                            "-DestinationPath %s -Force")
                    (quote-powershell archive)
                    (quote-powershell destination))
                   (format "Remove-Item -LiteralPath %s -Force"
                           (quote-powershell archive)))
                  "; ")))
            (format
             "powershell.exe -NoProfile -NonInteractive -ExecutionPolicy Bypass -Command %s"
             (shell-quote-argument script))))
      (let ((extract
             (pcase kind
               ('zip
                (format "unzip -q -o %s -d %s"
                        (shell-quote-argument archive)
                        (shell-quote-argument destination)))
               ('tar
                (format "tar xzf %s -C %s"
                        (shell-quote-argument archive)
                        (shell-quote-argument destination))))))
        (string-join
         (delq
          nil
          (list "set -e"
                (format "rm -rf %s" (shell-quote-argument destination))
                (format "mkdir -p %s" (shell-quote-argument destination))
                (format "curl -fL %s -o %s"
                        (shell-quote-argument url)
                        (shell-quote-argument archive))
                extract
                (format "chmod +x %s"
                        (shell-quote-argument
                         (expand-file-name
                          "netcoredbg/netcoredbg" destination)))
                (format "rm -f %s" (shell-quote-argument archive))))
         " && ")))))

(defun chief/dotnet-install-netcoredbg (&optional on-success)
  "Install netcoredbg, deduplicating work and then run ON-SUCCESS."
  (interactive)
  (let ((key "netcoredbg"))
    (if-let* ((active (gethash key chief/dotnet-tool-installations))
              ((buffer-live-p active))
              (process (get-buffer-process active))
              ((process-live-p process)))
        (progn
          (when on-success
            (puthash key
                     (append
                      (gethash key chief/dotnet-tool-install-callbacks)
                      (list on-success))
                     chief/dotnet-tool-install-callbacks))
          active)
      (remhash key chief/dotnet-tool-installations)
      (puthash key (and on-success (list on-success))
               chief/dotnet-tool-install-callbacks)
      (let* ((default-directory
              (file-name-as-directory (expand-file-name "~")))
             (compilation-read-command nil)
             (buffer
              (compilation-start
               (chief/dotnet-netcoredbg-install-command)
               'compilation-mode
               (lambda (_) "*install netcoredbg*"))))
        (puthash key buffer chief/dotnet-tool-installations)
        (with-current-buffer buffer
          (add-hook
           'compilation-finish-functions
           (lambda (_buffer status)
             (remhash key chief/dotnet-tool-installations)
             (let ((callbacks
                    (prog1
                        (gethash key chief/dotnet-tool-install-callbacks)
                      (remhash key chief/dotnet-tool-install-callbacks))))
               (when (and (string-prefix-p "finished" status)
                          (chief/dotnet-netcoredbg-executable))
                 (dolist (callback callbacks)
                   (condition-case err
                       (funcall callback)
                     (error
                      (message "netcoredbg post-install action failed: %s"
                               (error-message-string err))))))))
           nil t))
        buffer))))

(defun chief/dotnet-require-netcoredbg (description &optional on-success)
  "Return netcoredbg for DESCRIPTION or offer installation.
ON-SUCCESS resumes the requested action after installation."
  (or (chief/dotnet-netcoredbg-executable)
      (progn
        (when (and (not noninteractive)
                   (yes-or-no-p
                    (format "%s requires netcoredbg.  Install it now? "
                            description)))
          (chief/dotnet-install-netcoredbg on-success)
          (user-error
           "netcoredbg installation started; the action will resume when it finishes"))
        (user-error "%s requires the maintained netcoredbg adapter"
                    description))))

(defun chief/dotnet-require-global-tool
    (program package installer description &optional on-success)
  "Return PROGRAM's executable or offer to install PACKAGE with INSTALLER.
DESCRIPTION names the feature that needs the tool.  ON-SUCCESS resumes the
requested action after a successful installation."
  (or (chief/dotnet-tool-executable program)
      (progn
        (when (and (not noninteractive)
                   (yes-or-no-p
                    (format "%s requires %s.  Install it now? " description package)))
          (funcall installer on-success)
          (user-error "%s installation started; the action will resume when it finishes"
                      package))
        (user-error "%s requires the maintained dotnet tool %s" description package))))

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
  "Return dotnet VERB command for the selected target and ARGS.
Restrict projects to EXTENSION and favor one over a solution when
PREFER-PROJECT is non-nil."
  (append (chief/dotnet-command verb)
          (when-let* ((target (chief/dotnet-build-target extension prefer-project)))
            (list target))
          args))

(defun chief/dotnet-compile (command name &optional directory)
  "Run dotnet COMMAND using compilation buffer NAME in DIRECTORY."
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

(defun chief/dotnet-sdk-target-framework ()
  "Return a target framework matching the active dotnet SDK."
  (when-let* ((dotnet (chief/dotnet-tool-executable "dotnet"))
              (default-directory
               (file-name-as-directory (expand-file-name "~")))
              (version
               (car (ignore-errors (process-lines dotnet "--version"))))
              ((string-match "\\`\\([0-9]+\\)\\." version)))
    (format "net%s.0" (match-string 1 version))))

(defun chief/dotnet-assembly-target-framework (assembly)
  "Return a target framework inferred from ASSEMBLY's path."
  (when (string-match
         "/\\(net[0-9]+\\(?:\\.[0-9]+\\)?\\)/" assembly)
    (match-string 1 assembly)))

(defun chief/dotnet-project-assembly-name (project)
  "Return PROJECT assembly name."
  (or (chief/dotnet-project-property project 'AssemblyName)
      (file-name-base project)))

(defun chief/dotnet-project-output-dll (project)
  "Return PROJECT's expected or most recent build output dll path."
  (let* ((directory (file-name-directory project))
         (framework (chief/dotnet-project-target-framework project))
         (assembly (chief/dotnet-project-assembly-name project))
         (configuration-directory
          (expand-file-name
           (format "bin/%s" chief/dotnet-build-configuration)
           directory))
         (expected
          (and framework
               (expand-file-name
                (format "%s/%s.dll" framework assembly)
                configuration-directory)))
         (built
          (and (file-directory-p configuration-directory)
               (car
                (sort
                 (directory-files-recursively
                  configuration-directory
                  (format "/%s\\.dll\\'" (regexp-quote assembly)))
                 (lambda (left right)
                   (file-newer-than-file-p left right)))))))
    (or expected built
        (user-error "No TargetFramework or built output found in %s" project))))

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
  (chief/dotnet-require-netcoredbg
   name
   (lambda ()
     (chief/dotnet-debug-project project name)))
  (require 'dap-mode)
  (require 'dap-netcore)
  (setq dap-netcore-install-dir chief/dotnet-netcoredbg-install-directory)
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

(defun chief/dotnet-debug-test-attach (process project names)
  "Attach DAP to the test host announced by PROCESS.
PROJECT and NAMES describe the test invocation.  Return non-nil after attach."
  (unless (process-get process 'chief/dotnet-debug-test-attached)
    (when-let* ((buffer (process-buffer process))
                ((buffer-live-p buffer))
                (pid
                 (with-current-buffer buffer
                   (save-excursion
                     (goto-char (point-min))
                     (when (re-search-forward
                            "Process Id: [[:space:]]*\\([0-9]+\\)" nil t)
                       (string-to-number
                        (match-string-no-properties 1)))))))
      (condition-case err
          (progn
            (dap-debug
             (list :type "coreclr"
                   :request "attach"
                   :mode "attach"
                   :name (format ".NET test :: %s"
                                 (or (car names) project))
                   :processId pid))
            (process-put process 'chief/dotnet-debug-test-attached t)
            (when-let* ((timer
                         (process-get
                          process 'chief/dotnet-debug-test-timer)))
              (cancel-timer timer)
              (process-put process 'chief/dotnet-debug-test-timer nil))
            (set-process-filter process nil)
            t)
        (error
         ;; Leave the process filter and timer active: a synchronously failing
         ;; DAP launch must not consume the detected PID or strand VSTest.
         (process-put process 'chief/dotnet-debug-test-attach-error err)
         (message "Could not attach to .NET test host %s: %s"
                  pid (error-message-string err))
         nil)))))

(defun chief/dotnet-debug-test (extension names)
  "Debug .NET tests in EXTENSION project filtered by NAMES."
  (let ((source-buffer (current-buffer)))
    (chief/dotnet-require-netcoredbg
     ".NET test debugging"
     (lambda ()
       (when (buffer-live-p source-buffer)
         (with-current-buffer source-buffer
           (chief/dotnet-debug-test extension names))))))
  (require 'dap-mode)
  (require 'dap-netcore)
  (setq dap-netcore-install-dir chief/dotnet-netcoredbg-install-directory)
  (let* ((project (chief/dotnet-run-project extension))
         (framework (chief/dotnet-project-target-framework project))
         (buffer (get-buffer-create "*dotnet debug test*"))
         (existing (get-buffer-process buffer))
         (command
          (append (chief/dotnet-command
                   "test" project "--verbosity" "Quiet")
                  (when framework (list "--framework" framework))
                  (when names
                    (list "--filter" (chief/dotnet-test-filter names))))))
    (when (and existing (process-live-p existing))
      (user-error "A .NET test debug session is already waiting for attach"))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (setq buffer-read-only nil)
        (erase-buffer)
        (setq-local default-directory (file-name-directory project))))
    (let* ((process-environment
            (cons "VSTEST_HOST_DEBUG=1" process-environment))
           (process
            (make-process
             :name "dotnet debug test"
             :buffer buffer
             ;; Current VSTest writes its attach PID to stderr.  Merge both
             ;; streams before Emacs receives them so the filter can detect it.
             :command
             (list shell-file-name shell-command-switch
                   (concat
                    (mapconcat #'shell-quote-argument command " ")
                    " 2>&1"))
             ;; VSTest suppresses its debugger PID when attached to a PTY.
             :connection-type 'pipe
             :noquery t
             :filter
             (lambda (owner output)
               (when (buffer-live-p (process-buffer owner))
                 (with-current-buffer (process-buffer owner)
                   (let ((moving (= (point) (process-mark owner))))
                     (save-excursion
                       (goto-char (process-mark owner))
                       (insert output)
                       (set-marker (process-mark owner) (point)))
                     (when moving
                       (goto-char (process-mark owner)))))
                 (chief/dotnet-debug-test-attach owner project names)))
             :sentinel
             (lambda (owner event)
               (when-let* ((timer
                            (process-get
                             owner 'chief/dotnet-debug-test-timer)))
                 (cancel-timer timer))
               (when (and (memq (process-status owner) '(exit signal))
                          (buffer-live-p (process-buffer owner)))
                 (message "dotnet debug test %s" (string-trim event))
                 (display-buffer (process-buffer owner)))))))
      (let ((timer
             (run-at-time
              0.1 0.2
              (lambda ()
                (if (process-live-p process)
                    (chief/dotnet-debug-test-attach process project names)
                  (when-let* ((active
                               (process-get
                                process 'chief/dotnet-debug-test-timer)))
                    (cancel-timer active)))))))
        ;; The process filter can attach before `make-process' returns.  Do not
        ;; install an orphan polling timer after such an immediate success.
        (if (process-get process 'chief/dotnet-debug-test-attached)
            (cancel-timer timer)
          (process-put process 'chief/dotnet-debug-test-timer timer)))
      process)))

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
  "Apply whitespace-only `dotnet format' to the nearest C# project."
  (interactive)
  (let ((project (or (chief/dotnet-project-file "csproj")
                     (user-error "No C# project found"))))
    (chief/dotnet-compile
     (append (chief/dotnet-command "format" "whitespace")
             (list project))
     "*dotnet format csharp*"
     (file-name-directory project))))

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

(defun chief/fsharp-external-definition-info (err)
  "Extract external symbol metadata from an FSAC definition error ERR.

FsAutoComplete currently fails to decompile some assemblies produced for newer
.NET runtimes.  Return a plist containing the assembly, declaring type, and
member so a current ILSpy can provide the definition instead."
  (let* ((message (chief/lsp-request-error-message err))
         (whitespace "[[:space:]\n\r]*")
         (case-fold-search nil)
         assembly type member)
    (when (string-match
           "in file [\"'‘’]\\([^\"'‘’\n]+\\.dll\\)[\"'‘’]"
           message)
      (setq assembly (match-string 1 message)))
    (cond
     ((string-match
       (concat "\\(?:Method\\|Property\\|Field\\|Event\\)"
               whitespace "(\"\\([^\"]+\\)\""
               whitespace "," whitespace "\"\\([^\"]+\\)\"")
       message)
      (setq type (match-string 1 message)
            member (match-string 2 message)))
     ((string-match
       (concat "Constructor" whitespace "(\"\\([^\"]+\\)\"")
       message)
      (setq type (match-string 1 message)
            member (car (last (split-string type "[.+]" t)))))
     ((string-match
       (concat "Type" whitespace "[(]?" whitespace
               "\"\\([^\"]+\\)\"")
       message)
      (setq type (match-string 1 message))))
    (when (and assembly type (file-readable-p assembly))
      (list :assembly assembly :type type :member member :message message))))

(defun chief/dotnet-lsp-content-strings (value)
  "Return all strings nested in LSP content VALUE."
  (cond
   ((stringp value) (list value))
   ((vectorp value)
    (cl-mapcan #'chief/dotnet-lsp-content-strings (append value nil)))
   ((hash-table-p value)
    (let (strings)
      (maphash
       (lambda (_key item)
         (setq strings
               (nconc strings (chief/dotnet-lsp-content-strings item))))
       value)
      strings))
   ((consp value)
    (cl-mapcan #'chief/dotnet-lsp-content-strings value))))

(defun chief/fsharp-hover-documentation-data (hover)
  "Return FsAutoComplete documentation metadata embedded in HOVER."
  (when-let* ((contents (and hover (lsp:hover-contents hover)))
              (text (string-join
                     (chief/dotnet-lsp-content-strings contents) "\n"))
              ((string-match
                "command:fsharp\\.showDocumentation\\?\\([^'\">]+\\)" text))
              (encoded (match-string 1 text))
              (decoded (url-unhex-string encoded))
              (document
               (ignore-errors
                 (json-parse-string
                  decoded :object-type 'hash-table :array-type 'list
                  :null-object nil :false-object nil)))
              (item (car-safe document)))
    item))

(defun chief/fsharp-xml-doc-definition (signature)
  "Return declaring type and member from XML documentation SIGNATURE."
  (when (and (stringp signature)
             (string-match "\\`\\([TMPFE]\\):\\(.+\\)" signature))
    (let* ((kind (match-string 1 signature))
           (body (match-string 2 signature))
           (name (car (split-string body "(")))
           (separator (and (not (string= kind "T"))
                           (string-match "\\.[^.]+\\'" name)))
           (type (if separator (substring name 0 separator) name))
           (member (and separator (substring name (1+ separator)))))
      (when member
        (setq member
              (replace-regexp-in-string "``[0-9]+\\'" "" member)))
      (when (string= member "#ctor")
        (setq member (car (last (split-string type "[.+]" t)))))
      (list :type type :member member))))

(defun chief/dotnet-assembly-by-name (name &optional project)
  "Return the assembly named NAME available to PROJECT."
  (seq-find
   (lambda (file)
     (string-equal (file-name-base file) name))
   (chief/dotnet-reference-assembly-files project)))

(defun chief/fsharp-nonexistent-source-info (err params)
  "Recover external definition metadata for an FSAC source-path ERR.
PARAMS identifies the original source position.  FsAutoComplete can return
source paths from its build machine for framework symbols; its hover response
still contains an exact XML documentation signature and assembly identity."
  (let ((message (chief/lsp-request-error-message err)))
    (when (string-match-p "Range for nonexistent file found" message)
      (when-let* ((hover
                   (condition-case nil
                       (lsp-request "textDocument/hover" params)
                     (error nil)))
                  (documentation
                   (chief/fsharp-hover-documentation-data hover))
                  (signature (gethash "XmlDocSig" documentation))
                  (assembly-name (gethash "AssemblyName" documentation))
                  (definition (chief/fsharp-xml-doc-definition signature))
                  (assembly
                   (chief/dotnet-assembly-by-name assembly-name)))
        (append (list :assembly assembly :message message) definition)))))

(defun chief/fsharp-metadata-workspace-directory
    (assembly source-root &optional source-project)
  "Return the metadata workspace for ASSEMBLY and SOURCE-ROOT.
SOURCE-PROJECT distinguishes C# and F# projects in a mixed solution."
  (let ((project-id
         (substring
          (secure-hash 'sha1 (or source-project source-root)) 0 12))
        (assembly-id (substring (secure-hash 'sha1 assembly) 0 12)))
    (expand-file-name
     (format "%s/%s-%s/"
             project-id (file-name-base assembly) assembly-id)
     chief/fsharp-metadata-cache-directory)))

(defun chief/fsharp-metadata-file
    (assembly type source-root &optional source-project)
  "Return metadata for ASSEMBLY and TYPE from SOURCE-ROOT.
SOURCE-PROJECT keeps mixed-language synthetic projects independent."
  (let ((safe-type (replace-regexp-in-string "[^[:alnum:]_.-]" "_" type)))
    (expand-file-name
     (concat safe-type ".cs")
     (chief/fsharp-metadata-workspace-directory
      assembly source-root source-project))))

(defun chief/fsharp-metadata-reference-items (assembly reference-directory)
  "Return MSBuild references for ASSEMBLY and REFERENCE-DIRECTORY.

The project build output supplies package dependencies that are not colocated
with NuGet assemblies.  Framework assemblies remain SDK-provided to avoid
conflicting with reference packs."
  (let* ((assembly-name (file-name-base assembly))
         (dependencies
          (when (file-directory-p reference-directory)
            (seq-filter
             (lambda (file)
               (let ((name (file-name-base file)))
                 (and (not (string= name assembly-name))
                      (not (string-match-p "\\`System\\(?:\\..*\\)?\\'" name))
                      (not (member name '("mscorlib" "netstandard"
                                          "Microsoft.CSharp"
                                          "Microsoft.VisualBasic"))))))
             (file-expand-wildcards
              (expand-file-name "*.dll" reference-directory) t))))
         (references (cons assembly dependencies)))
    (mapconcat
     (lambda (file)
       (format
        (concat "    <Reference Include=\"%s\">\n"
                "      <HintPath>%s</HintPath>\n"
                "      <Private>false</Private>\n"
                "    </Reference>\n")
        (xml-escape-string (file-name-base file))
        (xml-escape-string (expand-file-name file))))
     references
     "")))

(defun chief/fsharp-write-metadata-project
    (workspace assembly source-project reference-directory)
  "Write a Roslyn workspace project for ASSEMBLY in WORKSPACE.
SOURCE-PROJECT and REFERENCE-DIRECTORY supply the F# project's references."
  (let* ((project-file (expand-file-name "FSharpMetadata.csproj" workspace))
         (framework
          (or (and source-project
                   (chief/dotnet-project-target-framework source-project))
              (chief/dotnet-assembly-target-framework assembly)
              (chief/dotnet-sdk-target-framework)
              "net8.0"))
         (project-reference
          (if source-project
              (format
               (concat "    <ProjectReference Include=\"%s\" "
                       "ReferenceOutputAssembly=\"true\" />\n")
               (xml-escape-string (expand-file-name source-project)))
            ""))
         (assembly-references
          (chief/fsharp-metadata-reference-items
           assembly reference-directory))
         (contents
          (format
           (concat "<Project Sdk=\"Microsoft.NET.Sdk\">\n"
                   "  <PropertyGroup>\n"
                   "    <TargetFramework>%s</TargetFramework>\n"
                   "    <LangVersion>preview</LangVersion>\n"
                   "    <Nullable>enable</Nullable>\n"
                   "    <AllowUnsafeBlocks>true</AllowUnsafeBlocks>\n"
                   "    <RestoreIgnoreFailedSources>true</RestoreIgnoreFailedSources>\n"
                   "    <NoWarn>0436;1591</NoWarn>\n"
                   "  </PropertyGroup>\n"
                   "  <ItemGroup>\n"
                   "%s%s"
                   "  </ItemGroup>\n"
                   "</Project>\n")
           (xml-escape-string framework)
           project-reference
           assembly-references)))
    (make-directory workspace t)
    (unless (and (file-readable-p project-file)
                 (with-temp-buffer
                   (insert-file-contents project-file)
                   (string= (buffer-string) contents)))
      (with-temp-file project-file
        (insert contents)))
    project-file))

(defun chief/dotnet-reference-output-directory (&optional project)
  "Return the best build-output directory for resolving PROJECT dependencies."
  (when-let* ((project (or project (chief/dotnet-project-file)))
              (bin (expand-file-name "bin" (file-name-directory project)))
              ((file-directory-p bin)))
    (let ((candidates
           (seq-filter
            #'file-directory-p
            (append
             (file-expand-wildcards (expand-file-name "*/*" bin) t)
             (file-expand-wildcards (expand-file-name "*/*/*" bin) t)))))
      (car
       (sort candidates
             (lambda (left right)
               (> (length (directory-files left nil "\\.dll\\'" t))
                  (length (directory-files right nil "\\.dll\\'" t)))))))))

(defalias 'chief/fsharp-reference-output-directory
  #'chief/dotnet-reference-output-directory)

(defun chief/fsharp-ilspy-run
    (ilspy assembly type references stderr-file)
  "Run ILSPY for TYPE in ASSEMBLY, resolving from REFERENCES.
Write standard error to STDERR-FILE and return the process status."
  (erase-buffer)
  (process-file ilspy nil (list t stderr-file) nil
                "--disable-updatecheck"
                "-r" references
                "-t" type assembly))

(defun chief/fsharp-forwarded-assembly
    (stderr-file assembly &optional reference-directory)
  "Return the implementation assembly named in ILSpy STDERR-FILE.
Search beside facade ASSEMBLY, in REFERENCE-DIRECTORY, and in installed shared
frameworks, preferring a runtime with the facade's major version."
  (when (file-readable-p stderr-file)
    (with-temp-buffer
      (insert-file-contents stderr-file)
      (goto-char (point-min))
      (when (re-search-forward
             "but only in \\([^[:space:]\n\r]+\\)" nil t)
        (let* ((name (match-string-no-properties 1))
               (filename (if (string-suffix-p ".dll" name t)
                             name
                           (concat name ".dll")))
               (major
                (and (string-match "/\\([0-9]+\\)\\.[0-9]" assembly)
                     (match-string 1 assembly)))
               (candidates
                (delete-dups
                 (append
                  (list (expand-file-name
                         filename (file-name-directory assembly)))
                  (when reference-directory
                    (list (expand-file-name filename reference-directory)))
                  (cl-loop
                   for root in chief/dotnet-root-directories
                   when root
                   append
                   (file-expand-wildcards
                    (expand-file-name
                     (format "shared/*/*/%s" filename) root)
                    t)))))
               (readable (seq-filter #'file-readable-p candidates)))
          (or (and major
                   (seq-find
                    (lambda (file)
                      (string-match-p
                       (format "/%s\\.[^/]+/%s\\'"
                               (regexp-quote major)
                               (regexp-quote filename))
                       file))
                    readable))
              (car readable)))))))

(defun chief/fsharp-metadata-assembly-file (output)
  "Return the resolved-assembly sidecar path for metadata OUTPUT."
  (concat output ".assembly"))

(defun chief/fsharp-metadata-resolved-assembly (output fallback)
  "Return the persisted implementation assembly for OUTPUT or FALLBACK.
An unreadable persisted identity returns nil so stale metadata is regenerated;
FALLBACK is used only when no implementation identity was ever recorded."
  (let ((sidecar (chief/fsharp-metadata-assembly-file output)))
    (cond
     ((file-exists-p sidecar)
      (when (file-readable-p sidecar)
        (with-temp-buffer
          (insert-file-contents sidecar)
          (let ((path (string-trim (buffer-string))))
            (and (file-readable-p path) path)))))
     ((file-readable-p output)
      ;; Recover caches created before the sidecar was introduced.
      (with-temp-buffer
        (insert-file-contents output nil 0 4096)
        (goto-char (point-min))
        (if (looking-at "// Decompiled by ILSpy from \\(.+\\)$")
            (let ((path (match-string-no-properties 1)))
              (and (file-readable-p path) path))
          fallback)))
     (t fallback))))

(defun chief/fsharp-write-metadata-assembly (output assembly)
  "Persist ASSEMBLY as the implementation backing metadata OUTPUT."
  (with-temp-file (chief/fsharp-metadata-assembly-file output)
    (insert (expand-file-name assembly) "\n")))

(defun chief/fsharp-decompile-external-definition (info)
  "Use ILSpy to decompile the external F# definition described by INFO.
Return the generated metadata file, or nil when ILSpy is unavailable."
  (let* ((assembly (plist-get info :assembly))
         (type (plist-get info :type))
         (source-root (chief/dotnet-project-root))
         (source-project (chief/dotnet-project-file))
         (workspace
          (chief/fsharp-metadata-workspace-directory
           assembly source-root source-project))
         (output
          (chief/fsharp-metadata-file
           assembly type source-root source-project))
         (ilspy (chief/dotnet-tool-executable "ilspycmd"))
         (references (or (chief/dotnet-reference-output-directory
                          source-project)
                         (file-name-directory assembly)))
         (cached-assembly
          (chief/fsharp-metadata-resolved-assembly output assembly)))
    (when ilspy
      (if (and (file-readable-p output)
               cached-assembly
               (file-readable-p cached-assembly)
               (not (file-newer-than-file-p assembly output))
               (not (file-newer-than-file-p cached-assembly output)))
          (progn
            (chief/fsharp-write-metadata-assembly output cached-assembly)
            (chief/fsharp-write-metadata-project
             workspace cached-assembly source-project references)
            output)
        (make-directory (file-name-directory output) t)
        (with-temp-buffer
          (let* ((stderr-file (make-temp-file "chief-ilspy-"))
                 (resolved-assembly assembly)
                 status)
            (unwind-protect
                (progn
                  (setq status
                        (chief/fsharp-ilspy-run
                         ilspy assembly type references stderr-file))
                  (unless (and (integerp status) (zerop status))
                    (when-let* ((forwarded
                                 (chief/fsharp-forwarded-assembly
                                  stderr-file assembly references)))
                      (setq resolved-assembly forwarded
                            status
                            (chief/fsharp-ilspy-run
                             ilspy forwarded type references stderr-file))))
                  (if (and (integerp status) (zerop status)
                           (> (buffer-size) 0))
                      (progn
                        (chief/fsharp-write-metadata-project
                         workspace resolved-assembly source-project references)
                        (goto-char (point-min))
                        (insert
                         (format "// Decompiled by ILSpy from %s\n\n"
                                 resolved-assembly))
                        (write-region nil nil output nil 'silent)
                        (chief/fsharp-write-metadata-assembly
                         output resolved-assembly)
                        output)
                    (message "ILSpy could not decompile %s from %s"
                             type assembly)
                    nil))
              (delete-file stderr-file))))))))

(defun chief/dotnet-external-metadata-file-p (file)
  "Return non-nil when FILE contains generated .NET metadata source."
  (and file
       (or (string-match-p "/MetadataAsSource/" file)
           (and (file-directory-p chief/fsharp-metadata-cache-directory)
                (file-in-directory-p
                 (expand-file-name file)
                 (expand-file-name chief/fsharp-metadata-cache-directory)))
           (file-exists-p (concat file ".metadata-uri")))))

(defun chief/dotnet-xref-metadata-item-p (item)
  "Return non-nil when Xref ITEM points to generated metadata source."
  (condition-case nil
      (chief/dotnet-external-metadata-file-p
       (xref-file-location-file (xref-item-location item)))
    (error nil)))

(defun chief/dotnet-external-metadata-buffer-p ()
  "Return non-nil when the current C# buffer contains generated metadata."
  (or chief/dotnet-metadata-source-root
      chief/dotnet-metadata-workspace-root
      (chief/dotnet-external-metadata-file-p buffer-file-name)))

(defun chief/dotnet-metadata-project-root ()
  "Return the synthetic project root for the current metadata buffer."
  (or chief/dotnet-metadata-workspace-root
      chief/dotnet-metadata-source-root
      (chief/dotnet-project-root)))

(defun chief/fsharp-start-metadata-lsp (buffer)
  "Start C# LSP tooling in generated F# metadata BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless (bound-and-true-p lsp-managed-mode)
        (chief/lsp-apply-default-disabled-clients)
        (chief/lsp-prime-session-root)
        (lsp)))))

(defun chief/dotnet-selected-csharp-server-available-p ()
  "Return non-nil when the configured C# language server is available."
  (pcase chief/dotnet-csharp-server
    ('roslyn (chief/dotnet-roslyn-available-p))
    ('csharp-ls (chief/dotnet-csharp-ls-available-p))))

(defun chief/dotnet-selected-csharp-package ()
  "Return the global tool package for the configured C# server."
  (pcase chief/dotnet-csharp-server
    ('roslyn "roslyn-language-server")
    ('csharp-ls "csharp-ls")))

(defun chief/dotnet-selected-csharp-installation-active-p ()
  "Return non-nil while the configured C# server is being installed."
  (when-let* ((package (chief/dotnet-selected-csharp-package))
              (buffer (gethash package chief/dotnet-tool-installations))
              ((buffer-live-p buffer))
              (process (get-buffer-process buffer)))
    (process-live-p process)))

(defun chief/dotnet-install-selected-csharp-server (on-success)
  "Install the configured C# language server, then call ON-SUCCESS."
  (pcase chief/dotnet-csharp-server
    ('roslyn
     (chief/dotnet-install-roslyn-language-server on-success))
    ('csharp-ls
     (chief/dotnet-install-csharp-ls on-success))))

(defun chief/fsharp-ensure-metadata-tooling (buffer)
  "Ensure maintained C# tooling is active in metadata BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cond
       ((bound-and-true-p lsp-managed-mode))
       ((chief/dotnet-selected-csharp-server-available-p)
        (chief/fsharp-start-metadata-lsp buffer))
       ((or noninteractive chief/fsharp-metadata-lsp-offered))
       ((chief/dotnet-selected-csharp-installation-active-p)
        (setq-local chief/fsharp-metadata-lsp-offered t)
        (chief/dotnet-install-selected-csharp-server
         (lambda ()
           (chief/fsharp-start-metadata-lsp buffer)))
        (message "C# tooling is installing; metadata startup is queued"))
       (t
        (setq-local chief/fsharp-metadata-lsp-offered t)
        (let ((label (if (eq chief/dotnet-csharp-server 'roslyn)
                         "Roslyn"
                       "csharp-ls")))
          (if (yes-or-no-p
               (format "Metadata navigation requires %s.  Install it now? "
                       label))
              (progn
                (chief/dotnet-install-selected-csharp-server
                 (lambda ()
                   (chief/fsharp-start-metadata-lsp buffer)))
                (message "Installing %s; metadata tooling will start automatically"
                         label))
            (message "Metadata buffer opened without LSP tooling"))))))))

(defun chief/fsharp-show-metadata-definition (file info &optional action)
  "Open metadata FILE at the symbol described by INFO using ACTION."
  (let* ((source-root (chief/dotnet-project-root))
         (source-solution (chief/dotnet-solution-file))
         (workspace (locate-dominating-file file "FSharpMetadata.csproj"))
         ;; Visit normally so C# mode, completion, formatting, keymaps, and
         ;; Roslyn navigation hooks are initialized as they are for a regular
         ;; C# source buffer.  Misleading generated-source diagnostics are
         ;; suppressed separately.
         (buffer (find-file-noselect file))
         (member (or (plist-get info :member)
                     (car (last (split-string (plist-get info :type) "[.+]" t))))))
    (with-current-buffer buffer
      (setq-local chief/dotnet-metadata-source-root source-root)
      (setq-local chief/dotnet-metadata-source-solution source-solution)
      (setq-local chief/dotnet-metadata-workspace-root workspace)
      (setq-local chief/lsp-diagnostics-disabled t)
      (when (eq chief/dotnet-csharp-server 'roslyn)
        (chief/dotnet-roslyn-apply-buffer-guards))
      (setq-local buffer-read-only t)
      (set-buffer-modified-p nil))
    (xref-push-marker-stack)
    (pcase action
      ('window (switch-to-buffer-other-window buffer))
      ('frame (switch-to-buffer-other-frame buffer))
      (_ (switch-to-buffer buffer)))
    (goto-char (point-min))
    (when (and member (not (string-empty-p member)))
      (let ((regexp (concat "\\_<" (regexp-quote member) "\\_>"))
            found)
        ;; Top-level members occur after nested helper implementations in ILSpy
        ;; output, so the final exact member occurrence is generally the useful
        ;; declaration.
        (while (re-search-forward regexp nil t)
          (setq found (match-beginning 0)))
        (when found
          (goto-char found))))
    (chief/fsharp-ensure-metadata-tooling buffer)))

(defun chief/fsharp-show-external-definition (info source-buffer action)
  "Decompile and show INFO from SOURCE-BUFFER using display ACTION."
  (when (buffer-live-p source-buffer)
    (with-current-buffer source-buffer
      (when-let* ((file (chief/fsharp-decompile-external-definition info)))
        (chief/fsharp-show-metadata-definition file info action)))))

(defun chief/fsharp-provision-ilspy-for-definition (info source-buffer action)
  "Offer to install ILSpy, then show INFO from SOURCE-BUFFER using ACTION."
  (cond
   (noninteractive
    (message "External F# definition navigation requires ilspycmd"))
   ((yes-or-no-p
     "External F# definition navigation requires ilspycmd.  Install it now? ")
    (chief/dotnet-install-ilspycmd
     (lambda ()
       (chief/fsharp-show-external-definition info source-buffer action)))
    (message "Installing ilspycmd; definition navigation will resume automatically"))
   (t
    (message "External F# definition navigation requires ilspycmd"))))

(defun chief/dotnet-import-namespace-at-point ()
  "Return the namespace imported by the C# or F# statement at point."
  (let ((position (point))
        (case-fold-search nil)
        regexp)
    (setq regexp
          (cond
           ((derived-mode-p 'csharp-mode 'csharp-ts-mode)
            (concat
             "^[ \t]*\\(?:global[ \t]+\\)?using[ \t]+"
             "\\(?:[[:alpha:]_][[:alnum:]_]*[ \t]*=[ \t]*\\)?"
             "\\(?:global::\\)?"
             "\\([[:alpha:]_][[:alnum:]_.]*\\)[ \t]*;"))
           ((derived-mode-p 'fsharp-mode 'fsharp-ts-mode)
            (concat
             "^[ \t]*open[ \t]+"
             "\\(?:global\\.\\)?"
             "\\([[:alpha:]_][[:alnum:]_'.]*\\)"))))
    (when regexp
      (save-excursion
        (goto-char (line-beginning-position))
        (when (and (re-search-forward regexp (line-end-position) t)
                   (<= (match-beginning 1) position)
                   (<= position (match-end 1)))
          (match-string-no-properties 1))))))

(defun chief/dotnet-project-reference-files (project)
  "Return readable project files referenced directly by PROJECT."
  (condition-case nil
      (let ((xml (chief/dotnet-project-xml project))
            references)
        (cl-labels
            ((walk
              (node)
              (when (listp node)
                (when (eq (xml-node-name node) 'ProjectReference)
                  (when-let* ((include
                               (alist-get 'Include
                                          (xml-node-attributes node))))
                    (dolist (entry (split-string include ";" t "[ \t\n]+"))
                      (when-let* ((expanded
                                   (with-temp-buffer
                                     (insert-file-contents project)
                                     (setq buffer-file-name project
                                           default-directory
                                           (file-name-directory project))
                                     ;; This is an internal parsing buffer, not
                                     ;; an editable visit of PROJECT.  Mark it
                                     ;; clean so killing it never prompts.
                                     (set-buffer-modified-p nil)
                                     (chief/dotnet-expand-msbuild-path entry)))
                                  ((not (string-match-p "[$%@](" expanded))))
                        (let* ((pattern
                                (expand-file-name
                                 (replace-regexp-in-string
                                  "\\\\" "/" expanded)
                                 (file-name-directory project)))
                               (paths
                                (if (string-match-p "[*?]" pattern)
                                    (file-expand-wildcards pattern t)
                                  (list pattern))))
                          (dolist (path paths)
                            (when (file-readable-p path)
                              (push path references))))))))
                (dolist (child (xml-node-children node))
                  (walk child)))))
          (walk xml))
        (delete-dups (nreverse references)))
    (error nil)))

(defun chief/dotnet-project-closure (project)
  "Return PROJECT and its recursive project-reference closure."
  (let ((queue (and project (list (expand-file-name project))))
        (seen (make-hash-table :test #'equal))
        result)
    (while queue
      (let* ((candidate (pop queue))
             (key (and (file-readable-p candidate)
                       (file-truename candidate))))
        (when (and key (not (gethash key seen)))
          (puthash key t seen)
          (push candidate result)
          (setq queue
                (append queue
                        (chief/dotnet-project-reference-files candidate))))))
    (nreverse result)))

(defun chief/dotnet-source-tree-directory-p (directory nested-projects)
  "Return non-nil when DIRECTORY is searchable source outside NESTED-PROJECTS."
  (let ((name (file-name-nondirectory (directory-file-name directory)))
        (excluded '("bin" "obj" ".git" ".idea" ".ionide"
                    ".cache" "scratch")))
    (and (not (member name excluded))
         (not
          (seq-some
           (lambda (nested)
             (or (file-equal-p directory nested)
                 (file-in-directory-p directory nested)))
           nested-projects)))))

(defun chief/dotnet-project-explicit-source-files (project)
  "Return source files named by Compile Include items in PROJECT."
  (condition-case nil
      (let ((xml (chief/dotnet-project-xml project))
            files)
        (cl-labels
            ((walk
              (node)
              (when (listp node)
                (when (eq (xml-node-name node) 'Compile)
                  (when-let* ((include
                               (alist-get 'Include
                                          (xml-node-attributes node))))
                    (dolist (entry (split-string include ";" t "[ \t\n]+"))
                      (unless (string-match-p "[$]" entry)
                        (let ((pattern
                               (expand-file-name
                                (replace-regexp-in-string "\\\\" "/" entry)
                                (file-name-directory project))))
                          (dolist (file
                                   (if (string-match-p "[*?]" pattern)
                                       (file-expand-wildcards pattern t)
                                     (list pattern)))
                            (when (file-readable-p file)
                              (push file files))))))))
                (dolist (child (xml-node-children node))
                  (walk child)))))
          (walk xml))
        (delete-dups (nreverse files)))
    (error nil)))

(defun chief/dotnet-project-default-source-files (project)
  "Return default SDK source files belonging directly to PROJECT."
  (let* ((directory (file-name-directory project))
         (extension (downcase (or (file-name-extension project) "")))
         (regexp
          (pcase extension
            ("csproj" "\\.cs\\'")
            ("fsproj" "\\.fs[fixy]?\\'")
            ("vbproj" "\\.vb\\'")
            (_ "\\.\\(?:cs\\|fs[fixy]?\\|vb\\)\\'")))
         (nested-projects
          (delete
           (file-name-directory (file-truename project))
           (delete-dups
            (mapcar
             (lambda (file)
               (file-name-directory (file-truename file)))
             (directory-files-recursively
              directory "\\.\\(?:cs\\|fs\\|vb\\)proj\\'" nil
              (lambda (candidate)
                (chief/dotnet-source-tree-directory-p candidate nil))))))))
    (directory-files-recursively
     directory regexp nil
     (lambda (candidate)
       (chief/dotnet-source-tree-directory-p candidate nested-projects)))))

(defun chief/dotnet-project-source-files (project)
  "Return source files compiled directly by PROJECT."
  (let* ((explicit (chief/dotnet-project-explicit-source-files project))
         (extension (downcase (or (file-name-extension project) "")))
         (defaults-enabled
          (not (string-equal
                (downcase
                 (or (chief/dotnet-project-property
                      project 'EnableDefaultCompileItems)
                     "true"))
                "false")))
         (defaults
          (when (and defaults-enabled
                     (or (string= extension "csproj")
                         (null explicit)))
            (chief/dotnet-project-default-source-files project))))
    (delete-dups (append explicit defaults))))

(defun chief/dotnet-type-declaration-regexps (symbol extension)
  "Return declaration regexps for type SYMBOL in source EXTENSION."
  (let* ((name (regexp-quote symbol))
         (csharp-modifier
          (concat
           "\\(?:public\\|private\\|protected\\|internal\\|static"
           "\\|abstract\\|sealed\\|partial\\|readonly\\|ref\\|unsafe"
           "\\|new\\|file\\|required\\)[ \t]+")))
    (pcase extension
      ("cs"
       (list
        (concat
         "^[ \t]*\\(?:\\[[^]\n]+\\][ \t]*\\)*"
         "\\(?:" csharp-modifier "\\)*"
         "\\(?:class\\|struct\\|interface\\|enum"
         "\\|record\\(?:[ \t]+\\(?:class\\|struct\\)\\)?\\)"
         "[ \t]+\\(" name "\\)\\(?:[ \t\r\n<(:={]\\|$\\)")
        (concat
         "^[ \t]*\\(?:" csharp-modifier "\\)*"
         "delegate[ \t]+[^;\n(]+[ \t*]\\(" name "\\)[ \t]*[<(]")))
      ((or "fs" "fsi" "fsx" "fsy")
       (list
        (concat
         "^[ \t]*\\(?:type\\|and\\)[ \t]+"
         "\\(?:public\\|internal\\|private\\)?[ \t]*"
         "\\(" name "\\)\\(?:[ \t\r\n<(:={]\\|$\\)"))))))

(defun chief/dotnet-type-source-locations (symbol &optional project)
  "Return cross-language definition locations for SYMBOL from PROJECT's graph."
  (when-let* ((project (or project (chief/dotnet-project-file)))
              ((not (string-empty-p symbol))))
    (let ((files
           (delete-dups
            (cl-mapcan #'chief/dotnet-project-source-files
                       (chief/dotnet-project-closure project))))
          locations)
      (dolist (file files)
        (let ((regexps
               (chief/dotnet-type-declaration-regexps
                symbol (downcase (or (file-name-extension file) "")))))
          (when regexps
            (with-temp-buffer
              (insert-file-contents file)
              (dolist (regexp regexps)
                (goto-char (point-min))
                (while (re-search-forward regexp nil t)
                  (let* ((position (match-beginning 1))
                         (line (line-number-at-pos position))
                         (column
                          (save-excursion
                            (goto-char position)
                            (current-column))))
                    (push
                     (xref-make
                      symbol
                      (xref-make-file-location file line column))
                     locations))))))))
      (nreverse locations))))

(defun chief/dotnet-display-source-locations (locations action)
  "Display Xref LOCATIONS using ACTION without silently choosing ambiguity."
  (cond
   ((null locations) nil)
   ((null (cdr locations))
    (xref-push-marker-stack)
    (xref-pop-to-location (car locations) action)
    t)
   (t
    (xref-show-xrefs (lambda () locations) action)
    t)))

(defun chief/dotnet-navigate-local-type (symbol source-buffer action)
  "Navigate local type SYMBOL from SOURCE-BUFFER using display ACTION.
Return non-nil when a source declaration was found."
  (when (buffer-live-p source-buffer)
    (with-current-buffer source-buffer
      (chief/dotnet-display-source-locations
       (chief/dotnet-type-source-locations symbol) action))))

(defun chief/dotnet-namespace-source-locations (namespace)
  "Return referenced-project source locations declaring NAMESPACE."
  (when-let* ((project (chief/dotnet-project-file)))
    (let* ((files
            (delete-dups
             (cl-mapcan #'chief/dotnet-project-source-files
                        (chief/dotnet-project-closure project))))
           (regexp
            (concat
             "^[ \t]*\\(?:namespace\\(?:[ \t]+rec\\)?"
             "\\|module\\(?:[ \t]+rec\\)?\\)[ \t]+"
             (regexp-quote namespace)
             "\\(?:[ \t]*[;={]\\|[ \t]*$\\)"))
           locations)
      (dolist (file files)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward regexp nil t)
            (let* ((position (match-beginning 0))
                   (line (line-number-at-pos position))
                   (column
                    (save-excursion
                      (goto-char position)
                      (current-column))))
              (push
               (xref-make
                namespace
                (xref-make-file-location file line column))
               locations)))))
      (nreverse locations))))

(defun chief/dotnet-assets-runtime-assemblies (project)
  "Return runtime assemblies recorded in PROJECT's NuGet assets file."
  (when-let* ((assets (chief/dotnet-project-assets-file project))
              ((file-readable-p assets)))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents assets)
          (let* ((document
                  (json-parse-buffer
                   :object-type 'hash-table :array-type 'list
                   :null-object nil :false-object nil))
                 (targets (gethash "targets" document))
                 (libraries (gethash "libraries" document))
                 (folders (gethash "packageFolders" document))
                 (target-name
                  (or (seq-find
                       (lambda (name) (not (string-match-p "/" name)))
                       (hash-table-keys targets))
                      (car (hash-table-keys targets))))
                 (target (and target-name (gethash target-name targets)))
                 (package-roots (hash-table-keys folders))
                 assemblies)
            (when target
              (maphash
               (lambda (library-name target-data)
                 (when (equal (gethash "type" target-data) "package")
                   (when-let* ((library (gethash library-name libraries))
                               (path (gethash "path" library)))
                     (dolist (assets-key '("runtime" "runtimeTargets"))
                       (when-let* ((runtime (gethash assets-key target-data)))
                         (maphash
                          (lambda (relative metadata)
                            (when (and (string-suffix-p ".dll" relative t)
                                       (or (equal assets-key "runtime")
                                           (equal (gethash "assetType" metadata)
                                                  "runtime")))
                              (when-let* ((file
                                          (seq-some
                                           (lambda (root)
                                             (let ((candidate
                                                    (expand-file-name
                                                     relative
                                                     (expand-file-name
                                                      path root))))
                                               (and (file-readable-p candidate)
                                                    candidate)))
                                           package-roots)))
                                (push file assemblies))))
                          runtime))))))
               target))
            (delete-dups (nreverse assemblies))))
      (error nil))))

(defun chief/dotnet-repl-runtime-assemblies (project)
  "Return implementation assemblies needed to evaluate PROJECT code."
  (let* ((output (ignore-errors (chief/dotnet-project-output-dll project)))
         (directory (and output (file-name-directory output)))
         (files
          (append
           (and directory (file-directory-p directory)
                (directory-files directory t "\\.dll\\'" t))
           (chief/dotnet-assets-runtime-assemblies project)))
         (seen (make-hash-table :test #'equal))
         result)
    (dolist (file files)
      (let ((name (downcase (file-name-base file))))
        (when (and (file-readable-p file) (not (gethash name seen)))
          (puthash name t seen)
          (push file result))))
    (nreverse result)))

(defun chief/dotnet-assets-reference-assemblies (project)
  "Return compile assemblies recorded in PROJECT's NuGet assets file."
  (when-let* ((assets (chief/dotnet-project-assets-file project))
              ((file-readable-p assets)))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents assets)
          (let* ((document
                  (json-parse-buffer
                   :object-type 'hash-table :array-type 'list
                   :null-object nil :false-object nil))
                 (targets (gethash "targets" document))
                 (libraries (gethash "libraries" document))
                 (folders (gethash "packageFolders" document))
                 (target-name
                  (seq-find
                   (lambda (name) (not (string-match-p "/" name)))
                   (hash-table-keys targets)))
                 (target (and target-name (gethash target-name targets)))
                 (package-root (car (hash-table-keys folders)))
                 assemblies)
            (when (and target package-root)
              (maphash
               (lambda (library-name target-data)
                 (when (equal (gethash "type" target-data) "package")
                   (when-let* ((library (gethash library-name libraries))
                               (path (gethash "path" library))
                               (compile (gethash "compile" target-data)))
                     (maphash
                      (lambda (relative _metadata)
                        (when (string-suffix-p ".dll" relative t)
                          (let ((file
                                 (expand-file-name
                                  relative
                                  (expand-file-name path package-root))))
                            (when (file-readable-p file)
                              (push file assemblies)))))
                      compile))))
               target))
            (nreverse assemblies)))
      (error nil))))

(defun chief/dotnet-shared-runtime-directories (&optional project)
  "Return installed shared-runtime directories suitable for PROJECT."
  (let* ((framework (and project
                         (chief/dotnet-project-target-framework project)))
         (major (and framework
                     (string-match "net\\([0-9]+\\)" framework)
                     (match-string 1 framework)))
         (directories
          (cl-loop
           for root in chief/dotnet-root-directories
           when root
           append (file-expand-wildcards
                   (expand-file-name "shared/*/*" root) t))))
    (sort
     (seq-filter
      (lambda (directory)
        (and (file-directory-p directory)
             (or (not major)
                 (string-prefix-p
                  (concat major ".")
                  (file-name-nondirectory
                   (directory-file-name directory))))))
      directories)
     (lambda (left right)
       (string>
        (file-name-nondirectory (directory-file-name left))
        (file-name-nondirectory (directory-file-name right)))))))

(defun chief/dotnet-reference-assembly-files (&optional project)
  "Return deduplicated assemblies available to PROJECT.

Implementation assemblies from build output and shared runtimes take
precedence over NuGet and SDK reference assemblies with the same file name."
  (when-let* ((project (or project (chief/dotnet-project-file))))
    (let* ((output (chief/dotnet-reference-output-directory project))
           (directories
            (append
             (and output (list output))
             (chief/dotnet-shared-runtime-directories project)
             (chief/dotnet-reference-directories project)))
           (files
            (append
             (cl-mapcan
              (lambda (directory)
                (and (file-directory-p directory)
                     (directory-files directory t "\\.dll\\'" t)))
              directories)
             (chief/dotnet-assets-reference-assemblies project)))
           (seen (make-hash-table :test #'equal))
           result)
      (dolist (file files)
        (let ((name (downcase (file-name-nondirectory file))))
          (when (and (file-readable-p file) (not (gethash name seen)))
            (puthash name t seen)
            (push file result))))
      (nreverse result))))

(defun chief/dotnet-ilspy-entity-types (ilspy assemblies)
  "Use ILSPY to return top-level entity names from ASSEMBLIES."
  (when assemblies
    (with-temp-buffer
      (let ((status
             (apply #'process-file ilspy nil t nil
                    (append (list "--disable-updatecheck" "-l" "cisde")
                            assemblies)))
            types)
        (when (and (integerp status) (zerop status))
          (goto-char (point-min))
          (while (re-search-forward
                  "^\\(?:Class\\|Interface\\|Struct\\|Delegate\\|Enum\\) \\(.+\\)$"
                  nil t)
            (let ((type (match-string-no-properties 1)))
              (unless (or (string-prefix-p "<" type)
                          (string-match-p "+" type))
                (push type types))))
          (delete-dups (nreverse types)))))))

(defun chief/dotnet-types-in-exact-namespace (types namespace)
  "Return TYPES declared directly in NAMESPACE."
  (let ((prefix (concat namespace ".")))
    (seq-filter
     (lambda (type)
       (and (string-prefix-p prefix type)
            (not (string-match-p
                  "\\."
                  (substring type (length prefix))))))
     types)))

(defun chief/dotnet-namespace-index-key (project namespace)
  "Return a cache key for NAMESPACE using PROJECT's current state."
  (let ((assets (chief/dotnet-project-assets-file project)))
    (list
     (file-truename project)
     namespace
     (and (file-exists-p assets)
          (file-attribute-modification-time (file-attributes assets)))
     (mapcar
      (lambda (assembly)
        (cons (file-truename assembly)
              (file-attribute-modification-time
               (file-attributes assembly))))
      (chief/dotnet-reference-assembly-files project)))))

(defun chief/dotnet-namespace-type-index (namespace project ilspy)
  "Return an assembly/type index for NAMESPACE in PROJECT using ILSPY.

The result is a plist with `:assemblies' and `:types'."
  (let* ((key (chief/dotnet-namespace-index-key project namespace))
         (cached (gethash key chief/dotnet-namespace-types-cache)))
    (or cached
        (let* ((assemblies (chief/dotnet-reference-assembly-files project))
               (parts (split-string namespace "\\." t))
               selected types)
          ;; Namespace and assembly names usually share a prefix.  Start with
          ;; the narrowest useful assembly set, then broaden only when needed.
          (cl-loop
           for count downfrom (length parts) to 1
           for prefix = (string-join (seq-take parts count) ".")
           for candidates =
           (seq-filter
            (lambda (assembly)
              (let ((name (file-name-base assembly)))
                (or (member name '("System.Private.CoreLib"
                                   "mscorlib" "netstandard"))
                    (string= name prefix)
                    (string-prefix-p (concat prefix ".") name))))
            assemblies)
           when candidates
           do (let ((matches
                     (chief/dotnet-types-in-exact-namespace
                      (chief/dotnet-ilspy-entity-types ilspy candidates)
                      namespace)))
                (when matches
                  (setq selected candidates
                        types matches)
                  (cl-return))))
          (unless types
            (setq selected assemblies
                  types
                  (chief/dotnet-types-in-exact-namespace
                   (chief/dotnet-ilspy-entity-types ilspy assemblies)
                   namespace)))
          (let ((value
                 (list :assemblies selected
                       :types (sort (delete-dups types) #'string<))))
            (puthash key value chief/dotnet-namespace-types-cache)
            value)))))

(defun chief/dotnet-ilspy-assemblies-contain-type-p (ilspy assemblies type)
  "Return non-nil when one of ASSEMBLIES defines TYPE according to ILSPY."
  (member type (chief/dotnet-ilspy-entity-types ilspy assemblies)))

(defun chief/dotnet-find-type-assembly (type assemblies project ilspy)
  "Return the assembly defining TYPE from ASSEMBLIES for PROJECT.
Use ILSPY to locate the owner with a bounded binary search."
  (let* ((key (chief/dotnet-namespace-index-key project type))
         (cached (gethash key chief/dotnet-type-assembly-cache)))
    (or (and cached (file-readable-p cached) cached)
        (cl-labels
            ((search-files
              (files)
              (when (and files
                         (chief/dotnet-ilspy-assemblies-contain-type-p
                          ilspy files type))
                (if (= (length files) 1)
                    (car files)
                  (let* ((middle (/ (length files) 2))
                         (left (seq-take files middle))
                         (right (seq-drop files middle)))
                    (or (search-files left)
                        (search-files right)))))))
          (when-let* ((assembly (search-files assemblies)))
            (puthash key assembly chief/dotnet-type-assembly-cache)
            assembly)))))

(defun chief/dotnet-show-external-namespace
    (namespace source-buffer action ilspy)
  "Choose and show a type from external NAMESPACE.
SOURCE-BUFFER supplies project context, ACTION controls display, and ILSPY is
its executable path."
  (when (buffer-live-p source-buffer)
    (with-current-buffer source-buffer
      (when-let* ((project (chief/dotnet-project-file))
                  (index (progn
                           (message "Indexing .NET namespace %s..." namespace)
                           (chief/dotnet-namespace-type-index
                            namespace project ilspy)))
                  (types (plist-get index :types)))
        (if (seq-empty-p types)
            (message "No definitions found for namespace %s" namespace)
          (let* ((type
                  (if (= (length types) 1)
                      (car types)
                    (if noninteractive
                        (car types)
                      (completing-read
                       (format "Type in %s: " namespace)
                       types nil t))))
                 (assembly
                  (chief/dotnet-find-type-assembly
                   type (plist-get index :assemblies) project ilspy)))
            (if (not assembly)
                (message "Could not resolve an assembly for %s" type)
              (let ((info (list :assembly assembly :type type)))
                (if-let* ((file
                           (chief/fsharp-decompile-external-definition info)))
                    (chief/fsharp-show-metadata-definition file info action)
                  (message "Could not decompile %s" type))))))))))

(defun chief/dotnet-provision-ilspy-for-namespace
    (namespace source-buffer action)
  "Offer to install ILSpy, then navigate NAMESPACE from SOURCE-BUFFER.
ACTION controls how the destination is displayed."
  (cond
   (noninteractive
    (message "Namespace navigation requires ilspycmd"))
   ((yes-or-no-p "External namespace navigation requires ilspycmd.  Install it now? ")
    (chief/dotnet-install-ilspycmd
     (lambda ()
       (when (buffer-live-p source-buffer)
         (with-current-buffer source-buffer
           (when-let* ((ilspy (chief/dotnet-tool-executable "ilspycmd")))
             (chief/dotnet-show-external-namespace
              namespace source-buffer action ilspy))))))
    (message "Installing ilspycmd; namespace navigation will resume automatically"))
   (t
    (message "External namespace navigation requires ilspycmd"))))

(defun chief/dotnet-navigate-namespace (namespace source-buffer action)
  "Navigate NAMESPACE from SOURCE-BUFFER using display ACTION."
  (when (buffer-live-p source-buffer)
    (with-current-buffer source-buffer
      (if-let* ((locations
                 (chief/dotnet-namespace-source-locations namespace)))
          (chief/dotnet-display-source-locations locations action)
        (if-let* ((ilspy (chief/dotnet-tool-executable "ilspycmd")))
            (chief/dotnet-show-external-namespace
             namespace source-buffer action ilspy)
          (chief/dotnet-provision-ilspy-for-namespace
           namespace source-buffer action))))))

(defun chief/fsharp-handle-definition-error
    (err source-buffer action params)
  "Handle FSAC definition ERR from SOURCE-BUFFER, using display ACTION.
PARAMS identifies the original definition request position."
  (when (buffer-live-p source-buffer)
    (with-current-buffer source-buffer
      (if-let* ((info
                 (or (chief/fsharp-external-definition-info err)
                     (chief/fsharp-nonexistent-source-info err params))))
          (if (chief/dotnet-tool-executable "ilspycmd")
              (chief/fsharp-show-external-definition info source-buffer action)
            (chief/fsharp-provision-ilspy-for-definition
             info source-buffer action))
        (message "F# definition failed: %s"
                 (car (split-string (chief/lsp-request-error-message err)
                                    "[\n\r]" t)))))))

(defun chief/dotnet-request-definition (&optional action)
  "Request the C# or F# definition at point and display it using ACTION.

Normal symbols come directly from the active language server.  Namespace
imports fall back to shared source/assembly indexing because neither Roslyn nor
FsAutoComplete returns definitions for `using' or `open' namespace segments.
Types from cross-language project references prefer their real C# or F# source
declarations when a server returns no location or generated metadata."
  (unless (chief/lsp-ensure-active-for-navigation)
    (user-error "No active .NET language server is available here"))
  (let ((source-buffer (current-buffer))
        (namespace (chief/dotnet-import-namespace-at-point))
        (symbol (or (chief/lsp-symbol-at-point)
                    (chief/token-at-point)
                    ""))
        (fsharp-p (derived-mode-p 'fsharp-mode 'fsharp-ts-mode))
        (params (lsp--text-document-position-params)))
    (lsp-request-async
     "textDocument/definition"
     params
     (lambda (locations)
       (when (buffer-live-p source-buffer)
         (with-current-buffer source-buffer
           ;; Language servers can resolve only the leading segment of an
           ;; import (for example `Orleans' in `open Orleans.Hosting').  Route
           ;; every import through the exact shared namespace resolver instead
           ;; of accepting that plausible but incorrect location.
           (if namespace
               (chief/dotnet-navigate-namespace
                namespace source-buffer action)
             (if-let* ((items (and locations
                                   (lsp--locations-to-xref-items locations))))
                 (let ((source-items
                        (seq-filter
                         (lambda (candidate)
                           (not (chief/dotnet-xref-metadata-item-p candidate)))
                         items)))
                   (cond
                    (source-items
                     (chief/dotnet-display-source-locations
                      source-items action))
                    ((chief/dotnet-navigate-local-type
                      symbol source-buffer action))
                    (t
                     (chief/dotnet-display-source-locations items action))))
               (if (chief/dotnet-navigate-local-type
                    symbol source-buffer action)
                   t
                 (message "Definition not found for: %s" symbol)))))))
     :error-handler
     (lambda (err)
       (cond
        (namespace
         (chief/dotnet-navigate-namespace namespace source-buffer action))
        ((chief/dotnet-navigate-local-type symbol source-buffer action))
        (fsharp-p
         (chief/fsharp-handle-definition-error
          err source-buffer action params))
        (t
         (message "C# definition failed: %s"
                  (car (split-string
                        (chief/lsp-request-error-message err)
                        "[\n\r]" t))))))
     :mode 'detached)))

(defun chief/fsharp-request-definition (&optional action)
  "Request an F# definition and display it using ACTION."
  (chief/dotnet-request-definition action))

(defun chief/fsharp-goto-definition ()
  "Go to the F# definition at point with maintained metadata fallbacks."
  (interactive)
  (chief/fsharp-request-definition))

(defun chief/fsharp-goto-definition-other-window ()
  "Go to the F# definition at point in another window."
  (interactive)
  (chief/fsharp-request-definition 'window))

(defun chief/csharp-goto-definition ()
  "Go to the C# definition at point with namespace metadata fallback."
  (interactive)
  (chief/dotnet-request-definition))

(defun chief/csharp-goto-definition-other-window ()
  "Go to the C# definition at point in another window."
  (interactive)
  (chief/dotnet-request-definition 'window))

;; Mode properties also fix buffers that were open when this file was reloaded.
(dolist (mode '(fsharp-mode fsharp-ts-mode))
  (put mode 'chief/lsp-definition-function
       #'chief/fsharp-goto-definition)
  (put mode 'chief/lsp-definition-other-window-function
       #'chief/fsharp-goto-definition-other-window))
(dolist (mode '(csharp-mode csharp-ts-mode))
  (put mode 'chief/lsp-definition-function
       #'chief/csharp-goto-definition)
  (put mode 'chief/lsp-definition-other-window-function
       #'chief/csharp-goto-definition-other-window))

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
  "Format the current F# project with maintained Fantomas tooling.
Offer to install Fantomas when neither a local nor global tool is available."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (project (chief/dotnet-build-target "fsproj" t))
         (directory (if project (file-name-directory project)
                      (chief/dotnet-project-root)))
         (command
          (or (chief/dotnet-local-tool-command "fantomas" directory)
              (when-let* ((fantomas
                           (chief/dotnet-tool-executable "fantomas")))
                (list fantomas directory))
              (list (chief/dotnet-require-global-tool
                     "fantomas" "fantomas" #'chief/dotnet-install-fantomas
                     "F# project formatting"
                     (lambda ()
                       (when (buffer-live-p source-buffer)
                         (with-current-buffer source-buffer
                           (chief/fsharp-format-project)))))
                    directory))))
    (chief/dotnet-compile command "*fantomas fsharp*" directory)))

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

(defun chief/dotnet-repl-context (&optional extension)
  "Return the nearest project file or workspace root for EXTENSION."
  (or (if extension
          (chief/dotnet-project-file extension)
        (chief/dotnet-project-file))
      (chief/dotnet-project-root)))

(defun chief/dotnet-repl-directory (&optional extension)
  "Return the project-specific REPL directory for EXTENSION."
  (let ((context (chief/dotnet-repl-context extension)))
    (if (file-directory-p context)
        (file-name-as-directory context)
      (file-name-directory context))))

(defun chief/dotnet-source-namespace ()
  "Return the C# or F# namespace/module declared by the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)
          (regexp
           (cond
            ((derived-mode-p 'csharp-mode 'csharp-ts-mode)
             (concat "^[ \t]*namespace[ \t]+"
                     "\\([[:alpha:]_][[:alnum:]_.]*\\)[ \t]*[;{]"))
            ((derived-mode-p 'fsharp-mode 'fsharp-ts-mode)
             (concat "^[ \t]*\\(?:namespace\\(?:[ \t]+rec\\)?"
                     "\\|module\\(?:[ \t]+rec\\)?\\)[ \t]+"
                     "\\([[:alpha:]_][[:alnum:]_'.]*\\)")))))
      (when (and regexp (re-search-forward regexp nil t))
        (match-string-no-properties 1)))))

(defun chief/dotnet-repl-buffer-name (language &optional extension)
  "Return a unique project-scoped REPL buffer for LANGUAGE and EXTENSION."
  (let* ((context (file-truename (chief/dotnet-repl-context extension)))
         (name (if (file-directory-p context)
                   (file-name-nondirectory (directory-file-name context))
                 (file-name-base context)))
         (id (substring (secure-hash 'sha1 context) 0 8)))
    (format "*%s:%s:%s*" language name id)))

(defun chief/dotnet-stop-repl-buffer (buffer-name)
  "Stop and kill the REPL named BUFFER-NAME."
  (when-let* ((buffer (get-buffer buffer-name)))
    (when-let* ((process (get-buffer-process buffer)))
      (delete-process process))
    (kill-buffer buffer)))

(defun chief/dotnet-comint-sentinel (process event)
  "Run the ordinary Comint sentinel for PROCESS and EVENT safely."
  (when-let* ((buffer (process-buffer process))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (internal-default-process-sentinel process event)))))

(defun chief/dotnet-repl-build-and-restart
    (extension language restart-function &optional after-restart-function)
  "Build the nearest EXTENSION project and restart its LANGUAGE REPL.
Call RESTART-FUNCTION only after a successful build, so ordinary project
source can be loaded as its compiled assembly rather than invalid script code.
Call AFTER-RESTART-FUNCTION in the source context when it is non-nil."
  (let* ((project (or (chief/dotnet-project-file extension)
                      (user-error "No .%s project found" extension)))
         (project (expand-file-name project))
         (key (file-truename project))
         (active (gethash key chief/dotnet-repl-reload-processes))
         (source-buffer (current-buffer))
         (directory (file-name-directory project))
         (buffer-name
          (format "*dotnet REPL build:%s:%s*"
                  language (file-name-base project))))
    (chief/polyglot-save-current-buffer)
    (if (and active (process-live-p active))
        (progn
          (display-buffer (process-buffer active))
          (message "%s project reload is already running" language)
          active)
      (let* ((process
              (chief/polyglot-run-command-async
               (chief/dotnet-command
                "build" project "-c" chief/dotnet-build-configuration
                "--nologo")
               directory
               buffer-name
               (lambda (_build-output)
                 (let ((output (chief/dotnet-project-output-dll project)))
                   (unless (file-readable-p output)
                     (user-error
                      "Build succeeded but the REPL assembly is missing: %s"
                      output)))
                 (if (buffer-live-p source-buffer)
                     (with-current-buffer source-buffer
                       (funcall restart-function)
                       (when after-restart-function
                         (funcall after-restart-function)))
                   (let ((default-directory directory))
                     (funcall restart-function)
                     (when after-restart-function
                       (funcall after-restart-function))))
                 (message "%s project rebuilt and loaded into its REPL"
                          language))))
             (sentinel (process-sentinel process)))
        (puthash key process chief/dotnet-repl-reload-processes)
        (set-process-sentinel
         process
         (lambda (finished event)
           (unwind-protect
               (funcall sentinel finished event)
             (when (memq (process-status finished) '(exit signal))
               (remhash key chief/dotnet-repl-reload-processes)))))
        (message "Building %s before loading it into the %s REPL..."
                 (file-name-nondirectory project) language)
        process))))

(defvar-local chief/dotnet-interactive-output ""
  "Unparsed process output in a .NET Interactive REPL buffer.")

(defvar-local chief/dotnet-interactive-target "csharp"
  "Kernel targeted by the current .NET Interactive REPL buffer.")

(defvar-local chief/dotnet-interactive-next-token 0
  "Counter used to make unique .NET Interactive command tokens.")

(defvar-local chief/dotnet-interactive-internal-tokens nil
  "Internal startup command tokens in this .NET Interactive buffer.")

(defvar-local chief/dotnet-interactive-diagnostic-tokens nil
  "Tokens whose diagnostics have already been displayed.")

(defvar-local chief/dotnet-interactive-setup-pending 0
  "Number of startup commands still pending in this REPL buffer.")

(defvar-local chief/dotnet-interactive-submissions-pending 0
  "Number of user submissions still pending in this REPL buffer.")

(defvar-local chief/dotnet-interactive-ready-p nil
  "Non-nil after .NET Interactive completed project-aware setup.")

(defvar-local chief/dotnet-interactive-queued-submissions nil
  "User submissions waiting for .NET Interactive project setup.")

(defvar-local chief/dotnet-interactive-project-output nil
  "Expected project assembly for this .NET Interactive REPL buffer.")

(defvar-local chief/dotnet-interactive-assembly-paths nil
  "Assembly dependency paths available to this .NET Interactive buffer.")

(defvar-local chief/dotnet-interactive-loaded-project-output nil
  "Project assembly successfully loaded by .NET Interactive.")

(defun chief/dotnet-repl-strip-input-face (&optional start end)
  "Remove Comint's uniform input face between START and END.

Language font-lock should color submitted REPL input instead of
`comint-highlight-input' painting the entire submission as one face."
  (let ((start (or start (point-min)))
        (end (or end (point-max))))
    (if (fboundp 'font-lock--remove-face-from-text-property)
        (font-lock--remove-face-from-text-property
         start end 'font-lock-face 'comint-highlight-input)
      (let ((position start) next face)
        (while (< position end)
          (setq next
                (or (next-single-property-change
                     position 'font-lock-face nil end)
                    end)
                face (get-text-property position 'font-lock-face))
          (cond
           ((eq face 'comint-highlight-input)
            (remove-text-properties position next '(font-lock-face nil)))
           ((and (listp face) (memq 'comint-highlight-input face))
            (put-text-property
             position next 'font-lock-face
             (delq 'comint-highlight-input (copy-sequence face)))))
          (setq position next))))))

(defun chief/dotnet-repl-font-lock-setup (language-mode)
  "Apply LANGUAGE-MODE's syntax and font-lock rules to this Comint buffer."
  (let (syntax defaults propertizer extend parse-properties
               comment-begin comment-finish cc-mode language-keywords)
    (with-temp-buffer
      (funcall language-mode)
      (setq syntax (syntax-table)
            defaults font-lock-defaults
            propertizer syntax-propertize-function
            extend syntax-propertize-extend-region-functions
            parse-properties parse-sexp-lookup-properties
            comment-begin comment-start
            comment-finish comment-end
            cc-mode (and (boundp 'c-buffer-is-cc-mode)
                         c-buffer-is-cc-mode)
            language-keywords (and (boundp 'c-font-lock-keywords)
                                   c-font-lock-keywords)))
    (when (bound-and-true-p font-lock-mode)
      (font-lock-mode -1))
    (set-syntax-table syntax)
    (setq-local font-lock-defaults defaults)
    (setq-local syntax-propertize-function propertizer)
    (setq-local syntax-propertize-extend-region-functions extend)
    (setq-local parse-sexp-lookup-properties parse-properties)
    (setq-local comment-start comment-begin)
    (setq-local comment-end comment-finish)
    (setq-local comint-highlight-input nil)
    (setq-local font-lock-multiline t)
    (when cc-mode
      (setq-local c-buffer-is-cc-mode cc-mode)
      (setq-local c-font-lock-keywords language-keywords))
    (chief/dotnet-repl-strip-input-face)
    (font-lock-mode 1)
    (font-lock-flush (point-min) (point-max))
    (when (fboundp 'font-lock-ensure)
      (font-lock-ensure (point-min) (point-max)))))

(defun chief/csharp-repl-font-lock-setup ()
  "Enable C# syntax highlighting in the current Comint buffer."
  (when (bound-and-true-p font-lock-mode)
    (font-lock-mode -1))
  (set-syntax-table csharp-mode-syntax-table)
  (let ((comint-map (current-local-map)))
    ;; Initialize CC Mode's C# language constants without replacing the
    ;; Comint-derived major mode or its process keymap.
    (c-initialize-cc-mode t)
    (c-init-language-vars csharp-mode)
    (c-common-init 'csharp-mode)
    (use-local-map comint-map))
  (setq-local c-doc-comment-style '((csharp-mode . codedoc)))
  (setq-local comint-highlight-input nil)
  (setq-local font-lock-multiline t)
  (chief/dotnet-repl-strip-input-face)
  (font-lock-mode 1)
  (font-lock-flush (point-min) (point-max))
  (when (fboundp 'font-lock-ensure)
    (font-lock-ensure (point-min) (point-max))))

(defun chief/fsharp-repl-font-lock-setup ()
  "Enable F# syntax highlighting in the current Comint buffer."
  (when (require 'fsharp-mode nil t)
    (chief/dotnet-repl-font-lock-setup #'fsharp-mode)))

(define-derived-mode chief/csharp-repl-mode comint-mode "C#-REPL"
  "Comint mode for .NET Interactive with C# syntax highlighting."
  (chief/csharp-repl-font-lock-setup))

(define-derived-mode chief/fsharp-repl-mode comint-mode "F#-REPL"
  "Comint mode for F# Interactive with F# syntax highlighting."
  (chief/fsharp-repl-font-lock-setup))

(defun chief/csharp-repl-command (&optional on-install)
  "Return the .NET Interactive C# command for the current project.
Run ON-INSTALL after provisioning a missing .NET Interactive tool."
  (let ((program
         (chief/dotnet-require-global-tool
          "dotnet-interactive" "microsoft.dotnet-interactive"
          #'chief/dotnet-install-interactive "The C# REPL" on-install)))
    (list program "stdio" "--default-kernel" "csharp"
          "--working-dir" (chief/dotnet-repl-directory "csproj"))))

(defun chief/dotnet-interactive-html-to-text (html)
  "Return a readable plain-text rendering of HTML."
  (condition-case nil
      (with-temp-buffer
        (insert html)
        (let ((document (libxml-parse-html-region (point-min) (point-max))))
          (erase-buffer)
          (shr-insert-document document)
          (string-trim (buffer-string))))
    (error html)))

(defun chief/dotnet-interactive-formatted-value (values)
  "Return the best display string from formatted VALUES."
  (when-let* ((visible
               (seq-filter
                (lambda (value)
                  (not (alist-get 'suppressDisplay value)))
                values))
              (formatted
               (or (seq-find
                    (lambda (value)
                      (equal (alist-get 'mimeType value) "text/plain"))
                    visible)
                   (car visible)))
              (value (alist-get 'value formatted)))
    (if (equal (alist-get 'mimeType formatted) "text/html")
        (chief/dotnet-interactive-html-to-text value)
      value)))

(defun chief/dotnet-interactive-output (process text)
  "Insert human-readable TEXT from PROCESS into its Comint buffer."
  (when (and (stringp text) (not (string-empty-p text)))
    (comint-output-filter process text)))

(defun chief/dotnet-interactive-output-line (process text &optional prefix)
  "Insert TEXT and a final newline from PROCESS, preceded by PREFIX."
  (when (and (stringp text) (not (string-empty-p text)))
    (chief/dotnet-interactive-output
     process
     (concat (or prefix "") text
             (unless (string-suffix-p "\n" text) "\n")))))

(defun chief/dotnet-interactive-prompt-at-end-p ()
  "Return non-nil when the current buffer ends in its REPL prompt."
  (save-excursion
    (goto-char (point-max))
    (let ((inhibit-field-text-motion t))
      (equal (buffer-substring-no-properties
              (line-beginning-position) (point-max))
             (format "%s> " chief/dotnet-interactive-target)))))

(defun chief/dotnet-interactive-prompt (process)
  "Insert a fresh language prompt for PROCESS."
  (when-let* ((buffer (process-buffer process))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (unless (chief/dotnet-interactive-prompt-at-end-p)
        (let ((mark (process-mark process)))
          (unless (or (= mark (point-min))
                      (eq (char-before mark) ?\n))
            (chief/dotnet-interactive-output process "\n"))
          (chief/dotnet-interactive-output
           process (format "%s> " chief/dotnet-interactive-target)))))))

(defun chief/dotnet-interactive-command-token (message)
  "Return the originating command token from protocol MESSAGE."
  (when-let* ((command (alist-get 'command message)))
    (alist-get 'token command)))

(defun chief/dotnet-interactive-internal-token-p (token)
  "Return non-nil when TOKEN belongs to an internal startup command."
  (and (stringp token)
       (string-prefix-p "chief-internal-" token)))

(defun chief/dotnet-interactive-ready (process)
  "Announce that PROCESS completed REPL initialization and show its prompt."
  (let ((project chief/dotnet-interactive-project-output)
        (loaded chief/dotnet-interactive-loaded-project-output))
    (chief/dotnet-interactive-output-line
     process
     (cond
      (loaded
       (format ".NET Interactive C# ready (loaded %s)"
               (file-name-nondirectory loaded)))
      (project
       (format ".NET Interactive C# ready (build and restart to load %s)"
               (file-name-nondirectory project)))
      (t ".NET Interactive C# ready")))
    (setq chief/dotnet-interactive-ready-p t)
    (chief/dotnet-interactive-prompt process)
    (let ((queued (nreverse chief/dotnet-interactive-queued-submissions)))
      (setq chief/dotnet-interactive-queued-submissions nil)
      (dolist (code queued)
        (chief/csharp-repl-send-direct code)))))

(defun chief/dotnet-interactive-finish-command
    (process token succeeded message)
  "Finish TOKEN from PROCESS, recording SUCCEEDED and optional MESSAGE."
  (let* ((internal (chief/dotnet-interactive-internal-token-p token))
         (label (and internal
                     (gethash token chief/dotnet-interactive-internal-tokens))))
    (unless succeeded
      (unless (gethash token chief/dotnet-interactive-diagnostic-tokens)
        (chief/dotnet-interactive-output-line
         process message (and internal "[startup] "))))
    (remhash token chief/dotnet-interactive-diagnostic-tokens)
    (if label
        (progn
          (when (and succeeded (file-name-absolute-p label))
            (setq chief/dotnet-interactive-loaded-project-output label))
          (remhash token chief/dotnet-interactive-internal-tokens)
          (setq chief/dotnet-interactive-setup-pending
                (max 0 (1- chief/dotnet-interactive-setup-pending)))
          (when (zerop chief/dotnet-interactive-setup-pending)
            (chief/dotnet-interactive-ready process)))
      (unless internal
        (setq chief/dotnet-interactive-submissions-pending
              (max 0 (1- chief/dotnet-interactive-submissions-pending)))
        (when (zerop chief/dotnet-interactive-submissions-pending)
          (chief/dotnet-interactive-prompt process))))))

(defun chief/dotnet-interactive-handle-event (process message)
  "Render one .NET Interactive protocol MESSAGE received from PROCESS."
  (let* ((event-type (alist-get 'eventType message))
         (event (alist-get 'event message))
         (token (chief/dotnet-interactive-command-token message))
         (internal (chief/dotnet-interactive-internal-token-p token))
         (prefix (and internal "[startup] ")))
    (pcase event-type
      ("KernelReady"
       (chief/dotnet-interactive-start-setup process))
      ((or "ReturnValueProduced" "DisplayedValueProduced"
           "DisplayedValueUpdated" "StandardOutputValueProduced"
           "StandardErrorValueProduced")
       (unless internal
         (chief/dotnet-interactive-output
          process
          (chief/dotnet-interactive-formatted-value
           (alist-get 'formattedValues event)))))
      ("DiagnosticsProduced"
       (let ((diagnostics (alist-get 'formattedDiagnostics event)))
         (when diagnostics
           (puthash token t chief/dotnet-interactive-diagnostic-tokens)
           (dolist (diagnostic diagnostics)
             (chief/dotnet-interactive-output-line
              process
              (chief/dotnet-interactive-formatted-value (list diagnostic))
              prefix)))))
      ("ErrorProduced"
       (chief/dotnet-interactive-output-line
        process
        (or (chief/dotnet-interactive-formatted-value
             (alist-get 'formattedValues event))
            (alist-get 'message event))
        prefix))
      ("IncompleteCodeSubmissionReceived"
       (unless internal
         (chief/dotnet-interactive-output-line
          process "Incomplete submission; send a complete C# statement.")))
      ("CommandSucceeded"
       (chief/dotnet-interactive-finish-command process token t nil))
      ("CommandFailed"
       (chief/dotnet-interactive-finish-command
        process token nil (alist-get 'message event))))))

(defun chief/dotnet-interactive-process-filter (process output)
  "Decode .NET Interactive protocol OUTPUT received from PROCESS."
  (when-let* ((buffer (process-buffer process))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (setq chief/dotnet-interactive-output
            (concat chief/dotnet-interactive-output output))
      (let ((start 0))
        (while (string-match "\n" chief/dotnet-interactive-output start)
          (let* ((line-end (match-beginning 0))
                 (next-start (match-end 0))
                 (line
                  (string-trim-right
                   (substring chief/dotnet-interactive-output start line-end)
                   "\r")))
            (unless (string-empty-p line)
              (condition-case nil
                  (chief/dotnet-interactive-handle-event
                   process
                   (json-parse-string
                    line :object-type 'alist :array-type 'list
                    :null-object nil :false-object nil))
                (error
                 (chief/dotnet-interactive-output-line
                  process line "[dotnet-interactive] "))))
            (setq start next-start)))
        (setq chief/dotnet-interactive-output
              (substring chief/dotnet-interactive-output start))))))

(defun chief/dotnet-interactive-submit (process code &optional internal label)
  "Submit CODE to PROCESS.
When INTERNAL is non-nil, hide ordinary output and associate LABEL with the
startup command."
  (when-let* ((buffer (process-buffer process))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (let ((token
             (format "chief-%s-%d"
                     (if internal "internal" "submission")
                     (cl-incf chief/dotnet-interactive-next-token))))
        (if internal
            (progn
              (puthash token (or label "setup")
                       chief/dotnet-interactive-internal-tokens)
              (cl-incf chief/dotnet-interactive-setup-pending))
          (cl-incf chief/dotnet-interactive-submissions-pending))
        (process-send-string
         process
         (concat
          (json-encode
           `((token . ,token)
             (commandType . "SubmitCode")
             (command
              (code . ,code)
              (targetKernelName . ,chief/dotnet-interactive-target))))
          "\n"))
        token))))

(defun chief/dotnet-interactive-assembly-resolver-code (assemblies)
  "Return C# code that resolves missing dependencies from ASSEMBLIES."
  (let ((entries
         (mapconcat
          (lambda (assembly)
            (format "[%s] = %s"
                    (json-encode-string (file-name-base assembly))
                    (json-encode-string (expand-file-name assembly))))
          assemblies
          ",\n")))
    (format
     (concat
      "var __chiefReplAssemblyPaths = new "
      "System.Collections.Generic.Dictionary<string, string>"
      "(System.StringComparer.OrdinalIgnoreCase) { %s };\n"
      "System.Runtime.Loader.AssemblyLoadContext.Default.Resolving += "
      "(context, assemblyName) => "
      "assemblyName.Name is string key && "
      "__chiefReplAssemblyPaths.TryGetValue(key, out var assemblyPath) "
      "? context.LoadFromAssemblyPath(assemblyPath) : null;")
     entries)))

(defun chief/dotnet-interactive-start-setup (process)
  "Configure PROCESS for a readable project-aware C# session."
  (chief/dotnet-interactive-submit
   process
   (concat
    "Microsoft.DotNet.Interactive.Formatting.Formatter."
    "SetPreferredMimeTypesFor(typeof(object), \"text/plain\");")
   t "formatter")
  (when chief/dotnet-interactive-assembly-paths
    (chief/dotnet-interactive-submit
     process
     (chief/dotnet-interactive-assembly-resolver-code
      chief/dotnet-interactive-assembly-paths)
     t "assembly resolver"))
  (when (and chief/dotnet-interactive-project-output
             (file-readable-p chief/dotnet-interactive-project-output))
    (chief/dotnet-interactive-submit
     process
     (format "#r %s"
             (json-encode-string chief/dotnet-interactive-project-output))
     t chief/dotnet-interactive-project-output)))

(defun chief/dotnet-interactive-input-sender (process string)
  "Submit Comint input STRING to .NET Interactive PROCESS."
  (if (string-empty-p (string-trim string))
      (chief/dotnet-interactive-prompt process)
    (chief/dotnet-interactive-submit process string)))

(defun chief/csharp-repl-send-direct (string)
  "Echo and submit STRING directly in the current C# REPL buffer."
  (when-let* ((process (get-buffer-process (current-buffer)))
              ((process-live-p process)))
    (if (not chief/dotnet-interactive-ready-p)
        (progn
          (push string chief/dotnet-interactive-queued-submissions)
          (chief/dotnet-interactive-output-line
           process string "[queued until project setup] "))
      (if (chief/dotnet-interactive-prompt-at-end-p)
          (progn
            (goto-char (point-max))
            (let ((inhibit-read-only t))
              (insert string))
            (comint-send-input))
        (chief/dotnet-interactive-prompt process)
        (chief/dotnet-interactive-output-line process string)
        (chief/dotnet-interactive-submit process string)))))

(defun chief/csharp-repl-buffer (&optional on-install)
  "Return the current project's live C# REPL buffer.
Run ON-INSTALL after provisioning a missing .NET Interactive tool."
  (let* ((project (chief/dotnet-project-file "csproj"))
         (root (chief/dotnet-repl-directory "csproj"))
         (project-output (and project
                              (ignore-errors
                                (chief/dotnet-project-output-dll project))))
         (assembly-paths
          (and project (chief/dotnet-repl-runtime-assemblies project)))
         (buffer-name (chief/dotnet-repl-buffer-name
                       "csharp-repl" "csproj"))
         (buffer (get-buffer-create buffer-name))
         (command (chief/csharp-repl-command on-install)))
    (unless (comint-check-proc buffer)
      (with-current-buffer buffer
        (chief/csharp-repl-mode))
      (let ((default-directory root)
            (process-connection-type nil)
            (process-environment (copy-sequence process-environment)))
        (setenv "DOTNET_INTERACTIVE_CLI_TELEMETRY_OPTOUT" "1")
        (setenv "DOTNET_NOLOGO" "1")
        (apply #'make-comint-in-buffer
               "chief-csharp-repl" buffer (car command) nil (cdr command)))
      (with-current-buffer buffer
        (setq-local default-directory root)
        (setq-local comint-process-echoes nil)
        (setq-local comint-input-sender
                    #'chief/dotnet-interactive-input-sender)
        (setq-local comint-prompt-regexp "^csharp> ")
        (setq-local comint-use-prompt-regexp t)
        (setq-local comint-prompt-read-only t)
        (setq-local comint-scroll-to-bottom-on-input t)
        (setq-local comint-scroll-to-bottom-on-output t)
        (setq-local chief/dotnet-interactive-output "")
        (setq-local chief/dotnet-interactive-next-token 0)
        (setq-local chief/dotnet-interactive-setup-pending 0)
        (setq-local chief/dotnet-interactive-submissions-pending 0)
        (setq-local chief/dotnet-interactive-ready-p nil)
        (setq-local chief/dotnet-interactive-queued-submissions nil)
        (setq-local chief/dotnet-interactive-loaded-project-output nil)
        (setq-local chief/dotnet-interactive-internal-tokens
                    (make-hash-table :test #'equal))
        (setq-local chief/dotnet-interactive-diagnostic-tokens
                    (make-hash-table :test #'equal))
        (setq-local chief/dotnet-interactive-project-output project-output)
        (setq-local chief/dotnet-interactive-assembly-paths assembly-paths)
        (set-process-filter
         (get-buffer-process buffer)
         #'chief/dotnet-interactive-process-filter)
        (set-process-sentinel
         (get-buffer-process buffer)
         #'chief/dotnet-comint-sentinel)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'chief/csharp-repl-mode)
        (chief/csharp-repl-font-lock-setup)))
    buffer))

(defun chief/csharp-start-repl ()
  "Start or switch to the current project's C# REPL."
  (interactive)
  (let ((source-buffer (current-buffer)))
    (pop-to-buffer
     (chief/csharp-repl-buffer
      (lambda ()
        (when (buffer-live-p source-buffer)
          (with-current-buffer source-buffer
            (chief/csharp-start-repl))))))))

(defun chief/csharp-restart-repl ()
  "Restart the current project's C# REPL."
  (interactive)
  (chief/dotnet-stop-repl-buffer
   (chief/dotnet-repl-buffer-name "csharp-repl" "csproj"))
  (chief/csharp-start-repl))

(defun chief/csharp-send-string (string &optional no-source-context)
  "Send STRING to the current project's C# REPL.
Unless NO-SOURCE-CONTEXT is non-nil, evaluate it with the source buffer's
namespace imported, like CIDER and SLY preserve namespace/package context."
  (let* ((source-buffer (current-buffer))
         (namespace (and (not no-source-context)
                         (chief/dotnet-source-namespace)))
         (payload (if namespace
                      (format "using %s;\n%s" namespace string)
                    string))
         (buffer
          (chief/csharp-repl-buffer
           (lambda ()
             (when (buffer-live-p source-buffer)
               (with-current-buffer source-buffer
                 (chief/csharp-send-string string no-source-context)))))))
    (with-current-buffer buffer
      (chief/csharp-repl-send-direct payload))
    (display-buffer buffer)))

(defun chief/csharp-send-region (start end)
  "Send the C# region from START to END to the REPL."
  (interactive "r")
  (chief/csharp-send-string (buffer-substring-no-properties start end)))

(defun chief/csharp-reload-project ()
  "Build the nearest C# project and restart its assembly-backed REPL."
  (interactive)
  (let ((namespace (chief/dotnet-source-namespace)))
    (chief/dotnet-repl-build-and-restart
     "csproj" "C#" #'chief/csharp-restart-repl
     (and namespace
          (lambda ()
            (chief/csharp-send-string
             (format "using %s;" namespace) t))))))

(defun chief/csharp-load-file ()
  "Load the current C# file into its project-scoped REPL.
Regular project source is built and loaded as an assembly because C# script
kernels reject namespace declarations.  Standalone and `.csx' files use the
kernel's `#load' directive directly."
  (interactive)
  (chief/polyglot-save-current-buffer)
  (let ((file (chief/polyglot-current-file "C#")))
    (if (and (chief/dotnet-project-file "csproj")
             (string-equal (downcase (or (file-name-extension file) ""))
                           "cs"))
        (chief/csharp-reload-project)
      (chief/csharp-send-string
       (format "#load %s" (json-encode-string file)) t))))

(defun chief/csharp-send-buffer ()
  "Evaluate the current C# buffer in its project-scoped REPL."
  (interactive)
  (if buffer-file-name
      (chief/csharp-load-file)
    (chief/csharp-send-region (point-min) (point-max))))

(defun chief/fsharp-repl-command ()
  "Return the project-aware command used for F# Interactive."
  (let* ((project (chief/dotnet-project-file "fsproj"))
         (output (and project
                      (ignore-errors
                        (chief/dotnet-project-output-dll project))))
         (library-directories
          (and project
               (delete-dups
                (mapcar #'file-name-directory
                        (chief/dotnet-repl-runtime-assemblies project))))))
    (append
     (chief/dotnet-command "fsi" "--readline-" "--nologo")
     (mapcar (lambda (directory) (concat "--lib:" directory))
             library-directories)
     (when (and output (file-readable-p output))
       (list (concat "--reference:" output))))))

(defun chief/fsharp-repl-input-sender (process string)
  "Send Comint input STRING to F# Interactive PROCESS.
Append FSI's required `;;' submission terminator when it is absent."
  (let ((payload (string-trim-right string)))
    (process-send-string
     process
     (concat payload
             (unless (or (string-empty-p payload)
                         (string-suffix-p ";;" payload))
               "\n;;")
             "\n"))))

(defun chief/fsharp-repl-buffer ()
  "Return the current project's live F# Interactive buffer."
  (let* ((root (chief/dotnet-repl-directory "fsproj"))
         (buffer-name (chief/dotnet-repl-buffer-name "fsi" "fsproj"))
         (buffer (get-buffer-create buffer-name))
         (command (chief/fsharp-repl-command)))
    (unless (comint-check-proc buffer)
      (with-current-buffer buffer
        (chief/fsharp-repl-mode))
      (let ((default-directory root))
        (apply #'make-comint-in-buffer
               "chief-fsi" buffer (car command) nil (cdr command)))
      (with-current-buffer buffer
        (setq-local default-directory root)
        (setq-local comint-process-echoes t)
        (setq-local comint-input-sender #'chief/fsharp-repl-input-sender)
        (setq-local comint-prompt-regexp "^> ")
        (setq-local comint-use-prompt-regexp t)
        (setq-local comint-prompt-read-only t)
        (setq-local comint-scroll-to-bottom-on-input t)
        (setq-local comint-scroll-to-bottom-on-output t)
        (set-process-sentinel
         (get-buffer-process buffer)
         #'chief/dotnet-comint-sentinel)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'chief/fsharp-repl-mode)
        (chief/fsharp-repl-font-lock-setup)))
    buffer))

(defun chief/fsharp-start-repl ()
  "Start or switch to the current project's F# Interactive session."
  (interactive)
  (pop-to-buffer (chief/fsharp-repl-buffer)))

(defun chief/fsharp-restart-repl ()
  "Restart the current project's F# Interactive session."
  (interactive)
  (chief/dotnet-stop-repl-buffer
   (chief/dotnet-repl-buffer-name "fsi" "fsproj"))
  (chief/fsharp-start-repl))

(defun chief/fsharp-send-string (string &optional no-source-context)
  "Send STRING to the current project's F# Interactive session.
Unless NO-SOURCE-CONTEXT is non-nil, evaluate it with the source buffer's
namespace or module open, like CIDER and SLY preserve evaluation context."
  (let* ((namespace (and (not no-source-context)
                         (chief/dotnet-source-namespace)))
         (payload (if namespace
                      (format "open %s\n%s" namespace string)
                    string))
         (buffer (chief/fsharp-repl-buffer))
         (process (get-buffer-process buffer)))
    (chief/fsharp-repl-input-sender process payload)
    (display-buffer buffer)))

(defun chief/fsharp-send-region (start end)
  "Send the F# region from START to END to F# Interactive."
  (interactive "r")
  (chief/fsharp-send-string (buffer-substring-no-properties start end)))

(defun chief/fsharp-reload-project ()
  "Build the nearest F# project and restart its assembly-backed FSI session."
  (interactive)
  (let ((namespace (chief/dotnet-source-namespace)))
    (chief/dotnet-repl-build-and-restart
     "fsproj" "F#" #'chief/fsharp-restart-repl
     (and namespace
          (lambda ()
            (chief/fsharp-send-string
             (format "open %s" namespace) t))))))

(defun chief/fsharp-eval-script-file ()
  "Evaluate the current F# script contents directly in FSI.
Direct evaluation keeps top-level script bindings in the active session, like
CIDER's load-buffer behavior, instead of hiding them in FSI's generated module."
  (interactive)
  (chief/polyglot-save-current-buffer)
  (chief/fsharp-send-string
   (buffer-substring-no-properties (point-min) (point-max)) t))

(defun chief/fsharp-load-source-file ()
  "Load the current F# file with FSI's literal `#load' directive.
This preserves FSI's normal generated-module behavior.  Use
`chief/fsharp-load-file' for project-aware or load-buffer semantics."
  (interactive)
  (chief/polyglot-save-current-buffer)
  (let ((file (replace-regexp-in-string
               "\"" "\"\"" (chief/polyglot-current-file "F#") t t)))
    (chief/fsharp-send-string (format "#load @\"%s\"" file) t)))

(defun chief/fsharp-load-file ()
  "Load the current F# file into its project-scoped FSI session.
Regular project source is built and loaded as an assembly, preserving project
references and F# compile order.  Standalone and `.fsx' files use `#load'."
  (interactive)
  (chief/polyglot-save-current-buffer)
  (let* ((file (chief/polyglot-current-file "F#"))
         (extension (downcase (or (file-name-extension file) ""))))
    (cond
     ((and (chief/dotnet-project-file "fsproj")
           (member extension '("fs" "fsi")))
      (chief/fsharp-reload-project))
     ((member extension '("fsx" "fsscript"))
      (chief/fsharp-eval-script-file))
     (t
      (chief/fsharp-load-source-file)))))

(defun chief/fsharp-send-buffer ()
  "Evaluate the current F# buffer in its project-scoped FSI session."
  (interactive)
  (if buffer-file-name
      (chief/fsharp-load-file)
    (chief/fsharp-send-region (point-min) (point-max))))

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

(defun chief/dotnet-roslyn-uri-to-path (uri)
  "Convert Roslyn URI to a path and retain metadata workspace ownership."
  (let* ((path (lsp--uri-to-path-1 uri))
         (metadata-p
          (and path
               (string-match-p "/MetadataAsSource/" path)))
         (resolved (if (and metadata-p (file-exists-p path))
                       (file-truename path)
                     path)))
    (when (and metadata-p resolved)
      (when-let* ((root (ignore-errors (lsp-workspace-root))))
        (puthash (file-truename resolved)
                 (file-name-as-directory root)
                 chief/dotnet-roslyn-metadata-roots)))
    resolved))

(defun chief/dotnet-roslyn-base-command ()
  "Return the command used to launch Microsoft's Roslyn language server."
  (or (chief/dotnet-local-tool-command "roslyn-language-server")
      (when-let* ((executable
                   (chief/dotnet-tool-executable "roslyn-language-server")))
        (list executable))
      (list "roslyn-language-server")))

(defun chief/dotnet-roslyn-command ()
  "Return the official Roslyn stdio language-server command."
  (append (chief/dotnet-roslyn-base-command)
          (list "--stdio"
                "--logLevel" chief/dotnet-roslyn-log-level)))

(defun chief/dotnet-xml-descendants (node tag)
  "Return NODE and its XML descendants whose element name is TAG."
  (when (and (listp node) (symbolp (car node)))
    (append
     (when (eq (xml-node-name node) tag) (list node))
     (cl-mapcan
      (lambda (child)
        (chief/dotnet-xml-descendants child tag))
      (xml-node-children node)))))

(defun chief/dotnet-slnx-projects (solution extension)
  "Return EXTENSION project members declared by SOLUTION."
  (when (file-readable-p solution)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents solution)
          (let* ((document (car (xml-parse-region (point-min) (point-max))))
                 (case-fold-search t)
                 projects)
            (dolist (project (chief/dotnet-xml-descendants document 'Project))
              (when-let* ((path (or (xml-get-attribute project 'Path)
                                    (xml-get-attribute project 'path)))
                          ((string-match-p
                            (format "\\.%s\\'" (regexp-quote extension))
                            path)))
                (push (expand-file-name path (file-name-directory solution))
                      projects)))
            (nreverse projects)))
      (error nil))))

(defun chief/dotnet-roslyn-workspace-targets (workspace)
  "Return the solution or C# projects Roslyn should open in WORKSPACE."
  (let* ((root (lsp--workspace-root workspace))
         (files (and (file-directory-p root)
                     (directory-files root t nil t)))
         (solutions
          (seq-filter
           (lambda (file)
             (string-match-p "\\.\\(?:sln\\|slnf\\)\\'" file))
           files))
         (slnx-files
          (seq-filter
           (lambda (file)
             (string-match-p "\\.slnx\\'" file))
           files))
         (slnx (car (sort slnx-files #'string<)))
         (mixed-slnx-p
          (and slnx (chief/dotnet-slnx-projects slnx "fsproj")))
         (slnx-csharp-projects
          (and mixed-slnx-p
               (chief/dotnet-slnx-projects slnx "csproj")))
         (excluded '("bin" "obj" "scratch" ".git" ".idea"))
         (projects
          (and (not solutions) (not slnx)
               (file-directory-p root)
               (directory-files-recursively
                root "\\.csproj\\'" nil
                (lambda (directory)
                  (not (member
                        (file-name-nondirectory
                         (directory-file-name directory))
                        excluded)))))))
    ;; Respect ordinary solution membership, solution filters, and pure-C#
    ;; `.slnx' membership.  Open only the C# members of mixed C#/F# `.slnx'
    ;; files because opening those solutions can leave package symbols in
    ;; Roslyn's miscellaneous project.
    (or (and solutions
             (list :solution (car (sort solutions #'string<))))
        (and slnx-csharp-projects
             (list :projects (sort slnx-csharp-projects #'string<)))
        (and slnx (list :solution slnx))
        (and projects (list :projects (sort projects #'string<))))))

(defun chief/dotnet-roslyn-initialized (workspace)
  "Explicitly load the solution or projects associated with WORKSPACE."
  (when-let* ((targets (chief/dotnet-roslyn-workspace-targets workspace)))
    (with-lsp-workspace workspace
      (if-let* ((solution (plist-get targets :solution)))
          (lsp-notify "solution/open"
                      `(:solution ,(lsp--path-to-uri solution)))
        (lsp-notify
         "project/open"
         `(:projects
           ,(vconcat
             (mapcar #'lsp--path-to-uri
                     (plist-get targets :projects)))))))))

(defun chief/dotnet-roslyn-workspace-live-p (workspace)
  "Return non-nil when WORKSPACE still has a live Roslyn process."
  (when-let* ((process (ignore-errors (lsp--workspace-proc workspace))))
    (process-live-p process)))

(defun chief/dotnet-buffer-uses-workspace-p (buffer workspace)
  "Return non-nil when BUFFER is live and currently attached to WORKSPACE."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (memq workspace (chief/lsp-current-workspaces)))))

(defun chief/dotnet-roslyn-finish-buffer-setup (buffer workspace)
  "Enable deferred Roslyn features in BUFFER for live WORKSPACE."
  (when (and (chief/dotnet-roslyn-workspace-live-p workspace)
             (chief/dotnet-buffer-uses-workspace-p buffer workspace))
    (with-current-buffer buffer
      (setq-local chief/lsp-inlay-hints-deferred nil)
      (setq-local lsp-lens-enable t)
      (chief/lsp-sanitize-inlay-hints)
      (chief/lsp-resume-diagnostics workspace)
      (when (fboundp 'lsp-lens-mode)
        (lsp-lens-mode 1)))))

(defun chief/dotnet-roslyn-complete-workspace-setup (workspace)
  "Mark live WORKSPACE ready and enable deferred Roslyn buffer features."
  (remhash workspace chief/dotnet-roslyn-initialization-timers)
  (if (not (chief/dotnet-roslyn-workspace-live-p workspace))
      (remhash workspace chief/dotnet-roslyn-ready-workspaces)
    (puthash workspace t chief/dotnet-roslyn-ready-workspaces)
    (dolist (buffer (lsp--workspace-buffers workspace))
      (chief/dotnet-roslyn-finish-buffer-setup buffer workspace))
    (lsp--info "%s: C# projects initialized successfully"
               (lsp--workspace-print workspace))))

(defun chief/dotnet-roslyn-project-initialized (workspace _params)
  "Finish configuring buffers after Roslyn loads projects for WORKSPACE."
  ;; Roslyn announces completion before its new document snapshot has fully
  ;; settled.  Marking the workspace ready only after this delay also keeps
  ;; buffers that attach during the grace period deferred.
  (remhash workspace chief/dotnet-roslyn-ready-workspaces)
  (when-let* ((timer
               (gethash workspace
                        chief/dotnet-roslyn-initialization-timers)))
    (cancel-timer timer))
  (puthash
   workspace
   (run-with-timer
    2 nil #'chief/dotnet-roslyn-complete-workspace-setup workspace)
   chief/dotnet-roslyn-initialization-timers))

(defun chief/dotnet-roslyn-finish-late-buffer-setup ()
  "Enable deferred features when joining an initialized Roslyn workspace."
  (when-let* ((workspace
               (seq-find
                (lambda (candidate)
                  (and (eq (lsp--client-server-id
                            (lsp--workspace-client candidate))
                           'chief-roslyn)
                       (gethash candidate
                                chief/dotnet-roslyn-ready-workspaces)))
                (lsp-workspaces))))
    (chief/dotnet-roslyn-finish-buffer-setup
     (current-buffer) workspace)))

(defun chief/dotnet-roslyn-apply-buffer-guards ()
  "Disable unsupported or misleading Roslyn features in this buffer."
  ;; Roslyn currently advertises a color provider without registering a C#
  ;; `textDocument/documentColor' handler.  Avoid a MethodNotFound response on
  ;; every change until the server's capability and handler agree.
  (setq-local lsp-enable-text-document-color nil)
  (remove-hook 'lsp-on-change-hook #'lsp--document-color t)
  ;; Decompiled metadata is useful for navigation and hover, but it is not a
  ;; buildable copy of its declaring assembly.  Compiling it beside the real
  ;; assembly yields hundreds of duplicate/inaccessible-symbol diagnostics.
  (chief/lsp-hold-diagnostics))

(defun chief/dotnet-roslyn-available-p ()
  "Return non-nil when the official Roslyn language server is available."
  (or (chief/dotnet-local-tool-ready-p "roslyn-language-server")
      (chief/dotnet-tool-executable "roslyn-language-server")))

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
  "Return an assembly containing fully qualified SYMBOL for optional PROJECT."
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
      (when (chief/dotnet-local-tool-ready-p "DNAKode.VbNet.Lsp")
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
  (or (chief/dotnet-local-tool-ready-p "csharp-ls")
      (chief/dotnet-tool-executable "csharp-ls")))

(defun chief/dotnet-fsautocomplete-available-p ()
  "Return non-nil when FsAutoComplete is available."
  (or (chief/dotnet-local-tool-ready-p "fsautocomplete")
      (chief/dotnet-tool-executable "fsautocomplete")))

(defun chief/dotnet-vbnet-ls-available-p ()
  "Return non-nil when the VB.NET language server is available."
  (or (chief/dotnet-local-tool-ready-p "vbnet-ls")
      (chief/dotnet-local-tool-ready-p "DNAKode.VbNet.Lsp")
      (chief/dotnet-tool-executable "vbnet-ls")))

(defun chief/dotnet-apply-solution-local ()
  "Expose the nearest solution file to LSP clients."
  (when-let* ((solution (chief/dotnet-solution-file)))
    (setq-local lsp-csharp-solution-file (expand-file-name solution))))

(defun chief/csharp-mode-setup ()
  "Configure C# buffers with LSP, REPL, tests, and dotnet commands."
  (setq-local compile-command "dotnet build")
  (when-let* ((file buffer-file-name)
              ((file-exists-p file))
              (root (gethash (file-truename file)
                             chief/dotnet-roslyn-metadata-roots)))
    (setq-local chief/dotnet-metadata-workspace-root root))
  (setq-local chief/lsp-diagnostics-disabled
              (and (chief/dotnet-external-metadata-buffer-p) t))
  (setq-local chief/lsp-root-function
              (if (or chief/dotnet-metadata-source-root
                      chief/dotnet-metadata-workspace-root)
                  #'chief/dotnet-metadata-project-root
                #'chief/dotnet-project-root))
  (chief/dotnet-ensure-tool-paths)
  (chief/dotnet-restore-before-lsp-start
   (chief/dotnet-project-file "csproj"))
  (chief/polyglot-completion-setup t)
  (chief/format-enable-on-save)
  (chief/repl-configure
   :start #'chief/csharp-start-repl
   :restart #'chief/csharp-restart-repl
   :send-region #'chief/csharp-send-region
   :send-buffer #'chief/csharp-send-buffer
   :load-file #'chief/csharp-load-file)
  (when (require 'lsp-csharp nil t)
    ;; Keep csharp-ls available as a maintained opt-in alternative, but use the
    ;; official Microsoft Roslyn stdio server by default.
    (setq-local lsp-csharp-csharpls-use-dotnet-tool nil)
    (setq-local lsp-csharp-csharpls-use-local-tool
                (chief/dotnet-local-tool-ready-p "csharp-ls"))
    (cond
     (chief/dotnet-metadata-workspace-root
      ;; The custom Roslyn client explicitly opens the synthetic project.
      ;; `lsp-csharp-solution-file' accepts solutions, not project files.
      (setq-local lsp-csharp-solution-file nil))
     (chief/dotnet-metadata-source-solution
      (setq-local lsp-csharp-solution-file
                  chief/dotnet-metadata-source-solution))
     (t
      (chief/dotnet-apply-solution-local)))
    (pcase chief/dotnet-csharp-server
      ('roslyn
       ;; Roslyn invalidates its initial document snapshot while projects load.
       ;; Resolving inlay hints before projectInitializationComplete can flood
       ;; the server with stale-version requests and even abort the process.
       (setq-local chief/lsp-inlay-hints-deferred
                   (not chief/dotnet-metadata-workspace-root))
       (setq-local chief/lsp-diagnostics-deferred
                   (not chief/dotnet-metadata-workspace-root))
       (setq-local lsp-lens-enable
                   (and chief/dotnet-metadata-workspace-root t))
       (setq-local lsp-enabled-clients '(chief-roslyn))
       (setq-local lsp-disabled-clients '(omnisharp csharp-ls csharp-roslyn)))
      ('csharp-ls
       (setq-local lsp-enabled-clients '(csharp-ls))
       (setq-local lsp-disabled-clients '(omnisharp csharp-roslyn chief-roslyn)))))
  ;; Run after lsp-mode/Flycheck's own hooks, including the later
  ;; `lsp-configure-hook', so provisional or generated-source diagnostics do
  ;; not leak into the UI.  This also covers the optional csharp-ls client.
  (add-hook 'lsp-mode-hook #'chief/lsp-hold-diagnostics 95 t)
  (add-hook 'lsp-configure-hook #'chief/lsp-hold-diagnostics 95 t)
  (when (eq chief/dotnet-csharp-server 'roslyn)
    (add-hook 'lsp-mode-hook
              #'chief/dotnet-roslyn-finish-late-buffer-setup 80 t)
    (add-hook 'lsp-mode-hook
              #'chief/dotnet-roslyn-apply-buffer-guards 95 t)
    (add-hook 'lsp-configure-hook
              #'chief/dotnet-roslyn-apply-buffer-guards 95 t)
    (chief/dotnet-roslyn-apply-buffer-guards))
  (when chief/dotnet-metadata-workspace-root
    ;; Roslyn metadata-as-source files live outside the workspace tree.  Attach
    ;; them immediately to the workspace that produced the URI.
    (chief/fsharp-start-metadata-lsp (current-buffer))))

(defun chief/fsharp-mode-setup ()
  "Configure F# buffers with FSAC, FSI, tests, and dotnet commands."
  (setq-local compile-command "dotnet build")
  (setq-local chief/lsp-root-function #'chief/dotnet-project-root)
  (chief/dotnet-ensure-tool-paths)
  (chief/dotnet-restore-before-lsp-start
   (chief/dotnet-project-file "fsproj"))
  (chief/polyglot-completion-setup t)
  (chief/format-enable-on-save)
  (chief/repl-configure
   :start #'chief/fsharp-start-repl
   :restart #'chief/fsharp-restart-repl
   :send-region #'chief/fsharp-send-region
   :send-buffer #'chief/fsharp-send-buffer
   :load-file #'chief/fsharp-load-file)
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
                (chief/dotnet-local-tool-ready-p "fsautocomplete"))
    (setq-local lsp-fsharp-workspace-extra-exclude-dirs
                '("bin" "obj" ".git" ".ionide" ".fake" "scratch"))))

(defun chief/vbnet-mode-setup ()
  "Configure VB.NET buffers with LSP completion and dotnet commands."
  (setq-local compile-command "dotnet build")
  (setq-local chief/lsp-root-function #'chief/dotnet-project-root)
  (chief/dotnet-ensure-tool-paths)
  (chief/dotnet-restore-before-lsp-start
   (chief/dotnet-project-file "vbproj"))
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

(defun chief/dotnet-razor-workspace-symbol-items (symbol)
  "Return exact workspace-symbol xref items for SYMBOL."
  (when (and (bound-and-true-p lsp-managed-mode)
             (fboundp 'lsp-request))
    (when-let* ((matches (condition-case nil
                            (lsp-request "workspace/symbol" `(:query ,symbol))
                          (error nil))))
      (cl-mapcan
       (lambda (match)
         (when (string= symbol (gethash "name" match))
           (when-let* ((location (gethash "location" match)))
             (lsp--locations-to-xref-items (list location)))))
       matches))))

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
         (lsp-items (and (not model-pos)
                         (not inject-pos)
                         (chief/dotnet-cshtml-buffer-p)
                         (chief/lsp-ensure-active-for-navigation)
                         (chief/lsp-location-items "textDocument/definition")))
         (workspace-items
          (and (not model-pos)
               (not inject-pos)
               (not lsp-items)
               (chief/dotnet-razor-workspace-symbol-items symbol)))
         (metadata-path (and (not model-pos)
                             (not inject-pos)
                             (not lsp-items)
                             (not workspace-items)
                             (chief/dotnet-razor-metadata-path symbol))))
    (cond
     (model-pos
      (xref-push-marker-stack)
      (goto-char model-pos))
     (inject-pos
      (xref-push-marker-stack)
      (goto-char inject-pos))
     (lsp-items
      (chief/dotnet-display-source-locations lsp-items nil))
     (workspace-items
      (chief/dotnet-display-source-locations workspace-items nil))
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
    (chief/dotnet-restore-before-lsp-start
     (chief/dotnet-project-file "csproj"))
    (when (require 'lsp-csharp nil t)
      (setq-local lsp-enabled-clients '(csharp-ls))
      (setq-local lsp-disabled-clients '(omnisharp csharp-roslyn))
      (setq-local lsp-csharp-csharpls-use-dotnet-tool nil)
      (chief/dotnet-apply-solution-local)
      (when (fboundp 'chief/lsp-managed-mode-setup)
        (chief/lsp-managed-mode-setup)))))

(defun chief/dotnet-xml-project-file-p (&optional file)
  "Return non-nil when FILE is a supported XML-based .NET file."
  (when-let* ((file (or file buffer-file-name)))
    (let ((case-fold-search t)
          (name (file-name-nondirectory file)))
      (or (string-match-p chief/dotnet-xml-file-regexp file)
          (member (downcase name)
                  '("nuget.config" "packages.config" "app.config"
                    "web.config"))
          (string-match-p "\\.\\(?:dll\\|exe\\)\\.config\\'"
                          (downcase name))))))

(defun chief/dotnet-quoted-value-at-point ()
  "Return the quoted value and point offset on the current line."
  (let ((position (point)) result)
    (save-excursion
      (goto-char (line-beginning-position))
      (while (and (not result)
                  (re-search-forward
                   "\\([\"']\\)\\([^\"'\n]+\\)\\1"
                   (line-end-position) t))
        (when (and (<= (match-beginning 2) position)
                   (<= position (match-end 2)))
          (setq result
                (cons (match-string-no-properties 2)
                      (- position (match-beginning 2)))))))
    result))

(defun chief/dotnet-value-segment-at-offset (value offset)
  "Return the semicolon-delimited segment in VALUE containing OFFSET."
  (let ((start 0) result)
    (while (and (not result)
                (string-match "[^;]+" value start))
      (let ((begin (match-beginning 0))
            (end (match-end 0)))
        (when (and (<= begin offset) (<= offset end))
          (setq result (string-trim (match-string 0 value))))
        (setq start (if (< end (length value)) (1+ end) end))))
    (or result (string-trim value))))

(defun chief/dotnet-msbuild-property-value (name)
  "Return a local or well-known MSBuild property value for NAME."
  (let* ((directory (file-name-directory buffer-file-name))
         (solution (ignore-errors (chief/dotnet-solution-file))))
    (or
     (pcase name
       ((or "MSBuildThisFileDirectory" "ProjectDir") directory)
       ((or "MSBuildThisFileFullPath" "ProjectPath") buffer-file-name)
       ("ProjectFile" (file-name-nondirectory buffer-file-name))
       ("ProjectName" (file-name-base buffer-file-name))
       ("ProjectExt" (concat "." (or (file-name-extension buffer-file-name) "")))
       ("MSBuildProjectDirectory" (directory-file-name directory))
       ("MSBuildProjectFullPath" buffer-file-name)
       ("SolutionDir" (and solution (file-name-directory solution)))
       ("SolutionPath" solution)
       ("SolutionFileName" (and solution (file-name-nondirectory solution)))
       ("SolutionName" (and solution (file-name-base solution))))
     (save-excursion
       (goto-char (point-min))
       (when (re-search-forward
              (format
               "<%s\\(?:[ \t\n][^>]*\\)?>\\([^<]+\\)</%s>"
               (regexp-quote name) (regexp-quote name))
              nil t)
         (string-trim (match-string-no-properties 1)))))))

(defun chief/dotnet-expand-msbuild-path (value)
  "Expand simple local MSBuild properties in path VALUE."
  (let ((expanded value)
        (limit 10)
        changed)
    (while (and (> limit 0)
                (string-match "\\$(\\([[:alnum:]_.-]+\\))" expanded))
      (setq limit (1- limit))
      (let ((begin (match-beginning 0))
            (end (match-end 0))
            (name (match-string 1 expanded)))
        (if-let* ((replacement
                   (chief/dotnet-msbuild-property-value name)))
            (setq expanded
                  (concat (substring expanded 0 begin)
                          replacement
                          (substring expanded end))
                  changed t)
          (setq limit 0))))
    (and (or changed (not (string-match-p "\\$(" expanded)))
         expanded)))

(defun chief/dotnet-unquoted-path-at-point ()
  "Return a possible unquoted .NET project path at point."
  (when-let* ((token (thing-at-point 'filename t)))
    (let ((value
           (replace-regexp-in-string
            (concat
             "\\`\\(?:@\\|[-/]\\(?:r\\|reference\\|lib\\|out\\|resource"
             "\\|linkresource\\|configfile\\):\\)")
            "" token t)))
      (string-trim value "[,;]+" "[,;]+"))))

(defun chief/dotnet-project-paths-at-point ()
  "Return existing project-relative paths named at point."
  (let* ((quoted (chief/dotnet-quoted-value-at-point))
         (candidate
          (if quoted
              (chief/dotnet-value-segment-at-offset
               (car quoted) (cdr quoted))
            (chief/dotnet-unquoted-path-at-point))))
    (when-let* ((raw-value candidate)
                (value (chief/dotnet-expand-msbuild-path raw-value))
                ((not (or (string-empty-p value)
                          (string-match-p "[$%@]([^)]+)" value)
                          (string-match-p
                           "\\`[[:alpha:]][[:alnum:]+.-]*://" value)
                          (string-match-p
                           "\\`{[-[:xdigit:]]+}\\'" value)))))
      (let* ((normalized (replace-regexp-in-string "\\\\" "/" value))
             (path (expand-file-name normalized
                                     (file-name-directory buffer-file-name)))
             (paths (if (string-match-p "[*?]" path)
                        (file-expand-wildcards path t)
                      (list path))))
        (seq-filter #'file-exists-p paths)))))

(defun chief/dotnet-project-path-locations-at-point ()
  "Return Xref items for project-relative paths named at point."
  (mapcar
   (lambda (file)
     (xref-make
      (file-relative-name file (file-name-directory buffer-file-name))
      (xref-make-file-location file 1 0)))
   (seq-filter #'file-regular-p (chief/dotnet-project-paths-at-point))))

(defun chief/dotnet-project-file-request-definition (&optional action)
  "Navigate a .NET project-file reference using display ACTION or LSP."
  (interactive)
  (if-let* ((locations (chief/dotnet-project-path-locations-at-point)))
      (chief/dotnet-display-source-locations locations action)
    (if (chief/lsp-ensure-active-for-navigation)
        (chief/lsp-show-location-list-async
         "textDocument/definition" nil action)
      (user-error "No project path or LSP definition is available here"))))

(defun chief/dotnet-project-file-goto-definition ()
  "Go to a referenced path or XML definition in a .NET project file."
  (interactive)
  (chief/dotnet-project-file-request-definition))

(defun chief/dotnet-project-file-goto-definition-other-window ()
  "Go to a referenced path or XML definition in another window."
  (interactive)
  (chief/dotnet-project-file-request-definition 'window))

(defun chief/dotnet-project-file-setup ()
  "Configure LSP and path navigation for XML-based .NET files."
  (when (chief/dotnet-xml-project-file-p)
    (setq-local compile-command "dotnet build")
    (setq-local chief/lsp-root-function #'chief/dotnet-project-root)
    (setq-local chief/lsp-definition-function
                #'chief/dotnet-project-file-goto-definition)
    (setq-local chief/lsp-definition-other-window-function
                #'chief/dotnet-project-file-goto-definition-other-window)
    (chief/polyglot-completion-setup t)
    (when (require 'lsp-xml nil t)
      (setq-local lsp-enabled-clients '(xmlls))
      (chief/lsp-managed-mode-setup))))

(defun chief/dotnet-json-project-file-p (&optional file)
  "Return non-nil when FILE is a supported JSON-based .NET file."
  (when-let* ((file (or file buffer-file-name))
              (downcased (downcase file))
              (name (file-name-nondirectory downcased)))
    (or (string-match-p chief/dotnet-json-file-regexp downcased)
        (member name
                '("global.json" "dotnet-tools.json" "packages.lock.json"
                  "project.assets.json" "launchsettings.json"
                  "servicedependencies.json" "connectedservice.json"
                  "local.settings.json" "host.json"))
        (string-match-p
         "\\`\\(?:appsettings\\|servicedependencies\\)\\(?:\\..+\\)?\\.json\\'"
         name))))

(defun chief/dotnet-plain-project-file-p (&optional file)
  "Return non-nil when FILE is a supported plain-text .NET workspace file."
  (when-let* ((file (or file buffer-file-name))
              (downcased (downcase file))
              (name (file-name-nondirectory downcased)))
    (or (string-match-p chief/dotnet-plain-file-regexp downcased)
        (member name
                '("paket.dependencies" "paket.references" "paket.lock"
                  "directory.build.rsp")))))

(defun chief/dotnet-solution-file-setup ()
  "Configure plain-text and JSON .NET workspace files."
  (let ((json-p (chief/dotnet-json-project-file-p)))
    (when (and buffer-file-name
               (or (chief/dotnet-plain-project-file-p) json-p))
      (setq-local compile-command "dotnet build")
      (setq-local chief/lsp-root-function #'chief/dotnet-project-root)
      (setq-local chief/lsp-definition-function
                  #'chief/dotnet-project-file-goto-definition)
      (setq-local chief/lsp-definition-other-window-function
                  #'chief/dotnet-project-file-goto-definition-other-window)
      ;; Generic JSON setup runs first and can seed lsp-mode with a JS/Git
      ;; workspace.  Re-prime the domain-specific .NET root before the deferred
      ;; server startup actually occurs.
      (when json-p
        (chief/lsp-prime-session-root)
        (when (fboundp 'chief/lsp-managed-mode-setup)
          (chief/lsp-managed-mode-setup))))))

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
  :mode ("\\.fsscript\\'" . fsharp-mode)
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

(add-to-list 'auto-mode-alist
             (cons chief/dotnet-xml-file-regexp 'nxml-mode))

(dolist (entry '(("\\.sln\\'" . conf-mode)
                 ("\\.slnf\\'" . json-mode)
                 ("\\.\\(?:editorconfig\\|globalconfig\\)\\'" . editorconfig-conf-mode)
                 ("\\.rsp\\'" . conf-mode)
                 ("global\\.json\\'" . json-mode)
                 ("[Nn]u[Gg]et\\.[Cc]onfig\\'" . nxml-mode)
                 ("packages\\.config\\'" . nxml-mode)
                 ("[Aa]pp\\.config\\'" . nxml-mode)
                 ("[Ww]eb\\.config\\'" . nxml-mode)
                 ("\\.\\(?:dll\\|exe\\)\\.config\\'" . nxml-mode)
                 ("paket\\.dependencies\\'" . conf-mode)
                 ("paket\\.references\\'" . conf-mode)
                 ("paket\\.lock\\'" . conf-mode)))
  (add-to-list 'auto-mode-alist entry))

(add-hook 'csharp-mode-hook #'chief/csharp-mode-setup)
(when (fboundp 'csharp-ts-mode)
  (add-hook 'csharp-ts-mode-hook #'chief/csharp-mode-setup))
(add-hook 'nxml-mode-hook #'chief/dotnet-project-file-setup)
(add-hook 'conf-mode-hook #'chief/dotnet-solution-file-setup)
(add-hook 'editorconfig-conf-mode-hook #'chief/dotnet-solution-file-setup)
;; Append after generic JS/JSON setup so the .NET workspace root wins before
;; lsp-mode's deferred server startup.
(add-hook 'js-json-mode-hook #'chief/dotnet-solution-file-setup t)
(add-hook 'json-mode-hook #'chief/dotnet-solution-file-setup t)
(add-hook 'json-ts-mode-hook #'chief/dotnet-solution-file-setup t)

(with-eval-after-load 'lsp-mode
  (require 'lsp-csharp nil t)
  (require 'lsp-fsharp nil t)
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
    :new-connection (lsp-stdio-connection #'chief/dotnet-roslyn-command
                                          #'chief/dotnet-roslyn-available-p)
    :activation-fn (lsp-activate-on "csharp")
    :uri->path-fn #'chief/dotnet-roslyn-uri-to-path
    :initialized-fn #'chief/dotnet-roslyn-initialized
    :notification-handlers
    (lsp-ht ("workspace/projectInitializationComplete"
             #'chief/dotnet-roslyn-project-initialized))
    :priority 2
    :server-id 'chief-roslyn))
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
              :override #'chief/dotnet-fsautocomplete-command)
  (advice-add 'lsp-fsharp--test-fsautocomplete-present
              :override #'chief/dotnet-fsautocomplete-available-p))

(with-eval-after-load 'csharp-mode
  (chief/repl-setup-standard-local-leader 'csharp-mode-map)
  (when (boundp 'csharp-ts-mode-map)
    (chief/repl-setup-standard-local-leader 'csharp-ts-mode-map))
  (chief/local-leader-def
    :keymaps '(csharp-mode-map csharp-ts-mode-map)
    "sR" #'chief/csharp-reload-project
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
  (chief/repl-setup-standard-local-leader 'fsharp-mode-map)
  (chief/local-leader-def
    :keymaps 'fsharp-mode-map
    "sR" #'chief/fsharp-reload-project
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
