;;; lang-sharplsp.el --- Explicit, project-scoped SharpLsp support -*- lexical-binding: t; -*-

(require 'lang-dotnet)
(require 'button)

(defgroup chief-sharplsp nil
  "SharpLsp integration, without changing the default .NET servers."
  :group 'chief-dotnet)

(defcustom chief/sharplsp-command nil
  "Trusted SharpLsp command and arguments, or nil for automatic discovery.
Never read an executable command from a project's configuration."
  :type '(choice (const nil) (repeat string))
  :group 'chief-sharplsp)

(defcustom chief/sharplsp-install-directory
  (locate-user-emacs-file "var/sharplsp/")
  "Directory containing a complete standalone SharpLsp distribution."
  :type 'directory :group 'chief-sharplsp)

(defcustom chief/sharplsp-tool-directory
  (locate-user-emacs-file "var/sharplsp-tools/")
  "Emacs-owned .NET tools used only in opted-in buffers, not global installs."
  :type 'directory :group 'chief-sharplsp)

(defcustom chief/sharplsp-csharpier-command nil
  "Optional CSharpier command prefix for buffer formatting."
  :type '(choice (const nil) (repeat string)) :group 'chief-sharplsp)

(defvar chief/sharplsp-navigation-owners (make-hash-table :test #'equal)
  "Canonical navigation target paths mapped to their originating config root.")
(defvar chief/sharplsp-solutions (make-hash-table :test #'equal)
  "Solutions explicitly loaded into both sidecars, keyed by config root.")
(defvar chief/sharplsp-warnings (make-hash-table :test #'equal))
(defvar-local chief/sharplsp-origin-root nil
  "SharpLsp project responsible for this source or generated library view.")
(put 'chief/sharplsp-origin-root 'permanent-local t)

(defun chief/sharplsp-field (object key)
  "Read JSON KEY from OBJECT in either lsp-mode representation."
  (chief/dotnet-lsp-field object key (intern (concat ":" key))))

(defun chief/sharplsp-point-position (point)
  "Convert POINT to a protocol Position in lsp-mode's configured representation."
  (let ((position (lsp-point-to-position point)))
    (lsp-make-position :line (plist-get position :line)
                       :character (plist-get position :character))))

(defun chief/sharplsp-position-point (position)
  "Convert either an incoming or outgoing POSITION without assuming hash/plist."
  (let ((line (chief/sharplsp-field position "line"))
        (character (chief/sharplsp-field position "character")))
    (unless (and (integerp line) (>= line 0) (integerp character) (>= character 0))
      (error "Invalid LSP position: %S" position))
    (let* ((point (lsp--position-to-point (lsp-make-position :line line :character character)))
           (roundtrip (chief/sharplsp-point-position point)))
      (unless (and (= line (chief/sharplsp-field roundtrip "line"))
                   (= character (chief/sharplsp-field roundtrip "character")))
        (error "LSP position is outside the current buffer: %S" position))
      point)))

(defun chief/sharplsp-canonical (path)
  "Return a canonical local PATH, also accepting a not-yet-created file."
  (and path (not (file-remote-p path)) (file-truename (expand-file-name path))))

(defun chief/sharplsp-project-root (&optional file)
  "Find the nearest explicit sharplsp.toml for FILE, stopping at a Git boundary.
A solution, project, .sharplsp cache, or editor working directory is not opt-in."
  (when-let* ((file (chief/sharplsp-canonical (or file buffer-file-name))))
    (let ((directory (file-name-directory file)) found done)
      (while (and directory (not done))
        (cond
         ((file-readable-p (expand-file-name "sharplsp.toml" directory))
          (setq found directory done t))
         ((file-exists-p (expand-file-name ".git" directory)) (setq done t))
         (t
          (let ((parent (file-name-directory (directory-file-name directory))))
            (setq directory (unless (equal parent directory) parent))))))
      found)))

(defun chief/sharplsp-metadata-file-p (&optional file)
  "Recognize generated C# library views, not arbitrary similarly named folders."
  (when-let* ((file (chief/sharplsp-canonical (or file buffer-file-name)))
              ((string-match-p "\\.cs\\'" file)))
    (or (file-in-directory-p file (expand-file-name chief/fsharp-metadata-cache-directory))
        (seq-some
         (lambda (directory)
           (file-in-directory-p file
                               (expand-file-name directory
                                                 (file-truename temporary-file-directory))))
         '("sharplsp-decompiled/" "MetadataAsSource/")))))

(defun chief/sharplsp-owned-metadata-p ()
  "Whether this generated view was reached from an opted-in SharpLsp project."
  (and (chief/sharplsp-metadata-file-p)
       (or chief/sharplsp-origin-root
           (gethash (chief/sharplsp-canonical buffer-file-name)
                    chief/sharplsp-navigation-owners))))

(defun chief/sharplsp-server-command ()
  "Resolve a trusted standalone server, keeping its adjacent sidecars intact."
  (let* ((binary (if (eq system-type 'windows-nt) "sharplsp.exe" "sharplsp"))
         (command
          (or chief/sharplsp-command
              (when-let* ((program (expand-file-name binary chief/sharplsp-install-directory))
                          ((file-executable-p program))) (list program))
              (when-let* ((program (executable-find "sharplsp"))) (list program))
              (seq-some
               (lambda (directory)
                 (let ((program (expand-file-name binary directory)))
                   (when (file-executable-p program) (list program))))
               (list chief/sharplsp-install-directory
                     ;; Reuse an already installed standalone distribution;
                     ;; Neovim itself is neither launched nor required.
                     (expand-file-name "~/.local/share/nvim/sharplsp/"))))))
    (when (and (consp command) (stringp (car command))
               (executable-find (car command)))
      command)))

(defun chief/sharplsp-razor-p ()
  "Whether the current buffer is a Razor component, page, or MVC view."
  (and buffer-file-name (derived-mode-p 'web-mode)
       (string-match-p "\\.\\(?:razor\\|cshtml\\)\\'" buffer-file-name)))

(defun chief/sharplsp-preferred-root ()
  "Return this buffer's explicit or navigation-inherited SharpLsp root."
  (when (and buffer-file-name
             (or (derived-mode-p 'csharp-mode 'csharp-ts-mode 'fsharp-mode 'fsharp-ts-mode)
                 (chief/sharplsp-razor-p))
             (not (chief/sharplsp-metadata-file-p)))
    (when-let* ((root (or (chief/sharplsp-project-root)
                         (gethash (chief/sharplsp-canonical buffer-file-name)
                                  chief/sharplsp-navigation-owners)))
                ((file-readable-p (expand-file-name "sharplsp.toml" root))))
      (if (chief/sharplsp-server-command)
          root
        (unless (gethash root chief/sharplsp-warnings)
          (puthash root t chief/sharplsp-warnings)
          (display-warning
           'chief-sharplsp
           (format "SharpLsp opt-in at %s, but no standalone server is installed; using the existing .NET server. Set chief/sharplsp-command or install the complete archive under %s"
                   root chief/sharplsp-install-directory)))))))

(defun chief/sharplsp-root ()
  "Return the current source buffer's chosen SharpLsp workspace root."
  (or chief/sharplsp-origin-root (chief/sharplsp-preferred-root)))

(defun chief/sharplsp-workspace ()
  "Return the SharpLsp workspace attached to the current buffer, if any."
  (seq-find (lambda (workspace)
              (eq (lsp--client-server-id (lsp--workspace-client workspace))
                  'chief-sharplsp))
            (chief/lsp-current-workspaces)))

(defun chief/sharplsp-remember-locations (locations)
  "Remember ownership before LOCATIONS cause new file buffers to initialize."
  (when-let* ((root (or (when-let* ((workspace (chief/sharplsp-workspace)))
                         (file-name-as-directory (lsp--workspace-root workspace)))
                       (and (chief/sharplsp-metadata-file-p) chief/sharplsp-origin-root))))
    (cl-labels
        ((visit (value)
           (cond
            ((or (hash-table-p value)
                 (and (listp value) (keywordp (car-safe value))))
             (dolist (key '("uri" "targetUri"))
               (when-let* ((uri (chief/sharplsp-field value key))
                           ((string-prefix-p "file:" uri))
                           (path (chief/sharplsp-canonical (lsp--uri-to-path-1 uri))))
                 (puthash path root chief/sharplsp-navigation-owners)))
             (if (hash-table-p value)
                 (maphash (lambda (_ child) (visit child)) value)
               (cl-loop for (_ child) on value by #'cddr do (visit child))))
            ((sequencep value)
             (unless (stringp value) (mapc #'visit value))))))
      (visit locations))))

(advice-add 'lsp--locations-to-xref-items :before #'chief/sharplsp-remember-locations)

(defun chief/sharplsp-remember-source-xrefs (items &rest _)
  "Keep origin ownership when the shared namespace resolver returns real source."
  (when-let* ((workspace (chief/sharplsp-workspace)))
    (dolist (item items)
      (unless (chief/dotnet-xref-metadata-item-p item)
        (when-let* ((file (ignore-errors (xref-file-location-file (xref-item-location item))))
                    (canonical (chief/sharplsp-canonical file)))
          (puthash canonical (file-name-as-directory (lsp--workspace-root workspace))
                   chief/sharplsp-navigation-owners))))))
(advice-add 'chief/dotnet-display-source-locations :before #'chief/sharplsp-remember-source-xrefs)

(defun chief/sharplsp-metadata-uri-path-a (path)
  "Carry origin context across subsequent Roslyn metadata/source-link hops."
  (when (and (chief/sharplsp-metadata-file-p) chief/sharplsp-origin-root
             (stringp path) (not (file-remote-p path)))
    (puthash (chief/sharplsp-canonical path) chief/sharplsp-origin-root
             chief/sharplsp-navigation-owners))
  path)
(advice-add 'lsp--uri-to-path :filter-return #'chief/sharplsp-metadata-uri-path-a)

(defun chief/sharplsp-show-references (command)
  "Translate SharpLsp's VS Code reference lens COMMAND into native Xref."
  (let* ((arguments (append (chief/sharplsp-field command "arguments") nil))
         (locations (nth 2 arguments)))
    (if locations
        (if-let* ((items (lsp--locations-to-xref-items locations)))
            (lsp-show-xrefs items nil t)
          (message "No references"))
      (let ((params (lsp--text-document-position-params)))
        (when (nth 1 arguments)
          (setq params (list :textDocument (list :uri (car arguments))
                             :position (nth 1 arguments))))
        (lsp-request-async
         "textDocument/references"
         (append params '(:context (:includeDeclaration :json-false)))
         (lambda (response)
           (if-let* ((items (lsp--locations-to-xref-items response)))
               (lsp-show-xrefs items nil t)
             (message "No references")))
         :mode 'detached)))))

(defun chief/sharplsp-prepare-lenses (lenses)
  "Give argument-free SharpLsp reference LENSES their actual declaration position."
  (when (chief/sharplsp-workspace)
    (dolist (lens lenses)
      (let ((command (chief/sharplsp-field lens "command")))
        (when (and (equal (chief/sharplsp-field command "command")
                          "editor.action.showReferences")
                   (seq-empty-p (chief/sharplsp-field command "arguments")))
          (lsp-put command :arguments
                   (vector (lsp--buffer-uri)
                           (chief/sharplsp-field (chief/sharplsp-field lens "range") "start"))))))))

(with-eval-after-load 'lsp-lens
  (advice-add 'lsp-lens--display :before #'chief/sharplsp-prepare-lenses))

(defun chief/sharplsp-initialized (workspace)
  "Ask for effective configuration, then explicitly load both sidecars."
  (with-lsp-workspace workspace
    (lsp-request-async
     "sharplsp/configuration" (make-hash-table :test #'equal)
     (lambda (configuration)
       (let* ((root (file-name-as-directory (lsp--workspace-root workspace)))
              (configured (chief/sharplsp-field
                           (chief/sharplsp-field configuration "csharp") "solution_path"))
              (solutions (append (directory-files root t "\\.sln\\'")
                                 (directory-files root t "\\.slnx\\'")))
              (solution (or (and (stringp configured) (not (string-empty-p configured))
                                 (expand-file-name configured root))
                            (and (= (length solutions) 1) (car solutions)))))
         (if solution
             (chief/sharplsp-load-solution solution workspace)
           (when solutions
             (message "SharpLsp: multiple solutions; use M-x chief/sharplsp-load-solution")))))
     :error-handler (lambda (error)
                      (display-warning 'chief-sharplsp
                                       (format "SharpLsp configuration failed: %s" error)))
     :mode 'detached)))

(defun chief/sharplsp-refresh-presentation (workspace)
  "Refresh actual hints/lenses after asynchronous sidecar project checking."
  (when (and (processp (lsp--workspace-proc workspace))
             (process-live-p (lsp--workspace-proc workspace)))
    (dolist (buffer (lsp--workspace-buffers workspace))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (memq workspace (lsp-workspaces))
            (when (fboundp 'lsp-lens-refresh) (lsp-lens-refresh t))
            (if (bound-and-true-p lsp-inlay-hints-mode)
                (lsp-update-inlay-hints (point-min) (point-max))
              (when (derived-mode-p 'fsharp-mode 'fsharp-ts-mode)
                (chief/sharplsp-fsharp-refresh-signatures)))))))))

(defun chief/sharplsp-load-solution (file &optional workspace)
  "Load FILE into BOTH language sidecars of WORKSPACE."
  (interactive
   (list (read-file-name "SharpLsp solution: " (chief/sharplsp-root) nil t)))
  (let* ((workspace (or workspace (chief/sharplsp-workspace)
                        (user-error "No active SharpLsp workspace")))
         (file (expand-file-name file (lsp--workspace-root workspace))))
    (unless (and (file-readable-p file) (string-match-p "\\.slnx?\\'" file))
      (user-error "Not a readable .sln/.slnx: %s" file))
    (with-lsp-workspace workspace
      (lsp-request-async
       "sharplsp/loadSolution" (list :solutionPath file)
       (lambda (response)
         (if (eq (chief/sharplsp-field response "success") :json-false)
             (display-warning 'chief-sharplsp (format "Solution load failed: %s" response))
           (puthash (file-name-as-directory (lsp--workspace-root workspace)) file
                    chief/sharplsp-solutions)
           (chief/sharplsp-refresh-presentation workspace)))
       :error-handler (lambda (error)
                        (display-warning 'chief-sharplsp
                                         (format "SharpLsp solution load failed: %s" error)))
       :mode 'detached))))

(defvar chief/sharplsp-mode-map (make-sparse-keymap))
(define-minor-mode chief/sharplsp-mode
  "Buffer-local SharpLsp commands; never overrides a non-opted-in project."
  :lighter " Sharp" :keymap chief/sharplsp-mode-map)

(defun chief/sharplsp-setup (root)
  "Use SharpLsp for ROOT without changing other buffers' .NET defaults."
  (setq-local exec-path (copy-sequence exec-path)
              process-environment (copy-sequence process-environment)
              chief/dotnet-tool-directories
              (cons chief/sharplsp-tool-directory chief/dotnet-tool-directories))
  (chief/dotnet-ensure-tool-paths)
  (setq-local chief/sharplsp-origin-root root
              chief/lsp-root-function #'chief/sharplsp-root
              lsp-enabled-clients '(chief-sharplsp)
              lsp-disabled-clients '(fsac chief-roslyn csharp-ls csharp-roslyn omnisharp)
              chief/lsp-inlay-hints-deferred nil
              chief/lsp-diagnostics-deferred nil
              lsp-lens-enable t)
  (when (and (or (derived-mode-p 'csharp-mode 'csharp-ts-mode) (chief/sharplsp-razor-p))
             (not (local-variable-p 'lsp-semantic-tokens-enable)))
    (setq-local lsp-semantic-tokens-enable t))
  (when (derived-mode-p 'csharp-mode 'csharp-ts-mode)
    (add-hook 'after-save-hook #'chief/sharplsp-hot-reload-after-save nil t))
  (remove-hook 'lsp-after-open-hook #'chief/dotnet-fsharp-load-current-project t)
  (chief/format-enable-on-save #'chief/sharplsp-format-buffer)
  (chief/sharplsp-mode 1)
  (when (derived-mode-p 'fsharp-mode 'fsharp-ts-mode)
    (chief/sharplsp-fsharp-presentation-setup)))

(defun chief/sharplsp-language-setup-a (function &rest arguments)
  "Choose SharpLsp before deferred startup, or preserve FUNCTION unchanged."
  (if-let* ((root (chief/sharplsp-preferred-root)))
      (let ((chief/dotnet-csharp-server 'sharplsp))
        (apply function arguments)
        (chief/sharplsp-setup root))
    (unless (chief/sharplsp-owned-metadata-p)
      (setq-local chief/sharplsp-origin-root nil))
    (when (chief/sharplsp-owned-metadata-p)
      ;; Generated files are not members of SharpLsp's semantic workspace.
      (setq-local chief/dotnet-csharp-server 'roslyn
                  chief/sharplsp-origin-root
                  (or chief/sharplsp-origin-root
                      (gethash (chief/sharplsp-canonical buffer-file-name)
                               chief/sharplsp-navigation-owners))
                  chief/dotnet-metadata-source-root
                  (or chief/sharplsp-origin-root default-directory)
                  chief/lsp-diagnostics-disabled t
                  ;; The generated file is outside the solution directory.
                  ;; lsp-workspace-root therefore returns nil for connection
                  ;; cwd even though we explicitly selected the origin root.
                  lsp-use-workspace-root-for-server-default-directory nil
                  buffer-read-only t)
      (when chief/sharplsp-origin-root
        (setq default-directory (file-name-as-directory chief/sharplsp-origin-root)))
      (setq-local chief/dotnet-metadata-source-solution
                  (gethash chief/sharplsp-origin-root chief/sharplsp-solutions)))
    (apply function arguments)
    (when (chief/sharplsp-owned-metadata-p)
      (setq-local chief/format-on-save-enabled nil)
      (chief/sharplsp-mode 1))))

(advice-add 'chief/csharp-mode-setup :around #'chief/sharplsp-language-setup-a)
(advice-add 'chief/fsharp-mode-setup :around #'chief/sharplsp-language-setup-a)

(defvar-local chief/sharplsp-metadata-ready nil)
(defvar-local chief/sharplsp-metadata-navigation nil)

(defun chief/sharplsp-metadata-definition (function action)
  "Wait for Roslyn's loose-document semantics before the first library hop.
Changing the buffer, cursor, window or revision cancels this queued navigation."
  (let* ((buffer (current-buffer)) (window (selected-window))
         (position (point)) (tick (buffer-chars-modified-tick))
         (token (make-symbol "metadata-navigation")) (deadline (+ (float-time) 8))
         (params (lsp--text-document-position-params)))
    (setq chief/sharplsp-metadata-navigation token)
    (unless (bound-and-true-p lsp-managed-mode) (lsp))
    (cl-labels
        ((valid ()
           (and (buffer-live-p buffer) (window-live-p window)
                (eq window (selected-window)) (eq (window-buffer window) buffer)
                (with-current-buffer buffer
                  (and (eq token chief/sharplsp-metadata-navigation)
                       (= tick (buffer-chars-modified-tick)) (= position (point))))))
         (finish ()
           (when (valid)
             (with-selected-window window
               (setq chief/sharplsp-metadata-navigation nil)
               (funcall function action))))
         (retry (&optional _error)
           (when (valid)
             (if (> (float-time) deadline) (finish)
               (run-with-timer 0.15 nil #'probe))))
         (probe ()
           (when (valid)
             (with-current-buffer buffer
               (if (not (bound-and-true-p lsp-managed-mode)) (retry)
                 (lsp-request-async
                  "textDocument/hover" params
                  (lambda (hover)
                    (when (valid)
                      (with-current-buffer buffer
                        (if (chief/sharplsp-field hover "contents")
                            (progn (setq chief/sharplsp-metadata-ready (car (lsp-workspaces)))
                                   (finish))
                          (retry)))))
                  :error-handler #'retry :mode 'detached))))))
      (probe))))

(defun chief/sharplsp-definition-a (function &optional action)
  "Use SharpLsp's native locations; retain existing default-server fallbacks."
  (cond
   ((and (chief/sharplsp-workspace) (chief/dotnet-import-namespace-at-point))
    ;; Preserve the existing exact namespace/source/assembly resolver. Servers
    ;; can answer nil or only an import's first segment; neither is a definition.
    (funcall function action))
   ((chief/sharplsp-workspace)
    (chief/lsp-request-locations-async
     "textDocument/definition"
     (lambda (items) (chief/dotnet-display-source-locations items action))))
   ((and (chief/sharplsp-owned-metadata-p)
         (not (memq chief/sharplsp-metadata-ready (lsp-workspaces))))
    (chief/sharplsp-metadata-definition function action))
   (t (funcall function action))))
(advice-add 'chief/dotnet-request-definition :around #'chief/sharplsp-definition-a)

(defun chief/sharplsp-remember-namespace-metadata (file &rest _)
  "Give the shared import resolver's generated view the same exact-file ownership."
  (when (and (chief/sharplsp-workspace) (chief/sharplsp-metadata-file-p file))
    (puthash (chief/sharplsp-canonical file) chief/sharplsp-origin-root
             chief/sharplsp-navigation-owners)))
(advice-add 'chief/fsharp-show-metadata-definition :before #'chief/sharplsp-remember-namespace-metadata)

(defun chief/sharplsp-install-server ()
  "Build the pinned complete server, both sidecars and profiler in Emacs's cache."
  (interactive)
  (unless (executable-find "bash") (user-error "A bash shell is required for the private build"))
  (let ((script (locate-user-emacs-file "scripts/install-sharplsp.sh")))
    (compilation-start
     (mapconcat #'shell-quote-argument
                (list "bash" script (expand-file-name chief/sharplsp-install-directory)) " ")
     'compilation-mode (lambda (_) "*SharpLsp complete installation*"))))

(defun chief/sharplsp-install-repl ()
  "Install the official C# interactive host into Emacs's private tool cache."
  (interactive)
  (chief/dotnet-ensure-tool-paths)
  (let ((directory (expand-file-name chief/sharplsp-tool-directory)))
    (compilation-start
     (chief/dotnet-command "tool" "install" "Microsoft.dotnet-interactive"
                           "--tool-path" directory "--allow-roll-forward")
     'compilation-mode (lambda (_) "*SharpLsp install REPL*"))))

(defun chief/sharplsp-formatter-command ()
  "Return an available dedicated formatter; SharpLsp does not expose formatting."
  (if (derived-mode-p 'fsharp-mode 'fsharp-ts-mode)
      (or (chief/dotnet-local-tool-command "fantomas")
          (when-let* ((program (chief/dotnet-tool-executable "fantomas")))
            (list program)))
    (let ((prefix
           (or chief/sharplsp-csharpier-command
               (chief/dotnet-local-tool-command "csharpier")
               (when-let* ((program (chief/dotnet-tool-executable "csharpier")))
                 (list program))
               (let ((program (expand-file-name "~/.local/share/nvim/mason/bin/csharpier")))
                 (when (file-executable-p program) (list program))))))
      (when prefix
        (append prefix (list "format" "--write-stdout" "--stdin-path"
                             (or buffer-file-name (expand-file-name "Buffer.cs"))))))))

(defun chief/sharplsp-format-buffer ()
  "Format unsaved text through Fantomas/CSharpier, without modifying the file.
Fantomas 8 has no stdin formatter. Its temporary input is adjacent to the
source so .editorconfig discovery still uses the real project settings."
  (interactive)
  (when (chief/sharplsp-metadata-file-p)
    (user-error "Generated library views are read-only"))
  (chief/dotnet-ensure-tool-paths)
  (let ((command (chief/sharplsp-formatter-command)))
    (if (not command)
        (if (called-interactively-p 'interactive)
            (user-error "Install Fantomas/CSharpier; SharpLsp does not expose LSP formatting")
          (message "SharpLsp: buffer formatting skipped; dedicated formatter unavailable"))
      (let* ((output (generate-new-buffer " *sharplsp-format*"))
             (stderr (make-temp-file "sharplsp-format-"))
             (position (point-marker))
             (fsharp (derived-mode-p 'fsharp-mode 'fsharp-ts-mode))
             (coding-system-for-write 'utf-8-unix)
             (coding-system-for-read 'utf-8-unix)
             input target)
        (unwind-protect
            (let ((status
                   (if fsharp
                       (progn
                         (setq input (make-temp-file
                                      (expand-file-name ".chief-format-"
                                                        (if buffer-file-name
                                                            (file-name-directory buffer-file-name)
                                                          default-directory))
                                      nil (concat "." (or (and buffer-file-name
                                                               (file-name-extension buffer-file-name))
                                                          "fs")))
                               target (make-temp-file "chief-format-out-" nil ".fs"))
                         (write-region (point-min) (point-max) input nil 'silent)
                         (prog1 (apply #'call-process (car command) nil (list output stderr)
                                       nil (append (cdr command) (list "--out" target input)))
                           (with-current-buffer output
                             (erase-buffer)
                             (insert-file-contents target))))
                     (apply #'call-process-region (point-min) (point-max)
                            (car command) nil (list output stderr) nil (cdr command)))))
              (unless (and (integerp status) (zerop status))
                (error "Formatter failed: %s"
                       (with-temp-buffer (insert-file-contents stderr) (buffer-string))))
              (when (and (> (buffer-size) 0) (zerop (buffer-size output))
                         (save-excursion (goto-char (point-min)) (re-search-forward "\\S-" nil t)))
                (error "Formatter returned an empty result; source was not changed"))
              (atomic-change-group
                (replace-region-contents (point-min) (point-max) output))
              (goto-char position))
          (set-marker position nil)
          (kill-buffer output)
          (dolist (file (list stderr input target))
            (when (and file (file-exists-p file)) (delete-file file))))))))

(defconst chief/sharplsp-extension-methods
  '("sharplsp/configuration" "sharplsp/loadSolution" "sharplsp/workspaceSymbols"
    "sharplsp/targetFramework" "sharplsp/setTargetFramework" "sharplsp/sortMembers"
    "sharplsp/statementStop" "sharplsp/hotReload"
    "sharplsp/nuget/targets" "sharplsp/nuget/installed" "sharplsp/nuget/search"
    "sharplsp/nuget/versions" "sharplsp/nuget/install" "sharplsp/nuget/uninstall"
    "sharplsp/nuget/unused" "sharplsp/nuget/consolidate")
  "Documented custom host requests; availability depends on the server version.")

(defun chief/sharplsp-show-result (title value)
  "Display VALUE in a terminal-compatible read-only JSON results buffer."
  (let ((buffer (get-buffer-create (format "*SharpLsp %s*" title))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (json-encode value))
        (json-pretty-print-buffer)
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buffer)))

(defun chief/sharplsp-read-json (prompt initial)
  "Read JSON parameters with PROMPT and INITIAL, without evaluating Lisp."
  (json-parse-string (read-string prompt (json-encode initial))
                     :object-type 'hash-table :array-type 'array
                     :null-object nil :false-object :json-false))

(defun chief/sharplsp-request (method params)
  "Send custom METHOD with PARAMS and display the actual server response.
This protocol workbench is not a port of VS Code's graphical panels."
  (interactive
   (list (completing-read "SharpLsp request: " chief/sharplsp-extension-methods nil t)
         (chief/sharplsp-read-json "Parameters (JSON): " (make-hash-table :test #'equal))))
  (let ((workspace (or (chief/sharplsp-workspace)
                       (user-error "No active SharpLsp workspace"))))
    (when (or (not (string-match-p "install\\|uninstall\\|consolidate\\|sortMembers\\|hotReload\\|setTargetFramework" method))
              (yes-or-no-p (format "%s may modify project state. Continue? " method)))
      (with-lsp-workspace workspace
        (lsp-request-async method params
                           (lambda (response) (chief/sharplsp-show-result method response))
                           :error-handler
                           (lambda (error)
                             (message "SharpLsp %s failed: %s" method error))
                           :mode 'detached)))))

(defun chief/sharplsp-server-command-execute (command arguments)
  "Execute an advertised server COMMAND with JSON ARGUMENTS.
Includes all profiler operations actually advertised by the running server."
  (interactive
   (let* ((workspace (or (chief/sharplsp-workspace)
                         (user-error "No active SharpLsp workspace")))
          (provider (chief/sharplsp-field (lsp--workspace-server-capabilities workspace)
                                           "executeCommandProvider"))
          (commands (append (chief/sharplsp-field provider "commands") nil)))
     (unless commands (user-error "This server advertises no executable commands"))
     (let ((command (completing-read "SharpLsp server command: " commands nil t)))
       (list command
             (chief/sharplsp-read-json
              "Arguments (JSON array): "
              (if (string-prefix-p "sharplsp.lspProfiler." command)
                  (vector (make-hash-table :test #'equal)) []))))))
  (unless (vectorp arguments) (user-error "Command arguments must be a JSON array"))
  (when (yes-or-no-p (format "Execute %s (may attach/profile a process or write files)? " command))
    (let ((workspace (or (chief/sharplsp-workspace)
                         (user-error "No active SharpLsp workspace"))))
      (with-lsp-workspace workspace
        (lsp-request-async
         "workspace/executeCommand" (list :command command :arguments arguments)
         (lambda (response) (chief/sharplsp-show-result command response))
         :error-handler (lambda (error) (message "SharpLsp command failed: %s" error))
         :mode 'detached)))))

(defun chief/sharplsp-status ()
  "Show actual backend, provenance, selected solution and server capabilities."
  (interactive)
  (let ((workspace (chief/sharplsp-workspace))
        (file buffer-file-name) (explicit (chief/sharplsp-project-root))
        (origin chief/sharplsp-origin-root) (metadata (chief/sharplsp-owned-metadata-p))
        (solution (gethash (chief/sharplsp-root) chief/sharplsp-solutions)))
    (with-help-window "*SharpLsp status*"
      (princ (format "File: %s\nExplicit opt-in: %s\nOrigin: %s\nBackend: %s\nSolution: %s\n"
                     file explicit origin
                     (if metadata "Roslyn (read-only generated library view)"
                       (if workspace "SharpLsp" "Existing default .NET support"))
                     solution))
      (when workspace
        (princ "\nAdvertised capabilities (not a promise of upstream feature parity):\n")
        (princ (pp-to-string (lsp--workspace-server-capabilities workspace)))))))

(defun chief/sharplsp-incoming-calls ()
  "Show incoming calls in the native, terminal-compatible hierarchy tree."
  (interactive)
  (require 'lsp-treemacs)
  (lsp-treemacs-call-hierarchy nil))

(defun chief/sharplsp-outgoing-calls ()
  "Show outgoing calls in the native, terminal-compatible hierarchy tree."
  (interactive)
  (require 'lsp-treemacs)
  (lsp-treemacs-call-hierarchy t))

(defun chief/sharplsp-menu ()
  "Open the SharpLsp action menu without a GUI-only dependency."
  (interactive)
  (let* ((fsharp (derived-mode-p 'fsharp-mode 'fsharp-ts-mode))
         (actions
          `(("Status / backend" . chief/sharplsp-status)
            ("Load solution in both sidecars" . chief/sharplsp-load-solution)
            ("Definition" . chief/goto-definition) ("Type definition" . lsp-find-type-definition)
            ("References" . chief/goto-references) ("Implementation" . chief/goto-implementation)
            ("Hover" . chief/show-hover-doc) ("Signature help" . chief/show-signature-help)
            ("Rename" . lsp-rename) ("Code actions" . lsp-execute-code-action)
            ("Document symbols" . consult-imenu) ("Workspace symbols" . consult-lsp-symbols)
            ("Incoming calls" . chief/sharplsp-incoming-calls)
            ("Outgoing calls" . chief/sharplsp-outgoing-calls)
            ("Select target framework" . chief/sharplsp-framework)
            ("Sort enclosing C# type" . chief/sharplsp-sort-members)
            ("NuGet installed packages" . chief/sharplsp-nuget-packages)
            ("NuGet search/install" . chief/sharplsp-nuget-search)
            ("NuGet unused packages" . chief/sharplsp-nuget-unused)
            ("NuGet consolidate packages" . chief/sharplsp-nuget-consolidate)
            ("Profiler processes / start capture" . chief/sharplsp-profiler-processes)
            ("Profiler stop CPU capture / report" . chief/sharplsp-profiler-cpu-stop)
            ("Hot reload: start paused C# baseline" . chief/sharplsp-hot-reload-start)
            ("Hot reload: apply saved C# source" . chief/sharplsp-hot-reload-apply)
            ("Hot reload: end baseline" . chief/sharplsp-hot-reload-end)
            ("Toggle hints" . lsp-inlay-hints-mode) ("Toggle code lenses" . lsp-lens-mode)
            ("Format buffer (dedicated formatter)" . chief/sharplsp-format-buffer)
            ("Start REPL" . chief/repl-start)
            ("Install managed C# REPL host" . chief/sharplsp-install-repl)
            ("Toggle F# declaration signatures" . chief/sharplsp-signatures-mode)
            ("Build project" . ,(if fsharp 'chief/fsharp-build-project 'chief/csharp-build-project))
            ("Run project" . ,(if fsharp 'chief/fsharp-run-project 'chief/csharp-run-project))
            ("Test at point" . ,(if fsharp 'chief/fsharp-test-at-point 'chief/csharp-test-at-point))
            ("Test project" . ,(if fsharp 'chief/fsharp-test-project 'chief/csharp-test-project))
            ("Debug project" . ,(if fsharp 'chief/fsharp-debug-project 'chief/csharp-debug-project))
            ("Debug test" . ,(if fsharp 'chief/fsharp-debug-test-at-point 'chief/csharp-debug-test-at-point))
            ("Extension request workbench (JSON)" . chief/sharplsp-request)
            ("Advertised server/profiler commands (JSON)" . chief/sharplsp-server-command-execute)))
         (choice (completing-read "SharpLsp: " actions nil t)))
    (call-interactively (cdr (assoc choice actions)))))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'chief/sharplsp-server-command
                                        (lambda () (and (chief/sharplsp-server-command) t)))
  :activation-fn (lambda (_file _mode) (and (chief/sharplsp-preferred-root) t))
  :multi-root nil :priority 10 :server-id 'chief-sharplsp
  :initialized-fn #'chief/sharplsp-initialized
  :notification-handlers (lsp-ht ("lspProfiler/cpu/progress" #'chief/sharplsp-profiler-progress)
                                  ("sharplsp/nuget/restoreProgress" #'chief/sharplsp-nuget-progress))
  :action-handlers (lsp-ht ("editor.action.showReferences" #'chief/sharplsp-show-references))))

(chief/local-leader-def
 :keymaps 'chief/sharplsp-mode-map
 "lf" #'chief/format-buffer
 "L" '(:ignore t :which-key "SharpLsp")
 "Ll" #'chief/sharplsp-menu "LI" #'chief/sharplsp-status
 "Lp" #'chief/sharplsp-load-solution "Lx" #'chief/sharplsp-request
 "LX" #'chief/sharplsp-server-command-execute "Lf" #'chief/sharplsp-format-buffer
 "LH" #'lsp-inlay-hints-mode "LS" #'chief/sharplsp-signatures-mode
 "LC" #'lsp-lens-mode "Ls" #'chief/show-signature-help
 "Ln" #'chief/sharplsp-nuget-packages "LN" #'chief/sharplsp-nuget-search
 "LP" #'chief/sharplsp-profiler-processes "Lt" #'chief/sharplsp-framework
 "Lm" #'chief/sharplsp-sort-members)

(require 'lang-sharplsp-fsharp)
(require 'lang-sharplsp-tools)
(require 'lang-sharplsp-debug)
(require 'lang-sharplsp-razor)

(provide 'lang-sharplsp)
;;; lang-sharplsp.el ends here
