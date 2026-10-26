;;; lang-sharplsp-tools.el --- Native SharpLsp tools and profiler UI -*- lexical-binding: t; -*-

(require 'tabulated-list)
(require 'treesit)

(defvar-local chief/sharplsp-tools-workspace nil)
(defvar-local chief/sharplsp-tools-row-action nil)
(defvar-local chief/sharplsp-tools-refresh nil)
(defvar-local chief/sharplsp-tools-kind nil)
(defvar-local chief/sharplsp-profiler-cpu-session nil)
(defvar-local chief/sharplsp-profiler-memory-session nil)

(defun chief/sharplsp-rpc (method params &optional workspace)
  "Call METHOD on the chosen SharpLsp WORKSPACE, never a default .NET server."
  (let ((workspace (or workspace chief/sharplsp-tools-workspace
                       (chief/sharplsp-workspace)
                       (user-error "No active SharpLsp workspace"))))
    (with-lsp-workspace workspace (lsp-request method params))))

(defun chief/sharplsp-profiler-rpc (method params &optional workspace)
  "Call the profiler's typed METHOD with one parameters object."
  (chief/sharplsp-rpc
   "workspace/executeCommand"
   (list :command (concat "sharplsp.lspProfiler." method) :arguments (vector params)) workspace))

(defun chief/sharplsp-tools-open-row ()
  (interactive)
  (let ((row (tabulated-list-get-id)))
    (unless row (user-error "No row at point"))
    (funcall (or chief/sharplsp-tools-row-action
                 (lambda (value) (chief/sharplsp-show-result "details" value))) row)))

(defun chief/sharplsp-tools-reload ()
  (interactive)
  (if chief/sharplsp-tools-refresh (funcall chief/sharplsp-tools-refresh)
    (user-error "This is a completed report; start another capture to refresh it")))

(define-derived-mode chief/sharplsp-tools-mode tabulated-list-mode "SharpLsp"
  "Terminal-compatible SharpLsp tool results. RET opens a row; g refreshes."
  (setq truncate-lines t)
  (define-key chief/sharplsp-tools-mode-map (kbd "RET") #'chief/sharplsp-tools-open-row)
  (define-key chief/sharplsp-tools-mode-map (kbd "g") #'chief/sharplsp-tools-reload)
  (define-key chief/sharplsp-tools-mode-map (kbd "j") #'next-line)
  (define-key chief/sharplsp-tools-mode-map (kbd "k") #'previous-line))

(define-derived-mode chief/sharplsp-panel-mode special-mode "SharpLsp Panel"
  "Keyboard-accessible SharpLsp controls, including in an Evil terminal session."
  (define-key chief/sharplsp-panel-mode-map (kbd "TAB") #'forward-button)
  (define-key chief/sharplsp-panel-mode-map (kbd "<backtab>") #'backward-button)
  (define-key chief/sharplsp-panel-mode-map (kbd "RET") #'push-button)
  (define-key chief/sharplsp-panel-mode-map (kbd "j") #'next-line)
  (define-key chief/sharplsp-panel-mode-map (kbd "k") #'previous-line))

(when (fboundp 'evil-set-initial-state)
  (evil-set-initial-state 'chief/sharplsp-tools-mode 'emacs)
  (evil-set-initial-state 'chief/sharplsp-panel-mode 'emacs))

(defun chief/sharplsp-tools-table (title columns rows workspace &optional action refresh kind)
  "Render structured ROWS as a native table, retaining their WORKSPACE."
  (let ((buffer (get-buffer-create (format "*SharpLsp %s*" title))))
    (with-current-buffer buffer
      (chief/sharplsp-tools-mode)
      (setq-local chief/sharplsp-tools-workspace workspace
                  chief/sharplsp-tools-row-action action
                  chief/sharplsp-tools-refresh refresh
                  chief/sharplsp-tools-kind kind
                  tabulated-list-format columns
                  tabulated-list-entries rows)
      (tabulated-list-init-header)
      (tabulated-list-print t))
    (pop-to-buffer buffer)
    buffer))

(defun chief/sharplsp-framework ()
  "Select a target framework using the server's real project context."
  (interactive)
  (let* ((params (list :textDocument (lsp--text-document-identifier)))
         (context (chief/sharplsp-rpc "sharplsp/targetFramework" params))
         (choices (append (chief/sharplsp-field context "available") nil)))
    (unless choices (user-error "This project has a single target framework"))
    (let ((selected (completing-read "Active target framework: " choices nil t nil nil
                                     (chief/sharplsp-field context "active"))))
      (chief/sharplsp-rpc "sharplsp/setTargetFramework"
                         (append params (list :targetFramework selected)))
      (message "SharpLsp framework: %s" selected))))

(defun chief/sharplsp-sort-members ()
  "Sort the enclosing C# type using server edits and native undo support."
  (interactive)
  (barf-if-buffer-read-only)
  (unless (and (derived-mode-p 'csharp-mode 'csharp-ts-mode)
               (treesit-language-available-p 'c-sharp))
    (user-error "Member sorting requires C# and its tree-sitter grammar"))
  (let* ((parser (treesit-parser-create 'c-sharp))
         (node (treesit-node-descendant-for-range
                (treesit-parser-root-node parser) (point) (point) t)))
    (while (and node (not (member (treesit-node-type node)
                                 '("class_declaration" "struct_declaration" "interface_declaration"
                                   "enum_declaration" "record_declaration"))))
      (setq node (treesit-node-parent node)))
    (unless node (user-error "Point is not inside a C# type"))
    (let* ((params (list :uri (lsp--buffer-uri)
                         :range (lsp--region-to-range (treesit-node-start node) (treesit-node-end node))
                         :sortConfig
                         '(:hierarchy ["accessibility" "category" "alphabetical"]
                           :accessibilityOrder ["public" "protected" "internal" "private"]
                           :categoryOrder ["field" "constructor" "property" "event" "method" "type"])))
           (tick (buffer-chars-modified-tick))
           (response (chief/sharplsp-rpc "sharplsp/sortMembers" params))
           (edits (chief/sharplsp-field response "edits")))
      (unless (= tick (buffer-chars-modified-tick)) (user-error "Buffer changed; sort was not applied"))
      (if edits (atomic-change-group (lsp--apply-text-edits edits "Sort members"))
        (message "Members are already sorted")))))

(defun chief/sharplsp-nuget-progress (workspace params)
  "Show real restore progress and surface restore failures in the editor."
  (let ((phase (chief/sharplsp-field params "phase"))
        (detail (chief/sharplsp-field params "message")))
    (when-let* ((buffer (get-buffer "*SharpLsp NuGet*")))
      (with-current-buffer buffer
        (when (eq workspace chief/sharplsp-tools-workspace)
          (setq header-line-format (format " NuGet restore: %s | %s | g refresh" phase (or detail ""))))))
    (when (equal phase "failed")
      (display-warning 'chief-sharplsp (format "NuGet restore failed: %s" detail)))))

(defun chief/sharplsp-nuget-target (&optional workspace)
  "Choose an actual solution project or central-package-management target."
  (let* ((workspace (or workspace chief/sharplsp-tools-workspace (chief/sharplsp-workspace)
                        (user-error "No active SharpLsp workspace")))
         (response (chief/sharplsp-rpc "sharplsp/nuget/targets"
                                      (list :workspaceRoot (lsp--workspace-root workspace)) workspace))
         (targets (chief/sharplsp-field response "targets"))
         (choices (mapcar (lambda (target)
                            (cons (chief/sharplsp-field target "path") target)) targets)))
    (unless choices (user-error "No NuGet targets in this workspace"))
    (cdr (assoc (completing-read "NuGet target: " choices nil t) choices))))

(defun chief/sharplsp-nuget-show (packages target workspace &optional search)
  "Show PACKAGES with native install/remove actions scoped to TARGET."
  (chief/sharplsp-tools-table
   "NuGet" [("Package" 43 t) ("Version" 20 t) ("Description" 65 t)]
   (mapcar (lambda (package)
             (list package (vector (or (chief/sharplsp-field package "id") "")
                                   (or (chief/sharplsp-field package "resolvedVersion")
                                       (chief/sharplsp-field package "version") "")
                                   (or (chief/sharplsp-field package "description") "")))) packages)
   workspace
   (lambda (package)
     (let* ((id (chief/sharplsp-field package "id"))
            (operation (completing-read (concat id ": ") '("Details" "Install/update" "Remove") nil t)))
       (pcase operation
         ("Details" (chief/sharplsp-show-result id package))
         (_
          (let* ((path (chief/sharplsp-field target "path"))
                 (buffer (find-buffer-visiting path))
                 (remove-p (equal operation "Remove"))
                 (version (unless remove-p
                            (let* ((response (chief/sharplsp-rpc "sharplsp/nuget/versions"
                                                                 (list :packageId id) workspace))
                                   (versions (append (chief/sharplsp-field response "versions") nil)))
                              (completing-read "Package version: " (reverse versions) nil t)))))
            (when (and buffer (buffer-modified-p buffer))
              (user-error "Save the modified project file before changing packages"))
            (when (yes-or-no-p (format "%s %s%s in %s? " operation id
                                      (if version (concat " " version) "") path))
              (let ((response (chief/sharplsp-rpc
                               (if remove-p "sharplsp/nuget/uninstall" "sharplsp/nuget/install")
                               (append (list :target target :packageId id)
                                       (when version (list :version version))) workspace)))
                (when (eq (chief/sharplsp-field response "success") :json-false)
                  (user-error "%s" (chief/sharplsp-field response "message")))
                (dolist (file (append (chief/sharplsp-field response "modifiedFiles") nil))
                  (when-let* ((visited (find-buffer-visiting file)))
                    (unless (buffer-modified-p visited)
                      (with-current-buffer visited (revert-buffer t t)))))
                (message "%s" (chief/sharplsp-field response "message")))))))))
   (lambda () (chief/sharplsp-nuget-packages target workspace search)) 'nuget))

(defun chief/sharplsp-nuget-packages (&optional target workspace search)
  "Browse installed packages, or SEARCH the feed with a prefix argument."
  (interactive (list nil nil (and current-prefix-arg (read-string "NuGet search: "))))
  (let* ((workspace (or workspace chief/sharplsp-tools-workspace (chief/sharplsp-workspace)
                        (user-error "No active SharpLsp workspace")))
         (target (or target (chief/sharplsp-nuget-target workspace)))
         (response (chief/sharplsp-rpc
                    (if search "sharplsp/nuget/search" "sharplsp/nuget/installed")
                    (append (list :target target)
                            (when search (list :query search :prerelease :json-false :take 50 :skip 0)))
                    workspace)))
    (chief/sharplsp-nuget-show (chief/sharplsp-field response "packages") target workspace search)))

(defun chief/sharplsp-nuget-search ()
  (interactive)
  (chief/sharplsp-nuget-packages nil nil (read-string "NuGet search: ")))

(defun chief/sharplsp-nuget-unused ()
  (interactive)
  (let* ((target (chief/sharplsp-nuget-target))
         (response (chief/sharplsp-rpc "sharplsp/nuget/unused"
                                      (list :projectPath (chief/sharplsp-field target "path")))))
    (chief/sharplsp-show-result "unused packages" response)))

(defun chief/sharplsp-nuget-consolidate ()
  "Preview package consolidation; apply only after explicit confirmation."
  (interactive)
  (let* ((workspace (or (chief/sharplsp-workspace) (user-error "No SharpLsp workspace")))
         (solution (or (gethash (file-name-as-directory (lsp--workspace-root workspace))
                                chief/sharplsp-solutions)
                       (user-error "No loaded solution")))
         (params (list :solutionPath solution :dryRun t)))
    (chief/sharplsp-show-result "consolidation preview"
                                (chief/sharplsp-rpc "sharplsp/nuget/consolidate" params workspace))
    (when (yes-or-no-p "Apply the displayed package consolidation? ")
      (let* ((targets (chief/sharplsp-field
                       (chief/sharplsp-rpc "sharplsp/nuget/targets"
                                            (list :workspaceRoot (file-name-directory solution)) workspace)
                       "targets"))
             (files (append (mapcar (lambda (target) (chief/sharplsp-field target "path")) targets)
                            (list (expand-file-name "Directory.Build.props" (file-name-directory solution))
                                  (expand-file-name "Directory.Packages.props" (file-name-directory solution))))))
        (dolist (file files)
          (when-let* ((buffer (find-buffer-visiting file)))
            (when (buffer-modified-p buffer)
              (user-error "Save %s before consolidating packages" file)))))
      (chief/sharplsp-show-result
       "consolidation" (chief/sharplsp-rpc "sharplsp/nuget/consolidate"
                                          (list :solutionPath solution :dryRun :json-false) workspace)))))

(defun chief/sharplsp-profiler-processes (&optional workspace)
  "Browse actual managed processes. RET chooses CPU capture or heap inspection."
  (interactive)
  (let* ((workspace (or workspace chief/sharplsp-tools-workspace (chief/sharplsp-workspace)
                        (user-error "No SharpLsp workspace")))
         (response (chief/sharplsp-profiler-rpc "processes" '(:workspace_roots []) workspace)))
    (chief/sharplsp-tools-table
     "Processes" [("PID" 9 t) ("Runtime" 12 t) ("Entry" 75 t) ("Version" 18 t)]
     (mapcar (lambda (process)
               (list process
                     (vector (format "%s" (chief/sharplsp-field process "pid"))
                             (or (chief/sharplsp-field process "runtime") "")
                             (or (chief/sharplsp-field process "entry") "")
                             (or (chief/sharplsp-field process "runtime_version") ""))))
             (chief/sharplsp-field response "processes"))
     workspace
     (lambda (process)
       (let* ((pid (chief/sharplsp-field process "pid"))
              (operation (completing-read "Profiler: " '("Start CPU capture" "Inspect heap") nil t)))
         (when (yes-or-no-p (format "%s for PID %s? " operation pid))
           (if (equal operation "Inspect heap")
               (chief/sharplsp-profiler-memory-start pid workspace)
             (chief/sharplsp-profiler-cpu-start pid workspace)))))
     (lambda () (chief/sharplsp-profiler-processes workspace)) 'processes)))

(defun chief/sharplsp-profiler-progress (_workspace params)
  "Display real sampling progress without emitting unknown-notification warnings."
  (when-let* ((buffer (get-buffer "*SharpLsp CPU capture*")))
    (with-current-buffer buffer
      (when (equal chief/sharplsp-profiler-cpu-session (chief/sharplsp-field params "session_id"))
        (setq header-line-format
              (format " %s samples | %ss | %s"
                      (chief/sharplsp-field params "sample_count")
                      (chief/sharplsp-field params "duration_secs")
                      (or (chief/sharplsp-field params "top_function") "sampling")))))))

(defun chief/sharplsp-profiler-cpu-start (pid &optional workspace)
  "Start sampling PID; present a stop/report button without a GUI dependency."
  (let* ((workspace (or workspace chief/sharplsp-tools-workspace (chief/sharplsp-workspace)))
         (result (chief/sharplsp-profiler-rpc "cpu.start" (list :pid pid) workspace))
         (session (chief/sharplsp-field result "session_id"))
         (buffer (get-buffer-create "*SharpLsp CPU capture*")))
    (unless session (error "Profiler returned no session: %S" result))
    (with-current-buffer buffer
      (chief/sharplsp-panel-mode)
      (setq-local chief/sharplsp-tools-workspace workspace
                  chief/sharplsp-profiler-cpu-session session)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "CPU capture of PID %s\nSession: %s\n\n" pid session))
        (insert-text-button "Snapshot (keep recording)" 'follow-link t
                            'action (lambda (_) (chief/sharplsp-profiler-cpu-stop session workspace t)))
        (insert "\n\n")
        (insert-text-button "Stop capture and show report" 'follow-link t
                            'action (lambda (_) (chief/sharplsp-profiler-cpu-stop session workspace)))))
    (pop-to-buffer buffer)
    session))

(defun chief/sharplsp-profiler-cpu-stop (&optional session workspace keep-recording)
  "Render sampled hotspots for SESSION; stop unless KEEP-RECORDING is non-nil."
  (interactive)
  (let* ((workspace (or workspace chief/sharplsp-tools-workspace (chief/sharplsp-workspace)))
         (session (or session chief/sharplsp-profiler-cpu-session
                      (completing-read
                       "CPU session: "
                       (mapcar (lambda (s) (chief/sharplsp-field s "session_id"))
                               (chief/sharplsp-field (chief/sharplsp-profiler-rpc "cpu.list" (make-hash-table :test #'equal) workspace)
                                                     "sessions")) nil t)))
         (report (chief/sharplsp-profiler-rpc (if keep-recording "cpu.snapshot" "cpu.stop")
                                             (list :session_id session) workspace)))
    (chief/sharplsp-tools-table
     "CPU report" [("Function" 85 t) ("Samples" 12 t) ("Self %" 12 t)]
     (mapcar (lambda (function)
               (list function (vector (or (chief/sharplsp-field function "name") "")
                                      (format "%s" (or (chief/sharplsp-field function "samples") ""))
                                      (format "%s" (or (chief/sharplsp-field function "self_percentage") "")))))
             (chief/sharplsp-field report "hot_functions")) workspace
     (lambda (function)
       (let ((file (chief/sharplsp-field function "file"))
             (line (chief/sharplsp-field function "line")))
         (if (and (stringp file) (not (file-remote-p file)) (file-readable-p file))
             (progn (find-file file) (goto-char (point-min))
                    (when (and (integerp line) (> line 0)) (forward-line (1- line))))
           (chief/sharplsp-show-result "function details" function)))))
    (with-current-buffer "*SharpLsp CPU report*"
      (setq-local header-line-format
                  (format " %s samples | %s | RET details | q close"
                          (chief/sharplsp-field report "total_samples")
                          (or (chief/sharplsp-field report "duration_secs") ""))))
    (chief/sharplsp-show-result "CPU artifacts and call tree" report)
    report))

(defun chief/sharplsp-profiler-memory-start (pid &optional workspace)
  "Open a real heap session for PID with snapshot/diff/references/GC controls."
  (let* ((workspace (or workspace chief/sharplsp-tools-workspace (chief/sharplsp-workspace)))
         (result (chief/sharplsp-profiler-rpc "memory.start" (list :pid pid) workspace))
         (session (chief/sharplsp-field result "memory_session_id"))
         (buffer (get-buffer-create "*SharpLsp Heap*")))
    (unless session (error "Profiler returned no heap session: %S" result))
    (with-current-buffer buffer
      (chief/sharplsp-panel-mode)
      (setq-local chief/sharplsp-tools-workspace workspace chief/sharplsp-profiler-memory-session session)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Heap inspection of PID %s\nSession: %s\n\n" pid session))
        (dolist (entry '(("Snapshot" . "snapshot") ("Diff against previous snapshot" . "diff")
                         ("Objects by type" . "objectsByType")
                         ("Object references" . "references") ("Force GC" . "gcCollect")
                         ("Close heap session" . "stop")))
          (let ((method (cdr entry)))
            (insert-text-button
             (car entry) 'follow-link t
             'action (lambda (_)
                       (when (or (not (equal method "gcCollect"))
                                 (yes-or-no-p (format "Force garbage collection in PID %s? " pid)))
                         (let ((params (list :memory_session_id session)))
                           (when (member method '("references" "objectsByType"))
                             (setq params (append params (list :type_name (read-string "Type: " "System.String")
                                                               :limit 50 :max_depth 5))))
                           (chief/sharplsp-show-result
                            (concat "heap " method)
                            (chief/sharplsp-profiler-rpc (concat "memory." method) params workspace))))))
            (insert "\n")))))
    (pop-to-buffer buffer)
    session))

(provide 'lang-sharplsp-tools)
;;; lang-sharplsp-tools.el ends here
