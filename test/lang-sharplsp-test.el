;;; lang-sharplsp-test.el --- SharpLsp isolation and presentation tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'lang-sharplsp)

(defmacro chief/sharplsp-test-project (&rest body)
  "Evaluate BODY with a temporary ROOT and no running language server."
  (declare (indent 0))
  `(let ((root (make-temp-file "chief-sharplsp-" t))
         (chief/sharplsp-navigation-owners (make-hash-table :test #'equal))
         (chief/sharplsp-solutions (make-hash-table :test #'equal)))
     (unwind-protect (progn ,@body) (delete-directory root t))))

(defun chief/sharplsp-test-write (file &optional text)
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert (or text "")))
  file)

(ert-deftest chief/sharplsp-requires-explicit-project-opt-in ()
  (chief/sharplsp-test-project
    (let ((file (chief/sharplsp-test-write (expand-file-name "app/Source.fs" root))))
      (chief/sharplsp-test-write (expand-file-name "app/App.fsproj" root))
      (make-directory (expand-file-name "app/.sharplsp" root))
      (should-not (chief/sharplsp-project-root file))
      (chief/sharplsp-test-write (expand-file-name "sharplsp.toml" root))
      (should (equal (chief/sharplsp-project-root file)
                     (file-name-as-directory (file-truename root)))))))

(ert-deftest chief/sharplsp-respects-nearest-config-and-git-worktree-boundaries ()
  (chief/sharplsp-test-project
    (let ((file (chief/sharplsp-test-write (expand-file-name "child/src/Source.cs" root))))
      (chief/sharplsp-test-write (expand-file-name "sharplsp.toml" root))
      (chief/sharplsp-test-write (expand-file-name "child/.git" root) "gitdir: elsewhere")
      (should-not (chief/sharplsp-project-root file))
      (chief/sharplsp-test-write (expand-file-name "child/sharplsp.toml" root))
      (should (equal (chief/sharplsp-project-root file)
                     (file-name-as-directory (file-truename (expand-file-name "child" root))))))))

(ert-deftest chief/sharplsp-does-not-use-editor-working-directory ()
  (chief/sharplsp-test-project
    (let ((default-directory (file-name-as-directory root)))
      (chief/sharplsp-test-write (expand-file-name "sharplsp.toml" root))
      (with-temp-buffer
        (should-not (chief/sharplsp-project-root))
        (setq buffer-file-name "/unrelated-repository/Source.fs")
        (should-not (chief/sharplsp-project-root))))))

(ert-deftest chief/sharplsp-navigation-context-does-not-opt-in-unrelated-siblings ()
  (chief/sharplsp-test-project
    (let* ((owner (file-name-as-directory (expand-file-name "owner" root)))
           (target (chief/sharplsp-test-write (expand-file-name "library/Source.fs" root)))
           (other (chief/sharplsp-test-write (expand-file-name "library/Other.fs" root))))
      (chief/sharplsp-test-write (expand-file-name "sharplsp.toml" owner))
      (puthash (chief/sharplsp-canonical target) owner chief/sharplsp-navigation-owners)
      (cl-letf (((symbol-function 'chief/sharplsp-server-command) (lambda () '("server"))))
        (with-temp-buffer
          (setq major-mode 'fsharp-mode buffer-file-name target)
          (should (equal (chief/sharplsp-preferred-root) owner))
          (setq chief/sharplsp-origin-root owner buffer-file-name other)
          (should-not (chief/sharplsp-preferred-root))
          (chief/sharplsp-test-write (expand-file-name "library/sharplsp.toml" root))
          (setq buffer-file-name target)
          (should (equal (chief/sharplsp-preferred-root)
                         (file-name-as-directory (file-truename (expand-file-name "library" root))))))))))

(ert-deftest chief/sharplsp-never-launches-project-local-executables ()
  (chief/sharplsp-test-project
    (let ((program (chief/sharplsp-test-write (expand-file-name "sharplsp" root)))
          (chief/sharplsp-command nil)
          (chief/sharplsp-install-directory (expand-file-name "trusted" root)))
      (cl-letf (((symbol-function 'executable-find) (lambda (_) nil))
                ((symbol-function 'file-executable-p) (lambda (path) (equal path program))))
        (should-not (chief/sharplsp-server-command))))))

(ert-deftest chief/sharplsp-invalid-command-override-is-not-silently-replaced ()
  (let ((chief/sharplsp-command '("/missing/sharplsp")))
    (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
      (should-not (chief/sharplsp-server-command)))))

(ert-deftest chief/sharplsp-metadata-recognition-is-scoped-to-system-temp ()
  (should (chief/sharplsp-metadata-file-p
           (expand-file-name "sharplsp-decompiled/key/System.Guid.cs" temporary-file-directory)))
  (should (chief/sharplsp-metadata-file-p
           (expand-file-name "MetadataAsSource/key/Type.cs" temporary-file-directory)))
  (should-not (chief/sharplsp-metadata-file-p "/a-project/sharplsp-decompiled/Source.cs"))
  (should-not (chief/sharplsp-metadata-file-p
               (expand-file-name "sharplsp-decompiled/Source.fs" temporary-file-directory))))

(ert-deftest chief/sharplsp-opt-in-is-buffer-local-and-excludes-default-clients ()
  (chief/sharplsp-test-project
    (let ((file (chief/sharplsp-test-write (expand-file-name "App.cs" root)))
          (original-server chief/dotnet-csharp-server)
          observed)
      (chief/sharplsp-test-write (expand-file-name "sharplsp.toml" root))
      (cl-letf (((symbol-function 'chief/sharplsp-server-command) (lambda () '("server"))))
        (with-temp-buffer
          (setq major-mode 'csharp-mode buffer-file-name file)
          (chief/sharplsp-language-setup-a
           (lambda () (setq observed chief/dotnet-csharp-server)))
          (should (eq observed 'sharplsp))
          (should (equal lsp-enabled-clients '(chief-sharplsp)))
          (should chief/sharplsp-mode)
          (should (eq chief/format-buffer-function #'chief/sharplsp-format-buffer))
          (should (memq 'fsac lsp-disabled-clients))))
      (should (eq chief/dotnet-csharp-server original-server))
      (with-temp-buffer
        (should-not chief/sharplsp-mode)))))

(ert-deftest chief/sharplsp-non-opted-in-setup-preserves-the-original ()
  (chief/sharplsp-test-project
    (with-temp-buffer
      (setq major-mode 'fsharp-mode buffer-file-name (expand-file-name "Source.fs" root))
      (chief/sharplsp-language-setup-a (lambda () (setq-local lsp-enabled-clients '(fsac))))
      (should (equal lsp-enabled-clients '(fsac)))
      (should-not chief/sharplsp-mode))))

(ert-deftest chief/sharplsp-generated-library-views-use-read-only-roslyn ()
  (chief/sharplsp-test-project
    (with-temp-buffer
      (setq major-mode 'csharp-mode
            buffer-file-name (expand-file-name "sharplsp-decompiled/test/Guid.cs"
                                             temporary-file-directory))
      (puthash (chief/sharplsp-canonical buffer-file-name)
               root chief/sharplsp-navigation-owners)
      (should-not (chief/sharplsp-preferred-root))
      (chief/sharplsp-language-setup-a
       (lambda ()
         (should (eq chief/dotnet-csharp-server 'roslyn))
         (setq-local lsp-enabled-clients '(chief-roslyn))))
      (should (equal lsp-enabled-clients '(chief-roslyn)))
      (should (equal chief/sharplsp-origin-root root))
      (should buffer-read-only)
      (should-not lsp-use-workspace-root-for-server-default-directory)
      (should (equal default-directory (file-name-as-directory root)))
      (should chief/lsp-diagnostics-disabled)
      (should-not chief/format-on-save-enabled))))

(ert-deftest chief/sharplsp-unowned-metadata-does-not-change-default-backend ()
  (with-temp-buffer
    (let ((chief/dotnet-csharp-server 'csharp-ls)
          (chief/sharplsp-navigation-owners (make-hash-table :test #'equal)))
      (setq major-mode 'csharp-mode
            buffer-file-name (expand-file-name "MetadataAsSource/unowned/Type.cs"
                                             temporary-file-directory))
      (chief/sharplsp-language-setup-a
       (lambda () (should (eq chief/dotnet-csharp-server 'csharp-ls))))
      (should-not chief/sharplsp-mode)
      (should-not buffer-read-only))))

(ert-deftest chief/sharplsp-external-library-connection-retains-a-valid-cwd ()
  (chief/sharplsp-test-project
    (let ((lsp--session (make-lsp-session)))
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "outside/Source.cs" temporary-file-directory))
        (setq-local chief/lsp-root-function (lambda () root))
        (chief/lsp-prime-session-root)
        (should-not lsp-use-workspace-root-for-server-default-directory)
        (should (file-equal-p (lsp--default-directory-for-connection) root))))))

(ert-deftest chief/sharplsp-reference-lenses-get-declaration-positions ()
  (with-temp-buffer
    (let* ((command (json-parse-string "{\"title\":\"2 references\",\"command\":\"editor.action.showReferences\",\"arguments\":[]}"))
           (position (json-parse-string "{\"line\":3,\"character\":4}"))
           (lens (list :command command :range (list :start position))))
      (cl-letf (((symbol-function 'chief/sharplsp-workspace) (lambda () t))
                ((symbol-function 'lsp--buffer-uri) (lambda () "file:///Source.fs")))
        (chief/sharplsp-prepare-lenses (list lens)))
      (should (equal (aref (gethash "arguments" command) 0) "file:///Source.fs"))
      (should (eq (aref (gethash "arguments" command) 1) position)))))

(ert-deftest chief/sharplsp-formatter-replaces-unsaved-text-without-saving-the-file ()
  (with-temp-buffer
    (setq major-mode 'csharp-mode)
    (insert "class C{}")
    (cl-letf (((symbol-function 'chief/dotnet-ensure-tool-paths) #'ignore)
              ((symbol-function 'chief/sharplsp-formatter-command) (lambda () '("formatter")))
              ((symbol-function 'call-process-region)
               (lambda (_begin _end _program _delete destination &rest _)
                 (with-current-buffer (car destination) (insert "class C { }\n")) 0)))
      (chief/sharplsp-format-buffer))
    (should (equal (buffer-string) "class C { }\n"))
    (should (buffer-modified-p))))

(ert-deftest chief/sharplsp-expired-navigation-context-does-not-opt-in ()
  (chief/sharplsp-test-project
    (let ((target (expand-file-name "Source.fs" root)))
      (puthash (chief/sharplsp-canonical target) root chief/sharplsp-navigation-owners)
      (with-temp-buffer
        (setq major-mode 'fsharp-mode buffer-file-name target)
        (should-not (chief/sharplsp-preferred-root))))))

(ert-deftest chief/sharplsp-format-failure-preserves-unsaved-source ()
  (with-temp-buffer
    (setq major-mode 'csharp-mode)
    (insert "class C { }")
    (cl-letf (((symbol-function 'chief/dotnet-ensure-tool-paths) #'ignore)
              ((symbol-function 'chief/sharplsp-formatter-command) (lambda () '("broken-formatter")))
              ((symbol-function 'call-process-region) (lambda (&rest _) 1)))
      (should-error (chief/sharplsp-format-buffer)))
    (should (equal (buffer-string) "class C { }"))))

(defun chief/sharplsp-test-hint (position label kind)
  (let ((hint (make-hash-table :test #'equal)))
    (puthash "position" (chief/sharplsp-point-position position) hint)
    (puthash "label" label hint)
    (puthash "kind" kind hint)
    hint))

(ert-deftest chief/sharplsp-fsharp-hints-do-not-fabricate-curried-parameter-syntax ()
  (with-temp-buffer
    (insert "module Test\nlet add value = value + 1\nlet result = add 2\n")
    (skip-unless (chief/sharplsp-fsharp-tree))
    (chief/sharplsp-signatures-mode 1)
    (goto-char (point-min)) (search-forward "let add")
    (let ((function-position (point)))
      (search-forward " value")
      (let* ((parameter-position (point))
             (hints (list (chief/sharplsp-test-hint function-position ": int -> int" 1)
                          (chief/sharplsp-test-hint parameter-position ": int" 1))))
        (should-not (chief/sharplsp-fsharp-filter-hints hints nil))
        (should (= (length chief/sharplsp-signature-overlays) 1))
        (should (overlay-start (car chief/sharplsp-signature-overlays)))
        (should (string-match-p "val add : int -> int"
                                (overlay-get (car chief/sharplsp-signature-overlays) 'before-string)))
        (should (string-match-p "let add value" (buffer-string)))))))

(ert-deftest chief/sharplsp-fsharp-bogus-operator-argument-labels-are-suppressed ()
  (with-temp-buffer
    (insert "module Test\nlet valid value = value < 1\n")
    (skip-unless (chief/sharplsp-fsharp-tree))
    (goto-char (point-min)) (search-forward "<") (backward-char)
    (should-not
     (chief/sharplsp-fsharp-filter-hints
      (list (chief/sharplsp-test-hint (point) "x:" 2)
            (chief/sharplsp-test-hint (point) "y:" 2)) nil))))

(ert-deftest chief/sharplsp-fsharp-callee-is-not-a-declaration-keyword ()
  (with-temp-buffer
    (insert "module Test\nlet add x y = x + y\nlet answer = add 1 2\n")
    (skip-unless (chief/sharplsp-fsharp-tree))
    (goto-char (point-min)) (search-forward "add 1 2")
    (let ((position (chief/sharplsp-fsharp-callee-position)))
      (should position)
      (goto-char (lsp--position-to-point position))
      (should (equal (thing-at-point 'symbol t) "add")))))

(ert-deftest chief/sharplsp-type-compaction-does-not-rewrite-unrelated-namespaces ()
  (should (equal (chief/sharplsp-fsharp-compact-type
                  ": OtherSystem.Foo * Other.System.Bar * System.Guid -> Microsoft.FSharp.Core.string" nil)
                 ": OtherSystem.Foo * Other.System.Bar * Guid -> string")))

(ert-deftest chief/sharplsp-protocol-workbench-covers-public-custom-host-methods ()
  (dolist (method '("configuration" "loadSolution" "workspaceSymbols" "targetFramework"
                    "setTargetFramework" "sortMembers" "statementStop" "hotReload"
                    "nuget/targets" "nuget/installed" "nuget/search" "nuget/versions"
                    "nuget/install" "nuget/uninstall" "nuget/unused" "nuget/consolidate"))
    (should (member (concat "sharplsp/" method) chief/sharplsp-extension-methods))))

(ert-deftest chief/sharplsp-profiler-commands-send-one-object-not-an-empty-array ()
  (let (recorded)
    (cl-letf (((symbol-function 'chief/sharplsp-rpc)
               (lambda (method params &optional _) (setq recorded (list method params)))))
      (chief/sharplsp-profiler-rpc "cpu.start" '(:pid 42) 'workspace))
    (should (equal (car recorded) "workspace/executeCommand"))
    (let* ((params (cadr recorded)) (arguments (plist-get params :arguments)))
      (should (equal (plist-get params :command) "sharplsp.lspProfiler.cpu.start"))
      (should (= (length arguments) 1))
      (should (= (plist-get (aref arguments 0) :pid) 42)))))

(ert-deftest chief/sharplsp-native-table-return-works-under-evil ()
  (save-window-excursion
    (let (activated)
      (unwind-protect
          (progn
            (chief/sharplsp-tools-table
             "ERT table" [("Value" 20 t)] '((row ["test value"])) 'workspace
             (lambda (row) (setq activated row)))
            (goto-char (point-min))
            (should (eq evil-state 'emacs))
            (execute-kbd-macro (kbd "RET"))
            (should (eq activated 'row)))
        (when-let* ((buffer (get-buffer "*SharpLsp ERT table*"))) (kill-buffer buffer))))))

(ert-deftest chief/sharplsp-native-panel-buttons-work-under-evil ()
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (chief/sharplsp-panel-mode)
      (let ((inhibit-read-only t) activated)
        (insert-text-button "Activate" 'action (lambda (_) (setq activated t)))
        (goto-char (point-min))
        (should (eq evil-state 'emacs))
        (execute-kbd-macro (kbd "RET"))
        (should activated)))))

(ert-deftest chief/sharplsp-debugger-defaults-are-untouched-outside-opt-in ()
  (let* ((args '(:type "coreclr" :request "launch" :program "/unrelated/Project.dll")) observed)
    (chief/sharplsp-debug-launch-a (lambda (value) (setq observed value)) args)
    (should (eq args observed))))

(ert-deftest chief/sharplsp-debug-launch-copies-env-and-preserves-explicit-values ()
  (chief/sharplsp-test-project
    (chief/sharplsp-test-write (expand-file-name "sharplsp.toml" root))
    (let* ((environment (make-hash-table :test #'equal))
           (args (list :type "coreclr" :request "launch" :program (expand-file-name "bin/App.dll" root)
                       :env environment :dap-server-path '("custom-adapter"))) observed)
      (puthash "DOTNET_MODIFIABLE_ASSEMBLIES" "explicit-user-value" environment)
      (chief/sharplsp-debug-launch-a (lambda (value) (setq observed value)) args)
      (should-not (eq observed args))
      (should-not (eq (plist-get observed :env) environment))
      (should (equal (gethash "DOTNET_MODIFIABLE_ASSEMBLIES" (plist-get observed :env))
                     "explicit-user-value"))
      (should (equal (plist-get observed :dap-server-path) '("custom-adapter"))))))

(ert-deftest chief/sharplsp-hot-reload-commits-vector-deltas-only-after-runtime-success ()
  (with-temp-buffer
    (setq major-mode 'csharp-mode buffer-file-name "/private/fixture/Program.cs")
    (let* ((session (make-symbol "session")) (workspace (make-symbol "workspace"))
           (chief/sharplsp-hot-sessions (make-hash-table :test #'eq))
           actions delta-file)
      (puthash session (list :workspace workspace :baseline '(:sessionId "baseline"))
               chief/sharplsp-hot-sessions)
      (cl-letf (((symbol-function 'dap--cur-session) (lambda () session))
                ((symbol-function 'chief/sharplsp-workspace) (lambda () workspace))
                ((symbol-function 'chief/sharplsp-rpc)
                 (lambda (_method params &optional _workspace)
                   (let ((action (plist-get params :action)))
                     (push action actions)
                     (when (equal action "update")
                       '(:status "applied" :assemblyName "Fixture"
                         :updates [(:metadataDelta "AA==" :ilDelta "AA==" :pdbDelta "AA==")])))))
                ((symbol-function 'chief/sharplsp-dap-request)
                 (lambda (_session command args)
                   (should (equal command "applyDeltas"))
                   (setq delta-file (plist-get args :metadataDeltaFile))
                   (dolist (key '(:metadataDeltaFile :ilDeltaFile :pdbDeltaFile :lineUpdatesFile))
                     (should (file-readable-p (plist-get args key)))))))
        (chief/sharplsp-hot-reload-apply))
      (should (equal (reverse actions) '("update" "commit")))
      (should-not (file-exists-p delta-file)))))

(ert-deftest chief/sharplsp-hot-reload-discarded-update-latches-uncertain-runtime ()
  (with-temp-buffer
    (setq major-mode 'csharp-mode buffer-file-name "/private/fixture/Program.cs")
    (let* ((session (make-symbol "session")) (workspace (make-symbol "workspace"))
           (chief/sharplsp-hot-sessions (make-hash-table :test #'eq)) actions)
      (puthash session (list :workspace workspace :baseline '(:sessionId "baseline"))
               chief/sharplsp-hot-sessions)
      (cl-letf (((symbol-function 'dap--cur-session) (lambda () session))
                ((symbol-function 'chief/sharplsp-workspace) (lambda () workspace))
                ((symbol-function 'chief/sharplsp-rpc)
                 (lambda (_method params &optional _workspace)
                   (push (plist-get params :action) actions)
                   (when (equal (plist-get params :action) "update")
                     '(:status "applied" :assemblyName "Fixture"
                       :updates [(:metadataDelta "AA==" :ilDelta "AA==" :pdbDelta "AA==")]))))
                ((symbol-function 'chief/sharplsp-dap-request) (lambda (&rest _) (error "runtime refused"))))
        (should-error (chief/sharplsp-hot-reload-apply)))
      (should (equal (reverse actions) '("update" "discard")))
      (should (plist-get (gethash session chief/sharplsp-hot-sessions) :broken)))))

(ert-deftest chief/sharplsp-curried-eldoc-does-not-display-stale-information ()
  (with-temp-buffer
    (insert "call value")
    (setq chief/sharplsp-fsharp-eldoc-info
          (list (buffer-chars-modified-tick) (point) "val call: int -> int"))
    (let (shown)
      (should (chief/sharplsp-fsharp-eldoc (lambda (text) (setq shown text))))
      (should (equal shown "val call: int -> int"))
      (backward-char)
      (should-not (chief/sharplsp-fsharp-eldoc #'ignore))
      (forward-char) (insert " ")
      (should-not (chief/sharplsp-fsharp-eldoc #'ignore)))))

(ert-deftest chief/sharplsp-imports-retain-the-shared-namespace-resolver ()
  (let (called)
    (cl-letf (((symbol-function 'chief/sharplsp-workspace) (lambda () 'workspace))
              ((symbol-function 'chief/dotnet-import-namespace-at-point) (lambda () "System"))
              ((symbol-function 'chief/lsp-request-locations-async) (lambda (&rest _) (ert-fail "Bypassed import resolver"))))
      (chief/sharplsp-definition-a (lambda (action) (setq called action)) 'window))
    (should (eq called 'window))))

(ert-deftest chief/sharplsp-csharp-semantic-tokens-respect-local-overrides ()
  (dolist (override '(absent nil t))
    (with-temp-buffer
      (setq major-mode 'csharp-mode)
      (unless (eq override 'absent) (setq-local lsp-semantic-tokens-enable override))
      (chief/sharplsp-setup "/fixture/")
      (should (eq lsp-semantic-tokens-enable (not (null override)))))))

(ert-deftest chief/sharplsp-razor-routing-requires-an-explicit-config ()
  (chief/sharplsp-test-project
    (let ((file (chief/sharplsp-test-write (expand-file-name "App.razor" root))) called)
      (with-temp-buffer
        (setq major-mode 'web-mode buffer-file-name file)
        (chief/sharplsp-razor-setup-a (lambda () (setq called t)))
        (should called)
        (should-not chief/sharplsp-mode)
        (chief/sharplsp-test-write (expand-file-name "sharplsp.toml" root))
        (cl-letf (((symbol-function 'chief/sharplsp-server-command) (lambda () '("server")))
                  ((symbol-function 'chief/polyglot-completion-setup) #'ignore))
          (chief/sharplsp-razor-setup-a (lambda () (ert-fail "Used default Razor backend"))))
        (should chief/sharplsp-mode)
        (should (equal lsp-enabled-clients '(chief-sharplsp)))
        (should (equal (lsp-buffer-language) "razor"))
        (should (memq #'chief/sharplsp-razor-html-completion completion-at-point-functions))
        (should (eq chief/format-buffer-function #'chief/sharplsp-razor-format))))))

(ert-deftest chief/sharplsp-razor-format-is-interactive-and-atomic ()
  (should (commandp #'chief/sharplsp-razor-format))
  (with-temp-buffer
    (insert "<h1>@Model.Title</h1>")
    (cl-letf (((symbol-function 'indent-region) (lambda (&rest _) (erase-buffer) (error "indent failed"))))
      (should-error (chief/sharplsp-razor-format)))
    (should (equal (buffer-string) "<h1>@Model.Title</h1>"))))

(ert-deftest chief/sharplsp-razor-projection-is-cleaned-up ()
  (with-temp-buffer
    (setq chief/sharplsp-razor-html-buffer (generate-new-buffer " *ERT projection*"))
    (let ((projection chief/sharplsp-razor-html-buffer))
      (chief/sharplsp-razor-kill-projection)
      (should-not (buffer-live-p projection)))))

(ert-deftest chief/sharplsp-debug-test-attachment-carries-project-ownership ()
  (chief/sharplsp-test-project
    (chief/sharplsp-test-write (expand-file-name "sharplsp.toml" root))
    (let ((project (chief/sharplsp-test-write (expand-file-name "Tests.csproj" root))) observed)
      (with-temp-buffer
        (chief/sharplsp-debug-test-attach-a
         (lambda (&rest _) (setq observed chief/sharplsp-debug-root)) nil project '("Test")))
      (should (equal observed (file-name-as-directory (file-truename root)))))))

(ert-deftest chief/sharplsp-attach-selects-its-fork-without-launch-mutations ()
  (let ((chief/sharplsp-debug-root "/fixture/") observed)
    (cl-letf (((symbol-function 'chief/sharplsp-debugger) (lambda () "/private/adapter")))
      (chief/sharplsp-debug-launch-a (lambda (args) (setq observed args))
                                    '(:type "coreclr" :request "attach" :processId 123)))
    (should (equal (plist-get observed :dap-server-path) '("/private/adapter" "--interpreter=vscode")))
    (should-not (plist-get observed :env))
    (should-not (plist-get observed :stopAtEntry))))

(ert-deftest chief/sharplsp-explicit-unrelated-program-does-not-inherit-current-buffer ()
  (let ((chief/sharplsp-origin-root "/opted/") observed)
    (cl-letf (((symbol-function 'chief/sharplsp-workspace) (lambda () 'workspace)))
      (let ((args '(:type "coreclr" :request "launch" :program "/unrelated/App.dll")))
        (chief/sharplsp-debug-launch-a (lambda (value) (setq observed value)) args)
        (should (eq args observed))))))

(ert-deftest chief/sharplsp-debug-launch-preserves-hot-reload-disable ()
  (let ((chief/sharplsp-debug-root "/fixture/") observed)
    (cl-letf (((symbol-function 'chief/dotnet-project-file) (lambda (&rest _) "/fixture/App.csproj")))
      (chief/sharplsp-debug-launch-a (lambda (args) (setq observed args))
                                    '(:type "coreclr" :request "launch" :program "/fixture/App.dll"
                                      :hotReload :json-false :stopAtEntry :json-false)))
    (should (eq (plist-get observed :hotReload) :json-false))
    (should (eq (plist-get observed :stopAtEntry) :json-false))))

(ert-deftest chief/sharplsp-empty-hot-reload-does-not-send-a-spurious-commit ()
  (with-temp-buffer
    (setq major-mode 'csharp-mode buffer-file-name "/private/fixture/Program.cs")
    (let* ((session 'session) (chief/sharplsp-hot-sessions (make-hash-table :test #'eq)) actions)
      (puthash session '(:workspace workspace :baseline (:sessionId "baseline")) chief/sharplsp-hot-sessions)
      (cl-letf (((symbol-function 'chief/sharplsp-workspace) (lambda () 'workspace))
                ((symbol-function 'chief/sharplsp-rpc)
                 (lambda (_method params &optional _) (push (plist-get params :action) actions)
                   '(:status "applied" :updates []))))
        (chief/sharplsp-hot-reload-apply session))
      (should (equal actions '("update"))))))

(defun chief/sharplsp-test-step-verdict (origin location carries &optional skipped stale)
  "Exercise the real stop classifier without starting a process."
  (let ((chief/sharplsp-debug-contexts (make-hash-table :test #'eq)) actions)
    (puthash 'session (list :workspace 'workspace :step "next" :generation 1 :origin origin
                           :origin-carries (plist-get origin :carries) :skipped (or skipped 0))
             chief/sharplsp-debug-contexts)
    (cl-letf (((symbol-function 'chief/sharplsp-debug-location) (lambda (&rest _) location))
              ((symbol-function 'chief/sharplsp-debug-statement)
               (lambda (&rest _) (when stale (chief/sharplsp-debug-context-put 'session :generation 2)) carries))
              ((symbol-function 'dap--make-request) (lambda (command _) command))
              ((symbol-function 'dap--send-message) (lambda (message &rest _) (push message actions)))
              ((symbol-function 'chief/sharplsp-debug-visible-stop) (lambda (&rest _) (push 'visible actions))))
      (chief/sharplsp-debug-classify-stop #'ignore 'session nil 1))
    actions))

(ert-deftest chief/sharplsp-stepping-skips-only-proven-structure ()
  (should (equal (chief/sharplsp-test-step-verdict nil '(:depth 2) :json-false) '("next")))
  (should (equal (chief/sharplsp-test-step-verdict nil '(:depth 2) t) '(visible)))
  (should (equal (chief/sharplsp-test-step-verdict nil '(:depth 2) nil) '(visible)))
  (should (equal (chief/sharplsp-test-step-verdict nil '(:depth 2) 'unknown) '(visible))))

(ert-deftest chief/sharplsp-stepping-coalesces-a-proven-same-statement ()
  (let ((location '(:file "/source.cs" :line 3 :column 2 :depth 2 :thread 1 :carries t)))
    (should (equal (chief/sharplsp-test-step-verdict location location t) '("next")))))

(ert-deftest chief/sharplsp-stepping-keeps-first-real-statement-on-a-brace-line ()
  (should (equal (chief/sharplsp-test-step-verdict
                  '(:file "/source.cs" :line 3 :column 2 :depth 2 :thread 1 :carries :json-false)
                  '(:file "/source.cs" :line 3 :column 4 :depth 2 :thread 1) t) '(visible))))

(ert-deftest chief/sharplsp-stepping-does-not-run-the-caller-after-frame-escape ()
  (should (equal (chief/sharplsp-test-step-verdict
                  '(:depth 3 :carries :json-false) '(:depth 2) :json-false) '(visible))))

(ert-deftest chief/sharplsp-stepping-is-bounded-and-discards-stale-results ()
  (should (equal (chief/sharplsp-test-step-verdict nil '(:depth 2) :json-false 8) '(visible)))
  (should-not (chief/sharplsp-test-step-verdict nil '(:depth 2) :json-false 0 t)))

(ert-deftest chief/sharplsp-stepping-normalizes-false-but-not-an-absent-key ()
  (dolist (entry '(((:statement nil) . :json-false) ((:statement t) . t) (nil . unknown)))
    (cl-letf (((symbol-function 'file-readable-p) (lambda (_) t))
              ((symbol-function 'chief/sharplsp-rpc) (lambda (&rest _) (car entry))))
      (should (eq (chief/sharplsp-debug-statement '(:file "/source.cs" :line 1) 'workspace) (cdr entry)))))
  (let ((response (make-hash-table :test #'equal)))
    (puthash "statement" nil response)
    (cl-letf (((symbol-function 'file-readable-p) (lambda (_) t))
              ((symbol-function 'chief/sharplsp-rpc) (lambda (&rest _) response)))
      (should (eq (chief/sharplsp-debug-statement '(:file "/source.cs" :line 1) 'workspace) :json-false)))))

(ert-deftest chief/sharplsp-automatic-baseline-forces-only-a-private-entry-stop ()
  (let ((chief/sharplsp-debug-root "/fixture/") (chief/sharplsp-hot-reload-on-save t) observed)
    (cl-letf (((symbol-function 'chief/dotnet-project-file) (lambda (&rest _) "/fixture/App.csproj")))
      (chief/sharplsp-debug-launch-a (lambda (args) (setq observed args))
                                    '(:type "coreclr" :request "launch" :program "/fixture/App.dll")))
    (should (eq (plist-get observed :stopAtEntry) t))
    (should (eq (plist-get observed :hotReload) t))
    (should (equal (gethash "DOTNET_MODIFIABLE_ASSEMBLIES" (plist-get observed :env)) "debug"))
    (let ((process-environment (copy-sequence process-environment)))
      (setenv "DOTNET_MODIFIABLE_ASSEMBLIES" "inherited-override")
      (cl-letf (((symbol-function 'chief/dotnet-project-file) (lambda (&rest _) "/fixture/App.csproj")))
        (chief/sharplsp-debug-launch-a (lambda (args) (setq observed args))
                                      '(:type "coreclr" :request "launch" :program "/fixture/App.dll")))
      (should (equal (gethash "DOTNET_MODIFIABLE_ASSEMBLIES" (plist-get observed :env)) "inherited-override"))
      (should-not (plist-get observed :stopAtEntry)))))

(provide 'lang-sharplsp-test)
;;; lang-sharplsp-test.el ends here
