;;; lang-sharplsp-debug.el --- SharpLsp Edit-and-Continue bridge -*- lexical-binding: t; -*-

(defvar chief/sharplsp-hot-sessions (make-hash-table :test #'eq))
(defvar chief/sharplsp-debug-contexts (make-hash-table :test #'eq))
(defvar chief/sharplsp-debug-root nil)
(defcustom chief/sharplsp-hot-reload-on-save t
  "Apply saved C# edits in opted-in debug sessions, with a compiler/runtime transaction."
  :type 'boolean :group 'chief-sharplsp)

(defun chief/sharplsp-debug-context-put (session key value)
  (puthash session (plist-put (gethash session chief/sharplsp-debug-contexts) key value)
           chief/sharplsp-debug-contexts))

(defun chief/sharplsp-debugger ()
  "Return SharpLsp's source-pinned adapter, without changing the default one."
  (let ((file (expand-file-name
               (if (eq system-type 'windows-nt) "netcoredbg/netcoredbg.exe" "netcoredbg/netcoredbg")
               chief/sharplsp-install-directory)))
    (when (file-executable-p file) file)))

(defun chief/sharplsp-debugger-a (function)
  (or (and (bound-and-true-p chief/sharplsp-mode) (chief/sharplsp-debugger))
      (funcall function)))
(advice-add 'chief/dotnet-netcoredbg-executable :around #'chief/sharplsp-debugger-a)

(defun chief/sharplsp-debug-workspace (root)
  "Find ROOT's existing workspace even from an asynchronous test process buffer."
  (seq-some (lambda (buffer)
              (with-current-buffer buffer
                (and (equal chief/sharplsp-origin-root root) (chief/sharplsp-workspace))))
            (buffer-list)))

(defun chief/sharplsp-debug-launch-a (function args)
  "Route launches AND attachments only when their project explicitly uses SharpLsp."
  (let* ((root (and (equal (plist-get args :type) "coreclr")
                    (or chief/sharplsp-debug-root
                        (if (plist-get args :program)
                            (chief/sharplsp-project-root (plist-get args :program))
                          (or (chief/sharplsp-project-root)
                              (and (chief/sharplsp-workspace) chief/sharplsp-origin-root))))))
         (workspace (and root (chief/sharplsp-debug-workspace root)))
         (launch (equal (plist-get args :request) "launch"))
         (project (and root launch (plist-get args :program)
                       (chief/dotnet-project-file "csproj"
                                                  (file-name-directory (plist-get args :program)) root)))
         initialize auto-continue)
    (when root
      (setq args (copy-sequence args))
      (when launch
        (unless (plist-member args :hotReload)
          (setq args (plist-put args :hotReload t)))
        (let* ((original (plist-get args :env))
               (environment (if original
                                (json-parse-string (json-encode original) :object-type 'hash-table)
                              (make-hash-table :test #'equal))))
          (when (eq (gethash "DOTNET_MODIFIABLE_ASSEMBLIES" environment 'unspecified) 'unspecified)
            (puthash "DOTNET_MODIFIABLE_ASSEMBLIES"
                     (or (getenv "DOTNET_MODIFIABLE_ASSEMBLIES") "debug") environment))
          (setq args (plist-put args :env environment)
                initialize (and chief/sharplsp-hot-reload-on-save (eq (plist-get args :hotReload) t) project
                                (string-suffix-p ".csproj" project)
                                (equal (gethash "DOTNET_MODIFIABLE_ASSEMBLIES" environment) "debug")))
          (when initialize
            ;; Initialize before exposing the first stop or permitting an edit.
            ;; Preserve an explicitly requested entry stop; otherwise resume it.
            (setq auto-continue (not (eq (plist-get args :stopAtEntry) t))
                  args (plist-put args :stopAtEntry t)))))
      (when-let* ((adapter (and (not (plist-get args :dap-server-path)) (chief/sharplsp-debugger))))
        (setq args (plist-put args :dap-server-path (list adapter "--interpreter=vscode")))))
    (prog1 (let ((chief/sharplsp-mode (and root t))) (funcall function args))
      (when-let* ((session (and root (dap--cur-session))))
        (puthash session (list :root root :workspace workspace :project project
                               :initialize initialize :auto-continue auto-continue :generation 0)
                 chief/sharplsp-debug-contexts)))))

(defun chief/sharplsp-debug-test-attach-a (function process project names)
  "Carry explicit test-project ownership through the asynchronous VSTest filter."
  (let ((chief/sharplsp-debug-root (chief/sharplsp-project-root project)))
    (funcall function process project names)))
(advice-add 'chief/dotnet-debug-test-attach :around #'chief/sharplsp-debug-test-attach-a)

(defun chief/sharplsp-dap-request (session command arguments)
  "Send a real DAP request and require a successful response."
  (let (response (deadline (+ (float-time) 30)))
    (dap--send-message (dap--make-request command arguments)
                       (lambda (value) (setq response value)) session)
    (while (and (not response) (< (float-time) deadline)
                (process-live-p (dap--debug-session-proc session)))
      (accept-process-output (dap--debug-session-proc session) 0.1))
    (unless response (error "DAP %s timed out" command))
    (unless (eq (chief/sharplsp-field response "success") t)
      (error "DAP %s: %s" command (chief/sharplsp-field response "message")))
    (chief/sharplsp-field response "body")))

(cl-defun chief/sharplsp-hot-reload-start (&optional session workspace project)
  "Start a compiler baseline for the current paused C# debug session."
  (interactive)
  (when (gethash (or session (dap--cur-session)) chief/sharplsp-hot-sessions)
    (message "SharpLsp hot reload baseline is already active")
    (cl-return-from chief/sharplsp-hot-reload-start t))
  (unless (or (derived-mode-p 'csharp-mode 'csharp-ts-mode) (chief/sharplsp-razor-p))
    (user-error "SharpLsp Edit-and-Continue uses the C# compiler"))
  (let* ((session (or session (dap--cur-session) (user-error "No active DAP session")))
         (workspace (or workspace (chief/sharplsp-workspace) (user-error "No SharpLsp workspace")))
         (thread (or (dap--debug-session-thread-id session) (user-error "Pause the process first")))
         (stack (chief/sharplsp-dap-request session "stackTrace"
                                           (list :threadId thread :startFrame 0 :levels 1)))
         (frame (elt (chief/sharplsp-field stack "stackFrames") 0))
         (capabilities
          (condition-case nil
              (let* ((value (chief/sharplsp-dap-request
                             session "evaluate"
                             (list :expression "System.Reflection.Metadata.MetadataUpdater.GetCapabilities()"
                                   :frameId (chief/sharplsp-field frame "id") :context "repl")))
                     (text (chief/sharplsp-field value "result")))
                (vconcat (split-string (string-trim text "\"" "\"") "[ \t\n]+" t)))
            (error ["Baseline"])))
         (result (chief/sharplsp-rpc
                  "sharplsp/hotReload"
                  (list :action "start" :projectPath (or project (chief/dotnet-project-file "csproj"))
                        :capabilities capabilities) workspace)))
    (unless (equal (chief/sharplsp-field result "status") "started")
      (user-error "Compiler baseline failed: %S" result))
    (puthash session (list :workspace workspace :project (or project (chief/dotnet-project-file "csproj"))
                           :baseline result) chief/sharplsp-hot-sessions)
    (message "SharpLsp hot reload baseline ready")))

(defun chief/sharplsp-hot-reload-apply (&optional session)
  "Apply this saved C# buffer to the paused process and commit only on success."
  (interactive)
  (when (buffer-modified-p) (user-error "Save the source before applying hot reload"))
  (let* ((session (or session (dap--cur-session) (user-error "No active DAP session")))
         (state (or (gethash session chief/sharplsp-hot-sessions)
                    (user-error "Start a SharpLsp hot reload baseline first")))
         (workspace (plist-get state :workspace))
         (baseline (plist-get state :baseline))
         (id (chief/sharplsp-field baseline "sessionId"))
         (params (list :sessionId id))
         temporary issued)
    (when (plist-get state :broken) (user-error "The runtime update was interrupted; restart debugging"))
    (unless (and (or (derived-mode-p 'csharp-mode 'csharp-ts-mode) (chief/sharplsp-razor-p))
                 (eq workspace (chief/sharplsp-workspace)))
      (user-error "This source is not attached to the hot reload workspace"))
    (when (plist-get state :applying) (user-error "A runtime update is already in progress"))
    (let ((timer (plist-get (gethash session chief/sharplsp-debug-contexts) :save-timer)))
      (when (timerp timer) (cancel-timer timer)))
    (puthash session (plist-put state :applying t) chief/sharplsp-hot-sessions)
    (setq temporary (make-temp-file "emacs-sharplsp-deltas-" t))
    (unwind-protect
        (let ((response (chief/sharplsp-rpc
                         "sharplsp/hotReload"
                         (append params (list :action "update"
                                              :documents (chief/sharplsp-hot-reload-documents session))) workspace)))
          (unless (equal (chief/sharplsp-field response "status") "applied")
            (user-error "Hot reload: %s: %S" (chief/sharplsp-field response "status")
                        (chief/sharplsp-field response "diagnostics")))
          (when (> (length (chief/sharplsp-field response "diagnostics")) 0)
            (if (zerop (length (chief/sharplsp-field response "updates")))
                (user-error "No runtime update: %S" (chief/sharplsp-field response "diagnostics"))
              (display-warning 'chief-sharplsp (format "%S" (chief/sharplsp-field response "diagnostics")))))
          (condition-case failure
              (progn
                (dolist (update (append (chief/sharplsp-field response "updates") nil))
                  (let ((arguments (list :dllFileName (concat (chief/sharplsp-field response "assemblyName") ".dll"))))
                    (dolist (entry '(("metadataDelta" . :metadataDeltaFile)
                                     ("ilDelta" . :ilDeltaFile) ("pdbDelta" . :pdbDeltaFile)))
                      (let ((file (expand-file-name (car entry) temporary))
                            (coding-system-for-write 'no-conversion))
                        (write-region (base64-decode-string (chief/sharplsp-field update (car entry)))
                                      nil file nil 'silent)
                        (setq arguments (append arguments (list (cdr entry) file)))))
                    (let ((file (expand-file-name "lines" temporary))
                          (coding-system-for-write 'no-conversion))
                      (write-region (make-string 4 0) nil file nil 'silent)
                      (setq arguments (append arguments (list :lineUpdatesFile file))))
                    (setq issued t)
                    (chief/sharplsp-dap-request session "applyDeltas" arguments)))
                (when issued
                  (chief/sharplsp-rpc "sharplsp/hotReload" (append params '(:action "commit")) workspace))
                (message (if issued "SharpLsp hot reload committed" "SharpLsp: no runtime changes to apply")))
            ((error quit)
             (when issued (puthash session (plist-put state :broken t) chief/sharplsp-hot-sessions))
             (ignore-errors (chief/sharplsp-rpc "sharplsp/hotReload"
                                               (append params '(:action "discard")) workspace))
             (signal (car failure) (cdr failure)))))
      (when-let* ((latest (gethash session chief/sharplsp-hot-sessions)))
        (puthash session (plist-put latest :applying nil) chief/sharplsp-hot-sessions))
      (delete-directory temporary t))))

(defun chief/sharplsp-hot-reload-end (&optional session)
  "Release compiler state when the corresponding debug session ends."
  (interactive)
  (let* ((session (or session (dap--cur-session)))
         (state (gethash session chief/sharplsp-hot-sessions))
         (context (gethash session chief/sharplsp-debug-contexts)))
    (when-let* ((timer (plist-get context :save-timer)))
      (when (timerp timer) (cancel-timer timer)))
    (if (and session (process-live-p (dap--debug-session-proc session))
             (not (memq (dap--debug-session-state session) '(terminated failed))))
        (progn
          (chief/sharplsp-debug-context-put session :save-timer nil)
          (chief/sharplsp-debug-context-put session :pending-save nil))
      (remhash session chief/sharplsp-debug-contexts))
    (when state
      (remhash session chief/sharplsp-hot-sessions)
      (ignore-errors
        (chief/sharplsp-rpc "sharplsp/hotReload"
                           (list :action "end" :sessionId
                                 (chief/sharplsp-field (plist-get state :baseline) "sessionId"))
                           (plist-get state :workspace))))))

(defun chief/sharplsp-hot-reload-member-p (file state)
  "Whether FILE belongs to the debug project's real reference graph."
  (let ((project (plist-get state :project)))
    (or (null project)
        (let* ((projects (chief/dotnet-project-closure project))
               (owner (chief/dotnet-project-file nil (file-name-directory file))))
          (or (and owner (seq-some (lambda (candidate) (file-equal-p owner candidate)) projects))
              (seq-some (lambda (candidate)
                          (seq-some (lambda (source) (file-equal-p source file))
                                    (chief/dotnet-project-explicit-source-files candidate))) projects))))))

(defun chief/sharplsp-hot-reload-documents (session)
  "Submit saved related documents atomically, never another project's unsaved text."
  (let* ((state (gethash session chief/sharplsp-hot-sessions))
         (workspace (plist-get state :workspace)) documents)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and buffer-file-name (not (buffer-modified-p))
                   (or (derived-mode-p 'csharp-mode 'csharp-ts-mode) (chief/sharplsp-razor-p))
                   (eq workspace (chief/sharplsp-workspace))
                   (chief/sharplsp-hot-reload-member-p buffer-file-name state))
          (push (list :filePath buffer-file-name :newText (buffer-string)) documents))))
    (vconcat documents)))

(defun chief/sharplsp-hot-reload-after-save ()
  "Debounce an automatic transactional update for this saved project's active process."
  (when (and chief/sharplsp-hot-reload-on-save chief/sharplsp-mode)
    (let ((source (current-buffer)) (workspace (chief/sharplsp-workspace)))
      (maphash
       (lambda (session state)
         (when (and (eq workspace (plist-get state :workspace))
                    (not (plist-get state :broken))
                    (process-live-p (dap--debug-session-proc session))
                    (chief/sharplsp-hot-reload-member-p buffer-file-name state))
           (let ((timer (plist-get (gethash session chief/sharplsp-debug-contexts) :save-timer)))
             (when (timerp timer) (cancel-timer timer)))
           (chief/sharplsp-debug-context-put
            session :save-timer
            (run-with-timer 0.2 nil #'chief/sharplsp-hot-reload-save-update session source))))
       chief/sharplsp-hot-sessions))))

(defun chief/sharplsp-hot-reload-save-update (session source)
  "Pause only a running process; a breakpoint stop remains a breakpoint stop."
  (when (and (gethash session chief/sharplsp-hot-sessions)
             (gethash session chief/sharplsp-debug-contexts)
             (buffer-live-p source) (process-live-p (dap--debug-session-proc session)))
    (condition-case failure
        (if (plist-get (gethash session chief/sharplsp-hot-sessions) :applying)
            (run-with-timer 0.2 nil #'chief/sharplsp-hot-reload-save-update session source)
          (if (dap--debug-session-thread-id session)
            (with-current-buffer source (chief/sharplsp-hot-reload-apply session))
          (let* ((threads (chief/sharplsp-dap-request session "threads" nil))
                 (thread (elt (chief/sharplsp-field threads "threads") 0)))
            (chief/sharplsp-debug-context-put session :pending-save source)
            (dap--send-message
             (dap--make-request "pause" (list :threadId (chief/sharplsp-field thread "id")))
             (lambda (response)
               (unless (eq (chief/sharplsp-field response "success") t)
                 (chief/sharplsp-debug-context-put session :pending-save nil)
                 (display-warning 'chief-sharplsp "Could not pause the process for hot reload"))) session))))
      (error (display-warning 'chief-sharplsp (error-message-string failure))))))

(defun chief/sharplsp-debug-visible-stop (function session event)
  "Deliver an unmodified, visible debugger stop to dap-mode and its normal hooks."
  (chief/sharplsp-debug-context-put session :step nil)
  (chief/sharplsp-debug-context-put session :origin nil)
  (setf (dap--debug-session-thread-id session) nil)
  (funcall function session event))

(defun chief/sharplsp-debug-frame (session event)
  (let* ((thread (chief/sharplsp-field (chief/sharplsp-field event "body") "threadId"))
         (stack (chief/sharplsp-dap-request session "stackTrace"
                                           (list :threadId thread :startFrame 0 :levels 1))))
    (elt (chief/sharplsp-field stack "stackFrames") 0)))

(defun chief/sharplsp-debug-initialize-stop (function session event)
  "Build the baseline before the first entry stop is exposed or resumed."
  (condition-case failure
      (let* ((context (gethash session chief/sharplsp-debug-contexts))
             (frame (chief/sharplsp-debug-frame session event))
             (file (dap--get-path-for-frame session frame))
             (thread (chief/sharplsp-field (chief/sharplsp-field event "body") "threadId")))
        (unless (and file (file-readable-p file)) (error "No C# source at the initial stop"))
        (with-current-buffer (find-file-noselect file)
          (unless (bound-and-true-p lsp-managed-mode) (lsp))
          (let ((deadline (+ (float-time) 12)))
            (while (and (not (chief/sharplsp-workspace)) (< (float-time) deadline))
              (accept-process-output nil 0.1)))
          (setf (dap--debug-session-thread-id session) thread)
          (puthash thread "entry" (dap--debug-session-thread-states session))
          (chief/sharplsp-hot-reload-start session (chief/sharplsp-workspace) (plist-get context :project))
          (chief/sharplsp-debug-context-put session :workspace (chief/sharplsp-workspace)))
        (if (and (plist-get context :auto-continue)
                 (equal (chief/sharplsp-field (chief/sharplsp-field event "body") "reason") "entry"))
            (dap-continue session thread)
          (chief/sharplsp-debug-visible-stop function session event)))
    ((error quit)
     (display-warning 'chief-sharplsp (format "Automatic hot reload baseline: %s" (error-message-string failure)))
     (chief/sharplsp-debug-visible-stop function session event))))

(defun chief/sharplsp-debug-save-stop (function session event source)
  "Apply at our private pause, then restore a previously running process."
  (let* ((thread (chief/sharplsp-field (chief/sharplsp-field event "body") "threadId"))
         (private-pause (equal (chief/sharplsp-field (chief/sharplsp-field event "body") "reason") "pause")))
    (chief/sharplsp-debug-context-put session :pending-save nil)
    (setf (dap--debug-session-thread-id session) thread)
    (puthash thread "pause" (dap--debug-session-thread-states session))
    (condition-case failure
        (with-current-buffer source (chief/sharplsp-hot-reload-apply session))
      (error (display-warning 'chief-sharplsp (error-message-string failure))))
    (if (and private-pause
             (not (plist-get (gethash session chief/sharplsp-hot-sessions) :broken)))
        (dap-continue session thread)
      (chief/sharplsp-debug-visible-stop function session event))))

(defun chief/sharplsp-debug-location (session &optional event)
  "Read the top source position AND full depth; unknown depths never coalesce."
  (let* ((thread (if event (chief/sharplsp-field (chief/sharplsp-field event "body") "threadId")
                   (dap--debug-session-thread-id session)))
         (stack (chief/sharplsp-dap-request session "stackTrace"
                                           (list :threadId thread :startFrame 0 :levels 1)))
         (frame (elt (chief/sharplsp-field stack "stackFrames") 0)))
    (list :file (dap--get-path-for-frame session frame)
          :line (chief/sharplsp-field frame "line") :column (chief/sharplsp-field frame "column")
          :depth (or (chief/sharplsp-field stack "totalFrames") 0) :thread thread)))

(defun chief/sharplsp-debug-statement (location workspace)
  "Return false only when the server proves this position is structural."
  (let ((file (plist-get location :file)))
    (if (and workspace file (not (file-remote-p file)) (file-readable-p file))
        (let* ((response (chief/sharplsp-rpc
                          "sharplsp/statementStop"
                          (list :uri (lsp--path-to-uri file)
                                :line (1- (plist-get location :line))
                                :character (max 0 (1- (or (plist-get location :column) 1)))) workspace))
               ;; lsp-mode decodes JSON false as nil. Require the boolean key
               ;; to exist: an absent/unknown result must never hide a stop.
               (value (if (hash-table-p response) (gethash "statement" response 'unknown)
                        (if (plist-member response :statement) (plist-get response :statement) 'unknown))))
          (cond ((memq value '(nil :json-false)) :json-false)
                ((eq value t) t)
                (t 'unknown)))
      'unknown)))

(defun chief/sharplsp-debug-step-a (function command session)
  "Remember the actual gesture and its statement, only for an opted-in session."
  (when-let* ((context (gethash session chief/sharplsp-debug-contexts)))
    (let ((origin (condition-case nil (chief/sharplsp-debug-location session) (error nil))))
      (chief/sharplsp-debug-context-put session :origin origin)
      (chief/sharplsp-debug-context-put
       session :origin-carries
       (condition-case nil (chief/sharplsp-debug-statement origin (plist-get context :workspace)) (error 'unknown))))
    (chief/sharplsp-debug-context-put session :step command)
    (chief/sharplsp-debug-context-put session :skipped 0))
  (funcall function command session))

(defun chief/sharplsp-debug-classify-stop (function session event generation)
  "Hide only syntax-proven structural STEP stops; errors and real stops stay visible."
  (let* ((context (gethash session chief/sharplsp-debug-contexts))
         (workspace (plist-get context :workspace))
         (command (plist-get context :step)))
    (when (= generation (or (plist-get context :generation) -1))
      (condition-case nil
          (let* ((location (chief/sharplsp-debug-location session event))
                 (origin (plist-get context :origin))
                 (carries (chief/sharplsp-debug-statement location workspace))
                 (origin-carries (plist-get context :origin-carries))
                 (depth (plist-get location :depth)) (origin-depth (or (plist-get origin :depth) 0))
                 (same (and (memq origin-carries '(t :json-false))
                            (> depth 0) (= depth origin-depth)
                            (plist-get origin :file)
                            (equal (plist-get origin :file) (plist-get location :file))
                            (equal (plist-get origin :line) (plist-get location :line))
                            (equal (plist-get origin :thread) (plist-get location :thread))))
                 (escaped (and (> origin-depth 0) (> depth 0) (< depth origin-depth)
                               (not (eq origin-carries t))))
                 (first-statement (and (eq origin-carries :json-false) (eq carries t)
                                       (not (equal (plist-get origin :column) (plist-get location :column))))))
            ;; The RPC can receive newer events while waiting. Never act on an old stop.
            (when (= generation (or (plist-get (gethash session chief/sharplsp-debug-contexts) :generation) -1))
              (if (and command (memq carries '(t :json-false)) (not escaped)
                       (< (or (plist-get context :skipped) 0) 8)
                       (or (eq carries :json-false) (and same (not first-statement))))
                  (progn
                    (chief/sharplsp-debug-context-put session :skipped (1+ (or (plist-get context :skipped) 0)))
                    (dap--send-message
                     (dap--make-request command (list :threadId (plist-get location :thread)))
                     (lambda (response)
                       (unless (eq (chief/sharplsp-field response "success") t)
                         (chief/sharplsp-debug-visible-stop function session event))) session))
                (chief/sharplsp-debug-visible-stop function session event))))
        (error (chief/sharplsp-debug-visible-stop function session event))))))

(defun chief/sharplsp-debug-deferred (callback function session event &rest args)
  "Run debugger work outside a process filter and discard terminated sessions."
  (when (and (gethash session chief/sharplsp-debug-contexts)
             (process-live-p (dap--debug-session-proc session))
             (not (memq (dap--debug-session-state session) '(terminated failed))))
    (apply callback function session event args)))

(defun chief/sharplsp-debug-event-a (function session event)
  "Integrate automatic baselines, save updates and source-aware stepping with DAP."
  (let ((context (gethash session chief/sharplsp-debug-contexts)))
    (if (not (and context (equal (chief/sharplsp-field event "event") "stopped")))
        (funcall function session event)
      (let* ((generation (1+ (or (plist-get context :generation) 0)))
             (reason (chief/sharplsp-field (chief/sharplsp-field event "body") "reason"))
             (pending (plist-get context :pending-save)))
        (chief/sharplsp-debug-context-put session :generation generation)
        (cond
         ((plist-get context :initialize)
          (chief/sharplsp-debug-context-put session :initialize nil)
          (run-with-timer 0 nil #'chief/sharplsp-debug-deferred
                          #'chief/sharplsp-debug-initialize-stop function session event))
         ((buffer-live-p pending)
          (chief/sharplsp-debug-context-put session :pending-save nil)
          (run-with-timer 0 nil #'chief/sharplsp-debug-deferred
                          #'chief/sharplsp-debug-save-stop function session event pending))
         ((and (equal reason "step") (plist-get context :step))
          (run-with-timer 0 nil #'chief/sharplsp-debug-deferred
                          #'chief/sharplsp-debug-classify-stop function session event generation))
         (t (chief/sharplsp-debug-visible-stop function session event)))))))

(with-eval-after-load 'dap-mode
  (advice-add 'dap-debug :around #'chief/sharplsp-debug-launch-a)
  (advice-add 'dap--on-event :around #'chief/sharplsp-debug-event-a)
  (advice-add 'dap--step :around #'chief/sharplsp-debug-step-a)
  (add-hook 'dap-terminated-hook #'chief/sharplsp-hot-reload-end))

(provide 'lang-sharplsp-debug)
;;; lang-sharplsp-debug.el ends here
