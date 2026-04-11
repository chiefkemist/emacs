;;; cljd_dart_nav.el --- CLJD -> Dart navigation helper -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'jsonrpc)
(require 'seq)
(require 'subr-x)
(require 'url-parse)

(setq dired-use-ls-dired nil)

(defvar cljd-dart-nav--root-sessions (make-hash-table :test #'equal)
  "Live Dart analysis sessions keyed by analysis root.")

(defvar cljd-dart-nav--shutdown-requested nil
  "Non-nil when the persistent helper should terminate.")

(defvar cljd-dart-nav--server-timeout 15.0
  "Default request timeout for server-mode requests.")

(defvar cljd-dart-nav--server-dart-executable nil
  "Default Dart executable for server-mode requests.")

(defun cljd-dart-nav--file-uri (path)
  "Return a file URI for PATH."
  (concat "file://" (expand-file-name path)))

(defun cljd-dart-nav--parse-args ()
  "Parse command-line arguments into a plist."
  (let ((args command-line-args-left)
        roots uri symbol member timeout dart-executable file server socket-path)
    (while args
      (pcase (pop args)
        ("--server" (setq server t))
        ("--socket" (setq socket-path (pop args)))
        ("--root" (push (pop args) roots))
        ("--uri" (setq uri (pop args)))
        ("--file" (setq file (pop args)))
        ("--symbol" (setq symbol (pop args)))
        ("--member" (setq member (pop args)))
        ("--dart-executable" (setq dart-executable (pop args)))
        ("--timeout" (setq timeout (string-to-number (pop args))))
        (_ nil)))
    (list :server server
          :socket socket-path
          :roots (nreverse roots)
          :uri uri
          :file file
          :symbol symbol
          :member member
          :dart-executable dart-executable
          :timeout (or timeout 15.0))))

(defun cljd-dart-nav--json-encode (object)
  "Encode OBJECT to JSON."
  (json-serialize object :false-object :json-false :null-object nil))

(defun cljd-dart-nav--emit-line (&optional object)
  "Emit OBJECT as one JSON line, or a blank line when OBJECT is nil."
  (when object
    (princ (cljd-dart-nav--json-encode object)))
  (princ "\n"))

(defun cljd-dart-nav--probe-file (root)
  "Return the synthetic Dart probe file under ROOT."
  (let ((lib-dir (expand-file-name "lib" root)))
    (expand-file-name "__cljd_dart_nav_probe__.dart"
                      (if (file-directory-p lib-dir) lib-dir root))))

(defun cljd-dart-nav--process-environment ()
  "Return an isolated process environment for the helper's Dart subprocess."
  (let* ((base (expand-file-name "cljd-dart-nav" temporary-file-directory))
         (home (expand-file-name "home" base))
         (config (expand-file-name "xdg-config" base))
         (cache (expand-file-name "xdg-cache" base))
         (state (expand-file-name "xdg-state" base)))
    (dolist (dir (list home config cache state))
      (make-directory dir t))
    (cons (format "HOME=%s" home)
          (cons (format "XDG_CONFIG_HOME=%s" config)
                (cons (format "XDG_CACHE_HOME=%s" cache)
                      (cons (format "XDG_STATE_HOME=%s" state)
                            process-environment))))))

(defun cljd-dart-nav--make-probe (uri body token)
  "Build a synthetic probe for URI using BODY and TOKEN."
  (let* ((text (format "import '%s' as cljdnav;\nvoid main() { %s }\n" uri body))
         (probe-line (or (nth 1 (split-string text "\n")) ""))
         (character (or (string-match-p (regexp-quote token) probe-line) 0)))
    (list :text text :line 1 :character character)))

(defun cljd-dart-nav--probe-sources (uri symbol member)
  "Return ordered synthetic Dart probes for URI, SYMBOL, and MEMBER."
  (let* ((member-tail (car (last (split-string (or member "") "\\." t))))
         (target (if member
                     (format "cljdnav.%s.%s" symbol member)
                   (format "cljdnav.%s" symbol)))
         (probes nil))
    (when member
      (setq probes
            (append probes
                    (list (cljd-dart-nav--make-probe
                           uri (format "%s();" target) (or member-tail member))
                          (cljd-dart-nav--make-probe
                           uri (format "var probe = %s;" target) (or member-tail member))
                          (cljd-dart-nav--make-probe
                           uri (format "%s;" target) (or member-tail member))))))
    (setq probes
          (append probes
                  (list (cljd-dart-nav--make-probe
                         uri (format "var probe = %s;" target) symbol)
                        (cljd-dart-nav--make-probe
                         uri (format "%s;" target) symbol)
                        (cljd-dart-nav--make-probe
                         uri (format "%s();" target) symbol)
                        (cljd-dart-nav--make-probe
                         uri (format "%s? typedProbe;" target) symbol))))
    probes))

(defun cljd-dart-nav--file-uri-path (uri)
  "Convert file URI URI to a decoded local path."
  (let* ((parsed (url-generic-parse-url uri))
         (host (url-host parsed))
         (filename (url-filename parsed))
         (path (url-unhex-string (or filename ""))))
    (if (and host (not (string-empty-p host)))
        (concat "/" host path)
      path)))

(defun cljd-dart-nav--location-payload (result symbol member)
  "Turn RESULT into a JSON-serializable payload for SYMBOL and MEMBER."
  (when result
    (let* ((first (cond
                   ((vectorp result) (and (> (length result) 0) (aref result 0)))
                   ((listp result) (car result))
                   (t result)))
           (uri (or (plist-get first :targetUri)
                    (plist-get first :uri)))
           (range (or (plist-get first :targetSelectionRange)
                      (plist-get first :targetRange)
                      (plist-get first :range)))
           (start (plist-get range :start)))
      (when (and uri start (string-prefix-p "file://" uri))
        `((ok . t)
          (file . ,(cljd-dart-nav--file-uri-path uri))
          (line . ,(or (plist-get start :line) 0))
          (character . ,(or (plist-get start :character) 0))
          (symbol . ,symbol)
          (member . ,member))))))

(defun cljd-dart-nav--file-payload (file range symbol member)
  "Turn FILE and RANGE into a JSON payload for SYMBOL and MEMBER."
  (when-let* ((start (plist-get range :start)))
    `((ok . t)
      (file . ,file)
      (line . ,(or (plist-get start :line) 0))
      (character . ,(or (plist-get start :character) 0))
      (symbol . ,symbol)
      (member . ,member))))

(defun cljd-dart-nav--symbol-list (symbols)
  "Normalize SYMBOLS into a list."
  (cond
   ((vectorp symbols) (append symbols nil))
   ((listp symbols) symbols)
   (t nil)))

(defun cljd-dart-nav--symbol-name-match-p (node name)
  "Return non-nil when NODE's name matches NAME."
  (let ((node-name (plist-get node :name)))
    (and (stringp node-name)
         (or (equal node-name name)
             (equal (car (last (split-string node-name "\\." t))) name)))))

(defun cljd-dart-nav--find-document-symbol (symbols parts)
  "Find nested document symbol in SYMBOLS matching PARTS."
  (when-let* ((nodes (cljd-dart-nav--symbol-list symbols))
              (part (car parts))
              (node (seq-find (lambda (item)
                                (cljd-dart-nav--symbol-name-match-p item part))
                              nodes)))
    (if-let* ((rest (cdr parts)))
        (or (cljd-dart-nav--find-document-symbol (plist-get node :children) rest)
            (cljd-dart-nav--find-document-symbol
             (seq-mapcat (lambda (item)
                           (cljd-dart-nav--symbol-list (plist-get item :children)))
                         nodes)
             parts))
      node)))

(defun cljd-dart-nav--document-symbol-payload (file symbols symbol member)
  "Build payload from FILE and document SYMBOLS for SYMBOL and MEMBER."
  (let* ((parts (append (list symbol)
                        (split-string (or member "") "\\." t)))
         (node (cljd-dart-nav--find-document-symbol symbols parts))
         (range (or (plist-get node :selectionRange)
                    (plist-get node :range)
                    (when-let* ((location (plist-get node :location)))
                      (plist-get location :range)))))
    (cljd-dart-nav--file-payload file range symbol member)))

(defun cljd-dart-nav--related-source-dir (file)
  "Return the related `lib/src/<name>' directory for FILE, if present."
  (let* ((base (file-name-base file))
         (lib-dir (file-name-directory file))
         (src-dir (and lib-dir
                       (expand-file-name (concat "src/" base) lib-dir))))
    (when (and src-dir (file-directory-p src-dir))
      src-dir)))

(defun cljd-dart-nav--document-symbol-candidate-files (file)
  "Return candidate files to inspect via document symbols for FILE."
  (let* ((dir (file-name-directory file))
         (related-dir (cljd-dart-nav--related-source-dir file))
         (same-dir-files (and dir
                              (directory-files dir t "\\.dart\\'" t)))
         (related-files (and related-dir
                             (directory-files-recursively related-dir "\\.dart\\'"))))
    (cl-remove-duplicates
     (delq nil (append (list file) same-dir-files related-files))
     :test #'equal)))

(defun cljd-dart-nav--read-file-string (file)
  "Return FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun cljd-dart-nav--file-signature (file)
  "Return a cheap cache signature for FILE, or nil when unreadable."
  (when (file-readable-p file)
    (let ((attrs (file-attributes file 'integer)))
      (list (file-attribute-size attrs)
            (file-attribute-modification-time attrs)))))

(defun cljd-dart-nav--make-connection (root dart-executable)
  "Create a JSON-RPC connection rooted at ROOT using DART-EXECUTABLE."
  (jsonrpc-process-connection
   :name "cljd-dart-nav"
   :events-buffer-config '(:size 0 :format nil)
   :notification-dispatcher #'ignore
   :request-dispatcher #'ignore
   :process
   (lambda ()
     (let ((process-environment (cljd-dart-nav--process-environment))
           (default-directory (file-name-as-directory (expand-file-name root))))
       (make-process
        :name "cljd-dart-nav"
        :command (list dart-executable
                       "language-server"
                       "--protocol=lsp"
                       "--client-id=chief.cljd-nav"
                       "--client-version=0.1"
                       "--suppress-analytics")
        :connection-type 'pipe
        :noquery t)))))

(defun cljd-dart-nav--initialize (connection root timeout)
  "Initialize CONNECTION for ROOT within TIMEOUT."
  (let ((root-uri (cljd-dart-nav--file-uri root)))
    (jsonrpc-request
     connection
     "initialize"
     `(:processId nil
       :rootUri ,root-uri
       :capabilities ,(make-hash-table :test 'equal)
       :workspaceFolders [(:uri ,root-uri
                             :name ,(file-name-nondirectory
                                     (directory-file-name (expand-file-name root))))]
       :clientInfo (:name "chief" :version "0.1")
       :initializationOptions (:onlyAnalyzeProjectsWithOpenFiles t))
     :timeout timeout)
    (jsonrpc-notify connection "initialized" (make-hash-table :test 'equal))))

(defun cljd-dart-nav--root-session-live-p (session)
  "Return non-nil when SESSION still owns a live JSON-RPC process."
  (when-let* ((connection (plist-get session :connection)))
    (ignore-errors
      (and (jsonrpc-running-p connection)
           (process-live-p (jsonrpc--process connection))))))

(defun cljd-dart-nav--shutdown-root-session (root)
  "Shut down and forget the cached analysis session for ROOT."
  (when-let* ((session (gethash root cljd-dart-nav--root-sessions))
              (connection (plist-get session :connection)))
    (ignore-errors
      (jsonrpc-request connection "shutdown" nil :timeout 5.0))
    (ignore-errors
      (jsonrpc-notify connection "exit" nil))
    (ignore-errors
      (when (jsonrpc-running-p connection)
        (delete-process (jsonrpc--process connection))))
    (ignore-errors
      (jsonrpc-shutdown connection t))
    (remhash root cljd-dart-nav--root-sessions)))

(defun cljd-dart-nav--shutdown-all-root-sessions ()
  "Shut down every cached analysis session."
  (maphash (lambda (root _session)
             (cljd-dart-nav--shutdown-root-session root))
           cljd-dart-nav--root-sessions))

(defun cljd-dart-nav--ensure-root-session (root dart-executable timeout)
  "Return a live cached analysis session for ROOT."
  (let ((session (gethash root cljd-dart-nav--root-sessions)))
    (unless (cljd-dart-nav--root-session-live-p session)
      (when session
        (cljd-dart-nav--shutdown-root-session root))
      (let ((connection (cljd-dart-nav--make-connection root dart-executable)))
        (cljd-dart-nav--initialize connection root timeout)
        (setq session
              (list :connection connection
                    :probe-uri (cljd-dart-nav--file-uri (cljd-dart-nav--probe-file root))
                    :documents (make-hash-table :test 'equal)
                    :document-symbol-cache (make-hash-table :test 'equal)))
        (puthash root session cljd-dart-nav--root-sessions)))
    session))

(defun cljd-dart-nav--sync-document (session uri text)
  "Ensure SESSION has URI open with TEXT, updating only when needed."
  (let* ((documents (plist-get session :documents))
         (state (gethash uri documents))
         (version (if state (plist-get state :version) 0))
         (connection (plist-get session :connection)))
    (cond
     ((null state)
      (setq version 1)
      (jsonrpc-notify
       connection
       "textDocument/didOpen"
       `(:textDocument (:uri ,uri
                        :languageId "dart"
                        :version ,version
                        :text ,text))))
     ((equal text (plist-get state :text))
      (setq version (plist-get state :version)))
     (t
      (setq version (1+ version))
      (jsonrpc-notify
       connection
       "textDocument/didChange"
       `(:textDocument (:uri ,uri :version ,version)
         :contentChanges [(:text ,text)]))))
    (puthash uri `(:text ,text :version ,version) documents)
    version))

(defun cljd-dart-nav--document-symbols-for-file (session file timeout)
  "Return cached or freshly requested document symbols for FILE."
  (let* ((cache (plist-get session :document-symbol-cache))
         (signature (cljd-dart-nav--file-signature file))
         (cached (gethash file cache)))
    (if (and cached (equal signature (plist-get cached :signature)))
        (plist-get cached :symbols)
      (let* ((uri (cljd-dart-nav--file-uri file))
             (text (cljd-dart-nav--read-file-string file))
             (connection (plist-get session :connection))
             (symbols (progn
                        (cljd-dart-nav--sync-document session uri text)
                        (jsonrpc-request
                         connection
                         "textDocument/documentSymbol"
                         `(:textDocument (:uri ,uri))
                         :timeout timeout))))
        (puthash file `(:signature ,signature :symbols ,symbols) cache)
        symbols))))

(defun cljd-dart-nav--document-symbol-request (session file timeout symbol member)
  "Resolve SYMBOL and MEMBER from FILE using cached document symbols."
  (let (payload)
    (dolist (candidate (cljd-dart-nav--document-symbol-candidate-files file))
      (unless payload
        (setq payload
              (cljd-dart-nav--document-symbol-payload
               candidate
               (cljd-dart-nav--document-symbols-for-file session candidate timeout)
               symbol
               member))))
    payload))

(defun cljd-dart-nav--query-root-with-session (session uri file symbol member timeout)
  "Resolve URI, SYMBOL, and MEMBER using persistent SESSION."
  (let ((probe-uri (plist-get session :probe-uri))
        payload)
    (dolist (probe (cljd-dart-nav--probe-sources uri symbol member))
      (unless payload
        (let ((text (plist-get probe :text))
              (line (plist-get probe :line))
              (character (plist-get probe :character)))
          (cljd-dart-nav--sync-document session probe-uri text)
          (setq payload
                (cljd-dart-nav--location-payload
                 (jsonrpc-request
                  (plist-get session :connection)
                  "textDocument/definition"
                  `(:textDocument (:uri ,probe-uri)
                    :position (:line ,line :character ,character))
                  :timeout timeout)
                 symbol
                 member)))))
    (or payload
        (when (and file (file-readable-p file))
          (cljd-dart-nav--document-symbol-request session file timeout symbol member)))))

(defun cljd-dart-nav--query-root (root uri file symbol member timeout dart-executable)
  "Query analysis ROOT for URI, SYMBOL, and MEMBER within TIMEOUT."
  (let ((inhibit-message t)
        (message-log-max nil))
    (condition-case err
        (cljd-dart-nav--query-root-with-session
         (cljd-dart-nav--ensure-root-session root dart-executable timeout)
         uri file symbol member timeout)
      (error
       (cljd-dart-nav--shutdown-root-session root)
       (signal (car err) (cdr err))))))

(defun cljd-dart-nav--resolve-request (roots uri file symbol member timeout dart-executable)
  "Resolve one request across ROOTS in order."
  (let (errors payload)
    (dolist (root roots)
      (when (and (not payload) (file-directory-p root))
        (condition-case err
            (setq payload
                  (cljd-dart-nav--query-root
                   root uri file symbol member timeout dart-executable))
          (error
           (push (format "%s: %s" root (error-message-string err)) errors)))))
    (or payload
        `((ok . :json-false)
          (symbol . ,symbol)
          (member . ,member)
          (error . ,(if errors
                        (string-join (nreverse errors) "; ")
                      "no definition result"))))))

(defun cljd-dart-nav--prewarm-roots (roots timeout dart-executable)
  "Eagerly initialize live analysis sessions for ROOTS."
  (let (prewarmed)
    (dolist (root roots)
      (when (file-directory-p root)
        (cljd-dart-nav--ensure-root-session root dart-executable timeout)
        (push root prewarmed)))
    (nreverse prewarmed)))

(defun cljd-dart-nav--parse-request (line)
  "Parse one JSON request LINE into an alist."
  (json-parse-string
   line
   :object-type 'alist
   :array-type 'list
   :null-object nil
   :false-object nil))

(defun cljd-dart-nav--server-send (client payload)
  "Send PAYLOAD as one JSON line to CLIENT."
  (when (process-live-p client)
    (process-send-string client (concat (cljd-dart-nav--json-encode payload) "\n"))))

(defun cljd-dart-nav--server-handle-request (client line)
  "Handle one JSON request LINE from CLIENT."
  (unless (string-empty-p (string-trim line))
    (condition-case err
        (let* ((request (cljd-dart-nav--parse-request line))
               (id (alist-get 'id request))
               (op (alist-get 'op request))
               (roots (alist-get 'roots request))
               (timeout (or (alist-get 'timeout request)
                            cljd-dart-nav--server-timeout))
               (dart-executable (or (alist-get 'dart_executable request)
                                    (alist-get 'dart-executable request)
                                    cljd-dart-nav--server-dart-executable)))
          (pcase op
            ("prewarm"
             (cljd-dart-nav--server-send
              client
              `((id . ,id)
                (ok . t)
                (prewarmed . ,(vconcat (cljd-dart-nav--prewarm-roots
                                        roots timeout dart-executable))))))
            ("resolve"
             (cljd-dart-nav--server-send
              client
              (append
               `((id . ,id))
               (cljd-dart-nav--resolve-request
                roots
                (alist-get 'uri request)
                (alist-get 'file request)
                (alist-get 'symbol request)
                (alist-get 'member request)
                timeout
                dart-executable))))
            ("shutdown"
             (setq cljd-dart-nav--shutdown-requested t)
             (cljd-dart-nav--server-send client `((id . ,id) (ok . t))))
            (_
             (cljd-dart-nav--server-send
              client
              `((id . ,id)
                (ok . :json-false)
                (error . ,(format "unknown op: %s" op)))))))
      (error
       (cljd-dart-nav--server-send
        client
        `((ok . :json-false)
          (error . ,(error-message-string err))))))))

(defun cljd-dart-nav--server-filter (client chunk)
  "Accumulate CHUNK from CLIENT and dispatch complete request lines."
  (let ((buffer (concat (or (process-get client :buffer) "") chunk))
        line)
    (while (string-match "\n" buffer)
      (setq line (substring buffer 0 (match-beginning 0))
            buffer (substring buffer (match-end 0)))
      (cljd-dart-nav--server-handle-request client line))
    (process-put client :buffer buffer)))

(defun cljd-dart-nav--server-sentinel (_client _event)
  "Ignore server client sentinel noise."
  nil)

(defun cljd-dart-nav--run-server (args)
  "Run the persistent navigation helper server with parsed ARGS."
  (let* ((cljd-dart-nav--server-timeout (plist-get args :timeout))
         (cljd-dart-nav--server-dart-executable
          (or (plist-get args :dart-executable) "dart"))
         (socket-path (or (plist-get args :socket)
                          (error "Missing --socket for persistent helper mode")))
         (socket-dir (file-name-directory socket-path))
         (server nil))
    (make-directory socket-dir t)
    (ignore-errors (delete-file socket-path))
    (setq server
          (make-network-process
           :name "cljd-dart-nav-server"
           :server t
           :family 'local
           :service socket-path
           :coding 'utf-8-unix
           :noquery t
           :filter #'cljd-dart-nav--server-filter
           :sentinel #'cljd-dart-nav--server-sentinel))
    (unwind-protect
        (while (and (process-live-p server)
                    (not cljd-dart-nav--shutdown-requested))
          (accept-process-output nil 1.0))
      (ignore-errors (delete-process server))
      (ignore-errors (delete-file socket-path))
      (cljd-dart-nav--shutdown-all-root-sessions))))

(defun cljd-dart-nav-main ()
  "Entry point for the CLJD -> Dart navigation helper."
  (let* ((args (cljd-dart-nav--parse-args))
         (roots (plist-get args :roots))
         (uri (plist-get args :uri))
         (file (plist-get args :file))
         (symbol (plist-get args :symbol))
         (member (plist-get args :member))
         (dart-executable (or (plist-get args :dart-executable) "dart"))
         (timeout (plist-get args :timeout)))
    (if (plist-get args :server)
        (cljd-dart-nav--run-server args)
      (unwind-protect
          (cljd-dart-nav--emit-line
           (cljd-dart-nav--resolve-request
            roots uri file symbol member timeout dart-executable))
        (cljd-dart-nav--shutdown-all-root-sessions)))))

(cljd-dart-nav-main)
