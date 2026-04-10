;;; cljd_dart_nav.el --- CLJD -> Dart navigation helper -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'jsonrpc)
(require 'seq)
(require 'subr-x)
(require 'url-parse)

(setq dired-use-ls-dired nil)

(defun cljd-dart-nav--file-uri (path)
  "Return a file URI for PATH."
  (concat "file://" (expand-file-name path)))

(defun cljd-dart-nav--parse-args ()
  "Parse command-line arguments into a plist."
  (let ((args command-line-args-left)
        roots uri symbol member timeout dart-executable file)
    (while args
      (pcase (pop args)
        ("--root" (push (pop args) roots))
        ("--uri" (setq uri (pop args)))
        ("--file" (setq file (pop args)))
        ("--symbol" (setq symbol (pop args)))
        ("--member" (setq member (pop args)))
        ("--dart-executable" (setq dart-executable (pop args)))
        ("--timeout" (setq timeout (string-to-number (pop args))))
        (_ nil)))
    (list :roots (nreverse roots)
          :uri uri
          :file file
          :symbol symbol
          :member member
          :dart-executable dart-executable
          :timeout (or timeout 15.0))))

(defun cljd-dart-nav--json-encode (object)
  "Encode OBJECT to JSON."
  (json-serialize object :false-object :json-false :null-object nil))

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

(defun cljd-dart-nav--document-symbol-request (connection file version timeout symbol member)
  "Resolve SYMBOL and MEMBER from FILE using document symbols."
  (let ((current-version version)
        payload)
    (dolist (candidate (cljd-dart-nav--document-symbol-candidate-files file))
      (unless payload
        (let ((uri (cljd-dart-nav--file-uri candidate))
              (text (with-temp-buffer
                      (insert-file-contents candidate)
                      (buffer-string))))
          (jsonrpc-notify
           connection
           "textDocument/didOpen"
           `(:textDocument (:uri ,uri
                            :languageId "dart"
                            :version ,current-version
                            :text ,text)))
          (setq payload
                (cljd-dart-nav--document-symbol-payload
                 candidate
                 (jsonrpc-request
                  connection
                  "textDocument/documentSymbol"
                  `(:textDocument (:uri ,uri))
                  :timeout timeout)
                 symbol
                 member))
          (cl-incf current-version))))
    payload))

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

(defun cljd-dart-nav--query-root (root uri file symbol member timeout dart-executable)
  "Query analysis ROOT for URI, SYMBOL, and MEMBER within TIMEOUT."
  (let ((connection (cljd-dart-nav--make-connection root dart-executable))
        (inhibit-message t)
        (message-log-max nil))
    (unwind-protect
        (progn
          (cljd-dart-nav--initialize connection root timeout)
          (let ((probe-uri (cljd-dart-nav--file-uri (cljd-dart-nav--probe-file root)))
                (version 0)
                payload)
            (dolist (probe (cljd-dart-nav--probe-sources uri symbol member))
              (unless payload
                (cl-incf version)
                (let ((text (plist-get probe :text))
                      (line (plist-get probe :line))
                      (character (plist-get probe :character)))
                  (if (= version 1)
                      (jsonrpc-notify
                       connection
                       "textDocument/didOpen"
                       `(:textDocument (:uri ,probe-uri
                                        :languageId "dart"
                                        :version ,version
                                        :text ,text)))
                    (jsonrpc-notify
                     connection
                     "textDocument/didChange"
                     `(:textDocument (:uri ,probe-uri :version ,version)
                       :contentChanges [(:text ,text)])))
                  (setq payload
                        (cljd-dart-nav--location-payload
                         (jsonrpc-request
                          connection
                          "textDocument/definition"
                          `(:textDocument (:uri ,probe-uri)
                            :position (:line ,line :character ,character))
                          :timeout timeout)
                         symbol member)))))
            (or payload
                (when (and file (file-readable-p file))
                  (cljd-dart-nav--document-symbol-request
                   connection file (1+ version) timeout symbol member)))))
      (ignore-errors
        (jsonrpc-request connection "shutdown" nil :timeout 5.0))
      (ignore-errors
        (jsonrpc-notify connection "exit" nil))
      (ignore-errors
        (when (jsonrpc-running-p connection)
          (delete-process (jsonrpc--process connection))))
      (ignore-errors (jsonrpc-shutdown connection t)))))

(defun cljd-dart-nav-main ()
  "Entry point for the CLJD -> Dart navigation helper."
  (let* ((args (cljd-dart-nav--parse-args))
         (roots (plist-get args :roots))
         (uri (plist-get args :uri))
         (file (plist-get args :file))
         (symbol (plist-get args :symbol))
         (member (plist-get args :member))
         (dart-executable (plist-get args :dart-executable))
         (timeout (plist-get args :timeout))
         (errors nil)
         payload)
    (dolist (root roots)
      (when (and (not payload) (file-directory-p root))
        (condition-case err
            (setq payload (cljd-dart-nav--query-root
                           root uri file symbol member timeout dart-executable))
          (error
           (push (format "%s: %s" root (error-message-string err)) errors)))))
    (princ
     (cljd-dart-nav--json-encode
      (or payload
          `((ok . :json-false)
            (symbol . ,symbol)
            (member . ,member)
            (error . ,(if errors
                          (string-join (nreverse errors) "; ")
                        "no definition result"))))))))

(cljd-dart-nav-main)
