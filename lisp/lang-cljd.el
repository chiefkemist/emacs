;;; lang-cljd.el --- ClojureDart tooling -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'comint)
(require 'compile)
(require 'core-projects)
(require 'core-repl)
(require 'core-lsp)
(require 'json)
(require 'inf-lisp)
(require 'seq)
(require 'subr-x)
(require 'url-parse)

(autoload 'clojure-mode "clojure-mode" nil t)

(defcustom chief/cljd-clj-command "clj"
  "Command used to run ClojureDart project tasks."
  :type 'string
  :group 'chief)

(defcustom chief/cljd-alias "cljd"
  "Clojure CLI alias used for ClojureDart tasks."
  :type 'string
  :group 'chief)

(defcustom chief/cljd-netcat-command "nc"
  "Command used to connect to the ClojureDart socket REPL."
  :type 'string
  :group 'chief)

(defcustom chief/cljd-preferred-flutter-device nil
  "Preferred Flutter device id for ClojureDart watcher startup."
  :type '(choice (const :tag "Auto" nil)
                 string)
  :group 'chief)

(defcustom chief/cljd-repl-start-timeout 120
  "Seconds to wait for the ClojureDart socket REPL to become available."
  :type 'integer
  :group 'chief)

(defcustom chief/cljd-dart-navigation-timeout 15.0
  "Seconds to wait asynchronously for hidden Dart analysis during CLJD navigation.
This does not block the Emacs UI; it bounds how long the CLJD -> Dart bridge will
wait before failing or falling back to opening the library file."
  :type 'number
  :group 'chief)

(defconst chief/cljd-repl-port-regexp
  "ClojureDart REPL listening on port \\([0-9]+\\)"
  "Regexp used to detect the ClojureDart socket REPL port from watch output.")

(define-derived-mode clojuredart-mode clojure-mode "ClojureDart")

(define-derived-mode chief/cljd-repl-mode inferior-lisp-mode "CLJD-REPL"
  "Major mode for ClojureDart socket REPL buffers."
  (setq-local comint-prompt-regexp "^[[:alnum:]._/-]+=> ")
  (setq-local comint-use-prompt-regexp t)
  (setq-local comint-process-echoes nil))

(defvar chief/cljd-last-flutter-device nil
  "Most recently used Flutter device id for ClojureDart watcher startup.")

(defvar chief/cljd-package-config-cache (make-hash-table :test #'equal)
  "Cache of decoded Dart package_config data keyed by project root.")

(defconst chief/cljd-dart-nav-helper-script
  (expand-file-name "../scripts/cljd_dart_nav.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Helper script used for asynchronous CLJD -> Dart definition lookups.")

(defvar chief/cljd-dart-nav-cache (make-hash-table :test #'equal)
  "Cache of resolved CLJD -> Dart definition locations.")

(defvar-local chief/cljd--dart-nav-request-token 0
  "Monotonic request token for CLJD -> Dart lookups in the current buffer.")

(defvar-local chief/cljd--dart-nav-process nil
  "Active asynchronous CLJD -> Dart lookup process for the current buffer.")

(defun chief/cljd-project-root ()
  "Return the current ClojureDart project root."
  (chief/project-preferred-root
   '("pubspec.yaml")
   '("deps.edn")
   (chief/project-current-root)
   default-directory))

(defun chief/cljd--deps-edn-file ()
  "Return the current project's deps.edn file, if any."
  (expand-file-name "deps.edn" (chief/cljd-project-root)))

(defun chief/cljd--package-config-file ()
  "Return the current project's Dart package config file."
  (expand-file-name ".dart_tool/package_config.json" (chief/cljd-project-root)))

(defun chief/cljd--repl-lock-file ()
  "Return the current project's ClojureDart REPL lock file."
  (expand-file-name "REPL.lock" (chief/cljd-project-root)))

(defun chief/cljd-project-kind ()
  "Return the current ClojureDart project kind."
  (let ((deps-edn (chief/cljd--deps-edn-file)))
    (cond
     ((and (file-readable-p deps-edn)
           (with-temp-buffer
             (insert-file-contents deps-edn)
             (re-search-forward ":kind[[:space:]\n]+:flutter\\_>" nil t)))
      'flutter)
     ((and (file-readable-p deps-edn)
           (with-temp-buffer
             (insert-file-contents deps-edn)
             (re-search-forward ":kind[[:space:]\n]+:dart\\_>" nil t)))
      'dart)
     ((file-exists-p (expand-file-name "pubspec.yaml" (chief/cljd-project-root)))
      'flutter)
     (t
     'dart))))

(defun chief/cljd--decode-json-file (file)
  "Decode FILE as JSON and return the resulting object."
  (with-temp-buffer
    (insert-file-contents file)
    (json-parse-buffer :array-type 'list :object-type 'alist :null-object nil :false-object nil)))

(defun chief/cljd-package-config ()
  "Return the current project's decoded package_config.json."
  (let* ((root (chief/cljd-project-root))
         (config-file (chief/cljd--package-config-file))
         (attrs (and (file-readable-p config-file)
                     (file-attributes config-file 'string)))
         (mtime (and attrs (file-attribute-modification-time attrs)))
         (cached (gethash root chief/cljd-package-config-cache)))
    (cond
     ((not (and mtime (file-readable-p config-file)))
      nil)
     ((and cached (equal (plist-get cached :mtime) mtime))
      (plist-get cached :data))
     (t
      (let ((data (chief/cljd--decode-json-file config-file)))
        (puthash root (list :mtime mtime :data data) chief/cljd-package-config-cache)
        data)))))

(defun chief/cljd--uri-path-from-file-uri (uri)
  "Convert file URI string URI to a local path."
  (let* ((parsed (url-generic-parse-url uri))
         (host (url-host parsed))
         (filename (url-filename parsed))
         (path (url-unhex-string (or filename ""))))
    (if (and host (not (string-empty-p host)))
        (concat "/" host path)
      path)))

(defun chief/cljd--resolve-config-uri (uri)
  "Resolve URI from package_config.json to a local path."
  (cond
   ((not uri) nil)
   ((string-prefix-p "file://" uri)
    (chief/cljd--uri-path-from-file-uri uri))
   (t
    (expand-file-name uri (file-name-directory (chief/cljd--package-config-file))))))

(defun chief/cljd--package-spec (package-name)
  "Return the package spec for PACKAGE-NAME from package_config.json."
  (when-let* ((config (chief/cljd-package-config)))
    (cl-find-if (lambda (pkg)
                  (equal (alist-get 'name pkg) package-name))
                (alist-get 'packages config))))

(defun chief/cljd--dart-sdk-lib-root ()
  "Return the Dart SDK lib root for the current project, if available."
  (or
   (when (fboundp 'chief/lsp-dart-sdk-dir)
     (when-let* ((sdk-dir (chief/lsp-dart-sdk-dir))
                 (lib-root (expand-file-name "lib" sdk-dir))
                 ((file-directory-p lib-root)))
       lib-root))
   (when-let* ((config (chief/cljd-package-config))
               (flutter-root (alist-get 'flutterRoot config)))
     (let ((dir (expand-file-name "bin/cache/dart-sdk/lib"
                                  (chief/cljd--resolve-config-uri flutter-root))))
       (when (file-directory-p dir)
         dir)))
   (when-let* ((dart (executable-find "dart")))
     (let* ((true-dart (file-truename dart))
            (sdk-root (expand-file-name ".." (file-name-directory true-dart)))
            (lib-root (expand-file-name "lib" sdk-root)))
       (when (file-directory-p lib-root)
         lib-root)))))

(defun chief/cljd--flutter-sdk-dir ()
  "Return the Flutter SDK directory for the current environment, if available."
  (or
   (when (fboundp 'chief/lsp-flutter-sdk-dir)
     (chief/lsp-flutter-sdk-dir))
   (when-let* ((flutter (executable-find "flutter"))
               (resolved (file-truename flutter))
               ((string-match "\\(.*?/flutter\\)/bin/flutter\\'" resolved)))
     (match-string 1 resolved))))

(defun chief/cljd--flutter-sdk-package-dir (package-name)
  "Return the Flutter SDK package dir for PACKAGE-NAME, if bundled."
  (when-let* ((sdk-dir (chief/cljd--flutter-sdk-dir))
              (package-dir (expand-file-name (format "packages/%s" package-name) sdk-dir))
              ((file-directory-p package-dir)))
    package-dir))

(defun chief/cljd--dart-ui-lib-root ()
  "Return the Flutter sky_engine lib root used for `dart:ui'."
  (or
   (when-let* ((sdk-dir (chief/cljd--flutter-sdk-dir))
               (dir (expand-file-name "bin/cache/pkg/sky_engine/lib/ui" sdk-dir))
               ((file-directory-p dir)))
     dir)
   (when-let* ((config (chief/cljd-package-config))
               (spec (chief/cljd--package-spec "sky_engine"))
               (root-uri (alist-get 'rootUri spec))
               (package-uri (or (alist-get 'packageUri spec) "lib/")))
     (let ((dir (expand-file-name "ui"
                                  (expand-file-name package-uri
                                                    (chief/cljd--resolve-config-uri root-uri)))))
       (when (file-directory-p dir)
         dir)))))

(defun chief/cljd-resolve-package-uri (uri)
  "Resolve package URI string URI to a local file path."
  (when (string-prefix-p "package:" uri)
    (let* ((tail (substring uri (length "package:")))
           (parts (split-string tail "/" t))
           (package-name (car parts))
           (relative-path (string-join (cdr parts) "/")))
      (or
       (when-let* ((spec (chief/cljd--package-spec package-name))
                   (root-uri (alist-get 'rootUri spec))
                   (package-uri (or (alist-get 'packageUri spec) "lib/"))
                   (root-path (chief/cljd--resolve-config-uri root-uri))
                   (base-dir (expand-file-name package-uri root-path))
                   (file (expand-file-name relative-path base-dir))
                   ((file-readable-p file)))
         file)
       (when-let* ((package-dir (chief/cljd--flutter-sdk-package-dir package-name))
                   (base-dir (expand-file-name "lib" package-dir))
                   (file (expand-file-name relative-path base-dir))
                   ((file-readable-p file)))
         file)))))

(defun chief/cljd-resolve-dart-sdk-uri (uri)
  "Resolve `dart:' URI string URI to a local SDK file path."
  (when (string-prefix-p "dart:" uri)
    (let* ((tail (substring uri (length "dart:")))
           (segments (split-string tail "/" t))
           (library (car segments))
           (rest (cdr segments))
           (base-dir (if (string= library "ui")
                         (chief/cljd--dart-ui-lib-root)
                       (when-let* ((sdk-root (chief/cljd--dart-sdk-lib-root)))
                         (expand-file-name library sdk-root))))
           (default-file (and base-dir
                              (expand-file-name (format "%s.dart" library) base-dir)))
           (explicit-file (and base-dir
                               rest
                               (expand-file-name (string-join rest "/") base-dir))))
      (cond
       ((and explicit-file (file-readable-p explicit-file))
        explicit-file)
       ((and default-file (file-readable-p default-file))
        default-file)))))

(defun chief/cljd-resolve-dart-reference-file (uri)
  "Resolve Dart reference URI string URI to a local file path."
  (or (chief/cljd-resolve-package-uri uri)
      (chief/cljd-resolve-dart-sdk-uri uri)
      (when (and (string-suffix-p ".dart" uri)
                 buffer-file-name)
        (let ((file (expand-file-name uri (file-name-directory buffer-file-name))))
          (when (file-readable-p file)
            file)))))

(defun chief/cljd--dart-package-root (package-name)
  "Return the package root for PACKAGE-NAME when it is locally known."
  (or
   (when-let* ((spec (chief/cljd--package-spec package-name))
               (root-uri (alist-get 'rootUri spec))
               (root-path (chief/cljd--resolve-config-uri root-uri))
               ((file-directory-p root-path)))
     root-path)
   (when (string= package-name "flutter")
     (chief/cljd--flutter-sdk-package-dir "flutter"))))

(defun chief/cljd--dart-file-analysis-root (file)
  "Return a plausible analysis root for Dart FILE."
  (when (and file (string-match "\\`\\(.+\\)/lib/.*\\'" file))
    (match-string 1 file)))

(defun chief/cljd--dart-analysis-root-candidates (uri)
  "Return candidate analysis roots for resolving Dart URI."
  (let* ((file (chief/cljd-resolve-dart-reference-file uri))
         (package-name (when (string-prefix-p "package:" uri)
                         (car (split-string (substring uri (length "package:")) "/" t))))
         (roots (delq nil
                      (list (chief/cljd-project-root)
                            (and package-name
                                 (chief/cljd--dart-package-root package-name))
                            (chief/cljd--dart-file-analysis-root file)))))
    (cl-remove-duplicates roots :test #'equal)))

(defun chief/cljd--dart-nav-cache-key (uri symbol &optional member)
  "Return the cache key for URI, SYMBOL, and optional MEMBER."
  (list (chief/cljd-project-root) uri symbol member))

(defun chief/cljd--dart-nav-helper-command (roots uri symbol &optional member file)
  "Return the helper command used to resolve URI, SYMBOL, and MEMBER for ROOTS."
  (unless (file-readable-p chief/cljd-dart-nav-helper-script)
    (user-error "Missing CLJD Dart navigation helper: %s" chief/cljd-dart-nav-helper-script))
  (append
   (list (or (and invocation-directory
                  (file-executable-p (expand-file-name invocation-name invocation-directory))
                  (expand-file-name invocation-name invocation-directory))
             invocation-name)
         "--batch" "-Q" "-l" chief/cljd-dart-nav-helper-script "--"
         "--timeout" (number-to-string chief/cljd-dart-navigation-timeout)
         "--dart-executable" (or (chief/lsp-dart-sdk-executable)
                                 (executable-find "dart")
                                 "dart")
         "--uri" uri
         "--symbol" symbol)
   (when file
     (list "--file" file))
   (when member
     (list "--member" member))
   (cl-mapcan (lambda (root)
                (list "--root" root))
              roots)))

(defun chief/cljd--dart-nav-process-filter (process chunk)
  "Accumulate PROCESS output CHUNK for the CLJD Dart navigation helper."
  (process-put process :stdout
               (concat (or (process-get process :stdout) "") chunk)))

(defun chief/cljd--dart-nav-parse-helper-output (stdout)
  "Parse helper STDOUT, tolerating harmless preamble noise."
  (let ((start 0)
        payload)
    (while (and (< start (length stdout)) (not payload))
      (let ((json-start (string-match "{" stdout start)))
        (if (null json-start)
            (setq start (length stdout))
          (setq payload
                (condition-case nil
                    (json-parse-string
                     (substring stdout json-start)
                     :object-type 'alist
                     :array-type 'list
                     :null-object nil
                     :false-object nil)
                  (error nil)))
          (setq start (1+ json-start)))))
    payload))

(defun chief/cljd--dart-nav-open-result (origin payload)
  "Open Dart navigation result PAYLOAD from ORIGIN."
  (let* ((file (alist-get 'file payload))
         (line (or (alist-get 'line payload) 0))
         (character (or (alist-get 'character payload) 0)))
    (when (and file (buffer-live-p origin))
      (with-current-buffer origin
        (when (fboundp 'chief/dart-remember-preferred-analysis-root)
          (chief/dart-remember-preferred-analysis-root (chief/cljd-project-root)))
        (xref-push-marker-stack)
        (find-file file)
        (goto-char (point-min))
        (forward-line line)
        (forward-char character)
        (recenter)))))

(defun chief/cljd--dart-nav-fallback-open-library (origin uri target)
  "Fallback from ORIGIN to opening the Dart library URI for TARGET."
  (if-let* ((file (chief/cljd-resolve-dart-reference-file uri)))
      (when (buffer-live-p origin)
        (with-current-buffer origin
          (message "Falling back to Dart library file for %s" target)
          (when (fboundp 'chief/dart-remember-preferred-analysis-root)
            (chief/dart-remember-preferred-analysis-root (chief/cljd-project-root)))
          (xref-push-marker-stack)
          (find-file file)))
    (message "No Dart definition target found for %s" target)))

(defun chief/cljd--dart-nav-process-sentinel (process _event)
  "Handle completion of the asynchronous CLJD -> Dart PROCESS."
  (when (memq (process-status process) '(exit signal))
    (let* ((origin (process-get process :origin-buffer))
           (token (process-get process :token))
           (uri (process-get process :uri))
           (target (process-get process :target))
           (stdout (string-trim (or (process-get process :stdout) "")))
           (stderr-buffer (process-get process :stderr-buffer))
           (stderr (when (buffer-live-p stderr-buffer)
                     (with-current-buffer stderr-buffer
                       (string-trim (buffer-string))))))
      (when (buffer-live-p origin)
        (with-current-buffer origin
          (when (eq chief/cljd--dart-nav-process process)
            (setq chief/cljd--dart-nav-process nil))
          (when (= token chief/cljd--dart-nav-request-token)
            (condition-case err
                (let ((payload (and (not (string-empty-p stdout))
                                    (chief/cljd--dart-nav-parse-helper-output stdout))))
                  (cond
                   ((and payload (alist-get 'ok payload))
                    (puthash (chief/cljd--dart-nav-cache-key
                              uri
                              (alist-get 'symbol payload)
                              (alist-get 'member payload))
                             payload
                             chief/cljd-dart-nav-cache)
                    (chief/cljd--dart-nav-open-result origin payload))
                   (payload
                    (message "Dart navigation helper could not resolve %s: %s"
                             target
                             (or (alist-get 'error payload) "unknown error"))
                    (chief/cljd--dart-nav-fallback-open-library origin uri target))
                   (t
                    (message "Dart navigation helper returned no data for %s" target))))
              (error
               (message "Failed to parse Dart navigation result for %s: %s"
                        target
                        (error-message-string err)))))))
      (when (and stderr (not (string-empty-p stderr)))
        (message "Dart navigation helper: %s" stderr))
      (when (buffer-live-p stderr-buffer)
        (kill-buffer stderr-buffer)))))

(defun chief/cljd-goto-dart-definition-async (uri symbol &optional member)
  "Resolve and jump to Dart definition for URI, SYMBOL, and optional MEMBER."
  (let* ((origin (current-buffer))
         (token (cl-incf chief/cljd--dart-nav-request-token))
         (target (if member
                     (format "%s.%s" symbol member)
                   symbol))
         (cache-key (chief/cljd--dart-nav-cache-key uri symbol member))
         (cached (gethash cache-key chief/cljd-dart-nav-cache))
         (roots (chief/cljd--dart-analysis-root-candidates uri))
         (file (chief/cljd-resolve-dart-reference-file uri)))
    (cond
     (cached
      (chief/cljd--dart-nav-open-result origin cached))
     ((null roots)
      (chief/cljd--dart-nav-fallback-open-library origin uri target))
     (t
      (when (process-live-p chief/cljd--dart-nav-process)
        (kill-process chief/cljd--dart-nav-process))
      (if-let* ((command (chief/cljd--dart-nav-helper-command roots uri symbol member file)))
          (let ((stderr-buffer (generate-new-buffer " *cljd-dart-nav-stderr*")))
            (setq chief/cljd--dart-nav-process
                  (make-process
                   :name "cljd-dart-nav"
                   :command command
                   :connection-type 'pipe
                   :noquery t
                   :buffer nil
                   :stderr stderr-buffer
                   :filter #'chief/cljd--dart-nav-process-filter
                   :sentinel #'chief/cljd--dart-nav-process-sentinel))
            (process-put chief/cljd--dart-nav-process :origin-buffer origin)
            (process-put chief/cljd--dart-nav-process :token token)
            (process-put chief/cljd--dart-nav-process :uri uri)
            (process-put chief/cljd--dart-nav-process :target target)
            (process-put chief/cljd--dart-nav-process :stderr-buffer stderr-buffer)
            (message "Resolving Dart definition for %s..." target))
        (chief/cljd--dart-nav-fallback-open-library origin uri target))))))

(defun chief/cljd-prewarm-dart-navigation ()
  "Warm the CLJD -> Dart helper path for the current project."
  (when-let* ((root (chief/cljd-project-root))
              ((chief/lsp-dart-sdk-executable)))
    ;; Nothing to do yet beyond proving the helper dependencies exist.
    root))

(defun chief/cljd--ns-form-bounds ()
  "Return the bounds of the current buffer's top-level `ns' form."
  (save-excursion
    (goto-char (point-min))
    (chief/cljd--goto-leading-form)
    (when (looking-at-p "(ns\\_>")
      (let ((start (point)))
        (condition-case nil
            (progn
              (forward-sexp 1)
              (cons start (point)))
          (scan-error nil))))))

(defun chief/cljd-dart-require-alist ()
  "Return an alist mapping ClojureDart aliases to Dart URIs."
  (when-let* ((bounds (chief/cljd--ns-form-bounds)))
    (save-excursion
      (goto-char (car bounds))
      (let ((end (cdr bounds))
            aliases)
        (while (re-search-forward
                "\\[\"\\([^\"]+\\)\"\\(?:[^][]\\|\n\\)*?:as[[:space:]\n]+\\([^][(){}[:space:]\n]+\\)"
                end
                t)
          (push (cons (match-string-no-properties 2)
                      (match-string-no-properties 1))
                aliases))
        (nreverse aliases)))))

(defun chief/cljd-dart-uri-at-point ()
  "Return the Dart URI referenced at point, if any."
  (or
   (when-let* ((string-bounds (bounds-of-thing-at-point 'string))
               (raw (buffer-substring-no-properties (car string-bounds) (cdr string-bounds)))
               (value (string-trim raw "\"" "\""))
               ((or (string-prefix-p "package:" value)
                    (string-prefix-p "dart:" value)
                    (string-suffix-p ".dart" value))))
     value)
   (when-let* ((symbol (thing-at-point 'symbol t))
               (alias (car (split-string symbol "/" t))))
     (alist-get alias (chief/cljd-dart-require-alist) nil nil #'equal))))

(defun chief/cljd-dart-symbol-at-point ()
  "Return the base Dart symbol name referenced at point after an alias slash."
  (when-let* ((symbol (thing-at-point 'symbol t))
              (parts (split-string symbol "/" t)))
    (when (= (length parts) 2)
      (car (split-string (cadr parts) "\\." t)))))

(defun chief/cljd-dart-member-at-point ()
  "Return the dotted member referenced at point after the base Dart symbol."
  (when-let* ((symbol (thing-at-point 'symbol t))
              (parts (split-string symbol "/" t)))
    (when (= (length parts) 2)
      (let ((members (cdr (split-string (cadr parts) "\\." t))))
        (when members
          (string-join members "."))))))

(defun chief/cljd-goto-definition ()
  "Jump to the Dart or ClojureDart definition at point."
  (interactive)
  (let* ((uri (chief/cljd-dart-uri-at-point))
         (dart-symbol (chief/cljd-dart-symbol-at-point))
         (dart-member (chief/cljd-dart-member-at-point)))
    (cond
     ((and uri dart-symbol)
      (chief/cljd-goto-dart-definition-async uri dart-symbol dart-member))
     (uri
     (if-let* ((file (chief/cljd-resolve-dart-reference-file uri)))
          (progn
            (when (fboundp 'chief/dart-remember-preferred-analysis-root)
              (chief/dart-remember-preferred-analysis-root (chief/cljd-project-root)))
            (xref-push-marker-stack)
            (find-file file))
        (user-error "No Dart library target found for %s" uri)))
     ((chief/lsp-ensure-active-for-navigation)
      (chief/lsp-goto-definition-direct))
     (t
      (user-error "No definition backend is available here")))))

(defun chief/cljd--command (&rest args)
  "Return a `clj -M:cljd' command list built from ARGS."
  (unless (executable-find chief/cljd-clj-command)
    (user-error "%s is not available on PATH" chief/cljd-clj-command))
  (append (list chief/cljd-clj-command
                (format "-M:%s" chief/cljd-alias))
          args))

(defun chief/cljd--pid-live-p (pid)
  "Return non-nil when PID is alive."
  (and (integerp pid)
       (> pid 0)
       (ignore-errors
         (process-attributes pid))))

(defun chief/cljd--repl-lock-state ()
  "Return parsed REPL.lock state for the current project, if available."
  (let ((lock-file (chief/cljd--repl-lock-file)))
    (when (file-readable-p lock-file)
      (with-temp-buffer
        (insert-file-contents lock-file)
        (when (re-search-forward "\\([0-9]+\\)\\(?:[[:space:]]+\\([0-9]+\\)\\)?"
                                 nil
                                 t)
          (let ((port (string-to-number (match-string 1)))
                (pid (when (match-string 2)
                       (string-to-number (match-string 2)))))
            (when (and (> port 0)
                       (or (null pid)
                           (chief/cljd--pid-live-p pid)))
              (list :port port :pid pid))))))))

(defun chief/cljd--flutter-devices ()
  "Return available Flutter device ids as a list of strings."
  (when (executable-find "flutter")
    (with-temp-buffer
      (when (zerop (call-process "flutter" nil t nil "devices" "--machine"))
        (goto-char (point-min))
        (let ((json-array-type 'list)
              (json-object-type 'alist)
              (json-key-type 'symbol))
          (mapcar (lambda (device)
                    (alist-get 'id device))
                  (json-parse-buffer :array-type 'list :object-type 'alist)))))))

(defun chief/cljd--select-flutter-device ()
  "Return a usable Flutter device id for the current host."
  (let* ((devices (delq nil (chief/cljd--flutter-devices)))
         (preferred chief/cljd-preferred-flutter-device)
         (cached chief/cljd-last-flutter-device)
         (auto (cond
                ((and (member preferred devices) preferred))
                ((and (member cached devices) cached))
                ((and (eq system-type 'darwin)
                      (member "macos" devices))
                 "macos")
                ((= (length devices) 1)
                 (car devices))
                (t nil))))
    (cond
     (auto
      (setq chief/cljd-last-flutter-device auto))
     ((null devices)
      nil)
     (t
      (setq chief/cljd-last-flutter-device
            (completing-read "Flutter device: " devices nil t nil nil (car devices)))))))

(defun chief/cljd--flutter-watch-args ()
  "Return watcher args for Flutter ClojureDart projects."
  (let ((device (chief/cljd--select-flutter-device)))
    (append (list "flutter")
            (when (and device (not (string-empty-p device)))
              (list "-d" device)))))

(defun chief/cljd--compilation-buffer-name (_mode)
  "Return the compilation buffer name for ClojureDart tasks."
  "*cljd*")

(defun chief/cljd-run-task (&rest args)
  "Run a ClojureDart task with ARGS in the project root."
  (let ((default-directory (chief/cljd-project-root))
        (compilation-read-command nil))
    (compilation-start
     (mapconcat #'shell-quote-argument
                (apply #'chief/cljd--command args)
                " ")
     'compilation-mode
     #'chief/cljd--compilation-buffer-name)))

(defun chief/cljd-init-project ()
  "Initialize the current ClojureDart project."
  (interactive)
  (chief/cljd-run-task "init"))

(defun chief/cljd-upgrade-project ()
  "Upgrade the current ClojureDart project."
  (interactive)
  (chief/cljd-run-task "upgrade"))

(defun chief/cljd-watch-flutter ()
  "Compile, watch, and run the current Flutter ClojureDart project."
  (interactive)
  (apply #'chief/cljd-run-task (chief/cljd--flutter-watch-args)))

(defun chief/cljd-watch-dart ()
  "Compile, watch, and run the current plain Dart ClojureDart project."
  (interactive)
  (chief/cljd-run-task "dart"))

(defun chief/cljd-watch-dwim ()
  "Run the appropriate watcher for the current ClojureDart project."
  (interactive)
  (pcase (chief/cljd-project-kind)
    ('flutter (chief/cljd-watch-flutter))
    (_ (chief/cljd-watch-dart))))

(defun chief/cljd--watch-buffer ()
  "Return the most relevant ClojureDart compilation buffer."
  (get-buffer "*cljd*"))

(defun chief/cljd--watch-process ()
  "Return the active ClojureDart watcher process, if any."
  (when-let* ((buffer (chief/cljd--watch-buffer)))
    (get-buffer-process buffer)))

(defun chief/cljd-detect-repl-port ()
  "Return the current ClojureDart socket REPL port, if visible."
  (or
   (plist-get (chief/cljd--repl-lock-state) :port)
   (when-let* ((process (chief/cljd--watch-process))
               ((process-live-p process))
               (buffer (process-buffer process)))
     (with-current-buffer buffer
       (save-excursion
         (goto-char (point-max))
         (when (re-search-backward chief/cljd-repl-port-regexp nil t)
           (string-to-number (match-string 1))))))))

(defun chief/cljd-repl-buffer-name ()
  "Return the per-project ClojureDart REPL buffer name."
  (format "*cljd-repl:%s*"
          (file-name-nondirectory
           (directory-file-name (chief/cljd-project-root)))))

(defun chief/cljd-repl-buffer ()
  "Return the current project's socket REPL buffer, if any."
  (get-buffer (chief/cljd-repl-buffer-name)))

(defun chief/cljd-repl-process ()
  "Return the current project's socket REPL process, if any."
  (when-let* ((buffer (chief/cljd-repl-buffer)))
    (get-buffer-process buffer)))

(defun chief/cljd-connect-repl (&optional port)
  "Connect to the ClojureDart socket REPL on PORT."
  (interactive)
  (let* ((root (chief/cljd-project-root))
         (buffer-name (chief/cljd-repl-buffer-name))
         (buffer (get-buffer-create buffer-name))
         (process (get-buffer-process buffer))
         (port (or port
                   (chief/cljd-detect-repl-port)
                   (read-number "ClojureDart socket REPL port: " 0))))
    (unless (and (integerp port) (> port 0))
      (user-error "No ClojureDart socket REPL port detected; start `clj -M:%s flutter` or `clj -M:%s dart` first"
                  chief/cljd-alias chief/cljd-alias))
    (unless (executable-find chief/cljd-netcat-command)
      (user-error "%s is not available on PATH" chief/cljd-netcat-command))
    (let ((new-process-p nil))
      (unless (process-live-p process)
        (setq new-process-p t)
        (setq process
              (make-comint-in-buffer
               "cljd-repl"
               buffer
               chief/cljd-netcat-command
               nil
               "localhost"
               (number-to-string port))))
      (with-current-buffer buffer
        (setq default-directory root)
        (chief/cljd-repl-mode)
        (when (and new-process-p (= (buffer-size) 0))
          (insert (format "; Connected to ClojureDart REPL on port %d.\n; If the prompt does not appear immediately, press RET once.\n\n"
                          port))))
      ;; ClojureDart's socket REPL often doesn't render its prompt/banner
      ;; until the first newline reaches the server.
      (when new-process-p
        (comint-send-string process "\n")))
    (pop-to-buffer-same-window buffer)))

(defun chief/cljd-restart-dwim ()
  "Restart the ClojureDart watcher."
  (interactive)
  (when-let* ((buffer (chief/cljd-repl-buffer))
              (process (get-buffer-process buffer)))
    (when (process-live-p process)
      (kill-process process)))
  (when-let* ((buffer (chief/cljd--watch-buffer))
              (process (get-buffer-process buffer)))
    (when (process-live-p process)
      (kill-process process)))
  (chief/cljd-watch-dwim))

(defun chief/cljd-wait-for-repl-port (&optional timeout)
  "Wait until the current project's socket REPL port is available.
TIMEOUT defaults to `chief/cljd-repl-start-timeout'."
  (let* ((timeout (or timeout chief/cljd-repl-start-timeout))
         (deadline (+ (float-time) timeout))
         (process (chief/cljd--watch-process))
         port)
    (while (and (not (setq port (chief/cljd-detect-repl-port)))
                (< (float-time) deadline)
                (or (and process (process-live-p process))
                    (chief/cljd--repl-lock-state)))
      (if (and process (process-live-p process))
          (accept-process-output process 0.25)
        (sleep-for 0.25))
      (setq process (chief/cljd--watch-process)))
    port))

(defun chief/cljd-start-repl ()
  "Start or connect to the current project's ClojureDart REPL."
  (interactive)
  (let ((repl-process (chief/cljd-repl-process))
        (watch-process (chief/cljd--watch-process)))
    (cond
     ((process-live-p repl-process)
      (pop-to-buffer-same-window (chief/cljd-repl-buffer)))
     ((when-let* ((port (chief/cljd-detect-repl-port)))
        (chief/cljd-connect-repl port)
        t))
     ((and watch-process (process-live-p watch-process))
      (message "Waiting for existing ClojureDart watcher...")
      (if-let* ((port (chief/cljd-wait-for-repl-port)))
          (chief/cljd-connect-repl port)
        (user-error "Timed out waiting for the ClojureDart socket REPL")))
     (t
      (chief/cljd-watch-dwim)
      (message "Waiting for ClojureDart REPL...")
      (if-let* ((port (chief/cljd-wait-for-repl-port)))
          (chief/cljd-connect-repl port)
        (user-error "Timed out waiting for the ClojureDart socket REPL"))))))

(defun chief/cljd--ensure-repl-process ()
  "Return the active ClojureDart REPL process."
  (or (chief/cljd-repl-process)
      (progn
        (call-interactively #'chief/cljd-start-repl)
        (or (chief/cljd-repl-process)
            (user-error "No active ClojureDart REPL process is available")))))

(defun chief/cljd--goto-leading-form ()
  "Move point to the first non-empty, non-comment form in the current buffer."
  (while (progn
           (skip-chars-forward " \t\r\n")
           (when (looking-at-p ";")
             (forward-line 1)
             t))))

(defun chief/cljd-buffer-namespace ()
  "Return the current buffer's namespace symbol name."
  (save-excursion
    (goto-char (point-min))
    (chief/cljd--goto-leading-form)
    (when (looking-at "(ns\\_>[[:space:]\n]+\\([[:alnum:]._/*+!?$%&=<>:-]+\\)")
      (match-string-no-properties 1))))

(defun chief/cljd--string-starts-with-ns-form-p (string)
  "Return non-nil when STRING starts with an `ns' form."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (chief/cljd--goto-leading-form)
    (looking-at-p "(ns\\_>")))

(defun chief/cljd--namespace-prefix (&optional namespace)
  "Return an `ns' form prefix for NAMESPACE, if available."
  (when-let* ((namespace (or namespace (chief/cljd-buffer-namespace))))
    (format "(ns %s)\n" namespace)))

(defun chief/cljd--prepare-eval-string (string &optional namespace)
  "Return STRING prepared for evaluation in NAMESPACE."
  (if (or (string-empty-p string)
          (chief/cljd--string-starts-with-ns-form-p string))
      string
    (if-let* ((prefix (chief/cljd--namespace-prefix namespace)))
        (concat prefix string)
      string)))

(defun chief/cljd--buffer-body-string ()
  "Return the current buffer body without the leading `ns' form."
  (save-excursion
    (goto-char (point-min))
    (chief/cljd--goto-leading-form)
    (condition-case nil
        (if (looking-at-p "(ns\\_>")
            (progn
              (forward-sexp 1)
              (buffer-substring-no-properties (point) (point-max)))
          (buffer-substring-no-properties (point-min) (point-max)))
      (scan-error
       (buffer-substring-no-properties (point-min) (point-max))))))

(defun chief/cljd-buffer-eval-string ()
  "Return the current buffer prepared for ClojureDart REPL evaluation."
  (let ((namespace (chief/cljd-buffer-namespace))
        (body (chief/cljd--buffer-body-string)))
    (if-let* ((prefix (chief/cljd--namespace-prefix namespace)))
        (concat prefix body)
      body)))

(defun chief/cljd-send-string (string)
  "Send STRING to the active ClojureDart socket REPL."
  (let ((process (chief/cljd--ensure-repl-process)))
    (comint-send-string process (chief/cljd--prepare-eval-string string))
    (unless (string-suffix-p "\n" string)
      (comint-send-string process "\n"))))

(defun chief/cljd-send-region (start end)
  "Send the region from START to END to the socket REPL."
  (interactive "r")
  (chief/cljd-send-string (buffer-substring-no-properties start end)))

(defun chief/cljd-send-line ()
  "Send the current line to the socket REPL."
  (interactive)
  (chief/cljd-send-region (line-beginning-position) (line-end-position)))

(defun chief/cljd-send-defun ()
  "Send the current top-level form to the socket REPL."
  (interactive)
  (save-excursion
    (mark-defun)
    (chief/cljd-send-region (region-beginning) (region-end))
    (deactivate-mark)))

(defun chief/cljd-send-buffer ()
  "Send the current buffer to the socket REPL."
  (interactive)
  (let ((process (chief/cljd--ensure-repl-process))
        (string (chief/cljd-buffer-eval-string)))
    (comint-send-string process string)
    (unless (string-suffix-p "\n" string)
      (comint-send-string process "\n"))))

(defun chief/cljd-mode-setup ()
  "Configure ClojureDart buffers."
  (setq-local lsp-enabled-clients '(clojure-lsp))
  (setq-local lsp-lens-enable nil)
  (setq-local chief/lsp-root-function #'chief/cljd-project-root)
  (setq-local chief/lsp-definition-function #'chief/cljd-goto-definition)
  (when (fboundp 'chief/dart-remember-preferred-analysis-root)
    (chief/dart-remember-preferred-analysis-root (chief/cljd-project-root)))
  (chief/cljd-prewarm-dart-navigation)
  (chief/repl-configure
   :start #'chief/cljd-start-repl
   :restart #'chief/cljd-restart-dwim
   :send-line #'chief/cljd-send-line
   :send-region #'chief/cljd-send-region
   :send-buffer #'chief/cljd-send-buffer
   :send-defun #'chief/cljd-send-defun
   :load-file #'chief/cljd-send-buffer))

(use-package clojure-mode
  :mode ("\\.cljd\\'" . clojuredart-mode)
  :hook (clojuredart-mode . chief/cljd-mode-setup))

(chief/repl-setup-standard-local-leader 'clojuredart-mode-map)

(chief/local-leader-def
  :keymaps 'clojuredart-mode-map
  "c" '(:ignore t :which-key "cljd")
  "ci" #'chief/cljd-init-project
  "cu" #'chief/cljd-upgrade-project
  "cw" #'chief/cljd-watch-dwim
  "cf" #'chief/cljd-watch-flutter
  "cd" #'chief/cljd-watch-dart
  "cc" #'chief/cljd-connect-repl)

(provide 'lang-cljd)
;;; lang-cljd.el ends here
