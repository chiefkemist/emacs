;;; lang-cljd.el --- ClojureDart tooling -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'comint)
(require 'compile)
(require 'core-projects)
(require 'core-repl)
(require 'core-lsp)
(require 'inf-lisp)
(require 'json)
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

(defcustom chief/cljd-preferred-flutter-device nil
  "Preferred Flutter device id for ClojureDart watcher startup."
  :type '(choice (const :tag "Auto" nil)
                 string)
  :group 'chief)

(defcustom chief/cljd-repl-start-timeout 120
  "Seconds to wait for the ClojureDart socket REPL to become available."
  :type 'integer
  :group 'chief)

(defcustom chief/cljd-repl-prompt-timeout 10
  "Seconds to wait for the initial ClojureDart REPL prompt after connecting."
  :type 'integer
  :group 'chief)

(defcustom chief/cljd-repl-watch-ready-timeout 45
  "Maximum seconds to wait for a Flutter watcher to become REPL-ready."
  :type 'integer
  :group 'chief)

(defcustom chief/cljd-repl-watch-settle-delay 30.0
  "Fallback seconds to wait after the REPL port appears for Flutter startup to settle."
  :type 'number
  :group 'chief)

(defcustom chief/cljd-dart-navigation-timeout 15.0
  "Seconds to wait asynchronously for hidden Dart analysis during CLJD navigation.
This does not block the Emacs UI; it bounds how long the CLJD -> Dart bridge will
wait before failing or falling back to opening the library file."
  :type 'number
  :group 'chief)

(defcustom chief/cljd-inline-result-duration 6.0
  "Seconds to keep inline CLJD evaluation results visible in source buffers."
  :type 'number
  :group 'chief)

(defconst chief/cljd-repl-port-regexp
  "ClojureDart REPL listening on port \\([0-9]+\\)"
  "Regexp used to detect the ClojureDart socket REPL port from watch output.")

(defconst chief/cljd-watch-reloaded-regexp
  "Reloaded .+ libraries in .+\\."
  "Regexp used to detect that Flutter finished its initial hot reload.")

(define-derived-mode clojuredart-mode clojure-mode "ClojureDart")

(defun chief/cljd-repl-output-hook (_string)
  "Resolve pending CLJD requests after REPL output arrives."
  (when-let* ((process (get-buffer-process (current-buffer))))
    (chief/cljd--drain-repl-requests process)))

(define-derived-mode chief/cljd-repl-mode inferior-lisp-mode "CLJD-REPL"
  "Major mode for ClojureDart socket REPL buffers."
  (setq-local comint-prompt-regexp "^[[:alnum:]._/-]+=> ")
  (setq-local comint-use-prompt-regexp t)
  (setq-local comint-process-echoes nil)
  (setq-local comint-prompt-read-only t)
  (define-key chief/cljd-repl-mode-map (kbd "RET") #'chief/cljd-repl-return)
  (define-key chief/cljd-repl-mode-map (kbd "<return>") #'chief/cljd-repl-return)
  (define-key chief/cljd-repl-mode-map (kbd "C-m") #'chief/cljd-repl-return)
  (define-key chief/cljd-repl-mode-map (kbd "C-j") #'chief/cljd-repl-return)
  (when (fboundp 'evil-define-key)
    (evil-define-key '(insert normal emacs) chief/cljd-repl-mode-map
      (kbd "RET") #'chief/cljd-repl-return
      (kbd "<return>") #'chief/cljd-repl-return
      (kbd "C-m") #'chief/cljd-repl-return
      (kbd "C-j") #'chief/cljd-repl-return))
  (add-hook 'comint-output-filter-functions #'chief/cljd-repl-output-hook nil t))

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

(defvar chief/cljd--dart-nav-sessions (make-hash-table :test #'equal)
  "Live CLJD -> Dart navigation helper sessions keyed by project root.")

(defvar chief/cljd--dart-nav-next-request-id 0
  "Monotonic id assigned to helper requests across all CLJD buffers.")

(defvar chief/cljd--dart-nav-cleanup-installed nil
  "Non-nil once CLJD Dart helper cleanup hooks were registered.")

(defvar-local chief/cljd--dart-nav-request-token 0
  "Monotonic request token for CLJD -> Dart lookups in the current buffer.")

(defvar-local chief/cljd--last-inline-result-overlay nil
  "Most recent inline ClojureDart evaluation result overlay in the current buffer.")

(defun chief/cljd--clear-inline-result-overlay (&optional overlay)
  "Delete OVERLAY, defaulting to the current buffer's CLJD result overlay."
  (let ((overlay (or overlay chief/cljd--last-inline-result-overlay)))
    (when (overlayp overlay)
      (delete-overlay overlay))
    (when (eq overlay chief/cljd--last-inline-result-overlay)
      (setq chief/cljd--last-inline-result-overlay nil))))

(defun chief/cljd--format-inline-result (result)
  "Return RESULT formatted for a one-line inline annotation."
  (let* ((text (string-trim result))
         (single-line (replace-regexp-in-string "[\r\n]+" " " text)))
    (truncate-string-to-width single-line 120 nil nil " ...")))

(defun chief/cljd--show-inline-result (marker result)
  "Show inline RESULT at MARKER in its source buffer."
  (when-let* ((buffer (marker-buffer marker))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (chief/cljd--clear-inline-result-overlay)
      (let* ((pos (marker-position marker))
             (overlay (make-overlay pos pos buffer)))
        (overlay-put overlay 'after-string
                     (propertize
                      (format "  => %s" (chief/cljd--format-inline-result result))
                      'face 'shadow))
        (setq-local chief/cljd--last-inline-result-overlay overlay)
        (run-at-time chief/cljd-inline-result-duration nil
                     #'chief/cljd--clear-inline-result-overlay overlay)))))

(defun chief/cljd--register-repl-request (process &optional result-marker &rest props)
  "Register a pending CLJD REPL request on PROCESS.
When RESULT-MARKER is non-nil, show the eventual result inline there.
PROPS are additional plist entries stored alongside the request."
  (when-let* ((buffer (process-buffer process))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (let* ((start (copy-marker (process-mark process)))
             (queue (process-get process :chief/cljd-pending-requests))
             (request (append (list :output-start start
                                    :result-marker result-marker)
                              props)))
        (process-put process :chief/cljd-pending-requests
                     (append queue (list request)))))))

(defun chief/cljd--trim-repl-output (text &optional sent-text)
  "Normalize CLJD REPL output TEXT for source-buffer display.
When SENT-TEXT is non-nil, strip that echoed submission from the front."
  (let* ((normalized (replace-regexp-in-string "\r" "" text))
         (trimmed (string-trim normalized))
         (sent (and sent-text
                    (string-trim (replace-regexp-in-string "\r" "" sent-text)))))
    (when (and sent (not (string-empty-p sent)))
      (when (string-prefix-p sent trimmed)
        (setq trimmed (string-trim (substring trimmed (length sent)))))
      (when (string-prefix-p (concat sent "\n") normalized)
        (setq trimmed (string-trim
                       (substring normalized (length (concat sent "\n")))))))
    (unless (string-empty-p trimmed)
      trimmed)))

(defun chief/cljd--drain-repl-requests (process)
  "Resolve completed CLJD REPL requests for PROCESS."
  (when-let* ((buffer (process-buffer process))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (let ((queue (process-get process :chief/cljd-pending-requests)))
        (while queue
          (let* ((request (car queue))
                 (start-marker (plist-get request :output-start))
                 (result-marker (plist-get request :result-marker)))
            (if (not (marker-buffer start-marker))
                (setq queue (cdr queue))
              (save-excursion
                (goto-char (marker-position start-marker))
                (if-let* ((prompt-pos (re-search-forward comint-prompt-regexp nil t)))
                    (let* ((start-pos (marker-position start-marker))
                           (prompt-beg (match-beginning 0))
                           (output (buffer-substring-no-properties
                                    start-pos
                                    prompt-beg))
                           (result (chief/cljd--trim-repl-output
                                    output
                                    (plist-get request :sent-text)))
                           (discard-output (plist-get request :discard-output)))
                      (setq queue (cdr queue))
                      (set-marker start-marker nil)
                      (if discard-output
                          (delete-region start-pos prompt-beg)
                        (when (and result result-marker)
                          (chief/cljd--show-inline-result result-marker result))))
                  (setq queue :pending)))))
          (process-put process :chief/cljd-pending-requests
                       (if (eq queue :pending)
                           (process-get process :chief/cljd-pending-requests)
                         queue))
          (when (eq queue :pending)
            (setq queue nil)))))))

(defun chief/cljd-repl-process-sentinel (process event)
  "Report CLJD REPL PROCESS lifecycle EVENTs in the REPL buffer."
  (when-let* ((buffer (process-buffer process))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "\n; CLJD REPL %s" (string-trim event)))
        (set-marker (process-mark process) (point-max))))))

(defun chief/cljd-repl-ready-p (&optional buffer)
  "Return non-nil when BUFFER has shown a CLJD prompt."
  (when-let* ((buffer (or buffer (chief/cljd-repl-buffer)))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (re-search-forward comint-prompt-regexp nil t)))))

(defun chief/cljd-wait-for-repl-prompt (process &optional timeout)
  "Wait for PROCESS to show its initial CLJD prompt within TIMEOUT seconds."
  (let* ((timeout (or timeout chief/cljd-repl-prompt-timeout))
         (deadline (+ (float-time) timeout))
         (buffer (process-buffer process)))
    (while (and (process-live-p process)
                (< (float-time) deadline)
                (not (chief/cljd-repl-ready-p buffer)))
      (accept-process-output process 0.25))
    (chief/cljd-repl-ready-p buffer)))

(defun chief/cljd--send-repl-input (process sent-text &optional result-marker already-in-buffer)
  "Send SENT-TEXT to PROCESS and optionally associate RESULT-MARKER.
When ALREADY-IN-BUFFER is non-nil, reuse the user's existing input text in the
REPL buffer instead of inserting another copy."
  (let* ((buffer (process-buffer process))
         (text (string-trim-right (or sent-text "") "[\r\n]+")))
    (with-current-buffer buffer
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (if already-in-buffer
            (progn
              (setq comint-last-input-start
                    (copy-marker (marker-position (process-mark process))))
              (insert "\n")
              (setq comint-last-input-end (copy-marker (point-max) t)))
          (insert text "\n"))
        (set-marker (process-mark process) (point-max) buffer)))
    (unless (string-empty-p text)
      (with-current-buffer buffer
        (comint-add-to-input-history text)))
    (chief/cljd--register-repl-request process result-marker :sent-text text)
    (process-send-string process (concat text "\n"))
    process))

(defun chief/cljd-repl-return ()
  "Submit the current CLJD REPL input."
  (interactive)
  (goto-char (point-max))
  (when-let* ((process (chief/cljd-repl-process)))
    (let ((result-marker (process-get process :chief/cljd-next-result-marker))
          (input-start (marker-position (process-mark process)))
          (input-end (point-max)))
      (process-put process :chief/cljd-next-result-marker nil)
      (process-put process :chief/cljd-next-sent-text nil)
      (chief/cljd--send-repl-input
       process
       (buffer-substring-no-properties input-start input-end)
       result-marker
       t))))

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

(defun chief/cljd--dart-nav-session-key (&optional root)
  "Return the session cache key for ROOT or the current CLJD project."
  (expand-file-name (or root (chief/cljd-project-root))))

(defun chief/cljd--dart-nav-helper-socket-path (key)
  "Return the Unix socket path used by the persistent helper for KEY."
  (let ((dir (expand-file-name "cljd-dart-nav" temporary-file-directory)))
    (make-directory dir t)
    (expand-file-name (format "%s.sock" (md5 key)) dir)))

(defun chief/cljd--dart-nav-helper-start-command (key)
  "Return the persistent helper command used for CLJD -> Dart lookups for KEY."
  (unless (file-readable-p chief/cljd-dart-nav-helper-script)
    (user-error "Missing CLJD Dart navigation helper: %s" chief/cljd-dart-nav-helper-script))
  (list (or (and invocation-directory
                 (file-executable-p (expand-file-name invocation-name invocation-directory))
                 (expand-file-name invocation-name invocation-directory))
            invocation-name)
        "--batch" "-Q" "-l" chief/cljd-dart-nav-helper-script "--"
        "--server"
        "--socket" (chief/cljd--dart-nav-helper-socket-path key)
        "--timeout" (number-to-string chief/cljd-dart-navigation-timeout)
        "--dart-executable" (or (chief/lsp-dart-sdk-executable)
                                (executable-find "dart")
                                "dart")))

(defun chief/cljd--dart-nav-session-live-p (session)
  "Return non-nil when SESSION still owns a live helper or connection."
  (or (and (processp (plist-get session :connection))
           (process-live-p (plist-get session :connection)))
      (and (processp (plist-get session :helper))
           (process-live-p (plist-get session :helper)))))

(defun chief/cljd--dart-nav-session (key)
  "Return the CLJD Dart helper session stored under KEY."
  (gethash key chief/cljd--dart-nav-sessions))

(defun chief/cljd--dart-nav-install-cleanup ()
  "Install one-time cleanup hooks for persistent CLJD Dart helpers."
  (unless chief/cljd--dart-nav-cleanup-installed
    (add-hook 'kill-emacs-hook #'chief/cljd-shutdown-dart-navigation-helpers)
    (setq chief/cljd--dart-nav-cleanup-installed t)))

(defun chief/cljd--dart-nav-kill-session-processes (session)
  "Terminate helper SESSION processes and temporary buffers."
  (when-let* ((timer (plist-get session :connect-timer)))
    (cancel-timer timer))
  (when-let* ((connection (plist-get session :connection)))
    (ignore-errors (delete-process connection)))
  (when-let* ((helper (plist-get session :helper)))
    (ignore-errors (kill-process helper)))
  (when-let* ((socket-path (plist-get session :socket-path)))
    (ignore-errors (delete-file socket-path)))
  (when-let* ((stderr-buffer (plist-get session :stderr-buffer))
              ((buffer-live-p stderr-buffer)))
    (kill-buffer stderr-buffer)))

(defun chief/cljd--dart-nav-fail-pending-requests (session reason)
  "Fail all pending navigation requests in SESSION with REASON."
  (let ((pending (plist-get session :pending)))
    (when (hash-table-p pending)
      (maphash
       (lambda (_id metadata)
         (when (eq (plist-get metadata :kind) 'resolve)
           (let ((origin (plist-get metadata :origin-buffer))
                 (token (plist-get metadata :token))
                 (uri (plist-get metadata :uri))
                 (target (plist-get metadata :target)))
             (when (buffer-live-p origin)
               (with-current-buffer origin
                 (when (= token chief/cljd--dart-nav-request-token)
                   (message "Dart navigation helper failed for %s: %s" target reason)
                   (chief/cljd--dart-nav-fallback-open-library origin uri target)))))))
       pending)
      (clrhash pending))))

(defun chief/cljd--dart-nav-dispose-session (key &optional reason)
  "Dispose of the persistent helper session stored under KEY.
When REASON is non-nil, fail pending resolve requests with it first."
  (when-let* ((session (chief/cljd--dart-nav-session key)))
    (setf (plist-get session :closing) t)
    (when reason
      (chief/cljd--dart-nav-fail-pending-requests session reason))
    (remhash key chief/cljd--dart-nav-sessions)
    (chief/cljd--dart-nav-kill-session-processes session)))

(defun chief/cljd-shutdown-dart-navigation-helpers ()
  "Shut down every persistent CLJD Dart navigation helper session."
  (interactive)
  (maphash (lambda (key _session)
             (chief/cljd--dart-nav-dispose-session key))
           chief/cljd--dart-nav-sessions)
  (clrhash chief/cljd--dart-nav-sessions))

(defun chief/cljd--dart-nav-parse-json (string)
  "Parse helper JSON STRING into an alist."
  (json-parse-string
   string
   :object-type 'alist
   :array-type 'list
   :null-object nil
   :false-object nil))

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

(defun chief/cljd--dart-nav-flush-session-queue (session)
  "Send any queued requests now that SESSION has a live socket connection."
  (when-let* ((connection (plist-get session :connection))
              ((process-live-p connection)))
    (dolist (payload (plist-get session :queue))
      (process-send-string connection payload))
    (setf (plist-get session :queue) nil)))

(defun chief/cljd--dart-nav-handle-response (session payload)
  "Handle helper SESSION response PAYLOAD."
  (let* ((request-id (alist-get 'id payload))
         (pending (plist-get session :pending))
         (metadata (and request-id (hash-table-p pending) (gethash request-id pending))))
    (when (and request-id (hash-table-p pending))
      (remhash request-id pending))
    (pcase (plist-get metadata :kind)
      ('prewarm
       (unless (alist-get 'ok payload)
         (message "CLJD Dart navigation prewarm failed: %s"
                  (or (alist-get 'error payload) "unknown error"))))
      ('resolve
       (let ((origin (plist-get metadata :origin-buffer))
             (token (plist-get metadata :token))
             (uri (plist-get metadata :uri))
             (target (plist-get metadata :target)))
         (when (buffer-live-p origin)
           (with-current-buffer origin
             (when (= token chief/cljd--dart-nav-request-token)
               (cond
                ((and (alist-get 'ok payload) (alist-get 'file payload))
                 (puthash (chief/cljd--dart-nav-cache-key
                           uri
                           (alist-get 'symbol payload)
                           (alist-get 'member payload))
                          payload
                          chief/cljd-dart-nav-cache)
                 (chief/cljd--dart-nav-open-result origin payload))
                ((alist-get 'ok payload)
                 (message "Dart navigation helper returned no location for %s" target)
                 (chief/cljd--dart-nav-fallback-open-library origin uri target))
                (t
                 (message "Dart navigation helper could not resolve %s: %s"
                          target
                          (or (alist-get 'error payload) "unknown error"))
                 (chief/cljd--dart-nav-fallback-open-library origin uri target)))))))))))

(defun chief/cljd--dart-nav-connection-filter (process chunk)
  "Handle CHUNK from a persistent helper socket PROCESS."
  (when-let* ((key (process-get process :session-key))
              (session (chief/cljd--dart-nav-session key)))
    (let ((buffer (concat (or (plist-get session :connection-stdout) "") chunk))
          line)
      (while (string-match "\n" buffer)
        (setq line (substring buffer 0 (match-beginning 0))
              buffer (substring buffer (match-end 0)))
        (unless (string-empty-p (string-trim line))
          (condition-case err
              (chief/cljd--dart-nav-handle-response
               session
               (chief/cljd--dart-nav-parse-json line))
            (error
             (message "Failed to parse CLJD Dart helper response: %s"
                      (error-message-string err))))))
      (setf (plist-get session :connection-stdout) buffer))))

(defun chief/cljd--dart-nav-connection-sentinel (process _event)
  "Handle helper socket PROCESS state changes."
  (when (memq (process-status process) '(closed failed exit signal))
    (when-let* ((key (process-get process :session-key))
                (session (chief/cljd--dart-nav-session key))
                ((not (plist-get session :closing))))
      (chief/cljd--dart-nav-dispose-session key "lost persistent helper connection"))))

(defun chief/cljd--dart-nav-connect-session (session key)
  "Connect SESSION stored under KEY to its helper Unix socket."
  (unless (and (processp (plist-get session :connection))
               (process-live-p (plist-get session :connection)))
    (let ((connection (make-network-process
                       :name (format "cljd-dart-nav-conn:%s" (file-name-nondirectory key))
                       :family 'local
                       :service (plist-get session :socket-path)
                       :coding 'utf-8-unix
                       :noquery t
                       :filter #'chief/cljd--dart-nav-connection-filter
                       :sentinel #'chief/cljd--dart-nav-connection-sentinel)))
      (process-put connection :session-key key)
      (setf (plist-get session :connection) connection)
      (when-let* ((timer (plist-get session :connect-timer)))
        (cancel-timer timer)
        (setf (plist-get session :connect-timer) nil))
      (chief/cljd--dart-nav-flush-session-queue session))))

(defun chief/cljd--dart-nav-maybe-connect-session (key)
  "Try to connect the persistent helper session stored under KEY."
  (when-let* ((session (chief/cljd--dart-nav-session key)))
    (cond
     ((plist-get session :closing)
      (when-let* ((timer (plist-get session :connect-timer)))
        (cancel-timer timer)
        (setf (plist-get session :connect-timer) nil)))
     ((and (processp (plist-get session :connection))
           (process-live-p (plist-get session :connection)))
      (when-let* ((timer (plist-get session :connect-timer)))
        (cancel-timer timer)
        (setf (plist-get session :connect-timer) nil)))
     ((and (processp (plist-get session :helper))
           (process-live-p (plist-get session :helper))
           (file-exists-p (plist-get session :socket-path)))
      (condition-case nil
          (chief/cljd--dart-nav-connect-session session key)
        (file-error nil))))))

(defun chief/cljd--dart-nav-helper-filter (_process _chunk)
  "Ignore helper stdout in persistent helper mode."
  nil)

(defun chief/cljd--dart-nav-helper-sentinel (process _event)
  "Handle persistent helper PROCESS state changes."
  (when (memq (process-status process) '(exit signal failed closed))
    (when-let* ((key (process-get process :session-key))
                (session (chief/cljd--dart-nav-session key)))
      (let ((stderr-buffer (plist-get session :stderr-buffer))
            (closing (plist-get session :closing)))
        (unless closing
          (let ((stderr (when (buffer-live-p stderr-buffer)
                          (with-current-buffer stderr-buffer
                            (string-trim (buffer-string))))))
            (chief/cljd--dart-nav-dispose-session
             key
             (if (and stderr (not (string-empty-p stderr)))
                 stderr
               "persistent helper exited unexpectedly"))))))))

(defun chief/cljd--dart-nav-start-session (key)
  "Start and register a persistent helper session for KEY."
  (chief/cljd--dart-nav-install-cleanup)
  (let* ((stderr-buffer (generate-new-buffer " *cljd-dart-nav-helper-stderr*"))
         (socket-path (chief/cljd--dart-nav-helper-socket-path key))
         (session (list :project-root key
                        :pending (make-hash-table :test #'eql)
                        :queue nil
                        :helper nil
                        :connection nil
                        :connect-timer nil
                        :closing nil
                        :prewarm-requested nil
                        :connection-stdout ""
                        :socket-path socket-path
                        :stderr-buffer stderr-buffer))
         (helper (make-process
                  :name (format "cljd-dart-nav-helper:%s" (file-name-nondirectory key))
                  :command (chief/cljd--dart-nav-helper-start-command key)
                  :connection-type 'pipe
                  :coding 'utf-8-unix
                  :noquery t
                  :buffer nil
                  :stderr stderr-buffer
                  :filter #'chief/cljd--dart-nav-helper-filter
                  :sentinel #'chief/cljd--dart-nav-helper-sentinel)))
    (process-put helper :session-key key)
    (setf (plist-get session :helper) helper)
    (puthash key session chief/cljd--dart-nav-sessions)
    (setf (plist-get session :connect-timer)
          (run-at-time 0.05 0.1 #'chief/cljd--dart-nav-maybe-connect-session key))
    session))

(defun chief/cljd--ensure-dart-nav-session (&optional root)
  "Return a live persistent helper session for ROOT or the current project."
  (let* ((key (chief/cljd--dart-nav-session-key root))
         (session (chief/cljd--dart-nav-session key)))
    (unless (chief/cljd--dart-nav-session-live-p session)
      (when session
        (chief/cljd--dart-nav-dispose-session key))
      (setq session (chief/cljd--dart-nav-start-session key)))
    session))

(defun chief/cljd--dart-nav-send-request (session payload &optional metadata)
  "Send PAYLOAD through persistent helper SESSION.
When METADATA is non-nil, track the request for later response handling."
  (let* ((request-id (cl-incf chief/cljd--dart-nav-next-request-id))
         (pending (plist-get session :pending))
         (request (append `((id . ,request-id)) payload))
         (encoded (concat (json-serialize request :false-object :json-false :null-object nil)
                          "\n")))
    (when metadata
      (puthash request-id metadata pending))
    (if (and (processp (plist-get session :connection))
             (process-live-p (plist-get session :connection)))
        (process-send-string (plist-get session :connection) encoded)
      (setf (plist-get session :queue)
            (append (plist-get session :queue) (list encoded))))
    request-id))

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
      (let ((session (chief/cljd--ensure-dart-nav-session)))
        (chief/cljd--dart-nav-send-request
         session
         `((op . "resolve")
           (roots . ,(vconcat roots))
           (timeout . ,chief/cljd-dart-navigation-timeout)
           (uri . ,uri)
           (file . ,file)
           (symbol . ,symbol)
           (member . ,member))
         `(:kind resolve
           :origin-buffer ,origin
           :token ,token
           :uri ,uri
           :target ,target))
        (message "Resolving Dart definition for %s..." target))))))

(defun chief/cljd-prewarm-dart-navigation ()
  "Warm the persistent CLJD -> Dart helper for the current project."
  (when-let* ((root (chief/cljd-project-root))
              ((chief/lsp-dart-sdk-executable)))
    (let ((session (chief/cljd--ensure-dart-nav-session root)))
      (unless (plist-get session :prewarm-requested)
        (setf (plist-get session :prewarm-requested) t)
        (chief/cljd--dart-nav-send-request
         session
         `((op . "prewarm")
           (roots . ,(vector (chief/cljd--dart-nav-session-key root)))
           (timeout . ,chief/cljd-dart-navigation-timeout))
         '(:kind prewarm)))
      root)))
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

(defun chief/cljd--legacy-repl-process-p (process)
  "Return non-nil when PROCESS is not the current CLJD REPL backend."
  (when (process-live-p process)
    (not (memq (process-get process :chief/cljd-backend)
               '(inferior-lisp socket-elisp)))))

(defun chief/cljd-repl-process-filter (process string)
  "Insert STRING from PROCESS into the CLJD REPL buffer via comint."
  (when-let* ((buffer (process-buffer process))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (comint-output-filter process string)))))

(defun chief/cljd--create-repl-process (buffer root port)
  "Create a CLJD network REPL process in BUFFER rooted at ROOT on PORT."
  (with-current-buffer buffer
    (setq default-directory root)
    (let* ((inhibit-read-only t)
           (proc (make-network-process :name "cljd-repl"
                                       :buffer buffer
                                       :host "localhost"
                                       :service port
                                       :coding 'utf-8-unix
                                       :noquery t
                                       :filter #'chief/cljd-repl-process-filter
                                       :sentinel #'chief/cljd-repl-process-sentinel)))
      (unless (process-live-p proc)
        (user-error "Failed to start the ClojureDart REPL transport"))
      (chief/cljd-repl-mode)
      (setq inferior-lisp-buffer buffer)
      (set-process-query-on-exit-flag proc nil)
      (process-put proc :chief/cljd-backend 'socket-elisp)
      (process-put proc :chief/cljd-pending-requests nil)
      (goto-char (point-max))
      (when (= (buffer-size) 0)
        (insert (format "; Connected to ClojureDart REPL on port %d.\n; If the prompt does not appear immediately, press RET once.\n\n"
                        port)))
      (set-marker (process-mark proc) (point-max) buffer)
      ;; Follow the upstream inferior-lisp guidance: a blank RET is enough to
      ;; make the prompt appear when the socket REPL has not printed it yet.
      (chief/cljd--register-repl-request proc nil :discard-output t)
      (process-send-string proc "\n")
      (chief/cljd-wait-for-repl-prompt proc)
      proc)))

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
    (when (chief/cljd--legacy-repl-process-p process)
      (kill-process process)
      (setq process nil))
    (unless (process-live-p process)
      (setq process (chief/cljd--create-repl-process buffer root port))
      (process-put process :chief/cljd-port port))
    (with-current-buffer buffer
      (goto-char (point-max))
      (when (fboundp 'evil-insert-state)
        (evil-insert-state)))
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

(defun chief/cljd-watch-ready-p (&optional buffer)
  "Return non-nil when BUFFER's Flutter watcher finished its initial reload."
  (if (not (eq (chief/cljd-project-kind) 'flutter))
      t
    (when-let* ((buffer (or buffer (chief/cljd--watch-buffer)))
                ((buffer-live-p buffer)))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-max))
          (when (re-search-backward chief/cljd-repl-port-regexp nil t)
            (re-search-forward chief/cljd-watch-reloaded-regexp nil t)))))))

(defun chief/cljd-wait-for-watch-ready (&optional timeout)
  "Wait until the current Flutter watcher is ready for interactive REPL evals."
  (if (not (eq (chief/cljd-project-kind) 'flutter))
      t
    (let* ((timeout (or timeout chief/cljd-repl-watch-ready-timeout))
           (deadline (+ (float-time) timeout))
           (settle-delay chief/cljd-repl-watch-settle-delay)
           (start (float-time))
           (process (chief/cljd--watch-process)))
      (while (and (< (float-time) deadline)
                  (and process (process-live-p process))
                  (not (or (chief/cljd-watch-ready-p)
                           (>= (- (float-time) start) settle-delay))))
        (accept-process-output process 0.25)
        (setq process (chief/cljd--watch-process)))
      (or (chief/cljd-watch-ready-p)
          (and process (process-live-p process))
          nil))))

(defun chief/cljd-start-repl ()
  "Start or connect to the current project's ClojureDart REPL."
  (interactive)
  (let ((repl-process (chief/cljd-repl-process))
        (watch-process (chief/cljd--watch-process))
        (lock-state (chief/cljd--repl-lock-state)))
    (when (chief/cljd--legacy-repl-process-p repl-process)
      (kill-process repl-process)
      (setq repl-process nil))
    (cond
     ((process-live-p repl-process)
      (pop-to-buffer-same-window (chief/cljd-repl-buffer)))
     ((when-let* ((port (chief/cljd-detect-repl-port)))
        (when (and watch-process
                   (process-live-p watch-process)
                   (not (chief/cljd-wait-for-watch-ready)))
          (user-error "Timed out waiting for the ClojureDart app to finish its initial reload"))
        (chief/cljd-connect-repl port)
        t))
     ((and watch-process (process-live-p watch-process))
      (if-let* ((port (chief/cljd-wait-for-repl-port)))
          (progn
            (unless (chief/cljd-wait-for-watch-ready)
              (user-error "Timed out waiting for the ClojureDart app to finish its initial reload"))
            (chief/cljd-connect-repl port))
        (user-error "Timed out waiting for the ClojureDart socket REPL")))
     ((plist-get lock-state :port)
      (user-error "ClojureDart REPL port %s is unavailable; restart the external watcher first"
                  (plist-get lock-state :port)))
     (t
      (chief/cljd-watch-dwim)
      (message "Waiting for ClojureDart REPL...")
      (if-let* ((port (chief/cljd-wait-for-repl-port)))
          (progn
            (unless (chief/cljd-wait-for-watch-ready)
              (user-error "Timed out waiting for the ClojureDart app to finish its initial reload"))
            (chief/cljd-connect-repl port))
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

(defun chief/cljd--wrap-in-current-namespace (string &optional namespace)
  "Prefix STRING with an `ns' form so it evaluates within NAMESPACE."
  (if-let* ((prefix (chief/cljd--namespace-prefix namespace)))
      (concat prefix (string-trim-right string "[\r\n]+"))
    string))

(defun chief/cljd--prepare-eval-string (string &optional _namespace)
  "Return STRING prepared for evaluation.
Namespace switching is handled as a separate REPL submission." 
  string)

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
  "Return the current buffer body prepared for ClojureDart REPL evaluation."
  (chief/cljd--buffer-body-string))

(defun chief/cljd--submit-to-repl (string &optional result-marker namespace)
  "Submit STRING to the CLJD REPL and associate RESULT-MARKER with its reply.
When NAMESPACE is non-nil, switch the REPL there first via a separate `ns' form."
  (let* ((process (chief/cljd--ensure-repl-process))
         (payload (string-trim-right string "[\r\n]+"))
         (ns-form (chief/cljd--namespace-prefix namespace)))
    (cond
     ((string-empty-p payload)
      (when ns-form
        (chief/cljd--send-repl-input
         process
         (string-trim-right ns-form "[\r\n]+"))))
     ((chief/cljd--string-starts-with-ns-form-p payload)
      (chief/cljd--send-repl-input process payload result-marker))
     (ns-form
      (chief/cljd--send-repl-input
       process
       (string-trim-right ns-form "[\r\n]+"))
      (chief/cljd-wait-for-repl-prompt process)
      (chief/cljd--send-repl-input process payload result-marker))
     (t
      (chief/cljd--send-repl-input process payload result-marker)))
    process))

(defun chief/cljd-last-sexp-bounds ()
  "Return the bounds of the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (cons (point) end))))

(defun chief/cljd-send-last-sexp ()
  "Send the sexp before point to the active ClojureDart REPL."
  (interactive)
  (pcase-let ((`(,start . ,end) (chief/cljd-last-sexp-bounds)))
    (chief/cljd-send-region start end)))

(defun chief/cljd-send-string (string)
  "Send STRING to the active ClojureDart socket REPL."
  (let* ((target-buffer (current-buffer))
         (result-marker (and (buffer-live-p target-buffer)
                             (with-current-buffer target-buffer
                               (copy-marker (line-end-position) t))))
         (namespace (unless (chief/cljd--string-starts-with-ns-form-p string)
                      (chief/cljd-buffer-namespace)))
         (payload (chief/cljd--prepare-eval-string string)))
    (chief/cljd--submit-to-repl payload result-marker namespace)))

(defun chief/cljd-send-region (start end)
  "Send the region from START to END to the socket REPL."
  (interactive "r")
  (let* ((string (buffer-substring-no-properties start end))
         (payload (chief/cljd--prepare-eval-string string))
         (namespace (unless (chief/cljd--string-starts-with-ns-form-p string)
                      (chief/cljd-buffer-namespace)))
         (result-marker (copy-marker end t)))
    (chief/cljd--submit-to-repl payload result-marker namespace)))

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
  (let ((string (chief/cljd-buffer-eval-string))
        (namespace (chief/cljd-buffer-namespace))
        (result-marker (copy-marker (point-max) t)))
    (chief/cljd--submit-to-repl string result-marker namespace)))

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

(define-key clojuredart-mode-map (kbd "C-x C-e") #'chief/cljd-send-last-sexp)
(define-key clojuredart-mode-map (kbd "C-M-x") #'chief/cljd-send-defun)
(define-key clojuredart-mode-map (kbd "C-c C-z") #'chief/cljd-start-repl)
(define-key clojuredart-mode-map (kbd "C-c C-k") #'chief/cljd-send-buffer)
(define-key clojuredart-mode-map (kbd "C-c C-r") #'chief/cljd-send-region)

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
