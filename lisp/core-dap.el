;;; core-dap.el --- Debug adapter protocol setup -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'compile)
(require 'subr-x)

(defvar dap-gdb-debug-program)

(declare-function dap-debug "dap-mode" (debug-args))
(declare-function dap-register-debug-provider "dap-mode" (language-id provider))
(declare-function dap-register-debug-template "dap-mode" (name settings))

(defcustom chief/native-c-command '("cc")
  "Compiler command used to debug a standalone C file."
  :type '(repeat string)
  :group 'chief)

(defcustom chief/native-cpp-command '("c++")
  "Compiler command used to debug a standalone C++ file."
  :type '(repeat string)
  :group 'chief)

(defcustom chief/native-debug-extra-args nil
  "Extra compiler/linker arguments for standalone C and C++ debug builds.
For multi-file projects, build with debug symbols using the project's build
system, then select a Launch executable template instead."
  :type '(repeat string)
  :group 'chief)

(defun chief/lldb-dap-command ()
  "Return the preferred LLDB DAP command list when available."
  (or (when-let* ((lldb-dap (executable-find "lldb-dap")))
        (list lldb-dap))
      (when-let* ((lldb-vscode (executable-find "lldb-vscode")))
        (list lldb-vscode))
      (when (and (eq system-type 'darwin)
                 (executable-find "xcrun"))
        (ignore-errors
          (when-let* ((output (car (process-lines "xcrun" "-f" "lldb-dap")))
                      (path (string-trim output))
                      ((file-executable-p path)))
            (list path))))))

(defun chief/dap-breakpoint-toggle ()
  "Toggle a source breakpoint with visible confirmation."
  (interactive)
  (unless buffer-file-name
    (user-error "Open a source file before setting a breakpoint"))
  (require 'dap-mode)
  (unless (bound-and-true-p dap-mode)
    (dap-mode 1))
  (unless (bound-and-true-p dap-ui-mode)
    (dap-ui-mode 1))
  (dap--after-open)
  (dap-breakpoint-toggle)
  (message "Breakpoint %s: %s:%d"
           (if (dap--get-breakpoint-at-point) "set" "removed")
           (file-name-nondirectory buffer-file-name)
           (line-number-at-pos)))

(defun chief/dap-breakpoint-overlay-label (overlay)
  "Give breakpoint OVERLAY a visible label in GUI and terminal frames."
  (when (overlayp overlay)
    (let ((face (overlay-get overlay 'face)))
      (when (memq face '(dap-ui-pending-breakpoint-face dap-ui-verified-breakpoint-face))
        (let ((verified (eq face 'dap-ui-verified-breakpoint-face)))
          ;; dap-ui's default line faces are empty, and its fringe bitmaps do
          ;; not render in terminals.  Keep the fringe and add a text fallback.
          (overlay-put overlay 'after-string
                       (propertize (if verified " [B]" " [B?]")
                                   'face (if verified 'success 'warning)
                                   'help-echo (if verified "Verified breakpoint"
                                                "Breakpoint not yet verified; SPC d l lists breakpoints")))))))
  overlay)

(defun chief/dap-restore-buffer-breakpoints ()
  "Restore source markers without requiring an LSP connection."
  (when (and buffer-file-name
             (bound-and-true-p dap-mode)
             (gethash buffer-file-name (dap--get-breakpoints)))
    ;; Changing major modes loses dap-ui's buffer-local overlay list, but not
    ;; the overlays themselves.  Do not duplicate labels after config reloads.
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (and (overlay-get overlay 'dap-ui-overlay)
                 (memq (overlay-get overlay 'face)
                       '(dap-ui-pending-breakpoint-face dap-ui-verified-breakpoint-face)))
        (delete-overlay overlay)))
    (dap--after-open)))

(add-hook 'find-file-hook #'chief/dap-restore-buffer-breakpoints)
(add-hook 'after-change-major-mode-hook #'chief/dap-restore-buffer-breakpoints)

(defun chief/dap-preserve-adapter-error (original session &optional callback)
  "Preserve DAP error details omitted from the top-level response message.
ORIGINAL constructs the initialization/launch handler for SESSION and CALLBACK."
  (let ((handler (funcall original session callback)))
    (lambda (response)
      (if (or (gethash "success" response) (gethash "message" response))
          (funcall handler response)
        (let* ((copy (copy-hash-table response))
               (body (gethash "body" response))
               (details (and (hash-table-p body) (gethash "error" body)))
               (description (and (hash-table-p details) (gethash "format" details))))
          (puthash "message"
                   (format "%s: %s" (or (gethash "command" response) "request")
                           (or description "Adapter rejected the request without error details"))
                   copy)
          (funcall handler copy))))))

(defun chief/dap-ignore-stale-initialized-event (original session event)
  "Do not configure SESSION after a failed launch when EVENT arrives late."
  (unless (and (equal (gethash "event" event) "initialized")
               (not (dap--session-running session)))
    (funcall original session event)))

(defun chief/native-debug-language ()
  "Return the native debug language for the current buffer, if supported."
  (cond
   ((derived-mode-p 'c++-mode 'c++-ts-mode) "cpp")
   ((derived-mode-p 'c-mode 'c-ts-mode) "c")
   ((derived-mode-p 'zig-mode) "zig")))

(defun chief/native-dap-configuration (program name cwd languages &optional args)
  "Return a native DAP launch for PROGRAM named NAME in CWD.
LANGUAGES is a vector of source languages; ARGS are program arguments."
  (require 'dap-mode)
  (let* ((cwd (file-name-as-directory (expand-file-name (or cwd default-directory))))
         (program (and program (expand-file-name program cwd)))
         (lldb-command (chief/lldb-dap-command))
         (type (cond
                (lldb-command
                 (require 'dap-lldb)
                 "lldb-vscode")
                ((executable-find "gdb")
                 (require 'dap-gdb)
                 "gdb")
                (t
                 (user-error
                  "No native DAP adapter found: install LLVM's lldb-dap or GDB 14+ with DAP support")))))
    (append
     (list :type type :request "launch" :name name
           :program program :cwd cwd :args (or args [])
           :sourceLanguages languages)
     (if lldb-command
         ;; On macOS, stopOnEntry stops in dyld rather than in source code.
         ;; Run to the user's source breakpoints instead.
         (list :dap-server-path lldb-command :stopOnEntry :json-false)
       (list :dap-server-path dap-gdb-debug-program
             :target program)))))

(defun chief/native-debug-build-configuration (language)
  "Prepare a standalone debug build and launch for LANGUAGE.
The returned configuration includes a private build command, which must
succeed before launching the executable."
  (unless (and buffer-file-name
               (equal language (chief/native-debug-language)))
    (user-error "Open a %s source file to use this template"
                (pcase language ("cpp" "C++") ("c" "C") (_ "Zig"))))
  (when (file-remote-p buffer-file-name)
    (user-error "Native debug builds currently require a local source file"))
  (let* ((file (expand-file-name buffer-file-name))
         (directory (file-name-directory file))
         (name (format "%s file :: %s"
                       (pcase language ("cpp" "C++") ("c" "C") (_ "Zig"))
                       (file-name-nondirectory file)))
         ;; Check the adapter before saving or building anything.
         (configuration (chief/native-dap-configuration
                         nil name directory (vector language)))
         binary command)
    (if (equal language "zig")
        (progn
          (require 'lang-zig)
          (unless (chief/zig-current-file-has-main-p)
            (user-error "Current file does not define `pub fn main'; use , d t for a Zig test"))
          (setq binary (chief/zig-artifact-path "-debug")
                command (chief/zig-command
                         "build-exe" file "-O" "Debug"
                         (concat "-femit-bin=" binary))))
      (let* ((compiler (if (equal language "cpp")
                           chief/native-cpp-command
                         chief/native-c-command))
             (cache (expand-file-name
                     "native-debug/"
                     (if (boundp 'chief/var-directory)
                         chief/var-directory
                       user-emacs-directory))))
        (unless (and compiler (executable-find (car compiler)))
          (user-error "Native compiler is not available: %s" (car compiler)))
        (make-directory cache t)
        (setq binary (expand-file-name
                      (concat (replace-regexp-in-string
                               "[^[:alnum:]_.-]" "_" (file-name-base file))
                              "-" (substring (secure-hash 'sha1 file) 0 12)
                              (if (eq system-type 'windows-nt) ".exe" ""))
                      cache)
              command (append compiler (list "-g" "-O0" file "-o" binary)
                              chief/native-debug-extra-args))))
    (when (buffer-modified-p)
      (save-buffer))
    (setq configuration (plist-put configuration :program binary))
    (when (equal (plist-get configuration :type) "gdb")
      (setq configuration (plist-put configuration :target binary)))
    (plist-put configuration :chief-build-command command)))

(defun chief/native-debug-build-and-start (configuration start)
  "Build CONFIGURATION, then call DAP's START callback only on success."
  (let* ((launch (copy-sequence configuration))
         (command (plist-get launch :chief-build-command))
         (default-directory (plist-get launch :cwd))
         finish)
    (cl-remf launch :chief-build-command)
    (setq finish
          (lambda (_buffer status)
            (remove-hook 'compilation-finish-functions finish t)
            (when (string= status "finished\n")
              (funcall start launch))))
    ;; Use DAP's asynchronous provider API rather than :dap-compilation: the
    ;; pinned dap-mode passes a bare plist to its list-of-tasks runner.
    (with-current-buffer
        (compilation-start
         (mapconcat #'shell-quote-argument command " ")
         'compilation-mode
         (lambda (_) (format "*native debug build %s*" (plist-get launch :name))))
      (add-hook 'compilation-finish-functions finish nil t))))

(defun chief/native-dap-populate (configuration)
  "Populate a named C, C++, or Zig debug CONFIGURATION."
  (let ((language (plist-get configuration :chief-language)))
    (if (equal (plist-get configuration :chief-action) "file")
        (apply-partially #'chief/native-debug-build-and-start
                         (chief/native-debug-build-configuration language))
      (let* ((directory (or (plist-get configuration :cwd) default-directory))
             (program (expand-file-name
                       (or (plist-get configuration :program)
                           (read-file-name "Executable to debug: " directory nil t))
                       directory))
             (cwd (or (plist-get configuration :cwd)
                      (read-directory-name "Working directory: " directory nil t)))
             (args (if (plist-member configuration :args)
                       (plist-get configuration :args)
                     (vconcat (split-string-and-unquote
                               (read-string "Program arguments: "))))))
        (when (file-remote-p program)
          (user-error "Native debugging currently requires a local executable"))
        (unless (and (file-regular-p program) (file-executable-p program))
          (user-error "Not an executable file: %s" program))
        (chief/native-dap-configuration
         program (plist-get configuration :name) cwd (vector language) args)))))

(defun chief/native-debug-current-file ()
  "Build the current standalone C, C++, or Zig file and debug it."
  (interactive)
  (let ((language (or (chief/native-debug-language)
                      (user-error "Open a C, C++, or Zig source file first"))))
    (require 'dap-mode)
    (dap-debug (list :type "chief-native" :chief-language language
                     :chief-action "file"))))

(defun chief/native-debug-executable ()
  "Select a compiled C, C++, or Zig executable and debug it."
  (interactive)
  (let ((language (or (chief/native-debug-language)
                      (completing-read "Source language: " '("c" "cpp" "zig") nil t))))
    (require 'dap-mode)
    (dap-debug (list :type "chief-native" :chief-language language
                     :name "Native :: Launch executable"))))

(defun chief/native-dap-register-templates ()
  "Expose C, C++, and Zig explicitly in the `dap-debug' picker."
  (dap-register-debug-provider "chief-native" #'chief/native-dap-populate)
  (dolist (language '(("C" . "c") ("C++" . "cpp") ("Zig" . "zig")))
    (dap-register-debug-template
     (concat (car language) " :: Build and debug current file")
     (list :type "chief-native" :chief-language (cdr language) :chief-action "file"))
    (dap-register-debug-template
     (concat (car language) " :: Launch executable")
     (list :type "chief-native" :chief-language (cdr language)))))

(with-eval-after-load 'cc-mode
  (chief/local-leader-def
    :keymaps '(c-mode-map c++-mode-map)
    "d" '(:ignore t :which-key "debug")
    "dd" #'chief/native-debug-current-file
    "de" #'chief/native-debug-executable))

(with-eval-after-load 'c-ts-mode
  (chief/local-leader-def
    :keymaps '(c-ts-mode-map c++-ts-mode-map)
    "d" '(:ignore t :which-key "debug")
    "dd" #'chief/native-debug-current-file
    "de" #'chief/native-debug-executable))

(use-package dap-mode
  :after lsp-mode
  :commands dap-debug
  :init
  ;; This module owns the global debugger UI.  Closing/reinitializing an LSP
  ;; buffer must not turn it off (lsp-unconfig-buffer otherwise does so).
  (setq lsp-enable-dap-auto-configure nil)
  :config
  (advice-add 'dap--session-init-resp-handler :around #'chief/dap-preserve-adapter-error)
  (advice-add 'dap--on-event :around #'chief/dap-ignore-stale-initialized-event)
  (setq dap-auto-configure-features '(sessions locals controls tooltip repl))
  (dap-auto-configure-mode 1)
  (require 'dap-dlv-go)
  (require 'dap-netcore)
  (require 'dap-node)
  (require 'dap-python)
  (when-let* ((lldb-command (chief/lldb-dap-command)))
    (setq dap-lldb-debug-program lldb-command)
    (require 'dap-lldb))
  (when (executable-find "gdb")
    (require 'dap-gdb))
  (chief/native-dap-register-templates)
  (setq dap-python-debugger 'debugpy)
  (global-set-key (kbd "<f5>") #'dap-debug)
  (global-set-key (kbd "<f6>") #'dap-continue)
  (global-set-key (kbd "<f7>") #'dap-next)
  (global-set-key (kbd "<f8>") #'dap-step-in)
  (global-set-key (kbd "<f9>") #'dap-step-out))

(chief/leader-def
  "d" '(:ignore t :which-key "debug")
  "dd" #'dap-debug
  "db" #'chief/dap-breakpoint-toggle
  "dB" #'dap-breakpoint-condition
  "dl" #'dap-ui-breakpoints
  "dc" #'dap-continue
  "di" #'dap-step-in
  "do" #'dap-step-out
  "dn" #'dap-next
  "dr" #'dap-debug-restart
  "dq" #'dap-disconnect
  "du" #'dap-ui-repl)

(use-package dap-ui
  :straight nil
  :after dap-mode
  :config
  (advice-add 'dap-ui--make-overlay :filter-return #'chief/dap-breakpoint-overlay-label)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (chief/dap-restore-buffer-breakpoints))))

(provide 'core-dap)
;;; core-dap.el ends here
