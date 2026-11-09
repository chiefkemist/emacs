;;; core-dap-test.el --- Native debugging regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'core-dap)
(require 'dap-mode)
(require 'cc-mode)
(require 'c-ts-mode)
(require 'lang-zig)
(require 'zig-mode)

(defun chief/test-call-with-breakpoint-source (function)
  "Call FUNCTION in an isolated file buffer with private breakpoint state."
  (let* ((root (make-temp-file "chief-breakpoints-" t))
         (source (expand-file-name "main.c" root))
         (dap-breakpoints-file (expand-file-name "breakpoints" root))
         (lsp--session (make-lsp-session)))
    (unwind-protect
        (progn
          (write-region "int main(void) {\n  return 0;\n}\n" nil source nil 'silent)
          (with-temp-buffer
            (setq buffer-file-name source)
            (insert-file-contents source)
            (set-buffer-modified-p nil)
            (goto-char (point-min))
            (forward-line 1)
            (funcall function)))
      (delete-directory root t))))

(ert-deftest chief/dap-breakpoint-toggle-displays-a-marker-and-confirms-both-states ()
  (chief/test-call-with-breakpoint-source
   (lambda ()
     (let (feedback)
       (cl-letf (((symbol-function 'message)
                  (lambda (format-string &rest args)
                    (setq feedback (apply #'format format-string args)))))
         (call-interactively #'chief/dap-breakpoint-toggle)
         (should (dap--get-breakpoint-at-point))
         (should (equal (substring-no-properties
                         (overlay-get (car dap-ui--breakpoint-overlays) 'after-string))
                        " [B?]"))
         (should (equal feedback "Breakpoint set: main.c:2"))
         (should (gethash buffer-file-name (dap--read-from-file dap-breakpoints-file)))
         (call-interactively #'chief/dap-breakpoint-toggle)
         (should-not (dap--get-breakpoint-at-point))
         (should-not dap-ui--breakpoint-overlays)
         (should (equal feedback "Breakpoint removed: main.c:2"))
         (should-not (gethash buffer-file-name (dap--read-from-file dap-breakpoints-file))))))))

(ert-deftest chief/dap-breakpoint-labels-are-visible-in-terminal-and-preserve-gui-fringes ()
  (dolist (graphical '(nil ns))
    (dolist (verified '(nil t))
      (with-temp-buffer
        (insert "return 0;\n")
        (cl-letf (((symbol-function 'window-system) (lambda (&optional _) graphical)))
          (let* ((remote (make-hash-table :test 'equal))
                 (_ (puthash "verified" verified remote))
                 (visuals (dap-ui--breakpoint-visuals '(:point 1) remote))
                 (overlay (dap-ui--make-overlay 1 10 visuals)))
            (should (equal (substring-no-properties (overlay-get overlay 'after-string))
                           (if verified " [B]" " [B?]")))
            (if graphical
                (should (eq (car (get-text-property
                                  0 'display (overlay-get overlay 'before-string)))
                            'left-fringe))
              (should-not (overlay-get overlay 'before-string)))))))))

(ert-deftest chief/dap-restores-file-breakpoint-markers-without-lsp ()
  (chief/test-call-with-breakpoint-source
   (lambda ()
     (let ((breakpoint (list :point (point))))
       (puthash buffer-file-name (list breakpoint) (dap--get-breakpoints))
       (should-not (bound-and-true-p lsp-mode))
       (chief/dap-restore-buffer-breakpoints)
       (should (eq (marker-buffer (plist-get breakpoint :marker)) (current-buffer))))
     (should dap-ui--breakpoint-overlays)
     ;; Simulate normal-mode discarding local bookkeeping during a reload.
     (kill-local-variable 'dap-ui--breakpoint-overlays)
     (chief/dap-restore-buffer-breakpoints)
     (should (= 1 (cl-count-if
                   (lambda (overlay) (overlay-get overlay 'dap-ui-overlay))
                   (overlays-in (point-min) (point-max)))))
     (should (memq #'chief/dap-restore-buffer-breakpoints find-file-hook))
     (should (memq #'chief/dap-restore-buffer-breakpoints after-change-major-mode-hook)))))

(ert-deftest chief/dap-is-not-disabled-when-an-lsp-buffer-is-unconfigured ()
  (should-not lsp-enable-dap-auto-configure)
  (with-temp-buffer
    (let ((lsp-unconfigure-hook nil)
          toggled)
      (cl-letf (((symbol-function 'dap-auto-configure-mode)
                 (lambda (&rest args) (setq toggled args))))
        (lsp-unconfig-buffer)
        (should-not toggled)))))

(ert-deftest chief/dap-preserves-lldb-launch-errors-without-top-level-message ()
  (let* ((response (json-parse-string
                    "{\"success\":false,\"command\":\"launch\",\"body\":{\"error\":{\"format\":\"No such file or directory: ~/project\"}}}"
                    :false-object nil))
         received)
    (funcall (chief/dap-preserve-adapter-error
              (lambda (_session _callback)
                (lambda (reply) (setq received reply)))
              nil)
             response)
    (should (equal (gethash "message" received)
                   "launch: No such file or directory: ~/project"))
    (should-not (gethash "message" response))
    (should (advice-member-p #'chief/dap-preserve-adapter-error
                            'dap--session-init-resp-handler))))

(ert-deftest chief/dap-preserves-existing-messages-and-success-responses ()
  (dolist (json '("{\"success\":true,\"command\":\"initialize\"}"
                  "{\"success\":false,\"command\":\"launch\",\"message\":\"Target unavailable\"}"))
    (let ((response (json-parse-string json :false-object nil))
          received)
      (funcall (chief/dap-preserve-adapter-error
                (lambda (_session _callback)
                  (lambda (reply) (setq received reply))) nil)
               response)
      (should (eq response received)))))

(ert-deftest chief/dap-ignores-late-initialized-events-after-launch-failure ()
  (let ((event (json-parse-string "{\"event\":\"initialized\"}")))
    (dolist (state '(failed terminated pending running))
      (let ((session (make-dap--debug-session :state state))
            configured)
        (chief/dap-ignore-stale-initialized-event
         (lambda (&rest _) (setq configured t)) session event)
        (should (eq configured (and (memq state '(pending running)) t)))))
    (should (advice-member-p #'chief/dap-ignore-stale-initialized-event 'dap--on-event))))

(ert-deftest chief/native-debug-templates-are-discoverable-and-reload-safe ()
  (chief/native-dap-register-templates)
  (chief/native-dap-register-templates)
  (dolist (language '("C" "C++" "Zig"))
    (dolist (action '("Build and debug current file" "Launch executable"))
      (let ((name (concat language " :: " action)))
        (should (= 1 (cl-count name dap-debug-template-configurations
                               :key #'car :test #'equal)))
        (should (equal (plist-get (cdr (assoc name dap-debug-template-configurations))
                                  :type)
                       "chief-native")))))
  (should (eq (gethash "chief-native" dap--debug-providers)
              #'chief/native-dap-populate)))

(ert-deftest chief/native-debug-recognizes-classic-and-treesit-modes ()
  (dolist (entry '((c-mode . "c") (c-ts-mode . "c")
                   (c++-mode . "cpp") (c++-ts-mode . "cpp")
                   (zig-mode . "zig") (emacs-lisp-mode . nil)))
    (with-temp-buffer
      (setq major-mode (car entry))
      (should (equal (chief/native-debug-language) (cdr entry))))))

(ert-deftest chief/native-debug-leader-and-local-leader-bindings ()
  (let ((map (evil-get-auxiliary-keymap general-override-mode-map 'normal)))
    (should (eq (lookup-key map (kbd "SPC d d")) #'dap-debug))
    (should (eq (lookup-key map (kbd "SPC d b")) #'chief/dap-breakpoint-toggle))
    (should (eq (lookup-key map (kbd "SPC d l")) #'dap-ui-breakpoints)))
  (dolist (keymap (list c-mode-map c++-mode-map c-ts-mode-map c++-ts-mode-map))
    (let ((map (evil-get-auxiliary-keymap keymap 'normal)))
      (should (eq (lookup-key map (kbd ", d d")) #'chief/native-debug-current-file))
      (should (eq (lookup-key map (kbd ", d e")) #'chief/native-debug-executable))))
  (let ((map (evil-get-auxiliary-keymap zig-mode-map 'normal)))
    (should (eq (lookup-key map (kbd ", d d")) #'chief/zig-debug-current-file))
    (should (eq (lookup-key map (kbd ", d t")) #'chief/zig-debug-test-at-point))
    (should (eq (lookup-key map (kbd ", d e")) #'chief/native-debug-executable))))

(ert-deftest chief/native-debug-finds-xcode-lldb-outside-path ()
  (let ((system-type 'darwin))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name) (and (equal name "xcrun") "/usr/bin/xcrun")))
              ((symbol-function 'process-lines)
               (lambda (&rest args)
                 (should (equal args '("xcrun" "-f" "lldb-dap")))
                 '("/Xcode/usr/bin/lldb-dap")))
              ((symbol-function 'file-executable-p)
               (lambda (path) (equal path "/Xcode/usr/bin/lldb-dap"))))
      (should (equal (chief/lldb-dap-command) '("/Xcode/usr/bin/lldb-dap"))))))

(ert-deftest chief/native-debug-lldb-launches-the-binary-without-stopping-in-dyld ()
  (cl-letf (((symbol-function 'chief/lldb-dap-command)
             (lambda () '("/adapter/lldb-dap"))))
    (let ((configuration (chief/native-dap-configuration
                          "/project/app" "C test" "/project/" ["c"] ["a b"])))
      (should (equal (plist-get configuration :type) "lldb-vscode"))
      (should (equal (plist-get configuration :program) "/project/app"))
      (should (equal (plist-get configuration :dap-server-path) '("/adapter/lldb-dap")))
      (should (equal (plist-get configuration :args) ["a b"]))
      (should (eq (plist-get configuration :stopOnEntry) :json-false)))))

(ert-deftest chief/native-debug-expands-cwd-and-program-before-sending-to-adapter ()
  (cl-letf (((symbol-function 'chief/lldb-dap-command)
             (lambda () '("/adapter/lldb-dap"))))
    (dolist (language '("c" "cpp" "zig"))
      (let ((configuration (chief/native-dap-configuration
                            "bin/my app" "test" "~/my project/" (vector language))))
        (should (equal (plist-get configuration :cwd)
                       (expand-file-name "~/my project/")))
        (should (equal (plist-get configuration :program)
                       (expand-file-name "~/my project/bin/my app")))))
    (should-not (plist-get (chief/native-dap-configuration nil "build" "~/" ["c"])
                           :program))))

(ert-deftest chief/native-debug-supports-gdb-fallback ()
  (cl-letf (((symbol-function 'chief/lldb-dap-command) #'ignore)
            ((symbol-function 'executable-find)
             (lambda (name) (and (equal name "gdb") "/adapter/gdb"))))
    (let ((configuration (chief/native-dap-configuration
                          "/project/app" "C test" "/project/" ["c"])))
      (should (equal (plist-get configuration :type) "gdb"))
      (should (equal (plist-get configuration :target) "/project/app"))
      (should (equal (plist-get configuration :dap-server-path) dap-gdb-debug-program)))))

(ert-deftest chief/native-debug-reports-missing-adapter ()
  (cl-letf (((symbol-function 'chief/lldb-dap-command) #'ignore)
            ((symbol-function 'executable-find) #'ignore))
    (should-error (chief/native-dap-configuration "/app" "test" "/" ["c"])
                  :type 'user-error)))

(ert-deftest chief/native-debug-builds-with-correct-compilers-and-debug-info ()
  (let* ((root (make-temp-file "chief native debug " t))
         (chief/var-directory (file-name-as-directory root))
         (chief/native-c-command '("test-cc"))
         (chief/native-cpp-command '("test-c++"))
         (chief/native-debug-extra-args '("-DTEST=1")))
    (unwind-protect
        (cl-letf (((symbol-function 'chief/lldb-dap-command)
                   (lambda () '("/adapter/lldb-dap")))
                  ((symbol-function 'executable-find) (lambda (name) name)))
          (dolist (entry '((c-mode "c" "test-cc" "main.c")
                           (c++-ts-mode "cpp" "test-c++" "main.cpp")))
            (with-temp-buffer
              (setq major-mode (nth 0 entry)
                    buffer-file-name (expand-file-name (nth 3 entry) root))
              (let* ((configuration (chief/native-debug-build-configuration (nth 1 entry)))
                     (command (plist-get configuration :chief-build-command))
                     (binary (plist-get configuration :program)))
                (should (equal (car command) (nth 2 entry)))
                (should (equal (cdr command)
                               (list "-g" "-O0" buffer-file-name "-o" binary "-DTEST=1")))
                (should (file-in-directory-p binary root))
                (should-not (equal binary buffer-file-name))
                (should (equal (plist-get configuration :cwd)
                               (file-name-as-directory root)))))))
      (delete-directory root t))))

(ert-deftest chief/native-debug-zig-build-uses-debug-optimization ()
  (let* ((root (make-temp-file "chief-zig-debug-" t))
         (chief/var-directory (file-name-as-directory root)))
    (unwind-protect
        (with-temp-buffer
          (setq major-mode 'zig-mode
                buffer-file-name (expand-file-name "main.zig" root))
          (insert "pub fn main() void {}\n")
          (set-buffer-modified-p nil)
          (cl-letf (((symbol-function 'chief/lldb-dap-command)
                     (lambda () '("/adapter/lldb-dap")))
                    ((symbol-function 'executable-find) (lambda (name) name)))
            (let* ((configuration (chief/native-debug-build-configuration "zig"))
                   (binary (plist-get configuration :program)))
              (should (equal (plist-get configuration :chief-build-command)
                             (list "zig" "build-exe" buffer-file-name "-O" "Debug"
                                   (concat "-femit-bin=" binary)))))))
      (delete-directory root t))))

(ert-deftest chief/native-debug-zig-rejects-files-without-main ()
  (with-temp-buffer
    (setq major-mode 'zig-mode buffer-file-name "/tmp/test.zig")
    (cl-letf (((symbol-function 'chief/lldb-dap-command)
               (lambda () '("/adapter/lldb-dap"))))
      (should-error (chief/native-debug-build-configuration "zig") :type 'user-error))))

(ert-deftest chief/native-debug-rejects-wrong-language-and-non-file-buffers ()
  (with-temp-buffer
    (should-error (chief/native-debug-build-configuration "c") :type 'user-error)
    (setq major-mode 'c-mode buffer-file-name "/tmp/main.c")
    (should-error (chief/native-debug-build-configuration "cpp") :type 'user-error)))

(ert-deftest chief/native-debug-launch-executable-prompts-for-binary-and-arguments ()
  (let ((program (make-temp-file "chief-native-app-")))
    (unwind-protect
        (progn
          (set-file-modes program #o700)
          (cl-letf (((symbol-function 'chief/lldb-dap-command)
                     (lambda () '("/adapter/lldb-dap")))
                    ((symbol-function 'read-file-name) (lambda (&rest _) program))
                    ((symbol-function 'read-directory-name) (lambda (&rest _) "~/"))
                    ((symbol-function 'read-string) (lambda (&rest _) "\"two words\" --flag")))
            (let ((configuration (chief/native-dap-populate
                                  '(:name "C++ executable" :chief-language "cpp"))))
              (should (equal (plist-get configuration :program) program))
              (should (equal (plist-get configuration :args) ["two words" "--flag"]))
              (should (equal (plist-get configuration :cwd) (expand-file-name "~/"))))))
      (delete-file program))))

(ert-deftest chief/native-debug-build-callback-requires-success-and-quotes-paths ()
  (dolist (status '("finished\n" "exited abnormally with code 1\n" "killed\n"))
    (let ((buffer (generate-new-buffer " *native-debug-test*"))
          captured-command finish launched)
      (unwind-protect
          (cl-letf (((symbol-function 'compilation-start)
                     (lambda (command &rest _)
                       (setq captured-command command)
                       buffer)))
            (chief/native-debug-build-and-start
             '(:name "test" :cwd "/tmp/" :program "/tmp/my app"
               :chief-build-command ("cc" "-g" "/tmp/my source.c" "-o" "/tmp/my app"))
             (lambda (configuration) (setq launched configuration)))
            (should (equal captured-command
                           (mapconcat #'shell-quote-argument
                                      '("cc" "-g" "/tmp/my source.c" "-o" "/tmp/my app") " ")))
            (with-current-buffer buffer
              (setq finish (car compilation-finish-functions))
              (funcall finish buffer status)
              (should-not (memq finish compilation-finish-functions)))
            (if (equal status "finished\n")
                (progn
                  (should (equal (plist-get launched :program) "/tmp/my app"))
                  (should-not (plist-member launched :chief-build-command)))
              (should-not launched)))
        (kill-buffer buffer)))))

(provide 'core-dap-test)
;;; core-dap-test.el ends here
