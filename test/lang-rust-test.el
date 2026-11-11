;;; lang-rust-test.el --- Cargo and standalone Rust regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'lang-rust)
(require 'lsp-rust)
(require 'rust-ts-mode)

(defun chief/test-with-rust-source (function &optional source)
  "Call FUNCTION in a private standalone source buffer, with its ROOT directory."
  (let* ((root (file-name-as-directory (make-temp-file "chief rust " t)))
         (chief/var-directory (expand-file-name "cache/" root))
         (file (expand-file-name "main.rs" root)))
    (unwind-protect
        (progn
          (write-region (or source "fn main() { println!(\"hello\"); }\n") nil file nil 'silent)
          (with-temp-buffer
            (setq buffer-file-name file default-directory root major-mode 'rust-ts-mode)
            (insert-file-contents file)
            (set-buffer-modified-p nil)
            (funcall function root)))
      (delete-directory root t))))

(ert-deftest chief/rust-standalone-root-does-not-inherit-a-generic-project ()
  (chief/test-with-rust-source
   (lambda (root)
     (cl-letf (((symbol-function 'chief/project-current-root) (lambda (&rest _) "/tmp/")))
       (should (chief/rust-standalone-p))
       (should-not (chief/rust-crate-root))
       (should-not (chief/rust-manifest-path))
       (should (equal (chief/rust-project-root) root))
       (should-error (chief/rust-workspace-directory) :type 'user-error)))))

(ert-deftest chief/rust-detached-options-include-siblings-not-nested-cargo-projects ()
  (chief/test-with-rust-source
   (lambda (root)
     (write-region "pub fn helper() {}\n" nil (expand-file-name "helper.rs" root) nil 'silent)
     (make-directory (expand-file-name "folder.rs" root))
     (make-directory (expand-file-name "cargo/src" root) t)
     (write-region "[package]\nname = \"nested\"\nversion = \"0.1.0\"\n"
                   nil (expand-file-name "cargo/Cargo.toml" root) nil 'silent)
     (write-region "fn main() {}\n" nil (expand-file-name "cargo/src/main.rs" root) nil 'silent)
     (let* ((original '(:linkedProjects [] :diagnostics (:enable t)))
            (options (chief/rust-analyzer-standalone-options original))
            (files (plist-get options :detachedFiles)))
       (should (equal files (vector buffer-file-name (expand-file-name "helper.rs" root))))
       (should-not (plist-member original :detachedFiles))
       (should (equal (plist-get (lsp-rust-analyzer--make-init-options) :detachedFiles) files))))))

(ert-deftest chief/rust-cargo-and-explicit-projects-keep-original-lsp-options ()
  (chief/test-with-rust-source
   (lambda (root)
     (let ((explicit '(:linkedProjects ["/custom/rust-project.json"])))
       (should (eq explicit (chief/rust-analyzer-standalone-options explicit))))
     (write-region "[package]\nname = \"sample\"\nversion = \"0.1.0\"\n"
                   nil (expand-file-name "Cargo.toml" root) nil 'silent)
     (should-not (chief/rust-standalone-p))
     (should (equal (chief/rust-crate-root) root))
     (let ((options '(:linkedProjects [])))
       (should (eq options (chief/rust-analyzer-standalone-options options)))))))

(ert-deftest chief/rust-rust-project-json-is-not-replaced-with-detached-crates ()
  (chief/test-with-rust-source
   (lambda (root)
     (write-region "{\"crates\":[]}" nil (expand-file-name "rust-project.json" root) nil 'silent)
     (should-not (chief/rust-standalone-p))
     (should-not (plist-get (lsp-rust-analyzer--make-init-options) :detachedFiles)))))

(ert-deftest chief/rust-debug-default-and-templates-work-without-cargo-runnables ()
  (let ((map (evil-get-auxiliary-keymap rust-ts-mode-map 'normal)))
    (should (eq (lookup-key map (kbd ", d d")) #'chief/rust-debug-dwim))
    (should (eq (lookup-key map (kbd ", d R")) #'chief/rust-debug-runnable)))
  (dolist (name '("Rust :: Debug standalone file" "Rust :: Debug standalone tests"))
    (should (assoc name dap-debug-template-configurations)))
  (should (eq (gethash "chief-rust" dap--debug-providers) #'chief/rust-dap-populate)))

(ert-deftest chief/rust-default-debug-preserves-cargo-runnables-and-uses-rustc-outside ()
  (chief/test-with-rust-source
   (lambda (root)
     (let (selected)
       (cl-letf (((symbol-function 'chief/rust-debug-runnable) (lambda () (setq selected 'cargo)))
                 ((symbol-function 'chief/rust-debug-current-file) (lambda () (setq selected 'standalone))))
         (chief/rust-debug-dwim)
         (should (eq selected 'standalone))
         (write-region "[package]\nname = \"sample\"\nversion = \"0.1.0\"\n"
                       nil (expand-file-name "Cargo.toml" root) nil 'silent)
         (chief/rust-debug-dwim)
         (should (eq selected 'cargo)))))))

(ert-deftest chief/rust-standalone-dap-builds-before-launch-with-explicit-adapter ()
  (chief/test-with-rust-source
   (lambda (root)
     (cl-letf (((symbol-function 'chief/lldb-dap-command) (lambda () '("/adapter/lldb-dap")))
               ((symbol-function 'executable-find) (lambda (name) name)))
       (dolist (tests '(nil t))
         (let (launch)
           (cl-letf (((symbol-function 'chief/native-debug-build-and-start)
                      (lambda (configuration _start) (setq launch configuration))))
             (funcall (chief/rust-dap-populate
                       (list :chief-tests tests :chief-test-path "tests::selected" :chief-exact t))
                      #'ignore))
           (let ((command (plist-get launch :chief-build-command)))
             (should (equal (car command) "rustc"))
             (should (member "-g" command))
             (should (member "opt-level=0" command))
             (should (eq (not (null (member "--test" command))) tests)))
           (should (file-in-directory-p (plist-get launch :program) root))
           (should (equal (plist-get launch :cwd) root))
           (should (equal (plist-get launch :dap-server-path) '("/adapter/lldb-dap")))
           (should (equal (plist-get launch :args)
                          (if tests ["tests::selected" "--exact" "--nocapture"] [])))))))))

(ert-deftest chief/rust-dap-loads-toolchain-printers-with-quoted-paths ()
  (cl-letf (((symbol-function 'chief/native-dap-configuration)
             (lambda (&rest _) '(:type "lldb-vscode")))
            ((symbol-function 'executable-find) (lambda (name) name))
            ((symbol-function 'process-lines)
             (lambda (&rest args)
               (should (equal args '("rustc" "--print" "sysroot")))
               '("/tool chain")))
            ((symbol-function 'file-readable-p) (lambda (_path) t)))
    (should (equal (plist-get (chief/rust-dap-configuration "/app" "test" "/tmp/") :initCommands)
                   ["command script import \"/tool chain/lib/rustlib/etc/lldb_lookup.py\""]))))

(ert-deftest chief/rust-library-checks-do-not-require-main ()
  (chief/test-with-rust-source
   (lambda (_root)
     (let ((command (chief/rust-rustc-command buffer-file-name "/tmp/library.rmeta")))
       (should (member "--crate-type=lib" command)))
     (should-not (member "--crate-type=lib"
                         (chief/rust-rustc-command buffer-file-name "/tmp/tests" t)))
     (should-error (chief/rust-run-standalone-file) :type 'user-error))
   "pub fn add(a: i32, b: i32) -> i32 { a + b }\n"))

(ert-deftest chief/rust-default-build-check-test-and-lint-dispatch-outside-cargo ()
  (chief/test-with-rust-source
   (lambda (_root)
     (let (calls)
       (cl-letf (((symbol-function 'chief/rust-build-standalone-file)
                  (lambda (&rest args) (push args calls)))
                 ((symbol-function 'chief/rust-test-standalone-file)
                  (lambda () (push 'tests calls))))
         (chief/rust-build-project)
         (chief/rust-check-project)
         (chief/rust-test-project)
         (chief/rust-lint-project)
         (should (equal (reverse calls) '(nil (t) tests (t "clippy-driver")))))))))

(ert-deftest chief/rust-rustfmt-uses-unsaved-text-and-never-saves-source ()
  (chief/test-with-rust-source
   (lambda (_root)
     (erase-buffer)
     (insert "fn main(){println!(\"unsaved\");}\n")
     (cl-letf (((symbol-function 'call-process-region)
                (lambda (_start _end program _delete destination _display &rest args)
                  (should (equal program "rustfmt"))
                  (should (member "stdout" args))
                  (with-current-buffer (car destination)
                    (insert "fn main() {\n    println!(\"unsaved\");\n}\n"))
                  0)))
       (chief/rust-format-project))
     (should (string-match-p "    println!" (buffer-string)))
     (should (buffer-modified-p))
     (should-not (with-temp-buffer
                   (insert-file-contents (expand-file-name "main.rs" default-directory))
                   (string-match-p "unsaved" (buffer-string)))))))

(ert-deftest chief/rust-rustfmt-failure-preserves-buffer-text ()
  (chief/test-with-rust-source
   (lambda (_root)
     (let ((before (buffer-string)))
       (cl-letf (((symbol-function 'call-process-region) (lambda (&rest _) 1)))
         (should-error (chief/rust-format-buffer) :type 'user-error))
       (should (equal before (buffer-string)))))))

(ert-deftest chief/rust-async-build-keeps-source-directory-and-callback-buffer ()
  (chief/test-with-rust-source
   (lambda (root)
     (let* ((source (current-buffer))
            (output-buffer " *chief-rust-cwd-test*")
            (default-directory temporary-file-directory)
            callback-buffer output
            (process (chief/rust-run-command-async
                      '("/bin/pwd") root output-buffer
                      (lambda (text) (setq callback-buffer (current-buffer) output text))))
            (deadline (+ (float-time) 5)))
       (unwind-protect
           (progn
             (while (and (not output) (< (float-time) deadline))
               (accept-process-output process 0.05))
             (should (eq callback-buffer source))
             (should (equal (file-truename (string-trim output)) (directory-file-name (file-truename root)))))
         (when (process-live-p process) (delete-process process))
         (when (get-buffer output-buffer) (kill-buffer output-buffer)))))))

(provide 'lang-rust-test)
;;; lang-rust-test.el ends here
