;;; ob-rust-script.el --- Org Babel Rust execution via rust-script -*- lexical-binding: t; -*-

(require 'ob)
(require 'org)
(require 'subr-x)

(declare-function chief/org-babel-execution-directory "core-org-babel" (params))
(declare-function chief/org-babel-write-temp-file "core-org-babel" (body extension params prefix))
(declare-function chief/org-babel-format-result "core-org-babel" (result params))
(declare-function chief/org-babel-run-command "core-org-babel" (command params))

(defun chief/org-babel-rust-script-executable ()
  "Return the `rust-script' executable, including the usual Cargo bin fallback."
  (or (executable-find "rust-script")
      (let ((cargo-bin (expand-file-name "~/.cargo/bin/rust-script")))
        (and (file-executable-p cargo-bin) cargo-bin))
      (user-error "rust-script is not available on PATH; install it with `cargo install rust-script'")))

(defun chief/org-babel-rust-param-arguments (key params)
  "Return shell-style argument list for Babel header KEY in PARAMS."
  (when-let* ((value (cdr (assq key params)))
              (text (string-trim (format "%s" value))))
    (unless (string-empty-p text)
      (split-string-and-unquote text))))

(defun chief/org-babel-rust-script-command (file params)
  "Return the `rust-script' command for FILE and Babel PARAMS."
  (let ((directory (chief/org-babel-execution-directory params)))
    (append
     (list (chief/org-babel-rust-script-executable)
           "--base-path" directory)
     (chief/org-babel-rust-param-arguments :rust-script-args params)
     (list file)
     (chief/org-babel-rust-param-arguments :cmdline params))))

(defun chief/org-babel-rust-param-false-p (value)
  "Return non-nil when Babel header VALUE means false."
  (member (downcase (string-trim (format "%s" value)))
          '("no" "nil" "false" "0" "off" "sync")))

(defun chief/org-babel-rust-async-p (params)
  "Return non-nil when Rust Babel execution should be asynchronous.
Rust uses asynchronous execution by default because `rust-script' may compile
Cargo dependencies.  Use `:async no' on a block to force synchronous execution."
  (not (chief/org-babel-rust-param-false-p
        (or (cdr (assq :async params)) "yes"))))

(defun chief/org-babel-rust-command-output (stdout-buffer stderr-buffer exit-status)
  "Return combined Rust process output from STDOUT-BUFFER and STDERR-BUFFER."
  (let ((stdout (with-current-buffer stdout-buffer
                  (string-trim-right (buffer-string))))
        (stderr (with-current-buffer stderr-buffer
                  (string-trim-right (buffer-string)))))
    (if (zerop exit-status)
        stdout
      (string-trim-right
       (concat (format "rust-script failed with exit code %s" exit-status)
               (unless (string-empty-p stdout)
                 (concat "\n\n[stdout]\n" stdout))
               (unless (string-empty-p stderr)
                 (concat "\n\n[stderr]\n" stderr)))))))

(defun chief/org-babel-rust-insert-async-result
    (buffer marker result result-params info lang exec-time)
  "Insert async Rust Babel RESULT back into BUFFER at source block MARKER."
  (when (and (buffer-live-p buffer)
             (marker-buffer marker))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char marker)
          (let ((inhibit-read-only t))
            (org-babel-insert-result result result-params info nil lang exec-time)))))))

(defun chief/org-babel-execute-rust-sync (body params)
  "Execute Rust BODY synchronously with `rust-script'."
  (let* ((file (chief/org-babel-write-temp-file body ".rs" params "chief-org-rust-"))
         (command (chief/org-babel-rust-script-command file params)))
    (unwind-protect
        (chief/org-babel-format-result
         (chief/org-babel-run-command command params)
         params)
      (when (file-exists-p file)
        (delete-file file)))))

(defun chief/org-babel-execute-rust-async (body params)
  "Start Rust BODY asynchronously with `rust-script' and update Babel results later."
  (let* ((org-buffer (current-buffer))
         (marker (copy-marker (point) nil))
         (info (org-babel-get-src-block-info))
         (result-params (cdr (assq :result-params params)))
         (file (chief/org-babel-write-temp-file body ".rs" params "chief-org-rust-"))
         (command (chief/org-babel-rust-script-command file params))
         (stdout-buffer (generate-new-buffer " *chief-org-babel-rust-stdout*"))
         (stderr-buffer (generate-new-buffer " *chief-org-babel-rust-stderr*"))
         (start-time (float-time))
         (process
          (make-process
           :name "org-babel-rust-script"
           :buffer stdout-buffer
           :stderr stderr-buffer
           :command command
           :connection-type 'pipe
           :noquery t
           :sentinel
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (let* ((exit-status (if (eq (process-status proc) 'exit)
                                       (process-exit-status proc)
                                     1))
                      (output (chief/org-babel-rust-command-output
                               stdout-buffer stderr-buffer exit-status))
                      (result (chief/org-babel-format-result output params))
                      (exec-time (- (float-time) start-time)))
                 (condition-case err
                     (progn
                       (chief/org-babel-rust-insert-async-result
                        org-buffer marker result result-params info "rust" exec-time)
                       (message "rust-script Org Babel block finished%s"
                                (if (zerop exit-status)
                                    ""
                                  (format " with exit code %s" exit-status))))
                   (error
                    (message "Failed to insert rust-script Org Babel result: %s"
                             (error-message-string err))))
                 (set-marker marker nil)
                 (when (file-exists-p file)
                   (delete-file file))
                 (when (buffer-live-p stdout-buffer)
                   (kill-buffer stdout-buffer))
                 (when (buffer-live-p stderr-buffer)
                   (kill-buffer stderr-buffer))))))))
    (message "Started rust-script Org Babel process %s" (process-id process))
    "rust-script running asynchronously..."))

(defun org-babel-execute:rust (body params)
  "Execute a Rust source block with Org Babel using `rust-script'.
This supports inline `//! ```cargo' dependency manifests in Rust blocks.
Execution is asynchronous by default; add `:async no' to force blocking sync."
  (if (and (derived-mode-p 'org-mode)
           (chief/org-babel-rust-async-p params))
      (chief/org-babel-execute-rust-async body params)
    (chief/org-babel-execute-rust-sync body params)))

(provide 'ob-rust-script)
;;; ob-rust-script.el ends here
