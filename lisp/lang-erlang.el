;;; lang-erlang.el --- Erlang, rebar3, and ELP support -*- lexical-binding: t; -*-

(require 'compile)
(require 'comint)
(require 'core-projects)
(require 'project)
(require 'subr-x)

(defun chief/erlang-project-root ()
  "Return the current Erlang project root, if any."
  (chief/project-preferred-root
   '("rebar.config" "rebar.config.script")
   '(".git")
   (chief/project-current-root)
   default-directory))

(defun chief/erlang-project-shell-p (&optional root)
  "Return non-nil when ROOT looks like a rebar3 project."
  (let ((root (or root (chief/erlang-project-root))))
    (and root
         (or (file-exists-p (expand-file-name "rebar.config" root))
             (file-exists-p (expand-file-name "rebar.config.script" root))))))

(defun chief/erlang-shell-command (&optional root)
  "Return the preferred Erlang shell command for ROOT."
  (let ((root (or root (chief/erlang-project-root))))
    (cond
     ((and (chief/erlang-project-shell-p root)
           (executable-find "rebar3"))
      (list (executable-find "rebar3") "shell"))
     ((executable-find "erl")
      (list (executable-find "erl")))
     (t
      (user-error "Neither `rebar3` nor `erl` is available on PATH")))))

(defun chief/erlang-repl-buffer-name ()
  "Return the Erlang REPL buffer name for the current project."
  (format "*erlang[%s]*"
          (file-name-nondirectory
           (directory-file-name
            (or (chief/erlang-project-root) default-directory)))))

(defun chief/erlang-ensure-repl-buffer ()
  "Return a live Erlang REPL buffer for the current project."
  (let* ((root (file-name-as-directory
                (expand-file-name (or (chief/erlang-project-root) default-directory))))
         (command (chief/erlang-shell-command root))
         (buffer (get-buffer-create (chief/erlang-repl-buffer-name))))
    (unless (comint-check-proc buffer)
      (let ((default-directory root))
        (apply #'make-comint-in-buffer
               "chief-erlang"
               buffer
               (car command)
               nil
               (cdr command)))
      (with-current-buffer buffer
        (setq-local default-directory root)
        (setq-local comint-process-echoes nil)))
    buffer))

(defun chief/erlang-start-repl ()
  "Start or switch to the current project's Erlang shell."
  (interactive)
  (pop-to-buffer (chief/erlang-ensure-repl-buffer)))

(defun chief/erlang-restart-repl ()
  "Restart the current project's Erlang shell."
  (interactive)
  (when-let* ((buffer (get-buffer (chief/erlang-repl-buffer-name))))
    (when-let* ((process (get-buffer-process buffer)))
      (delete-process process))
    (kill-buffer buffer))
  (chief/erlang-start-repl))

(defun chief/erlang-send-string (string)
  "Send STRING to the current project's Erlang shell."
  (let* ((buffer (chief/erlang-ensure-repl-buffer))
         (process (get-buffer-process buffer)))
    (comint-send-string process string)
    (unless (string-suffix-p "\n" string)
      (comint-send-string process "\n"))
    (display-buffer buffer)))

(defun chief/erlang-terminate-form (string)
  "Ensure STRING ends with an Erlang form terminator."
  (let ((string (string-trim-right string)))
    (cond
     ((string-empty-p string) "")
     ((string-match-p "\\.[[:space:]]*\\'" string) string)
     (t (concat string ".")))))

(defun chief/erlang-send-line ()
  "Send the current line to the Erlang shell."
  (interactive)
  (chief/erlang-send-string
   (chief/erlang-terminate-form
    (buffer-substring-no-properties
     (line-beginning-position)
     (line-end-position)))))

(defun chief/erlang-send-region (start end)
  "Send the region from START to END to the Erlang shell."
  (interactive "r")
  (chief/erlang-send-string
   (chief/erlang-terminate-form
    (buffer-substring-no-properties start end))))

(defun chief/erlang-send-defun ()
  "Send the current Erlang form to the Erlang shell."
  (interactive)
  (save-excursion
    (mark-defun)
    (chief/erlang-send-region (region-beginning) (region-end))
    (deactivate-mark)))

(defun chief/erlang-load-file ()
  "Compile the current Erlang source file in the Erlang shell."
  (interactive)
  (if (and buffer-file-name
           (string-match-p "\\.erl\\'" buffer-file-name))
      (let ((directory (file-name-directory buffer-file-name))
            (module (file-name-base buffer-file-name)))
        (save-buffer)
        (chief/erlang-send-string (format "cd(%S)." directory))
        (chief/erlang-send-string (format "c(%s)." module)))
    (chief/erlang-send-region (point-min) (point-max))))

(defun chief/erlang-send-buffer ()
  "Send the current buffer to the Erlang shell."
  (interactive)
  (if buffer-file-name
      (chief/erlang-load-file)
    (chief/erlang-send-region (point-min) (point-max))))

(defun chief/erlang-compile (command name &optional directory)
  "Run Erlang COMMAND in DIRECTORY using compilation buffer NAME."
  (let ((default-directory (or directory (chief/erlang-project-root) default-directory))
        (compilation-read-command nil))
    (compilation-start
     (mapconcat #'shell-quote-argument command " ")
     'compilation-mode
     (lambda (_) name))))

(defun chief/erlang-project-command (&rest args)
  "Return an Erlang project command built from ARGS."
  (let ((root (chief/erlang-project-root)))
    (cond
     ((and (chief/erlang-project-shell-p root)
           (executable-find "rebar3"))
      (cons "rebar3" args))
     ((and buffer-file-name (executable-find "erlc"))
      (append (list "erlc")
              (when (file-name-directory buffer-file-name)
                (list "-o" (file-name-directory buffer-file-name)))
              (list (expand-file-name buffer-file-name))))
     (t
      (user-error "Neither rebar3 project tooling nor erlc is available")))))

(defun chief/erlang-compile-project ()
  "Run `rebar3 compile' or `erlc' for the current Erlang project."
  (interactive)
  (chief/erlang-compile (chief/erlang-project-command "compile") "*erlang compile*"))

(defun chief/erlang-test-project ()
  "Run `rebar3 eunit' for the current Erlang project."
  (interactive)
  (chief/erlang-compile (chief/erlang-project-command "eunit") "*erlang eunit*"))

(defun chief/erlang-common-test-project ()
  "Run `rebar3 ct' for the current Erlang project."
  (interactive)
  (chief/erlang-compile (chief/erlang-project-command "ct") "*erlang ct*"))

(defun chief/erlang-mode-setup ()
  "Configure the current Erlang buffer."
  (setq-local compile-command
              (if (chief/erlang-project-shell-p)
                  "rebar3 eunit"
                "erlc"))
  (setq-local chief/lsp-root-function #'chief/erlang-project-root)
  (chief/repl-configure
   :start #'chief/erlang-start-repl
   :restart #'chief/erlang-restart-repl
   :send-line #'chief/erlang-send-line
   :send-region #'chief/erlang-send-region
   :send-buffer #'chief/erlang-send-buffer
   :send-defun #'chief/erlang-send-defun
   :load-file #'chief/erlang-load-file)
  (when (require 'lsp-mode nil t)
    (setq-local lsp-enabled-clients '(elp))
    (setq-local lsp-disabled-clients '(erlang-ls)))
  (when (fboundp 'chief/lsp-managed-mode-setup)
    (chief/lsp-managed-mode-setup)))

(chief/safe-use-package erlang
  :mode ("\\.erl\\'" . erlang-mode)
  :mode ("\\.hrl\\'" . erlang-mode)
  :mode ("\\.xrl\\'" . erlang-mode)
  :mode ("\\.yrl\\'" . erlang-mode)
  :hook (erlang-mode . chief/erlang-mode-setup))

(with-eval-after-load 'erlang
  (chief/repl-setup-standard-local-leader 'erlang-mode-map)
  (chief/local-leader-def
    :keymaps 'erlang-mode-map
    "c" '(:ignore t :which-key "compile")
    "cb" #'chief/erlang-compile-project
    "ct" #'chief/erlang-test-project
    "cT" #'chief/erlang-common-test-project))

(provide 'lang-erlang)
;;; lang-erlang.el ends here
