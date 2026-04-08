;;; lang-elixir.el --- Elixir, Phoenix, and Expert support -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'compile)
(require 'comint)
(require 'core-projects)
(require 'project)
(require 'subr-x)

(defvar chief/elixir-project-repl-modes (make-hash-table :test #'equal)
  "Remember the preferred Elixir REPL mode for each project root.")

(defun chief/elixir-project-root ()
  "Return the current Elixir project root, if any."
  (chief/project-preferred-root
   '("mix.exs")
   (chief/project-current-root)
   default-directory))

(defun chief/elixir-template-buffer-p (&optional file)
  "Return non-nil when FILE or the current buffer is an Elixir template."
  (let ((file (or file buffer-file-name)))
    (and file
         (string-match-p "\\.\\(heex\\|leex\\|eex\\|surface\\)\\'" file))))

(define-minor-mode chief/elixir-template-minor-mode
  "Minor mode for Elixir template buffers."
  :lighter nil
  :keymap (make-sparse-keymap))

(defun chief/elixir-mix-project-p (&optional root)
  "Return non-nil when ROOT looks like a Mix project."
  (let ((root (or root (chief/elixir-project-root))))
    (and root
         (file-exists-p (expand-file-name "mix.exs" root)))))

(defun chief/elixir-default-repl-mode (&optional root)
  "Return the default REPL mode for ROOT."
  (if (chief/elixir-mix-project-p root) 'mix 'plain))

(defun chief/elixir-normalize-repl-mode (mode &optional root)
  "Normalize MODE for ROOT."
  (let ((root (or root (chief/elixir-project-root))))
    (if (and (memq mode '(mix phoenix))
             (not (chief/elixir-mix-project-p root)))
        'plain
      (or mode (chief/elixir-default-repl-mode root)))))

(defun chief/elixir-project-repl-mode (&optional root)
  "Return the preferred REPL mode for ROOT."
  (let ((root (or root (chief/elixir-project-root) default-directory)))
    (chief/elixir-normalize-repl-mode
     (gethash root chief/elixir-project-repl-modes)
     root)))

(defun chief/elixir-set-project-repl-mode (mode &optional root)
  "Remember MODE as the preferred Elixir REPL mode for ROOT."
  (let ((root (file-name-as-directory
               (expand-file-name (or root (chief/elixir-project-root) default-directory)))))
    (puthash root (chief/elixir-normalize-repl-mode mode root)
             chief/elixir-project-repl-modes)))

(defun chief/elixir-repl-command (&optional mode root)
  "Return the command list to start Elixir MODE in ROOT."
  (let* ((root (or root (chief/elixir-project-root)))
         (iex (or (executable-find "iex")
                  (user-error "iex is not available on PATH")))
         (mode (chief/elixir-normalize-repl-mode mode root)))
    (pcase mode
      ('phoenix (list iex "-S" "mix" "phx.server"))
      ('mix (list iex "-S" "mix"))
      (_ (list iex)))))

(defun chief/elixir-source-auto-mode ()
  "Select the best available Elixir major mode for the current buffer."
  (interactive)
  (if (and (fboundp 'elixir-ts-mode)
           (fboundp 'treesit-ready-p)
           (treesit-ready-p 'elixir t))
      (elixir-ts-mode)
    (elixir-mode)))

(defun chief/elixir-template-auto-mode ()
  "Select the best available Elixir template major mode for the current buffer."
  (interactive)
  (if (and (fboundp 'heex-ts-mode)
           (fboundp 'treesit-ready-p)
           (treesit-ready-p 'heex t)
           (not (and buffer-file-name
                     (string-match-p "\\.eex\\'" buffer-file-name))))
      (heex-ts-mode)
    (web-mode)))

(defun chief/elixir-repl-buffer-name (&optional mode root)
  "Return the buffer name for Elixir MODE in ROOT."
  (let* ((root (or root (chief/elixir-project-root) default-directory))
         (mode (chief/elixir-normalize-repl-mode mode root))
         (project-name (file-name-nondirectory (directory-file-name root))))
    (format "*elixir[%s:%s]*" project-name mode)))

(defun chief/elixir-ensure-repl-buffer (&optional mode)
  "Return a live Elixir REPL buffer for MODE in the current project."
  (let* ((root (file-name-as-directory
                (expand-file-name (or (chief/elixir-project-root) default-directory))))
         (mode (chief/elixir-normalize-repl-mode mode root))
         (command (chief/elixir-repl-command mode root))
         (buffer (get-buffer-create (chief/elixir-repl-buffer-name mode root))))
    (chief/elixir-set-project-repl-mode mode root)
    (unless (comint-check-proc buffer)
      (let ((default-directory root))
        (apply #'make-comint-in-buffer
               "chief-elixir"
               buffer
               (car command)
               nil
               (cdr command)))
      (with-current-buffer buffer
        (setq-local default-directory root)
        (setq-local comint-process-echoes nil)))
    buffer))

(defun chief/elixir-open-repl (&optional mode)
  "Start or switch to the current project's Elixir REPL using MODE."
  (interactive)
  (pop-to-buffer (chief/elixir-ensure-repl-buffer mode)))

(defun chief/elixir-start-repl ()
  "Start or switch to the preferred Elixir REPL for the current project."
  (interactive)
  (chief/elixir-open-repl (chief/elixir-project-repl-mode)))

(defun chief/elixir-open-plain-repl ()
  "Open a plain IEx session for the current project."
  (interactive)
  (chief/elixir-open-repl 'plain))

(defun chief/elixir-open-mix-repl ()
  "Open `iex -S mix' for the current project."
  (interactive)
  (chief/elixir-open-repl 'mix))

(defun chief/elixir-open-phoenix-repl ()
  "Open `iex -S mix phx.server' for the current project."
  (interactive)
  (chief/elixir-open-repl 'phoenix))

(defun chief/elixir-restart-repl ()
  "Restart the preferred Elixir REPL for the current project."
  (interactive)
  (let* ((root (or (chief/elixir-project-root) default-directory))
         (mode (chief/elixir-project-repl-mode root))
         (buffer (get-buffer (chief/elixir-repl-buffer-name mode root))))
    (when buffer
      (when-let* ((process (get-buffer-process buffer)))
        (delete-process process))
      (kill-buffer buffer))
    (chief/elixir-open-repl mode)))

(defun chief/elixir-send-string (string &optional mode)
  "Send STRING to the Elixir REPL selected by MODE."
  (let* ((buffer (chief/elixir-ensure-repl-buffer mode))
         (process (get-buffer-process buffer)))
    (comint-send-string process string)
    (unless (string-suffix-p "\n" string)
      (comint-send-string process "\n"))
    (display-buffer buffer)))

(defun chief/elixir-send-line ()
  "Send the current line to the Elixir REPL."
  (interactive)
  (chief/elixir-send-string
   (buffer-substring-no-properties
    (line-beginning-position)
    (line-end-position))))

(defun chief/elixir-send-region (start end)
  "Send the region from START to END to the Elixir REPL."
  (interactive "r")
  (chief/elixir-send-string
   (buffer-substring-no-properties start end)))

(defun chief/elixir-send-buffer ()
  "Send the current buffer to the Elixir REPL."
  (interactive)
  (chief/elixir-send-region (point-min) (point-max)))

(defun chief/elixir-send-defun ()
  "Send the current defun to the Elixir REPL."
  (interactive)
  (save-excursion
    (mark-defun)
    (chief/elixir-send-region (region-beginning) (region-end))
    (deactivate-mark)))

(defun chief/elixir-file-load-form (file)
  "Return an Elixir expression that loads FILE into the active REPL."
  (let ((file (expand-file-name file)))
    (if (string-match-p "\\.exs\\'" file)
        (format "Code.require_file(%S)" file)
      (format "Code.compile_file(%S)" file))))

(defun chief/elixir-load-file ()
  "Load the current Elixir file in the Elixir REPL."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file"))
  (save-buffer)
  (chief/elixir-send-string (chief/elixir-file-load-form buffer-file-name)))

(defun chief/elixir-compile (command name &optional directory)
  "Run Elixir COMMAND in DIRECTORY using compilation buffer NAME."
  (let ((default-directory (or directory (chief/elixir-project-root) default-directory))
        (compilation-read-command nil))
    (compilation-start
     (mapconcat #'shell-quote-argument command " ")
     'compilation-mode
     (lambda (_) name))))

(defun chief/elixir-mix-command (&rest args)
  "Return a Mix command list built from ARGS."
  (unless (chief/elixir-mix-project-p)
    (user-error "This buffer is not inside a Mix project"))
  (unless (executable-find "mix")
    (user-error "mix is not available on PATH"))
  (cons "mix" args))

(defun chief/elixir-compile-project ()
  "Run `mix compile' for the current project."
  (interactive)
  (chief/elixir-compile (chief/elixir-mix-command "compile") "*mix compile*"))

(defun chief/elixir-test-project ()
  "Run `mix test' for the current project."
  (interactive)
  (chief/elixir-compile (chief/elixir-mix-command "test") "*mix test*"))

(defun chief/elixir-format-project ()
  "Run `mix format' for the current project."
  (interactive)
  (chief/elixir-compile (chief/elixir-mix-command "format") "*mix format*"))

(defun chief/elixir-format-file ()
  "Format the current Elixir file with `mix format'."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file"))
  (save-buffer)
  (chief/elixir-compile
   (append (chief/elixir-mix-command "format")
           (list (expand-file-name buffer-file-name)))
   "*mix format file*"))

(defun chief/elixir-expert-command ()
  "Return the command list used to start Expert."
  (or (when (fboundp 'chief/lsp-probed-executable-command)
        (chief/lsp-probed-executable-command "expert" '("--stdio") '("--help")))
      (when (fboundp 'chief/lsp-probed-executable-command)
        (chief/lsp-probed-executable-command "start_expert" '("--stdio") '("--help")))
      (when-let* ((expert (executable-find "expert")))
        (list expert "--stdio"))
      (when-let* ((expert (executable-find "start_expert")))
        (list expert "--stdio"))))

(defun chief/elixir-expert-active-p ()
  "Return non-nil when the current buffer is managed by Expert."
  (and (bound-and-true-p lsp-managed-mode)
       (boundp 'lsp-workspaces)
       (cl-some
        (lambda (workspace)
          (eq (lsp--client-server-id (lsp--workspace-client workspace)) 'expert))
        lsp-workspaces)))

(defun chief/elixir-retry-navigation (command)
  "Run COMMAND, retrying briefly while Expert finishes indexing."
  (let ((attempt 0)
        (max-attempts 12)
        (delay 0.25)
        done)
    (while (and (not done)
                (< attempt max-attempts))
      (setq attempt (1+ attempt))
      (condition-case err
          (progn
            (call-interactively command)
            (setq done t))
        (user-error
         (if (< attempt max-attempts)
             (sleep-for delay)
           (signal (car err) (cdr err))))))))

(defun chief/elixir-goto-definition ()
  "Jump to the definition at point, retrying briefly for Expert."
  (interactive)
  (if (chief/elixir-expert-active-p)
      (chief/elixir-retry-navigation #'chief/lsp-goto-definition-direct)
    (call-interactively #'chief/lsp-goto-definition-direct)))

(defun chief/elixir-goto-references ()
  "Find references at point, retrying briefly for Expert."
  (interactive)
  (if (chief/elixir-expert-active-p)
      (chief/elixir-retry-navigation #'chief/find-references)
    (call-interactively #'chief/find-references)))

(defun chief/elixir-configure-repl ()
  "Configure buffer-local Elixir REPL integration."
  (chief/repl-configure
   :start #'chief/elixir-start-repl
   :restart #'chief/elixir-restart-repl
   :send-line #'chief/elixir-send-line
   :send-region #'chief/elixir-send-region
   :send-buffer #'chief/elixir-send-buffer
   :send-defun #'chief/elixir-send-defun
   :load-file #'chief/elixir-load-file))

(defun chief/elixir-setup-lsp ()
  "Configure LSP for the current Elixir buffer."
  (when (require 'lsp-mode nil t)
    (setq-local lsp-enabled-clients '(expert))
    (setq-local lsp-disabled-clients '(elixir-ls))
    (setq-local chief/lsp-definition-function #'chief/elixir-goto-definition)
    (setq-local chief/lsp-references-function #'chief/elixir-goto-references))
  (when (fboundp 'chief/lsp-managed-mode-setup)
    (chief/lsp-managed-mode-setup)))

(defun chief/elixir-mode-setup ()
  "Configure the current Elixir source buffer."
  (setq-local compile-command
              (if (chief/elixir-mix-project-p)
                  "mix test"
                "elixir"))
  (setq-local chief/lsp-root-function #'chief/elixir-project-root)
  (chief/elixir-configure-repl)
  (chief/elixir-setup-lsp))

(defun chief/elixir-template-mode-setup ()
  "Configure the current Elixir template buffer."
  (when (chief/elixir-template-buffer-p)
    (setq-local compile-command
                (if (chief/elixir-mix-project-p)
                    "mix test"
                  "elixir"))
    (setq-local chief/lsp-root-function #'chief/elixir-project-root)
    (chief/elixir-template-minor-mode 1)
    (chief/elixir-configure-repl)
    (chief/elixir-setup-lsp)))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-disabled-clients 'elixir-ls)
  (add-to-list 'lsp-language-id-configuration '("\\.exs?\\'" . "elixir"))
  (add-to-list 'lsp-language-id-configuration '("\\.eex\\'" . "elixir"))
  (add-to-list 'lsp-language-id-configuration '("\\.leex\\'" . "phoenix-heex"))
  (add-to-list 'lsp-language-id-configuration '("\\.heex\\'" . "phoenix-heex"))
  (add-to-list 'lsp-language-id-configuration '("\\.surface\\'" . "phoenix-heex"))
  (add-to-list 'lsp-language-id-configuration '(heex-ts-mode . "phoenix-heex"))
  (lsp-register-custom-settings
   '(("workspaceSymbols.minQueryLength" 0)))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection #'chief/elixir-expert-command)
    :activation-fn (lsp-activate-on "elixir" "phoenix-heex")
    :major-modes '(elixir-mode elixir-ts-mode heex-ts-mode)
    :priority 3
    :server-id 'expert)))

(chief/safe-use-package elixir-mode
  :mode ("\\.ex\\'" . chief/elixir-source-auto-mode)
  :mode ("\\.exs\\'" . chief/elixir-source-auto-mode)
  :hook (elixir-mode . chief/elixir-mode-setup))

(add-to-list 'auto-mode-alist '("\\.heex\\'" . chief/elixir-template-auto-mode))
(add-to-list 'auto-mode-alist '("\\.leex\\'" . chief/elixir-template-auto-mode))
(add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.surface\\'" . chief/elixir-template-auto-mode))

(when (fboundp 'elixir-ts-mode)
  (add-hook 'elixir-ts-mode-hook #'chief/elixir-mode-setup))

(when (fboundp 'heex-ts-mode)
  (add-hook 'heex-ts-mode-hook #'chief/elixir-template-mode-setup))

(with-eval-after-load 'web-mode
  (add-to-list 'web-mode-engines-alist '("phoenix" . "\\.[hl]?eex\\'"))
  (add-to-list 'web-mode-engines-alist '("phoenix" . "\\.surface\\'"))
  (add-to-list 'web-mode-content-types-alist '("html" . "\\.surface\\'"))
  (add-hook 'web-mode-hook #'chief/elixir-template-mode-setup))

(with-eval-after-load 'elixir-mode
  (chief/repl-setup-standard-local-leader 'elixir-mode-map)
  (chief/local-leader-def
    :keymaps 'elixir-mode-map
    "c" '(:ignore t :which-key "compile")
    "cb" #'chief/elixir-compile-project
    "ct" #'chief/elixir-test-project
    "cf" #'chief/elixir-format-file
    "cF" #'chief/elixir-format-project
    "r" '(:ignore t :which-key "repl mode")
    "ro" #'chief/elixir-start-repl
    "ri" #'chief/elixir-open-plain-repl
    "rm" #'chief/elixir-open-mix-repl
    "rp" #'chief/elixir-open-phoenix-repl))

(with-eval-after-load 'elixir-ts-mode
  (chief/repl-setup-standard-local-leader 'elixir-ts-mode-map)
  (chief/local-leader-def
    :keymaps 'elixir-ts-mode-map
    "c" '(:ignore t :which-key "compile")
    "cb" #'chief/elixir-compile-project
    "ct" #'chief/elixir-test-project
    "cf" #'chief/elixir-format-file
    "cF" #'chief/elixir-format-project
    "r" '(:ignore t :which-key "repl mode")
    "ro" #'chief/elixir-start-repl
    "ri" #'chief/elixir-open-plain-repl
    "rm" #'chief/elixir-open-mix-repl
    "rp" #'chief/elixir-open-phoenix-repl))

(chief/repl-setup-standard-local-leader 'chief/elixir-template-minor-mode-map)
(chief/local-leader-def
  :keymaps 'chief/elixir-template-minor-mode-map
  "c" '(:ignore t :which-key "compile")
  "cb" #'chief/elixir-compile-project
  "ct" #'chief/elixir-test-project
  "cf" #'chief/elixir-format-file
  "cF" #'chief/elixir-format-project
  "r" '(:ignore t :which-key "repl mode")
  "ro" #'chief/elixir-start-repl
  "ri" #'chief/elixir-open-plain-repl
  "rm" #'chief/elixir-open-mix-repl
  "rp" #'chief/elixir-open-phoenix-repl)

(provide 'lang-elixir)
;;; lang-elixir.el ends here
