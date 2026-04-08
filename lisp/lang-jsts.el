;;; lang-jsts.el --- JavaScript and TypeScript tooling -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'comint)
(require 'core-projects)
(require 'js)
(require 'project)
(require 'subr-x)

(require 'typescript-ts-mode nil t)

(defcustom chief/jsts-default-runtime 'auto
  "Default runtime used for JavaScript and TypeScript buffers."
  :type '(choice (const auto)
                 (const node)
                 (const deno)
                 (const bun)
                 (const quickjs))
  :group 'chief)

(defcustom chief/jsts-node-typescript-file-args
  '("--experimental-strip-types" "--experimental-transform-types")
  "Arguments used when executing TypeScript files with Node.js."
  :type '(repeat string)
  :group 'chief)

(defcustom chief/jsts-quickjs-command "qjs"
  "Executable used for the QuickJS runtime."
  :type 'string
  :group 'chief)

(defvar chief/jsts-runtime-history nil
  "History for `chief/jsts-set-runtime'.")

(defvar-local chief/jsts-runtime nil
  "Buffer-local JS/TS runtime override.
When nil or `auto', this module detects the runtime from the project.")

(defun chief/jsts-project-root ()
  "Return the current JS/TS project root."
  (chief/project-preferred-root
   '("deno.json" "deno.jsonc"
     "pnpm-workspace.yaml"
     "turbo.json"
     "nx.json"
     "lerna.json"
     "bunfig.toml"
     "bun.lock"
     "bun.lockb"
     "package-lock.json"
     "pnpm-lock.yaml"
     "yarn.lock")
   '("package.json" "tsconfig.json" "jsconfig.json")
   (chief/project-current-root)
   default-directory))

(defun chief/jsts-project-file-p (name &optional root)
  "Return non-nil when NAME exists under ROOT."
  (let ((root (or root (chief/jsts-project-root))))
    (and root (file-exists-p (expand-file-name name root)))))

(defun chief/jsts-typescript-buffer-p ()
  "Return non-nil when the current buffer contains TypeScript-family code."
  (derived-mode-p 'typescript-ts-mode 'tsx-ts-mode))

(defun chief/jsts-deno-project-p (&optional root)
  "Return non-nil when ROOT looks like a Deno project."
  (let ((root (or root (chief/jsts-project-root))))
    (or (chief/jsts-project-file-p "deno.json" root)
        (chief/jsts-project-file-p "deno.jsonc" root))))

(defun chief/jsts-bun-project-p (&optional root)
  "Return non-nil when ROOT looks like a Bun project."
  (let ((root (or root (chief/jsts-project-root))))
    (or (chief/jsts-project-file-p "bunfig.toml" root)
        (chief/jsts-project-file-p "bun.lock" root)
        (chief/jsts-project-file-p "bun.lockb" root))))

(defun chief/jsts-node-project-p (&optional root)
  "Return non-nil when ROOT looks like a Node.js project."
  (let ((root (or root (chief/jsts-project-root))))
    (or (chief/jsts-project-file-p "package.json" root)
        (chief/jsts-project-file-p "node_modules" root))))

(defun chief/jsts-runtime-display-name (&optional runtime)
  "Return a human-readable display name for RUNTIME."
  (capitalize (symbol-name (or runtime (chief/jsts-current-runtime)))))

(defun chief/jsts-current-runtime ()
  "Return the runtime for the current JS/TS buffer."
  (let ((explicit (and chief/jsts-runtime
                       (not (eq chief/jsts-runtime 'auto))
                       chief/jsts-runtime))
        (root (chief/jsts-project-root)))
    (or explicit
        (if (not (eq chief/jsts-default-runtime 'auto))
            chief/jsts-default-runtime
          (cond
           ((chief/jsts-deno-project-p root) 'deno)
           ((chief/jsts-bun-project-p root) 'bun)
           ((chief/jsts-node-project-p root) 'node)
           ((executable-find "node") 'node)
           ((executable-find "bun") 'bun)
           ((executable-find "deno") 'deno)
           ((executable-find chief/jsts-quickjs-command) 'quickjs))))))

(defun chief/jsts-deno-config-file (&optional root)
  "Return the Deno config file for ROOT, if any."
  (let ((root (or root (chief/jsts-project-root))))
    (or (and (chief/jsts-project-file-p "deno.json" root)
             (expand-file-name "deno.json" root))
        (and (chief/jsts-project-file-p "deno.jsonc" root)
             (expand-file-name "deno.jsonc" root)))))

(defun chief/jsts-deno-permission-flag (flag value)
  "Return a Deno permission FLAG assembled from VALUE."
  (when (and value
             (not (equal value "no"))
             (not (equal value "nil"))
             (not (equal value nil)))
    (if (member value '(t yes true "yes" "true"))
        (format "--%s" flag)
      (format "--%s=%s" flag value))))

(defun chief/jsts-deno-permission-arguments (&optional params)
  "Return Deno permission arguments derived from PARAMS."
  (delq
   nil
   (mapcar
    (pcase-lambda (`(,header . ,flag))
      (chief/jsts-deno-permission-flag flag (cdr (assq header params))))
    '((:allow-read . "allow-read")
      (:allow-write . "allow-write")
      (:allow-net . "allow-net")
      (:allow-env . "allow-env")
      (:allow-run . "allow-run")
      (:allow-sys . "allow-sys")
      (:allow-import . "allow-import")
      (:allow-ffi . "allow-ffi")
      (:deny-read . "deny-read")
      (:deny-write . "deny-write")
      (:deny-net . "deny-net")
      (:deny-env . "deny-env")
      (:deny-run . "deny-run")
      (:deny-sys . "deny-sys")))))

(defun chief/jsts-runtime-extra-arguments (&optional params)
  "Return extra runtime arguments from PARAMS."
  (let ((value (cdr (assq :runtime-options params))))
    (cond
     ((stringp value) (split-string-and-unquote value))
     ((listp value) value)
     (t nil))))

(defun chief/jsts-repl-command (&optional runtime)
  "Return the command list used to start a REPL for RUNTIME."
  (let ((runtime (or runtime (chief/jsts-current-runtime))))
    (pcase runtime
      ('node
       (if (chief/jsts-typescript-buffer-p)
           (when-let* ((tsx (executable-find "tsx")))
             (list tsx))
         (when-let* ((node (executable-find "node")))
           (list node))))
      ('deno
       (when-let* ((deno (executable-find "deno")))
         (list deno "repl")))
      ('bun
       (when-let* ((bun (executable-find "bun")))
         (list bun "repl")))
      ('quickjs
       (when-let* ((qjs (executable-find chief/jsts-quickjs-command)))
         (list qjs))))))

(defun chief/jsts-script-command (file &optional runtime language params)
  "Return the command list used to execute FILE.
RUNTIME defaults to the current buffer runtime, LANGUAGE defaults to the
current buffer language, and PARAMS optionally supplies Org header arguments."
  (let* ((runtime (or runtime (chief/jsts-current-runtime)))
         (language (or language (if (chief/jsts-typescript-buffer-p)
                                    'typescript
                                  'javascript)))
         (extra (chief/jsts-runtime-extra-arguments params)))
    (pcase runtime
      ('node
       (when-let* ((node (executable-find "node")))
         (append (list node)
                 (when (eq language 'typescript)
                   chief/jsts-node-typescript-file-args)
                 extra
                 (list file))))
      ('deno
       (when-let* ((deno (executable-find "deno")))
         (append (list deno "run")
                 (chief/jsts-deno-permission-arguments params)
                 extra
                 (list file))))
      ('bun
       (when-let* ((bun (executable-find "bun")))
         (append (list bun "run")
                 extra
                 (list file))))
      ('quickjs
       (when-let* ((qjs (executable-find chief/jsts-quickjs-command)))
         (append (list qjs)
                 extra
                 (list file)))))))

(defun chief/jsts-language-extension (&optional language)
  "Return a suitable file extension for LANGUAGE."
  (pcase language
    ('typescript (if (derived-mode-p 'tsx-ts-mode) ".tsx" ".ts"))
    (_ ".js")))

(defun chief/jsts-write-temp-file (body &optional language)
  "Write BODY to a temporary file for LANGUAGE and return the file path."
  (let* ((language (or language (if (chief/jsts-typescript-buffer-p)
                                    'typescript
                                  'javascript)))
         (root (chief/jsts-project-root))
         (temporary-file-directory root)
         (file (make-temp-file
                (expand-file-name "chief-jsts-" root)
                nil
                (chief/jsts-language-extension language))))
    (with-temp-file file
      (insert body))
    file))

(defun chief/jsts-run-command (command &optional directory)
  "Run COMMAND from DIRECTORY and return its trimmed output.
Signal an error when the command exits unsuccessfully."
  (let ((default-directory (or directory (chief/jsts-project-root)))
        (stdout (generate-new-buffer " *chief-jsts-stdout*"))
        (stderr-file (make-temp-file "chief-jsts-stderr-")))
    (unwind-protect
        (let ((status (apply #'call-process
                             (car command)
                             nil
                             (list stdout stderr-file)
                             nil
                             (cdr command))))
          (if (zerop status)
              (with-current-buffer stdout
                (string-trim-right (buffer-string)))
            (error "%s"
                   (with-temp-buffer
                     (insert-file-contents stderr-file)
                     (string-trim (buffer-string))))))
      (kill-buffer stdout)
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

(defun chief/jsts-run-file (&optional file runtime language params)
  "Run FILE using RUNTIME and LANGUAGE.
When FILE is nil, use the current buffer file or a temporary file.
PARAMS optionally supplies Org Babel header arguments."
  (interactive)
  (let* ((language (or language (if (chief/jsts-typescript-buffer-p)
                                    'typescript
                                  'javascript)))
         (file (or file
                   buffer-file-name
                   (chief/jsts-write-temp-file (buffer-substring-no-properties
                                                (point-min)
                                                (point-max))
                                               language))))
    (unless-let* ((command (chief/jsts-script-command file runtime language params)))
      (user-error "No executable is configured for the %s runtime" (chief/jsts-runtime-display-name runtime)))
    (chief/jsts-run-command command (chief/jsts-project-root))))

(defun chief/jsts-describe-runtime ()
  "Display the current JS/TS runtime choice."
  (interactive)
  (message "%s runtime for this buffer: %s"
           (if (chief/jsts-typescript-buffer-p) "TypeScript" "JavaScript")
           (chief/jsts-runtime-display-name)))

(defun chief/jsts-set-runtime (runtime)
  "Set the current buffer runtime to RUNTIME."
  (interactive
   (list
    (intern
     (completing-read
      "Runtime: "
      '("auto" "node" "deno" "bun" "quickjs")
      nil
      t
      nil
      'chief/jsts-runtime-history
      (symbol-name (or chief/jsts-runtime chief/jsts-default-runtime))))))
  (setq-local chief/jsts-runtime runtime)
  (chief/jsts-configure-lsp)
  (when (bound-and-true-p lsp-managed-mode)
    (lsp-workspace-restart))
  (message "Set this buffer's runtime to %s" (chief/jsts-runtime-display-name runtime)))

(defun chief/jsts-repl-buffer-name (&optional runtime)
  "Return the REPL buffer name for RUNTIME."
  (let* ((runtime (or runtime (chief/jsts-current-runtime)))
         (project (file-name-nondirectory
                   (directory-file-name (chief/jsts-project-root)))))
    (format "*%s[%s:%s]*"
            (if (chief/jsts-typescript-buffer-p) "ts" "js")
            runtime
            project)))

(defun chief/jsts-unsupported-repl-message (&optional runtime)
  "Return the error message for an unsupported REPL under RUNTIME."
  (pcase (or runtime (chief/jsts-current-runtime))
    ('node
     (if (chief/jsts-typescript-buffer-p)
         "Node.js does not support TypeScript REPL input directly. Install `tsx` or use Bun/Deno for interactive TypeScript."
       "Node.js is not available on PATH"))
    ('quickjs "QuickJS is not available on PATH")
    ('bun "Bun is not available on PATH")
    ('deno "Deno is not available on PATH")
    (_ "No JS/TS runtime is configured for this buffer")))

(defun chief/jsts-ensure-repl-buffer ()
  "Return a live REPL buffer for the current JS/TS buffer."
  (let* ((runtime (chief/jsts-current-runtime))
         (buffer-name (chief/jsts-repl-buffer-name runtime))
         (buffer (get-buffer-create buffer-name))
         (command (chief/jsts-repl-command runtime))
         (default-directory (chief/jsts-project-root)))
    (unless command
      (user-error "%s" (chief/jsts-unsupported-repl-message runtime)))
    (unless (comint-check-proc buffer)
      (let ((process-environment (copy-sequence process-environment)))
        (apply #'make-comint-in-buffer
               (format "chief-%s-repl" runtime)
               buffer
               (car command)
               nil
               (cdr command)))
      (with-current-buffer buffer
        (setq-local default-directory default-directory)
        (setq-local comint-scroll-to-bottom-on-input t)
        (setq-local comint-scroll-to-bottom-on-output t)
        (setq-local comint-process-echoes nil)))
    buffer))

(defun chief/jsts-start-repl ()
  "Start or switch to the current buffer's JS/TS REPL."
  (interactive)
  (pop-to-buffer (chief/jsts-ensure-repl-buffer)))

(defun chief/jsts-restart-repl ()
  "Restart the current buffer's JS/TS REPL."
  (interactive)
  (let ((buffer (get-buffer (chief/jsts-repl-buffer-name))))
    (when buffer
      (when-let* ((process (get-buffer-process buffer)))
        (delete-process process))
      (kill-buffer buffer)))
  (chief/jsts-start-repl))

(defun chief/jsts-send-string (string)
  "Send STRING to the current buffer's JS/TS REPL."
  (let* ((buffer (chief/jsts-ensure-repl-buffer))
         (process (get-buffer-process buffer)))
    (comint-send-string process string)
    (unless (string-suffix-p "\n" string)
      (comint-send-string process "\n"))
    (display-buffer buffer)))

(defun chief/jsts-send-line ()
  "Send the current line to the current buffer's JS/TS REPL."
  (interactive)
  (chief/jsts-send-string
   (buffer-substring-no-properties
    (line-beginning-position)
    (line-end-position))))

(defun chief/jsts-send-region (start end)
  "Send the region from START to END to the current buffer's JS/TS REPL."
  (interactive "r")
  (chief/jsts-send-string (buffer-substring-no-properties start end)))

(defun chief/jsts-send-buffer ()
  "Send the current buffer to the current buffer's JS/TS REPL."
  (interactive)
  (chief/jsts-send-region (point-min) (point-max)))

(defun chief/jsts-load-file ()
  "Load the current file into the current buffer's JS/TS REPL."
  (interactive)
  (if buffer-file-name
      (chief/jsts-send-string (buffer-substring-no-properties (point-min) (point-max)))
    (chief/jsts-send-buffer)))

(defun chief/jsts-configure-lsp ()
  "Configure per-project JS/TS LSP client selection."
  (setq-local lsp-disabled-clients '(jsts-ls flow-ls))
  (if (chief/jsts-deno-project-p)
      (progn
        (setq-local lsp-enabled-clients '(deno-ls))
        (setq-local lsp-disabled-clients '(ts-ls jsts-ls flow-ls))
        (when-let* ((config (chief/jsts-deno-config-file)))
          (setq-local lsp-clients-deno-config config)))
    (setq-local lsp-enabled-clients '(ts-ls))
    (setq-local lsp-disabled-clients '(deno-ls jsts-ls flow-ls))))

(defun chief/jsts-mode-setup ()
  "Configure runtime, REPL, and LSP support for JS/TS buffers."
  (setq-local js-indent-level 2)
  (setq-local chief/lsp-root-function #'chief/jsts-project-root)
  (when (boundp 'typescript-ts-mode-indent-offset)
    (setq-local typescript-ts-mode-indent-offset 2))
  (chief/repl-configure
   :start #'chief/jsts-start-repl
   :restart #'chief/jsts-restart-repl
   :send-line #'chief/jsts-send-line
   :send-region #'chief/jsts-send-region
   :send-buffer #'chief/jsts-send-buffer
   :load-file #'chief/jsts-load-file)
  (chief/jsts-configure-lsp)
  (when (fboundp 'chief/lsp-managed-mode-setup)
    (chief/lsp-managed-mode-setup)))

(with-eval-after-load 'lsp-mode
  (require 'lsp-javascript)
  (setq lsp-clients-typescript-prefer-use-project-ts-server t)
  (setq lsp-javascript-format-enable t)
  (setq lsp-typescript-format-enable t))

(add-hook 'js-mode-hook #'chief/jsts-mode-setup)
(add-hook 'js-ts-mode-hook #'chief/jsts-mode-setup)
(when (fboundp 'typescript-ts-mode)
  (add-hook 'typescript-ts-mode-hook #'chief/jsts-mode-setup))
(when (fboundp 'tsx-ts-mode)
  (add-hook 'tsx-ts-mode-hook #'chief/jsts-mode-setup))

(with-eval-after-load 'js
  (chief/repl-setup-standard-local-leader 'js-mode-map)
  (chief/repl-setup-standard-local-leader 'js-ts-mode-map)
  (chief/local-leader-def
    :keymaps '(js-mode-map js-ts-mode-map)
    "r" '(:ignore t :which-key "runtime")
    "rr" #'chief/jsts-set-runtime
    "ri" #'chief/jsts-describe-runtime
    "rf" #'chief/jsts-run-file))

(with-eval-after-load 'typescript-ts-mode
  (chief/repl-setup-standard-local-leader 'typescript-ts-mode-map)
  (chief/repl-setup-standard-local-leader 'tsx-ts-mode-map)
  (chief/local-leader-def
    :keymaps '(typescript-ts-mode-map tsx-ts-mode-map)
    "r" '(:ignore t :which-key "runtime")
    "rr" #'chief/jsts-set-runtime
    "ri" #'chief/jsts-describe-runtime
    "rf" #'chief/jsts-run-file))

(provide 'lang-jsts)
;;; lang-jsts.el ends here
