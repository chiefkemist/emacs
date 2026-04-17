;;; lang-jsts.el --- JavaScript and TypeScript tooling -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'comint)
(require 'core-projects)
(require 'js)
(require 'project)
(require 'subr-x)

(require 'typescript-ts-mode nil t)
(require 'web-mode nil t)

(chief/safe-use-package json-mode
  :commands (json-mode jsonc-mode))

(put 'typescript-mode 'derived-mode-parent 'prog-mode)

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

(defcustom chief/jsts-esbuild-command "esbuild"
  "Executable used to transpile JS/TS snippets for runtimes that need plain JS."
  :type 'string
  :group 'chief)

(defvar chief/jsts-runtime-history nil
  "History for `chief/jsts-set-runtime'.")

(defvar-local chief/jsts-runtime nil
  "Buffer-local JS/TS runtime override.
When nil or `auto', this module detects the runtime from the project.")

(defun chief/jsts-workspace-root ()
  "Return the current JS/TS workspace root, if any."
  (chief/project-preferred-root
   '("pnpm-workspace.yaml"
     "turbo.json"
     "nx.json"
     "lerna.json"
     "package-lock.json"
     "pnpm-lock.yaml"
     "yarn.lock")
   (chief/project-current-root)
   default-directory))

(defun chief/jsts-package-root ()
  "Return the nearest JS/TS package root, if any."
  (chief/project-preferred-root
   '("deno.json"
     "deno.jsonc"
     "bunfig.toml"
     "bun.lock"
     "bun.lockb"
     "package.json"
     "tsconfig.json"
     "jsconfig.json")
   default-directory))

(defun chief/jsts-project-root ()
  "Return the preferred JS/TS workspace root for the current buffer."
  (let* ((package-root (chief/jsts-package-root))
         (workspace-root (chief/jsts-workspace-root))
         (deno-root (when package-root
                      (or (file-exists-p (expand-file-name "deno.json" package-root))
                          (file-exists-p (expand-file-name "deno.jsonc" package-root))))))
    (or (and deno-root package-root)
        workspace-root
        package-root
        (chief/project-current-root)
        (file-name-as-directory (expand-file-name default-directory)))))

(defun chief/jsts-execution-root ()
  "Return the preferred package root for running code and starting REPLs."
  (or (chief/jsts-package-root)
      (chief/jsts-project-root)
      (file-name-as-directory (expand-file-name default-directory))))

(defun chief/jsts-project-file-p (name &optional root)
  "Return non-nil when NAME exists under ROOT."
  (let ((root (or root (chief/jsts-project-root))))
    (and root (file-exists-p (expand-file-name name root)))))

(defun chief/jsts-buffer-file-name (&optional file)
  "Return FILE or the current buffer file name, if any."
  (or file buffer-file-name ""))

(defun chief/jsts-buffer-kind (&optional file)
  "Return the content kind for FILE or the current buffer.
The result is one of `javascript', `jsx', `typescript', `tsx', `json',
`jsonc', or nil."
  (let ((name (downcase (chief/jsts-buffer-file-name file))))
    (cond
     ((or (derived-mode-p 'jsonc-mode)
          (string-match-p "\\.jsonc\\'" name))
      'jsonc)
     ((or (derived-mode-p 'json-ts-mode)
          (derived-mode-p 'json-mode)
          (derived-mode-p 'js-json-mode)
          (string-match-p "\\.json\\'" name))
      'json)
     ((or (derived-mode-p 'tsx-ts-mode)
          (string-match-p "\\.tsx\\'" name))
      'tsx)
     ((or (derived-mode-p 'typescript-ts-mode)
          (string-match-p "\\.\\(?:d\\.\\)?\\(?:ts\\|mts\\|cts\\)\\'" name))
      'typescript)
     ((or (and (derived-mode-p 'web-mode)
               (string-match-p "\\.\\(?:m?jsx\\|tsx\\)\\'" name))
          (string-match-p "\\.\\(?:jsx\\|mjsx\\)\\'" name))
      'jsx)
     ((or (derived-mode-p 'js-ts-mode)
          (derived-mode-p 'js-mode)
          (string-match-p "\\.\\(?:js\\|mjs\\|cjs\\)\\'" name))
      'javascript))))

(defun chief/jsts-web-template-buffer-p (&optional file)
  "Return non-nil when FILE or the current buffer is JSX- or TSX-like."
  (memq (chief/jsts-buffer-kind file) '(jsx tsx)))

(defun chief/jsts-typescript-buffer-p ()
  "Return non-nil when the current buffer contains TypeScript-family code."
  (memq (chief/jsts-buffer-kind) '(typescript tsx)))

(defun chief/jsts-code-buffer-p ()
  "Return non-nil when the current buffer contains executable JS/TS code."
  (memq (chief/jsts-buffer-kind) '(javascript jsx typescript tsx)))

(defun chief/jsts-json-buffer-p ()
  "Return non-nil when the current buffer contains JSON-family data."
  (memq (chief/jsts-buffer-kind) '(json jsonc)))

(defun chief/jsts-buffer-language ()
  "Return the execution language for the current buffer."
  (pcase (chief/jsts-buffer-kind)
    ((or 'tsx 'typescript) 'typescript)
    ((or 'jsx 'javascript) 'javascript)
    (_ nil)))

(defun chief/jsts-language-display-name (&optional language)
  "Return a human-readable display name for LANGUAGE."
  (pcase (or language (chief/jsts-buffer-kind) (chief/jsts-buffer-language))
    ('tsx "TSX")
    ('jsx "JSX")
    ('typescript "TypeScript")
    ('javascript "JavaScript")
    ('json "JSON")
    ('jsonc "JSONC")
    (_ "JavaScript/TypeScript")))

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
  "Return the runtime for the current JS/TS buffer or related config file."
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

(defun chief/jsts-esbuild-available-p ()
  "Return non-nil when `esbuild' is available."
  (executable-find chief/jsts-esbuild-command))

(defun chief/jsts-transpile-file-to-js (file language)
  "Transpile FILE written in LANGUAGE to a temporary JavaScript module.
Return the path to the transpiled file."
  (unless (chief/jsts-esbuild-available-p)
    (user-error "esbuild is required to run %s on QuickJS" (chief/jsts-language-display-name language)))
  (let ((output (make-temp-file "chief-jsts-esbuild-" nil ".mjs")))
    (chief/jsts-run-command
     (list chief/jsts-esbuild-command
           file
           "--format=esm"
           "--platform=neutral"
           "--target=es2022"
           (format "--outfile=%s" output))
     (chief/jsts-execution-root))
    output))

(defun chief/jsts-transpile-string-to-js (string language)
  "Transpile STRING written in LANGUAGE to JavaScript and return it."
  (let* ((source (chief/jsts-write-temp-file string language))
         (output nil))
    (unwind-protect
        (progn
          (setq output (chief/jsts-transpile-file-to-js source language))
          (with-temp-buffer
            (insert-file-contents output)
            (buffer-string)))
      (when (and source (file-exists-p source))
        (delete-file source))
      (when (and output (file-exists-p output))
        (delete-file output)))))

(defun chief/jsts-treesit-ready-for-mode-p (language mode)
  "Return non-nil when LANGUAGE and MODE can be used right now."
  (and (fboundp mode)
       (fboundp 'treesit-ready-p)
       (treesit-ready-p language t)))

(defun chief/jsts-javascript-major-mode ()
  "Activate the preferred JavaScript major mode."
  (interactive)
  (if (chief/jsts-treesit-ready-for-mode-p 'javascript 'js-ts-mode)
      (js-ts-mode)
    (js-mode)))

(defun chief/jsts-typescript-major-mode ()
  "Activate the preferred TypeScript major mode."
  (interactive)
  (if (chief/jsts-treesit-ready-for-mode-p 'typescript 'typescript-ts-mode)
      (typescript-ts-mode)
    (js-mode)))

(defun chief/jsts-tsx-major-mode ()
  "Activate the preferred TSX major mode."
  (interactive)
  (cond
   ((chief/jsts-treesit-ready-for-mode-p 'tsx 'tsx-ts-mode)
    (tsx-ts-mode))
   ((fboundp 'web-mode)
    (web-mode))
   (t
    (js-mode))))

(defun chief/jsts-json-major-mode ()
  "Activate the preferred JSON major mode."
  (interactive)
  (cond
   ((chief/jsts-treesit-ready-for-mode-p 'json 'json-ts-mode)
    (json-ts-mode))
   ((fboundp 'json-mode)
    (json-mode))
   ((fboundp 'js-json-mode)
    (js-json-mode))
   (t
    (js-mode))))

(defun chief/jsts-jsonc-major-mode ()
  "Activate the preferred JSONC major mode."
  (interactive)
  (cond
   ((fboundp 'jsonc-mode)
    (jsonc-mode))
   ((chief/jsts-treesit-ready-for-mode-p 'json 'json-ts-mode)
    (json-ts-mode))
   ((fboundp 'json-mode)
    (json-mode))
   ((fboundp 'js-json-mode)
    (js-json-mode))
   (t
    (js-mode))))

(defun chief/jsts-node-repl-command (&optional language)
  "Return the Node.js REPL command for LANGUAGE."
  (let ((language (or language (chief/jsts-buffer-kind))))
    (if (memq language '(typescript tsx jsx))
        (when-let* ((tsx (executable-find "tsx")))
          (list tsx))
      (when-let* ((node (executable-find "node")))
        (list node)))))

(defun chief/jsts-repl-command (&optional runtime language)
  "Return the command list used to start a REPL for RUNTIME and LANGUAGE."
  (let ((runtime (or runtime (chief/jsts-current-runtime)))
        (language (or language (chief/jsts-buffer-kind))))
    (pcase runtime
      ('node
       (chief/jsts-node-repl-command language))
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
         (language (or language (chief/jsts-buffer-kind) (chief/jsts-buffer-language) 'javascript))
         (extra (chief/jsts-runtime-extra-arguments params)))
    (pcase runtime
      ('node
       (cond
        ((memq language '(typescript tsx jsx))
         (when-let* ((tsx (executable-find "tsx")))
           (append (list tsx) extra (list file))))
        ((when-let* ((node (executable-find "node")))
           (append (list node) extra (list file))))))
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
       (when (memq language '(javascript json jsonc))
         (when-let* ((qjs (executable-find chief/jsts-quickjs-command)))
           (append (list qjs)
                   extra
                   (list file))))))))

(defun chief/jsts-language-extension (&optional language)
  "Return a suitable file extension for LANGUAGE."
  (pcase (or language (chief/jsts-buffer-kind) (chief/jsts-buffer-language))
    ('tsx ".tsx")
    ('typescript ".ts")
    ('jsx ".jsx")
    ('jsonc ".jsonc")
    ('json ".json")
    (_ ".js")))

(defun chief/jsts-write-temp-file (body &optional language)
  "Write BODY to a temporary file for LANGUAGE and return the file path."
  (let* ((language (or language (chief/jsts-buffer-kind) (chief/jsts-buffer-language) 'javascript))
         (root (chief/jsts-execution-root))
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
  (let ((default-directory (or directory (chief/jsts-execution-root)))
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
  (let* ((runtime (or runtime (chief/jsts-current-runtime)))
         (language (or language (chief/jsts-buffer-kind) (chief/jsts-buffer-language) 'javascript))
         (source-file (or file
                          buffer-file-name
                          (chief/jsts-write-temp-file (buffer-substring-no-properties
                                                       (point-min)
                                                       (point-max))
                                                      language))))
    (if (and (eq runtime 'quickjs)
             (memq language '(typescript tsx jsx)))
        (let ((transpiled-file nil))
          (unwind-protect
              (progn
                (setq transpiled-file (chief/jsts-transpile-file-to-js source-file language))
                (chief/jsts-run-command
                 (append (list (or (executable-find chief/jsts-quickjs-command)
                                   (user-error "QuickJS is not available on PATH")))
                         (chief/jsts-runtime-extra-arguments params)
                         (list transpiled-file))
                 (chief/jsts-execution-root)))
            (when (and transpiled-file (file-exists-p transpiled-file))
              (delete-file transpiled-file))))
      (let ((command (chief/jsts-script-command source-file runtime language params)))
        (unless command
          (user-error "No executable is configured for %s on the %s runtime"
                      (chief/jsts-language-display-name language)
                      (chief/jsts-runtime-display-name runtime)))
        (chief/jsts-run-command command (chief/jsts-execution-root))))))

(defun chief/jsts-describe-runtime ()
  "Display the current JS/TS runtime choice."
  (interactive)
  (message "%s runtime for this buffer: %s"
           (chief/jsts-language-display-name)
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

(defun chief/jsts-project-label ()
  "Return a human-readable project label for the current buffer."
  (let* ((project-root (chief/jsts-project-root))
         (execution-root (chief/jsts-execution-root))
         (project-name (file-name-nondirectory
                        (directory-file-name project-root)))
         (relative (unless (equal execution-root project-root)
                     (directory-file-name
                      (file-relative-name execution-root project-root)))))
    (if (and relative (not (string-empty-p relative)) (not (equal relative ".")))
        (format "%s:%s" project-name relative)
      project-name)))

(defun chief/jsts-repl-buffer-name (&optional runtime)
  "Return the REPL buffer name for RUNTIME."
  (let ((runtime (or runtime (chief/jsts-current-runtime))))
    (format "*%s[%s:%s]*"
            (pcase (chief/jsts-buffer-kind)
              ((or 'typescript 'tsx) "ts")
              (_ "js"))
            runtime
            (chief/jsts-project-label))))

(defun chief/jsts-unsupported-repl-message (&optional runtime)
  "Return the error message for an unsupported REPL under RUNTIME."
  (pcase (or runtime (chief/jsts-current-runtime))
    ('node
     (if (memq (chief/jsts-buffer-kind) '(typescript tsx jsx))
         "Node.js requires `tsx` on PATH for interactive TypeScript/JSX/TSX."
       "Node.js is not available on PATH"))
    ('quickjs
     (if (and (memq (chief/jsts-buffer-kind) '(typescript tsx jsx))
              (not (chief/jsts-esbuild-available-p)))
         "QuickJS needs `esbuild` on PATH for interactive TypeScript/JSX/TSX."
       "QuickJS is not available on PATH"))
    ('bun "Bun is not available on PATH")
    ('deno "Deno is not available on PATH")
    (_ "No JS/TS runtime is configured for this buffer")))

(defun chief/jsts-ensure-repl-buffer ()
  "Return a live REPL buffer for the current JS/TS buffer."
  (let* ((runtime (chief/jsts-current-runtime))
         (language (chief/jsts-buffer-kind))
         (buffer-name (chief/jsts-repl-buffer-name runtime))
         (buffer (get-buffer-create buffer-name))
         (command (chief/jsts-repl-command runtime language))
         (default-directory (chief/jsts-execution-root)))
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
  (let* ((runtime (chief/jsts-current-runtime))
         (language (or (chief/jsts-buffer-kind) (chief/jsts-buffer-language) 'javascript))
         (payload (if (and (eq runtime 'quickjs)
                           (memq language '(typescript tsx jsx)))
                      (chief/jsts-transpile-string-to-js string language)
                    string))
         (buffer (chief/jsts-ensure-repl-buffer))
         (process (get-buffer-process buffer)))
    (comint-send-string process payload)
    (unless (string-suffix-p "\n" payload)
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
  (when (chief/jsts-code-buffer-p)
    (setq-local lsp-disabled-clients '(jsts-ls flow-ls))
    (if (chief/jsts-deno-project-p)
        (progn
          (setq-local lsp-enabled-clients '(deno-ls))
          (setq-local lsp-disabled-clients '(ts-ls jsts-ls flow-ls))
          (when-let* ((config (chief/jsts-deno-config-file)))
            (setq-local lsp-clients-deno-config config)))
      (setq-local lsp-enabled-clients '(ts-ls))
      (setq-local lsp-disabled-clients '(deno-ls jsts-ls flow-ls)))))

(defun chief/jsts-common-setup ()
  "Configure shared workspace integration for JS/TS-related buffers."
  (setq-local chief/lsp-root-function #'chief/jsts-project-root)
  (when (boundp 'js-indent-level)
    (setq-local js-indent-level 2))
  (when (boundp 'typescript-ts-mode-indent-offset)
    (setq-local typescript-ts-mode-indent-offset 2))
  (when (boundp 'typescript-indent-level)
    (setq-local typescript-indent-level 2))
  (when (boundp 'web-mode-code-indent-offset)
    (setq-local web-mode-code-indent-offset 2))
  (when (boundp 'web-mode-markup-indent-offset)
    (setq-local web-mode-markup-indent-offset 2))
  (when (boundp 'web-mode-css-indent-offset)
    (setq-local web-mode-css-indent-offset 2)))

(defun chief/jsts-related-mode-setup ()
  "Configure JSON and JSONC buffers related to JS/TS projects."
  (chief/jsts-common-setup)
  (chief/repl-configure)
  (when (fboundp 'chief/lsp-managed-mode-setup)
    (chief/lsp-managed-mode-setup)))

(defun chief/jsts-mode-setup ()
  "Configure runtime, REPL, and LSP support for JS/TS buffers."
  (when (chief/jsts-code-buffer-p)
    (chief/jsts-common-setup)
    (chief/repl-configure
     :start #'chief/jsts-start-repl
     :restart #'chief/jsts-restart-repl
     :send-line #'chief/jsts-send-line
     :send-region #'chief/jsts-send-region
     :send-buffer #'chief/jsts-send-buffer
     :load-file #'chief/jsts-load-file)
    (chief/jsts-configure-lsp)
    (when (fboundp 'chief/lsp-managed-mode-setup)
      (chief/lsp-managed-mode-setup))))

(defun chief/jsts-web-mode-setup ()
  "Configure `web-mode' buffers that actually contain JSX or TSX."
  (when (chief/jsts-web-template-buffer-p)
    (chief/jsts-mode-setup)))

(defconst chief/jsts-auto-mode-alist
  '(("\\.jsonc\\'" . chief/jsts-jsonc-major-mode)
    ("\\.tsx\\'" . chief/jsts-tsx-major-mode)
    ("\\.\\(?:d\\.\\)?\\(?:mts\\|cts\\|ts\\)\\'" . chief/jsts-typescript-major-mode)
    ("\\.mjsx\\'" . chief/jsts-javascript-major-mode)
    ("\\.jsx\\'" . chief/jsts-javascript-major-mode)
    ("\\.mjs\\'" . chief/jsts-javascript-major-mode)
    ("\\.cjs\\'" . chief/jsts-javascript-major-mode)
    ("\\.js\\'" . chief/jsts-javascript-major-mode)
    ("\\.json\\'" . chief/jsts-json-major-mode))
  "Preferred auto-mode dispatch for JS/TS-related files.")

(dolist (entry (reverse chief/jsts-auto-mode-alist))
  (add-to-list 'auto-mode-alist entry))

(with-eval-after-load 'lsp-mode
  (require 'lsp-javascript)
  (setq lsp-clients-typescript-prefer-use-project-ts-server t)
  (setq lsp-javascript-format-enable t)
  (setq lsp-typescript-format-enable t)
  (when-let* ((tsls (executable-find "typescript-language-server")))
    (setq lsp-clients-typescript-tls-path tsls))
  (when-let* ((npm (executable-find "npm")))
    (setq lsp-clients-typescript-npm-location npm))
  (dolist (entry '(("\\.mjs\\'" . "javascript")
                   ("\\.cjs\\'" . "javascript")
                   ("\\.mjsx\\'" . "javascriptreact")
                   ("\\.mts\\'" . "typescript")
                   ("\\.cts\\'" . "typescript")
                   ("\\.d\\.ts\\'" . "typescript")
                   ("\\.d\\.mts\\'" . "typescript")
                   ("\\.d\\.cts\\'" . "typescript")))
    (add-to-list 'lsp-language-id-configuration entry)))

(add-hook 'js-mode-hook #'chief/jsts-mode-setup)
(add-hook 'js-ts-mode-hook #'chief/jsts-mode-setup)
(add-hook 'web-mode-hook #'chief/jsts-web-mode-setup)
(when (fboundp 'typescript-ts-mode)
  (add-hook 'typescript-ts-mode-hook #'chief/jsts-mode-setup))
(when (fboundp 'tsx-ts-mode)
  (add-hook 'tsx-ts-mode-hook #'chief/jsts-mode-setup))
(add-hook 'js-json-mode-hook #'chief/jsts-related-mode-setup)
(when (fboundp 'json-mode)
  (add-hook 'json-mode-hook #'chief/jsts-related-mode-setup))
(when (fboundp 'jsonc-mode)
  (add-hook 'jsonc-mode-hook #'chief/jsts-related-mode-setup))
(when (fboundp 'json-ts-mode)
  (add-hook 'json-ts-mode-hook #'chief/jsts-related-mode-setup))

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

(with-eval-after-load 'web-mode
  (chief/repl-setup-standard-local-leader 'web-mode-map)
  (chief/local-leader-def
    :keymaps 'web-mode-map
    "r" '(:ignore t :which-key "runtime")
    "rr" #'chief/jsts-set-runtime
    "ri" #'chief/jsts-describe-runtime
    "rf" #'chief/jsts-run-file))

(provide 'lang-jsts)
;;; lang-jsts.el ends here
