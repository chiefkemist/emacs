;;; lang-mojo.el --- Mojo 1.x and Pixi support -*- lexical-binding: t; -*-

;;; Commentary:

;; Mojo is new enough that this module deliberately owns the editor integration
;; instead of depending on an external Mojo major mode or an incomplete
;; Tree-sitter grammar.  The compiler, formatter, REPL, and language server are
;; always resolved through the current Pixi workspace.

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'comint)
(require 'core-format)
(require 'core-lsp)
(require 'core-projects)
(require 'core-repl)
(require 'imenu)
(require 'json)
(require 'python)
(require 'subr-x)
(require 'xref)

(declare-function lsp--display-contents "lsp-mode" (contents))
(declare-function lsp--locations-to-xref-items "lsp-mode" (locations))
(declare-function lsp--text-document-position-params "lsp-mode" ())
(declare-function dap-debug "dap-mode" (debug-args))
(declare-function lsp-completion-mode "lsp-completion" (&optional arg))
(declare-function lsp-execute-code-action "lsp-mode" ())
(declare-function lsp-inlay-hints-mode "lsp-mode" (&optional arg))
(declare-function lsp-rename "lsp-mode" ())
(declare-function lsp-request-async "lsp-mode"
                  (method params callback &rest args))
(declare-function lsp-semantic-tokens-mode "lsp-semantic-tokens" (&optional arg))
(declare-function lsp-workspace-restart "lsp-mode" ())

(defconst chief/mojo--repl-integration-version 3
  "Version of the managed REPL frontend requiring a fresh process.")

(defvar-local chief/mojo--repl-configured-version nil
  "Managed REPL integration version configured in this buffer.")

(defvar-local chief/mojo--repl-session-id nil
  "Unique marker prefix for the current managed Mojo REPL process.")

(defvar-local chief/mojo--repl-evaluation-counter 0
  "Number of evaluations submitted to the current managed Mojo REPL.")

(defvar-local chief/mojo--repl-expected-marker nil
  "Completion marker expected from the current Mojo evaluation.")

(defvar-local chief/mojo--repl-pending-output ""
  "Process output held until the current Mojo evaluation completes.")

(defvar-local chief/mojo--repl-busy nil
  "Non-nil while the managed Mojo REPL is evaluating input.")

(defvar-local chief/mojo--repl-prompt "mojo> "
  "Prompt displayed by the managed Mojo REPL frontend.")

(defgroup chief-mojo nil
  "Mojo 1.x development through project-local Pixi environments."
  :group 'chief)

(defcustom chief/mojo-pixi-environment 'auto
  "Pixi environment used for Mojo tools.
The value `auto' prefers an installed `default' environment containing the
Mojo SDK, then another installed Mojo environment.  Nil lets Pixi select its
default environment, and a string always selects that named environment."
  :type '(choice (const :tag "Auto-detect Mojo SDK" auto)
                 (const :tag "Pixi default" nil)
                 string)
  :group 'chief-mojo)

(defcustom chief/mojo-package-spec "mojo>=1.1.0,<2"
  "Conda package specification used when installing Mojo into a Pixi project."
  :type 'string
  :group 'chief-mojo)

(defcustom chief/mojo-modular-channel "https://conda.modular.com/max"
  "Conda channel containing stable Mojo SDK packages."
  :type 'string
  :group 'chief-mojo)

(defcustom chief/mojo-jupyter-package-specs '("jupyterlab" "ipykernel")
  "Conda package specifications installed for Mojo notebook editing."
  :type '(repeat string)
  :group 'chief-mojo)

(defcustom chief/mojo-include-directories '("src")
  "Project-relative or absolute import directories passed to Mojo tools.
Only directories that exist are included."
  :type '(repeat directory)
  :group 'chief-mojo)

(defcustom chief/mojo-compiler-arguments nil
  "Additional compiler arguments passed before a Mojo source path."
  :type '(repeat string)
  :group 'chief-mojo)

(defcustom chief/mojo-runtime-arguments nil
  "Default runtime arguments passed after a Mojo source path."
  :type '(repeat string)
  :group 'chief-mojo)

(defcustom chief/mojo-format-on-save t
  "When non-nil, format Mojo buffers through the Pixi SDK before saving."
  :type 'boolean
  :group 'chief-mojo)

(defcustom chief/mojo-indent-offset 4
  "Number of spaces used for one Mojo indentation level."
  :type 'integer
  :safe 'integerp
  :group 'chief-mojo)

(defcustom chief/mojo-project-excluded-directories
  '(".git" ".pixi" ".jj" ".cache" "build" "dist" "node_modules" "target")
  "Directory names excluded from Mojo project-wide file discovery."
  :type '(repeat string)
  :group 'chief-mojo)

(defcustom chief/mojo-analysis-ready-delay 1.0
  "Seconds to wait after `textDocument/didOpen' before semantic requests.
Mojo 1.1 can crash when completion or navigation is requested while its
initial document analysis is still starting.  Diagnostics can mark the buffer
ready sooner when the server publishes them."
  :type 'number
  :group 'chief-mojo)

(defcustom chief/mojo-stdlib-source-directory
  (expand-file-name "var/mojo/sources/mojo-v1.1.0/Mojo/stdlib"
                    user-emacs-directory)
  "Local checkout of the official Mojo standard-library sources.
This is used only when Mojo's language server returns no definition location
for a standard-library symbol."
  :type 'directory
  :group 'chief-mojo)

(defcustom chief/mojo-dap-stop-on-entry nil
  "When non-nil, Mojo DAP sessions stop as soon as the program launches."
  :type 'boolean
  :group 'chief-mojo)

(defvar chief/mojo-project-environments (make-hash-table :test #'equal)
  "Session-local Pixi environment selections keyed by workspace root.")

(defvar-local chief/mojo-lsp-ready nil
  "Non-nil when the Mojo server is safe to receive semantic requests.")

(defvar-local chief/mojo-analysis-ready-timer nil
  "Timer providing a bounded fallback for Mojo analysis readiness.")

(defvar-local chief/mojo-pending-semantic-requests nil
  "Callbacks waiting for the current Mojo document to become ready.")

(defconst chief/mojo-keywords
  '("alias" "and" "as" "assert" "async" "await" "break" "capturing"
    "case" "class" "comptime" "continue" "def" "deinit" "del" "elif"
    "else" "except" "finally" "fn" "for" "from" "global" "if" "imm"
    "import" "in" "is" "lambda" "let" "match" "mut" "nonlocal" "not"
    "or" "out" "owned" "pass" "raise" "raises" "read" "ref" "return"
    "struct" "trait" "try" "var" "where" "while" "with" "yield")
  "Mojo 1.x language keywords and argument conventions.")

(defconst chief/mojo-builtin-types
  '("AnyType" "Bool" "DType" "Error" "Float16" "Float32" "Float64"
    "Int" "Int8" "Int16" "Int32" "Int64" "List" "NoneType" "Optional"
    "Pointer" "Scalar" "SIMD" "Span" "String" "StringSpan" "UInt"
    "UInt8" "UInt16" "UInt32" "UInt64")
  "Common Mojo 1.x built-in types used for fallback highlighting.")

(defconst chief/mojo-builtins
  '("abort" "abs" "all" "any" "ascii" "bin" "chr" "debug_assert"
    "enumerate" "hash" "hex" "input" "len" "map" "max" "min" "next"
    "oct" "open" "ord" "pow" "print" "range" "repr" "reversed" "round"
    "slice" "sort" "swap" "type_of" "zip")
  "Common Mojo built-ins used for fallback highlighting.")

(defvar chief/mojo-mode-syntax-table
  (let ((table (make-syntax-table python-mode-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    table)
  "Syntax table used by `chief/mojo-mode'.")

(defconst chief/mojo-definition-regexp
  "^[ \t]*\\(?:async[ \t]+\\)?\\(?:def\\|fn\\|class\\|struct\\|trait\\)[ \t]+\\([[:alpha:]_][[:alnum:]_]*\\)"
  "Regexp matching a named Mojo declaration.")

(defconst chief/mojo-test-regexp
  "^[ \t]*\\(?:async[ \t]+\\)?\\(?:def\\|fn\\)[ \t]+\\(test_[[:alnum:]_]+\\)[ \t]*[([]"
  "Regexp matching a Mojo TestSuite test function.")

(defvar chief/mojo-font-lock-keywords
  `(("^[ \t]*\\(?:async[ \t]+\\)?\\(?:def\\|fn\\)[ \t]+\\([[:alpha:]_][[:alnum:]_]*\\)"
     1 font-lock-function-name-face)
    ("^[ \t]*\\(?:class\\|struct\\|trait\\)[ \t]+\\([[:alpha:]_][[:alnum:]_]*\\)"
     1 font-lock-type-face)
    ("@[[:alpha:]_][[:alnum:]_]*" . font-lock-preprocessor-face)
    (,(regexp-opt chief/mojo-keywords 'symbols) . font-lock-keyword-face)
    (,(regexp-opt '("True" "False" "None") 'symbols) . font-lock-constant-face)
    (,(regexp-opt chief/mojo-builtin-types 'symbols) . font-lock-type-face)
    (,(regexp-opt chief/mojo-builtins 'symbols) . font-lock-builtin-face)
    ("\\_<[[:digit:]]+\\(?:\\.[[:digit:]]+\\)?\\(?:[eE][+-]?[[:digit:]]+\\)?\\_>"
     . font-lock-number-face))
  "Conservative Mojo font-lock rules supplemented by LSP semantic tokens.")

(defvar chief/mojo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'chief/mojo-run-or-check)
    (define-key map (kbd "C-c C-b") #'chief/mojo-build-current-file)
    (define-key map (kbd "C-c C-k") #'chief/mojo-check-file)
    (define-key map (kbd "C-c C-t") #'chief/mojo-test-at-point)
    map)
  "Keymap for `chief/mojo-mode'.")

(defun chief/mojo--pixi-root-p (directory)
  "Return non-nil when DIRECTORY contains a Pixi workspace manifest.
Package-only manifests in a Pixi build monorepo do not form workspace roots."
  (or (chief/mojo--manifest-defines-workspace-p
       (expand-file-name "pixi.toml" directory))
      (chief/mojo--manifest-defines-workspace-p
       (expand-file-name "pyproject.toml" directory))))

(defun chief/mojo--manifest-defines-workspace-p (file)
  "Return non-nil when Pixi manifest FILE owns an independent workspace.
Package-only manifests used by Pixi build intentionally return nil."
  (and (file-readable-p file)
       (with-temp-buffer
         (insert-file-contents file nil 0 131072)
         (goto-char (point-min))
         (re-search-forward
          "^[ \t]*\\[\\(?:tool\\.pixi\\.\\)?\\(?:workspace\\|project\\)\\(?:\\.\\|\\]\\)"
          nil t))))

(defun chief/mojo--nested-workspace-directory-p (directory root)
  "Return non-nil when DIRECTORY below ROOT owns another Pixi workspace."
  (and (not (file-equal-p directory root))
       (let ((pixi (expand-file-name "pixi.toml" directory))
             (pyproject (expand-file-name "pyproject.toml" directory)))
         (or (chief/mojo--manifest-defines-workspace-p pixi)
             (chief/mojo--manifest-defines-workspace-p pyproject)))))

(defun chief/mojo--start-directory (&optional start)
  "Return a directory suitable for project searches beginning at START."
  (or start
      (and buffer-file-name (file-name-directory buffer-file-name))
      default-directory))

(defun chief/mojo-pixi-root (&optional start)
  "Return the nearest Pixi workspace root above START."
  (when-let* ((root (locate-dominating-file
                     (chief/mojo--start-directory start)
                     #'chief/mojo--pixi-root-p)))
    (file-name-as-directory (expand-file-name root))))

(defun chief/mojo-legacy-project-root (&optional start)
  "Return the nearest legacy `mojoproject.toml' root above START."
  (when-let* ((root (locate-dominating-file
                     (chief/mojo--start-directory start)
                     "mojoproject.toml")))
    (file-name-as-directory (expand-file-name root))))

(defun chief/mojo-project-root ()
  "Return the preferred project root for the current Mojo buffer."
  (chief/project-preferred-root
   (chief/mojo-pixi-root)
   (chief/mojo-legacy-project-root)
   (chief/project-current-root)
   default-directory))

(defun chief/mojo-require-pixi-root ()
  "Return the current Pixi root or signal a helpful user error."
  (or (chief/mojo-pixi-root)
      (user-error
       "No Pixi workspace found; create one with M-x chief/mojo-init-project")))

(defun chief/mojo-pixi-executable ()
  "Return the Pixi executable or signal a user error."
  (or (executable-find "pixi")
      (user-error "Pixi is not available on PATH")))

(defun chief/mojo--environment-setting (&optional root)
  "Return the configured environment setting for ROOT."
  (let ((root (or root (chief/mojo-pixi-root))))
    (or (and root (gethash root chief/mojo-project-environments))
        chief/mojo-pixi-environment)))

(defun chief/mojo-installed-sdk-environments (&optional root)
  "Return installed Pixi environments containing a full Mojo SDK under ROOT."
  (let* ((root (or root (chief/mojo-pixi-root)))
         (directory (and root (expand-file-name ".pixi/envs" root)))
         environments)
    (when (file-directory-p directory)
      (dolist (entry (directory-files directory t directory-files-no-dot-files-regexp))
        (when (and (file-directory-p entry)
                   (file-exists-p (expand-file-name "share/max/modular.cfg" entry))
                   (or (file-executable-p (expand-file-name "bin/mojo" entry))
                       (file-executable-p (expand-file-name "Scripts/mojo.exe" entry))))
          (push (file-name-nondirectory entry) environments))))
    (sort environments
          (lambda (left right)
            (cond
             ((string= left "default") (not (string= right "default")))
             ((string= right "default") nil)
             (t (string-lessp left right)))))))

(defun chief/mojo-resolved-environment (&optional root)
  "Return the Pixi environment Mojo commands should use under ROOT.
A nil return value asks Pixi to use its default environment."
  (let ((setting (chief/mojo--environment-setting root)))
    (cond
     ((eq setting 'auto)
      (car (chief/mojo-installed-sdk-environments root)))
     ((and (stringp setting) (not (string-empty-p setting))) setting)
     (t nil))))

(defun chief/mojo--environment-arguments (&optional root)
  "Return Pixi environment-selection arguments for Mojo commands under ROOT."
  (when-let* ((environment (chief/mojo-resolved-environment root)))
    (list "--environment" environment)))

(defun chief/mojo-pixi-command (&rest command)
  "Return COMMAND wrapped for execution in the current Pixi environment."
  (let ((root (chief/mojo-require-pixi-root)))
    (append
     (list (chief/mojo-pixi-executable)
           "run"
           "--manifest-path" root)
     (chief/mojo--environment-arguments)
     (list "--no-progress" "--color" "never" "--frozen" "--executable")
     command)))

(defun chief/mojo-command (&rest args)
  "Return the Pixi-backed `mojo' command with ARGS."
  (apply #'chief/mojo-pixi-command "mojo" args))

(defun chief/mojo-include-directories (&optional root)
  "Return configured and discovered Mojo import directories under ROOT.
Discovery includes the workspace root, nested Pixi members, their `src'
directories, and parents of Mojo package directories containing
`__init__.mojo'."
  (let* ((root (or root (chief/mojo-require-pixi-root)))
         (predicate
          (lambda (directory)
            (let ((name
                   (file-name-nondirectory (directory-file-name directory))))
              (and (not (string-prefix-p "." name))
                   (not (member name chief/mojo-project-excluded-directories))
                   (not (chief/mojo--nested-workspace-directory-p
                         (file-name-as-directory (expand-file-name directory))
                         (file-name-as-directory (expand-file-name root))))))))
         (configured
          (cl-loop for directory in chief/mojo-include-directories
                   for path = (if (file-name-absolute-p directory)
                                  (expand-file-name directory)
                                (expand-file-name directory root))
                   when (file-directory-p path)
                   collect path))
         (package-parents
          (cl-loop for init in (directory-files-recursively
                                root "__init__\\.mojo\\'"
                                nil predicate nil)
                   collect (file-name-directory
                            (directory-file-name
                             (file-name-directory init)))))
         (workspace-directories
          (cl-loop for manifest in (directory-files-recursively
                                    root "pixi\\.toml\\'"
                                    nil predicate nil)
                   for directory = (file-name-directory manifest)
                   unless (and (not (file-equal-p directory root))
                               (chief/mojo--manifest-defines-workspace-p manifest))
                   append (list directory
                                (expand-file-name "src" directory)))))
    (mapcar #'file-name-as-directory
            (delete-dups
             (cl-remove-if-not
              #'file-directory-p
              (append (list root)
                      configured package-parents workspace-directories))))))

(defun chief/mojo-include-arguments (&optional root)
  "Return `-I' arguments for configured import directories under ROOT."
  (cl-mapcan (lambda (directory) (list "-I" directory))
             (chief/mojo-include-directories root)))

(defun chief/mojo-lsp-command ()
  "Return the project-local Pixi command for the Mojo language server."
  (append (chief/mojo-pixi-command "mojo-lsp-server")
          (chief/mojo-include-arguments)))

(defun chief/mojo-pixi-environment-root (&optional root)
  "Return the selected Pixi environment directory under ROOT."
  (let ((root (or root (chief/mojo-pixi-root))))
    (when root
      (expand-file-name
       (or (chief/mojo-resolved-environment root) "default")
       (expand-file-name ".pixi/envs" root)))))

(defun chief/mojo-pixi-environment-names (&optional root)
  "Return environments declared by the Pixi workspace under ROOT."
  (let* ((root (or root (chief/mojo-require-pixi-root)))
         (pixi (chief/mojo-pixi-executable))
         (data (chief/mojo--process-json
                (list pixi "info" "--manifest-path" root "--json") root)))
    (delete-dups
     (delq nil
           (mapcar (lambda (entry) (alist-get 'name entry))
                   (alist-get 'environments_info data))))))

(defun chief/mojo-select-pixi-environment (environment)
  "Select Pixi ENVIRONMENT for this Mojo workspace and restart its LSP."
  (interactive
   (let ((names (chief/mojo-pixi-environment-names)))
     (list (completing-read "Mojo Pixi environment: " names nil t nil nil
                            (chief/mojo-resolved-environment)))))
  (let ((root (chief/mojo-require-pixi-root)))
    (puthash root environment chief/mojo-project-environments)
    (when (bound-and-true-p lsp-mode)
      (lsp-workspace-restart))
    (message "Mojo now uses Pixi environment %s in %s" environment root)))

(defun chief/mojo-lsp-available-p ()
  "Return non-nil when the current Pixi environment contains the Mojo LSP."
  (when-let* ((pixi (executable-find "pixi"))
              (environment (chief/mojo-pixi-environment-root)))
    (and pixi
         (file-executable-p (expand-file-name "bin/mojo" environment))
         (file-executable-p
          (expand-file-name "bin/mojo-lsp-server" environment)))))

(defun chief/mojo-project-tooling-installable-p ()
  "Return non-nil when Mojo can be installed into the current Pixi project."
  (and (executable-find "pixi") (chief/mojo-pixi-root)))

(defun chief/mojo--shell-command (command)
  "Return shell-quoted COMMAND as a string."
  (mapconcat #'shell-quote-argument command " "))

(defun chief/mojo-compile (command name &optional directory on-success)
  "Run COMMAND in DIRECTORY using compilation buffer NAME.
Call ON-SUCCESS after a successful exit when it is non-nil."
  (let* ((default-directory
          (file-name-as-directory
           (expand-file-name
            (or directory (chief/mojo-project-root) default-directory))))
         (compilation-read-command nil)
         (buffer
          (compilation-start
           (chief/mojo--shell-command command)
           'compilation-mode
           (lambda (_) name))))
    (when on-success
      (with-current-buffer buffer
        (add-hook
         'compilation-finish-functions
         (lambda (_buffer status)
           (when (string-prefix-p "finished" status)
             (funcall on-success)))
         nil t)))
    buffer))

(defun chief/mojo--compile-sequence (commands name directory on-success)
  "Run shell COMMANDS sequentially in a compilation buffer.
NAME, DIRECTORY, and ON-SUCCESS have the same meaning as in
`chief/mojo-compile'."
  (let* ((default-directory (file-name-as-directory (expand-file-name directory)))
         (compilation-read-command nil)
         (command (string-join (mapcar #'chief/mojo--shell-command commands)
                               " && "))
         (buffer (compilation-start command 'compilation-mode (lambda (_) name))))
    (when on-success
      (with-current-buffer buffer
        (add-hook
         'compilation-finish-functions
         (lambda (_buffer status)
           (when (string-prefix-p "finished" status)
             (funcall on-success)))
         nil t)))
    buffer))

(defun chief/mojo-refresh-tooling-buffers ()
  "Refresh formatting support in open Mojo buffers after SDK installation."
  (when (boundp 'chief/lsp-command-probe-cache)
    (clrhash chief/lsp-command-probe-cache))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'chief/mojo-mode)
        (when (and chief/mojo-format-on-save
                   (chief/mojo-lsp-available-p))
          (chief/format-enable-on-save #'chief/mojo-format-buffer))))))

(defun chief/mojo-install-project-tooling (&optional on-success)
  "Install the pinned Mojo SDK into the current Pixi workspace.
The Modular and conda-forge channels are added first.  Call ON-SUCCESS after a
successful installation when supplied by the shared LSP installer."
  (interactive)
  (let* ((root (chief/mojo-require-pixi-root))
         (pixi (chief/mojo-pixi-executable))
         (environment (chief/mojo-resolved-environment root))
         (environment-args
          (when (and environment (not (string= environment "default")))
            (list "--environment" environment)))
         (channel-command
          (append (list pixi "workspace" "channel" "add"
                        "--manifest-path" root "--prepend")
                  environment-args
                  (list chief/mojo-modular-channel "conda-forge")))
         (add-command
          (append (list pixi "add" "--manifest-path" root
                        "--no-progress" "--color" "never")
                  environment-args
                  (list chief/mojo-package-spec))))
    (chief/mojo--compile-sequence
     (list channel-command add-command)
     "*pixi install mojo*"
     root
     (lambda ()
       (chief/mojo-refresh-tooling-buffers)
       (when on-success
         (funcall on-success))))))

(defun chief/mojo-init-project (directory)
  "Create a Pixi Mojo 1.x project in DIRECTORY and install the SDK."
  (interactive
   (list (read-directory-name "New Mojo project directory: " nil nil nil)))
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (parent (file-name-directory (directory-file-name directory)))
         (pixi (chief/mojo-pixi-executable)))
    (when (chief/mojo--pixi-root-p directory)
      (user-error "A Pixi workspace already exists in %s" directory))
    (make-directory parent t)
    (chief/mojo--compile-sequence
     (list
      (list pixi "init" directory
            "--channel" chief/mojo-modular-channel
            "--channel" "conda-forge")
      (list pixi "add" "--manifest-path" directory
            "--no-progress" "--color" "never"
            chief/mojo-package-spec))
     "*pixi init mojo*"
     parent
     (lambda ()
       (message "Created Mojo project in %s" directory)))))

(defun chief/mojo-current-file ()
  "Return the current Mojo source file as an absolute path."
  (unless buffer-file-name
    (user-error "This buffer is not visiting a Mojo file"))
  (expand-file-name buffer-file-name))

(defun chief/mojo-save-current-buffer ()
  "Save the current Mojo buffer when it is modified."
  (when (and buffer-file-name (buffer-modified-p))
    (save-buffer)))

(defun chief/mojo-save-file-buffer (file)
  "Save FILE's visiting buffer when it has unsaved changes."
  (when-let* ((buffer (get-file-buffer (expand-file-name file))))
    (with-current-buffer buffer
      (when (buffer-modified-p)
        (save-buffer)))))

(defun chief/mojo-project-source-files (&optional root)
  "Return Mojo sources below ROOT, excluding generated and nested workspaces."
  (let ((root (file-name-as-directory
               (expand-file-name (or root (chief/mojo-require-pixi-root))))))
    (cl-remove-if
     (lambda (file)
       (string-prefix-p "." (file-name-nondirectory file)))
     (directory-files-recursively
      root "\\.\\(?:mojo\\|🔥\\)\\'" nil
      (lambda (directory)
        (let ((name (file-name-nondirectory (directory-file-name directory))))
          (and (not (string-prefix-p "." name))
               (not (member name chief/mojo-project-excluded-directories))
               (not (chief/mojo--nested-workspace-directory-p
                     (file-name-as-directory (expand-file-name directory))
                     root)))))
      nil))))

(defun chief/mojo--file-regexp-p (file regexp)
  "Return non-nil when Mojo FILE contains REGEXP outside comments or strings."
  (let ((buffer (get-file-buffer (expand-file-name file))))
    (if buffer
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-min))
            (catch 'found
              (while (re-search-forward regexp nil t)
                (unless (save-excursion
                          (nth 8 (syntax-ppss (match-beginning 0))))
                  (throw 'found t))))))
      (with-temp-buffer
        (insert-file-contents file)
        (set-syntax-table chief/mojo-mode-syntax-table)
        (setq-local syntax-propertize-function
                    python-syntax-propertize-function)
        (syntax-propertize (point-max))
        (goto-char (point-min))
        (catch 'found
          (while (re-search-forward regexp nil t)
            (unless (save-excursion
                      (nth 8 (syntax-ppss (match-beginning 0))))
              (throw 'found t))))))))

(defun chief/mojo-file-defines-main-p (file)
  "Return non-nil when FILE defines an executable Mojo `main'."
  (chief/mojo--file-regexp-p
   file "^[ \t]*\\(?:async[ \t]+\\)?\\(?:def\\|fn\\)[ \t]+main[ \t]*[([]"))

(defun chief/mojo-file-defines-tests-p (file)
  "Return non-nil when FILE defines at least one Mojo TestSuite test."
  (chief/mojo--file-regexp-p file chief/mojo-test-regexp))

(defun chief/mojo-project-runnable-files (&optional root)
  "Return executable Mojo files below ROOT."
  (cl-remove-if-not #'chief/mojo-file-defines-main-p
                    (chief/mojo-project-source-files root)))

(defun chief/mojo-project-test-files (&optional root)
  "Return Mojo TestSuite files below ROOT."
  (cl-remove-if-not #'chief/mojo-file-defines-tests-p
                    (chief/mojo-project-source-files root)))

(defun chief/mojo-project-package-directories (&optional root)
  "Return Mojo source package directories below ROOT.
A source package is a directory containing `__init__.mojo'."
  (let ((root (or root (chief/mojo-require-pixi-root))))
    (mapcar #'file-name-directory
            (cl-remove-if-not
             (lambda (file)
               (string= (file-name-nondirectory file) "__init__.mojo"))
             (chief/mojo-project-source-files root)))))

(defun chief/mojo-current-package-directory ()
  "Return the nearest Mojo source package containing the current file."
  (when-let* ((file (and buffer-file-name (expand-file-name buffer-file-name)))
              (root (chief/mojo-pixi-root file))
              (package (locate-dominating-file
                        (file-name-directory file) "__init__.mojo"))
              ((file-in-directory-p package root)))
    (file-name-as-directory (expand-file-name package))))

(defun chief/mojo-read-project-package ()
  "Select and return a Mojo source package in the current workspace."
  (let* ((root (chief/mojo-require-pixi-root))
         (packages (chief/mojo-project-package-directories root)))
    (unless packages
      (user-error "No Mojo package directory containing `__init__.mojo' was found"))
    (if (= (length packages) 1)
        (car packages)
      (chief/mojo-read-project-file "Mojo package: " packages
                                    (chief/mojo-current-package-directory)))))

(defun chief/mojo-read-project-file (prompt files &optional initial)
  "Read one of FILES with PROMPT, preferring INITIAL."
  (let* ((root (chief/mojo-require-pixi-root))
         (choices
          (mapcar (lambda (file)
                    (cons (file-relative-name file root) file))
                  files))
         (initial-name (and initial (file-relative-name initial root))))
    (cdr (assoc (completing-read prompt choices nil t nil nil initial-name)
                choices))))

(defun chief/mojo-resolve-runnable-file (&optional always-prompt)
  "Return an executable Mojo file in the current Pixi project.
Use the current file when it defines `main'.  ALWAYS-PROMPT forces project
selection even when the current file is executable."
  (let* ((current (and buffer-file-name (expand-file-name buffer-file-name)))
         (files (chief/mojo-project-runnable-files)))
    (cond
     ((null files)
      (user-error
       "No Mojo file defining `main' was found; use `, c c' to check this module"))
     ((and (not always-prompt) current
           (member current files))
      current)
     ((and (not always-prompt) (= (length files) 1))
      (car files))
     (t
      (chief/mojo-read-project-file "Mojo executable: " files current)))))

(defun chief/mojo-cache-directory ()
  "Return the directory used for transient Mojo artifacts."
  (let ((directory
         (expand-file-name
          "mojo/"
          (if (boundp 'chief/var-directory)
              chief/var-directory
            user-emacs-directory))))
    (make-directory directory t)
    directory))

(defun chief/mojo-artifact-path (&optional suffix extension file)
  "Return a stable transient artifact path for FILE.
Append SUFFIX before EXTENSION.  FILE defaults to the current source file."
  (let* ((file (or file (chief/mojo-current-file)))
         (name (replace-regexp-in-string
                "[^[:alnum:]_.-]" "_" (file-name-base file)))
         (hash (substring (secure-hash 'sha1 file) 0 12)))
    (expand-file-name
     (format "%s-%s%s%s"
             name hash (or suffix "")
             (or extension
                 (if (eq system-type 'windows-nt) ".exe" "")))
     (chief/mojo-cache-directory))))

(defun chief/mojo-read-arguments (prompt defaults)
  "Read shell-style arguments with PROMPT and DEFAULTS."
  (split-string-and-unquote
   (read-string prompt (string-join defaults " "))))

(defun chief/mojo-run-current-file (&optional prompt-arguments always-prompt)
  "Run a project Mojo executable through Pixi.
The current file is used when it defines `main'; otherwise select a project
entrypoint.  With PROMPT-ARGUMENTS, edit runtime arguments.  ALWAYS-PROMPT
forces entrypoint selection."
  (interactive (list current-prefix-arg nil))
  (let* ((file (chief/mojo-resolve-runnable-file always-prompt))
         (arguments
          (if prompt-arguments
              (chief/mojo-read-arguments
               "Mojo program arguments: " chief/mojo-runtime-arguments)
            chief/mojo-runtime-arguments))
         (command
          (append (chief/mojo-command "run")
                  chief/mojo-compiler-arguments
                  (chief/mojo-include-arguments)
                  (list file)
                  arguments)))
    (chief/mojo-save-file-buffer file)
    (chief/mojo-compile
     command
     (format "*mojo run %s*" (file-name-nondirectory file))
     (chief/mojo-require-pixi-root))))

(defun chief/mojo-run-project-file (&optional prompt-arguments)
  "Select and run a project Mojo executable.
With PROMPT-ARGUMENTS, edit runtime arguments."
  (interactive "P")
  (chief/mojo-run-current-file prompt-arguments t))

(defun chief/mojo-run-or-check (&optional prompt-arguments)
  "Run a project entrypoint, or check the current module when none exists.
With PROMPT-ARGUMENTS, edit runtime arguments when running."
  (interactive "P")
  (if (chief/mojo-project-runnable-files)
      (chief/mojo-run-current-file prompt-arguments)
    (chief/mojo-check-file)))

(defun chief/mojo-build-current-file (&optional prompt-output always-prompt)
  "Build a project Mojo executable to a transient binary.
The current file is preferred when it defines `main'.  With PROMPT-OUTPUT,
prompt for the output path.  ALWAYS-PROMPT forces entrypoint selection."
  (interactive (list current-prefix-arg nil))
  (let* ((file (chief/mojo-resolve-runnable-file always-prompt))
         (default-output (chief/mojo-artifact-path nil nil file))
         (output (if prompt-output
                     (read-file-name "Mojo output executable: "
                                     (file-name-directory default-output)
                                     default-output nil
                                     (file-name-nondirectory default-output))
                   default-output))
         (command
          (append (chief/mojo-command "build")
                  chief/mojo-compiler-arguments
                  (chief/mojo-include-arguments)
                  (list file "-o" output))))
    (chief/mojo-save-file-buffer file)
    (chief/mojo-compile
     command
     (format "*mojo build %s*" (file-name-nondirectory file))
     (chief/mojo-require-pixi-root))))

(defun chief/mojo-build-project-file (&optional prompt-output)
  "Select and build a project Mojo executable.
With PROMPT-OUTPUT, prompt for its output path."
  (interactive "P")
  (chief/mojo-build-current-file prompt-output t))

(defun chief/mojo--emit-extension (emit)
  "Return a conventional filename extension for Mojo EMIT output."
  (pcase emit
    ("exe" (if (eq system-type 'windows-nt) ".exe" ""))
    ("shared-lib" (pcase system-type
                    ('windows-nt ".dll")
                    ('darwin ".dylib")
                    (_ ".so")))
    ("object" (if (eq system-type 'windows-nt) ".obj" ".o"))
    ("llvm" ".ll")
    ("llvm-bitcode" ".bc")
    ("asm" ".s")
    (_ "")))

(defun chief/mojo-build-artifact (emit &optional prompt-output)
  "Build the current Mojo file as an artifact of type EMIT.
EMIT is one of the output types documented by `mojo build --emit'.  With
PROMPT-OUTPUT, prompt for the destination path.  Non-executable outputs can be
built from module-only files."
  (interactive
   (list (completing-read "Mojo output type: "
                          '("exe" "shared-lib" "object" "llvm"
                            "llvm-bitcode" "asm")
                          nil t)
         current-prefix-arg))
  (let* ((file (if (string= emit "exe")
                   (chief/mojo-resolve-runnable-file)
                 (chief/mojo-current-file)))
         (default-output
          (chief/mojo-artifact-path
           (concat "-" (replace-regexp-in-string "[^[:alnum:]]" "-" emit))
           (chief/mojo--emit-extension emit) file))
         (output (if prompt-output
                     (read-file-name "Mojo artifact: "
                                     (file-name-directory default-output)
                                     default-output nil
                                     (file-name-nondirectory default-output))
                   default-output))
         (command
          (append (chief/mojo-command "build")
                  chief/mojo-compiler-arguments
                  (chief/mojo-include-arguments)
                  (list "--emit" emit file "-o" output))))
    (chief/mojo-save-file-buffer file)
    (chief/mojo-compile
     command
     (format "*mojo build %s %s*" emit (file-name-nondirectory file))
     (chief/mojo-require-pixi-root)
     (lambda () (message "Wrote Mojo %s artifact to %s" emit output)))))

(defun chief/mojo-doc-current-file (&optional prompt-output)
  "Generate JSON documentation for the current Mojo file.
With PROMPT-OUTPUT, prompt for the output path."
  (interactive "P")
  (chief/mojo-save-current-buffer)
  (let* ((file (chief/mojo-current-file))
         (default-output (chief/mojo-artifact-path "-docs" ".json"))
         (output (if prompt-output
                     (read-file-name "Mojo documentation JSON: "
                                     (file-name-directory default-output)
                                     default-output nil
                                     (file-name-nondirectory default-output))
                   default-output))
         (command
          (append (chief/mojo-command "doc")
                  (chief/mojo-include-arguments)
                  (list file "-o" output))))
    (chief/mojo-compile
     command "*mojo doc*" (chief/mojo-require-pixi-root)
     (lambda () (message "Wrote Mojo documentation to %s" output)))))

(defun chief/mojo-doc-project (&optional prompt-output)
  "Generate one JSON documentation file for the entire Mojo workspace.
With PROMPT-OUTPUT, prompt for the output path."
  (interactive "P")
  (let* ((root (chief/mojo-require-pixi-root))
         (sources (chief/mojo-project-source-files root))
         (default-output
          (expand-file-name "workspace-docs.json" (chief/mojo-cache-directory)))
         (output (if prompt-output
                     (read-file-name "Mojo workspace documentation JSON: "
                                     (file-name-directory default-output)
                                     default-output nil
                                     (file-name-nondirectory default-output))
                   default-output)))
    (unless sources
      (user-error "No Mojo source files found below %s" root))
    (dolist (file sources)
      (chief/mojo-save-file-buffer file))
    (chief/mojo-compile
     (append (chief/mojo-command "doc")
             (chief/mojo-include-arguments)
             (list root "-o" output))
     "*mojo doc workspace*" root
     (lambda () (message "Wrote Mojo workspace documentation to %s" output)))))

(defun chief/mojo-precompile-package (&optional always-prompt prompt-output)
  "Precompile a Mojo source package to a `.mojoc' file.
The package containing point is preferred.  ALWAYS-PROMPT selects a package
from the workspace.  PROMPT-OUTPUT prompts for the destination path."
  (interactive (list current-prefix-arg current-prefix-arg))
  (let* ((root (chief/mojo-require-pixi-root))
         (package
          (or (and (not always-prompt) (chief/mojo-current-package-directory))
              (chief/mojo-read-project-package)))
         (name (file-name-nondirectory (directory-file-name package)))
         (default-output
          (expand-file-name (concat name ".mojoc") (chief/mojo-cache-directory)))
         (output (if prompt-output
                     (read-file-name "Precompiled Mojo package: "
                                     (file-name-directory default-output)
                                     default-output nil
                                     (file-name-nondirectory default-output))
                   default-output)))
    (dolist (file (chief/mojo-project-source-files package))
      (chief/mojo-save-file-buffer file))
    (chief/mojo-compile
     (append (chief/mojo-command "precompile")
             chief/mojo-compiler-arguments
             (chief/mojo-include-arguments)
             (list package "-o" output))
     (format "*mojo precompile %s*" name) root
     (lambda () (message "Wrote precompiled Mojo package to %s" output)))))

(defun chief/mojo--cleanup-after-compilation (buffer path)
  "Delete PATH after BUFFER's compilation process finishes."
  (with-current-buffer buffer
    (add-hook
     'compilation-finish-functions
     (lambda (_buffer _status)
       (condition-case nil
           (if (file-directory-p path)
               (delete-directory path t)
             (when (file-exists-p path)
               (delete-file path)))
         (file-error nil)))
     nil t))
  buffer)

(defun chief/mojo-check-file (&optional file)
  "Compile-check Mojo FILE as a module without requiring a `main' function."
  (interactive)
  (let* ((file (or file (chief/mojo-current-file)))
         (output (make-temp-file "chief-mojo-check-" nil ".json"))
         (command
          (append (chief/mojo-command "doc")
                  (chief/mojo-include-arguments)
                  (list file "-o" output))))
    (chief/mojo-save-file-buffer file)
    (chief/mojo--cleanup-after-compilation
     (chief/mojo-compile
      command
      (format "*mojo check %s*" (file-name-nondirectory file))
      (chief/mojo-require-pixi-root))
     output)))

(defun chief/mojo-check-project ()
  "Compile-check every Mojo module in the nearest Pixi workspace."
  (interactive)
  (let* ((root (chief/mojo-require-pixi-root))
         (files (chief/mojo-project-source-files root))
         (output-directory (make-temp-file "chief-mojo-check-project-" t)))
    (unless files
      (delete-directory output-directory)
      (user-error "No Mojo source files found below %s" root))
    (dolist (file files)
      (chief/mojo-save-file-buffer file))
    (chief/mojo--cleanup-after-compilation
     (chief/mojo--compile-sequence
      (cl-loop for file in files
               for index from 1
               collect
               (append (chief/mojo-command "doc")
                       (chief/mojo-include-arguments)
                       (list file "-o"
                             (expand-file-name
                              (format "%04d.json" index)
                              output-directory))))
      "*mojo check project*" root
      (lambda ()
        (message "Checked %d Mojo modules" (length files))))
     output-directory)))

(defun chief/mojo-command-output (command)
  "Run COMMAND synchronously and return its combined output."
  (let ((default-directory (chief/mojo-require-pixi-root)))
    (with-temp-buffer
      (let ((status (apply #'process-file
                           (car command) nil (list t t) nil (cdr command))))
        (unless (zerop status)
          (error "Command failed (%d): %s" status (string-trim (buffer-string))))
        (string-trim (buffer-string))))))

(defun chief/mojo-version ()
  "Display the Mojo version selected by the current Pixi workspace."
  (interactive)
  (message "%s" (chief/mojo-command-output (chief/mojo-command "--version"))))

(defun chief/mojo-doctor ()
  "Display an actionable report for the current Mojo/Pixi workspace."
  (interactive)
  (let* ((root (chief/mojo-require-pixi-root))
         (environment (or (chief/mojo-resolved-environment root) "default"))
         (sources (chief/mojo-project-source-files root))
         (runnables (chief/mojo-project-runnable-files root))
         (tests (chief/mojo-project-test-files root))
         (packages (chief/mojo-project-package-directories root))
         (tasks (chief/mojo-pixi-task-names))
         (includes (chief/mojo-include-directories root))
         (lsp-ready chief/mojo-lsp-ready)
         (buffer (get-buffer-create "*Mojo Doctor*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Mojo workspace: %s\n" root))
        (insert (format "Pixi:           %s\n" (chief/mojo-pixi-executable)))
        (insert (format "Environment:    %s\n" environment))
        (insert (format "SDK:            %s\n"
                        (condition-case error
                            (chief/mojo-command-output
                             (chief/mojo-command "--version"))
                          (error (error-message-string error)))))
        (insert (format "LSP installed:  %s\n"
                        (if (chief/mojo-lsp-available-p) "yes" "no")))
        (insert (format "LSP ready:      %s\n"
                        (if lsp-ready "yes" "no")))
        (insert (format "DAP installed:  %s\n"
                        (if (chief/mojo-dap-available-p) "yes" "no")))
        (insert (format "Notebook magic: %s\n"
                        (if (chief/mojo-notebook-magic-available-p) "yes" "no")))
        (insert (format "Native kernel:  %s\n"
                        (if (chief/mojo-jupyter-kernel-available-p) "yes" "no")))
        (insert (format "JupyterLab:     %s\n"
                        (if (chief/mojo-jupyter-available-p) "yes" "no")))
        (insert (format "Sources:        %d\n" (length sources)))
        (insert (format "Entrypoints:    %d\n" (length runnables)))
        (dolist (file runnables)
          (insert (format "  run:  %s\n" (file-relative-name file root))))
        (insert (format "Test files:     %d\n" (length tests)))
        (dolist (file tests)
          (insert (format "  test: %s\n" (file-relative-name file root))))
        (insert (format "Packages:       %d\n" (length packages)))
        (dolist (package packages)
          (insert (format "  pkg:  %s\n" (file-relative-name package root))))
        (insert (format "Pixi tasks:     %s\n"
                        (if tasks (string-join tasks ", ") "none")))
        (insert "Import paths:\n")
        (dolist (directory includes)
          (insert (format "  -I %s\n" directory)))
        (insert "\nCore commands:\n")
        (insert "  , c c   check current module\n")
        (insert "  , c C   check whole workspace\n")
        (insert "  , c a/p build an artifact/precompile a package\n")
        (insert "  , r r   run current/project entrypoint\n")
        (insert "  , r p   choose project entrypoint\n")
        (insert "  , t t/f/p   test point/file/project\n")
        (insert "  , s s   start REPL\n")
        (insert "  , d d/p DAP-debug current/project entrypoint\n")
        (insert "  , d c/C debugger CLI current/project entrypoint\n")
        (insert "  , d a/A attach DAP/debugger CLI to a process\n")
        (insert "  , d b/g/s debug a binary/use CUDA-GDB/start bare CLI\n")
        (insert "  , p j/J/n start/install/create JupyterLab notebooks\n")
        (insert "  Org Babel: #+begin_src mojo (complete programs or auto-wrapped)\n")
        (insert "  gd / gr / K / gK   definition/references/hover/signature\n")
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buffer)))

(defun chief/mojo-buffer-test-items ()
  "Return plist entries for TestSuite tests in the current buffer."
  (let (items)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward chief/mojo-test-regexp nil t)
        (let ((name (match-string-no-properties 1))
              (position (match-beginning 0))
              (end (match-end 0)))
          (unless (nth 8 (syntax-ppss position))
            (push (list :name name :position position) items))
          ;; `syntax-ppss' moves point to its explicit position argument.
          (goto-char end))))
    (nreverse items)))

(defun chief/mojo-test-item-at-point ()
  "Return the Mojo test declaration containing point, or nil."
  (save-excursion
    (chief/mojo-beginning-of-defun)
    (when (looking-at chief/mojo-test-regexp)
      (list :name (match-string-no-properties 1)
            :position (match-beginning 0)))))

(defun chief/mojo-read-test-name ()
  "Return the test at point, or prompt for one in the current file."
  (let* ((items (chief/mojo-buffer-test-items))
         (at-point (chief/mojo-test-item-at-point)))
    (unless items
      (user-error "No `def test_...' declarations found in this file"))
    (or (plist-get at-point :name)
        (completing-read "Mojo test: "
                         (mapcar (lambda (item) (plist-get item :name)) items)
                         nil t))))

(defun chief/mojo-test-command (&optional name file)
  "Return the Mojo TestSuite command for FILE.
FILE defaults to the current file.  When NAME is non-nil, run only that test."
  (append (chief/mojo-command "run")
          chief/mojo-compiler-arguments
          (chief/mojo-include-arguments)
          (list (or file (chief/mojo-current-file)))
          (when name (list "--only" name))))

(defun chief/mojo-require-executable-test-file (file)
  "Require FILE to contain TestSuite tests and an executable `main'."
  (unless (chief/mojo-file-defines-tests-p file)
    (user-error "No `def test_...' declarations found in %s"
                (file-name-nondirectory file)))
  (unless (chief/mojo-file-defines-main-p file)
    (user-error
     "%s has tests but no TestSuite `main'; add TestSuite.discover_tests[...]().run()"
     (file-name-nondirectory file))))

(defun chief/mojo-test-buffer ()
  "Run every TestSuite test in the current Mojo file."
  (interactive)
  (let ((file (chief/mojo-current-file)))
    (chief/mojo-require-executable-test-file file)
    (chief/mojo-save-current-buffer)
    (chief/mojo-compile (chief/mojo-test-command nil file)
                        "*mojo test file*"
                        (chief/mojo-require-pixi-root))))

(defun chief/mojo-test-at-point ()
  "Run the TestSuite test at point, prompting when necessary."
  (interactive)
  (let ((file (chief/mojo-current-file))
        (name (chief/mojo-read-test-name)))
    (chief/mojo-require-executable-test-file file)
    (chief/mojo-save-current-buffer)
    (chief/mojo-compile (chief/mojo-test-command name file)
                        (format "*mojo test %s*" name)
                        (chief/mojo-require-pixi-root))))

(defun chief/mojo--process-json (command directory)
  "Run COMMAND in DIRECTORY and parse its JSON stdout."
  (let ((default-directory directory)
        (stderr-file (make-temp-file "chief-mojo-json-")))
    (unwind-protect
        (with-temp-buffer
          (let ((status (apply #'process-file
                               (car command) nil (list t stderr-file) nil
                               (cdr command))))
            (when (zerop status)
              (goto-char (point-min))
              (json-parse-buffer :object-type 'alist :array-type 'list))))
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

(defun chief/mojo-pixi-task-names ()
  "Return task names available in the selected Pixi environment."
  (when-let* ((root (chief/mojo-pixi-root))
              (pixi (executable-find "pixi"))
              (data
               (chief/mojo--process-json
                (append (list pixi "task" "list" "--manifest-path" root "--json")
                        (chief/mojo--environment-arguments))
                root)))
    (delete-dups
     (cl-loop for environment in data
              append
              (cl-loop for feature in (alist-get 'features environment)
                       append
                       (cl-loop for task in (alist-get 'tasks feature)
                                for name = (alist-get 'name task)
                                when name collect name))))))

(defun chief/mojo-pixi-task-command (task &rest args)
  "Return the Pixi command for TASK with ARGS."
  (let ((root (chief/mojo-require-pixi-root)))
    (append
     (list (chief/mojo-pixi-executable) "run" "--manifest-path" root)
     (chief/mojo--environment-arguments)
     (list "--no-progress" "--color" "never" "--frozen" task)
     args)))

(defun chief/mojo-read-pixi-task ()
  "Read and return a task defined by the current Pixi workspace."
  (let ((tasks (chief/mojo-pixi-task-names)))
    (unless tasks
      (user-error "No Pixi tasks are defined in this workspace"))
    (completing-read "Pixi task: " tasks nil t)))

(defun chief/mojo-run-pixi-task (task &optional prompt-arguments)
  "Run Pixi TASK for the current Mojo project.
With PROMPT-ARGUMENTS, prompt for additional task arguments."
  (interactive (list (chief/mojo-read-pixi-task) current-prefix-arg))
  (let ((arguments
         (when prompt-arguments
           (chief/mojo-read-arguments "Pixi task arguments: " nil))))
    (chief/mojo-compile
     (apply #'chief/mojo-pixi-task-command task arguments)
     (format "*pixi %s*" task)
     (chief/mojo-require-pixi-root))))

(defun chief/mojo-test-project ()
  "Run all project tests through a Pixi task or discovered TestSuite files."
  (interactive)
  (if (member "test" (chief/mojo-pixi-task-names))
      (chief/mojo-run-pixi-task "test" current-prefix-arg)
    (let* ((root (chief/mojo-require-pixi-root))
           (files (chief/mojo-project-test-files root))
           (invalid
            (cl-remove-if #'chief/mojo-file-defines-main-p files)))
      (unless files
        (user-error
         "No Pixi `test' task or Mojo files defining `test_...' were found"))
      (when invalid
        (user-error
         "Test files without a TestSuite `main': %s"
         (mapconcat (lambda (file) (file-relative-name file root))
                    invalid ", ")))
      (dolist (file files)
        (chief/mojo-save-file-buffer file))
      (chief/mojo--compile-sequence
       (mapcar (lambda (file) (chief/mojo-test-command nil file)) files)
       "*mojo test project*" root
       (lambda ()
         (message "Passed %d Mojo test files" (length files)))))))

(defun chief/mojo-run-arbitrary-command (command)
  "Run an arbitrary Mojo COMMAND inside the current Pixi environment."
  (interactive (list (read-shell-command "Mojo command: " "mojo ")))
  (let ((args (split-string-and-unquote command)))
    (unless (and args (string= (car args) "mojo"))
      (setq args (cons "mojo" args)))
    (chief/mojo-compile
     (apply #'chief/mojo-pixi-command args)
     "*mojo command*"
     (chief/mojo-require-pixi-root))))

(defun chief/mojo-format-buffer ()
  "Format the current buffer with the Mojo formatter from its Pixi SDK."
  (interactive)
  (let ((command (chief/mojo-command "format" "--quiet" "-")))
    (let ((stdout (generate-new-buffer " *chief-mojo-format*"))
          (stderr-file (make-temp-file "chief-mojo-format-"))
          (point-marker (point-marker))
          (modified (buffer-modified-p))
          (coding-system-for-read 'utf-8-unix)
          (coding-system-for-write 'utf-8-unix))
      (unwind-protect
          (let ((status (apply #'call-process-region
                               (point-min) (point-max)
                               (car command)
                               nil (list stdout stderr-file) nil
                               (cdr command))))
            (if (zerop status)
                (let ((changed
                       (not (string=
                             (buffer-substring-no-properties
                              (point-min) (point-max))
                             (with-current-buffer stdout
                               (buffer-string))))))
                  (replace-region-contents (point-min) (point-max) stdout)
                  (goto-char (min (marker-position point-marker) (point-max)))
                  (set-buffer-modified-p (or modified changed)))
              (error "%s"
                     (with-temp-buffer
                       (insert-file-contents stderr-file)
                       (string-trim (buffer-string))))))
        (kill-buffer stdout)
        (when (file-exists-p stderr-file)
          (delete-file stderr-file))
        (set-marker point-marker nil)))))

(defun chief/mojo-format-project ()
  "Format every Mojo source in the nearest Pixi workspace."
  (interactive)
  (let* ((root (chief/mojo-require-pixi-root))
         (sources (chief/mojo-project-source-files root)))
    (unless sources
      (user-error "No Mojo source files found below %s" root))
    (dolist (file sources)
      (chief/mojo-save-file-buffer file))
    (chief/mojo-compile
     (append (chief/mojo-command "format") (list "--quiet" root))
     "*mojo format workspace*" root
     (lambda ()
       (dolist (file sources)
         (when-let* ((buffer (get-file-buffer file)))
           (with-current-buffer buffer
             (revert-buffer :ignore-auto :noconfirm))))
       (message "Formatted %d Mojo source files" (length sources))))))

(defun chief/mojo-notebook-magic-available-p ()
  "Return non-nil when the SDK contains its documented `%%mojo' magic."
  (let ((environment (chief/mojo-pixi-environment-root)))
    (and environment
         (or (file-expand-wildcards
              (expand-file-name
               "lib/python*/site-packages/mojo/notebook.py" environment))
             (file-exists-p
              (expand-file-name
               "Lib/site-packages/mojo/notebook.py" environment))))))

(defun chief/mojo-jupyter-kernel-available-p ()
  "Return non-nil when the selected SDK contains its native Jupyter kernel."
  (let ((environment (chief/mojo-pixi-environment-root)))
    (and environment
         (file-exists-p
          (expand-file-name "share/jupyter/kernels/mojo/mojokernel.py"
                            environment))
         (file-exists-p
          (expand-file-name
           (pcase system-type
             ('darwin "lib/libMojoJupyter.dylib")
             ('windows-nt "Library/bin/MojoJupyter.dll")
             (_ "lib/libMojoJupyter.so"))
           environment)))))

(defun chief/mojo-prepare-jupyter-kernel ()
  "Create a project-local Jupyter kernel spec for the selected Mojo SDK.
Some SDK packages ship a `$PYTHON' placeholder in their bundled kernel spec.
The generated spec uses the actual Pixi interpreter without modifying global
Jupyter configuration.  Return the directory to prepend to `JUPYTER_PATH'."
  (let* ((root (chief/mojo-require-pixi-root))
         (environment (chief/mojo-pixi-environment-root root))
         (python (expand-file-name
                  (if (eq system-type 'windows-nt)
                      "python.exe" "bin/python")
                  environment))
         (kernel-script
          (expand-file-name "share/jupyter/kernels/mojo/mojokernel.py"
                            environment))
         (modular-home (expand-file-name "share/max" environment))
         (hash (substring (secure-hash 'sha1 environment) 0 12))
         (jupyter-root
          (expand-file-name (format "jupyter-%s/share/jupyter/" hash)
                            (chief/mojo-cache-directory)))
         (kernel-directory
          (expand-file-name "kernels/mojo-project" jupyter-root))
         (spec-file (expand-file-name "kernel.json" kernel-directory)))
    (unless (and (file-executable-p python) (file-readable-p kernel-script))
      (user-error "The selected Mojo SDK has an incomplete Jupyter kernel"))
    (make-directory kernel-directory t)
    (with-temp-file spec-file
      (insert
       (json-serialize
        `((argv . [,python ,kernel-script "-f" "{connection_file}"
                   "--modular-home" ,modular-home])
          (display_name . ,(format "Mojo (%s)"
                                   (file-name-nondirectory
                                    (directory-file-name root))))
          (language . "mojo")
          (interrupt_mode . "signal"))
        :null-object nil :false-object :json-false)))
    jupyter-root))

(defun chief/mojo-jupyter-available-p ()
  "Return non-nil when JupyterLab is installed in the selected Pixi environment."
  (let ((environment (chief/mojo-pixi-environment-root)))
    (and environment
         (or (file-executable-p (expand-file-name "bin/jupyter-lab" environment))
             (file-executable-p
              (expand-file-name "Scripts/jupyter-lab.exe" environment)))
         (let ((metadata (expand-file-name "conda-meta" environment)))
           (and (file-directory-p metadata)
                (directory-files metadata nil "\\`ipykernel-.*\\.json\\'" t))))))

(defun chief/mojo-install-jupyter ()
  "Install JupyterLab and IPython kernel support in the Pixi environment."
  (interactive)
  (let* ((root (chief/mojo-require-pixi-root))
         (environment (chief/mojo-resolved-environment root))
         (command
          (append
           (list (chief/mojo-pixi-executable) "add" "--manifest-path" root
                 "--no-progress" "--color" "never")
           (when environment (list "--environment" environment))
           chief/mojo-jupyter-package-specs)))
    (chief/mojo-compile
     command "*pixi install Mojo JupyterLab*" root
     (lambda () (message "Installed JupyterLab for Mojo in %s" root)))))

(defun chief/mojo-start-jupyter (&optional notebook)
  "Start project-local JupyterLab, optionally opening NOTEBOOK.
The Python kernel supports `import mojo.notebook' and `%%mojo'.  SDKs that
also ship the native REPL-based kernel expose it through a corrected local
kernel specification."
  (interactive)
  (unless (or (chief/mojo-notebook-magic-available-p)
              (chief/mojo-jupyter-kernel-available-p))
    (user-error "The selected Mojo SDK has no notebook integration"))
  (unless (chief/mojo-jupyter-available-p)
    (user-error
     "JupyterLab is not installed; run M-x chief/mojo-install-jupyter"))
  (let* ((root (chief/mojo-require-pixi-root))
         (name (format "mojo-jupyter[%s]"
                       (file-name-nondirectory (directory-file-name root))))
         (buffer (get-buffer-create (format "*%s*" name)))
         (command
          (append (chief/mojo-pixi-command "jupyter-lab")
                  (when notebook (list (expand-file-name notebook)))
                  (list "--notebook-dir" root))))
    (unless (comint-check-proc buffer)
      (let ((default-directory root)
            (process-environment (copy-sequence process-environment))
            (kernel-root
             (when (chief/mojo-jupyter-kernel-available-p)
               (chief/mojo-prepare-jupyter-kernel))))
        (when kernel-root
          (setenv "JUPYTER_PATH"
                  (if-let* ((existing (getenv "JUPYTER_PATH")))
                      (concat kernel-root path-separator existing)
                    kernel-root)))
        (apply #'make-comint-in-buffer name buffer (car command) nil
               (cdr command))))
    (pop-to-buffer buffer)))

(defun chief/mojo-create-notebook (file)
  "Create an official Mojo cell-magic notebook at FILE and open JupyterLab."
  (interactive
   (list (read-file-name "New Mojo notebook: "
                         (chief/mojo-require-pixi-root) nil nil
                         "mojo-notebook.ipynb")))
  (unless (chief/mojo-jupyter-available-p)
    (user-error
     "JupyterLab is not installed; run M-x chief/mojo-install-jupyter"))
  (unless (string-suffix-p ".ipynb" file t)
    (setq file (concat file ".ipynb")))
  (when (and (file-exists-p file)
             (not (yes-or-no-p (format "Overwrite %s? " file))))
    (user-error "Cancelled"))
  (with-temp-file file
    (insert
     (json-serialize
      `((cells
         . [((cell_type . "code")
             (execution_count . nil)
             (metadata . ,(make-hash-table))
             (outputs . [])
             (source . ["import mojo.notebook\n"]))
            ((cell_type . "code")
             (execution_count . nil)
             (metadata . ,(make-hash-table))
             (outputs . [])
             (source . ["%%mojo\n" "def main():\n"
                        "    print(\"Hello from Mojo!\")\n"]))])
        (metadata
         . ((kernelspec . ((display_name . "Python 3 (ipykernel)")
                           (language . "python")
                           (name . "python3")))
            (language_info . ((name . "python")))))
        (nbformat . 4)
        (nbformat_minor . 5))
      :null-object nil :false-object :json-false)))
  (chief/mojo-start-jupyter file))

(defun chief/mojo-repl-command ()
  "Return the Pixi-backed command used to start the Mojo REPL."
  (chief/mojo-command "repl"))

(defun chief/mojo-repl-buffer-name (&optional root)
  "Return the Mojo REPL buffer name for ROOT."
  (format "*mojo[%s]*"
          (file-name-nondirectory
           (directory-file-name (or root (chief/mojo-require-pixi-root))))))

(define-derived-mode chief/mojo-repl-mode comint-mode "Mojo-Native-REPL"
  "Comint mode for native prompt-driven Mojo REPL consumers such as Babel."
  :syntax-table chief/mojo-mode-syntax-table
  (setq-local font-lock-defaults '(chief/mojo-font-lock-keywords))
  (setq-local comint-prompt-regexp "^[0-9]+[>.] ")
  (setq-local paragraph-start comint-prompt-regexp)
  (setq-local comint-use-prompt-regexp t)
  (setq-local comint-process-echoes t))

(defun chief/mojo--repl-incomplete-input-p (input)
  "Return non-nil when REPL INPUT clearly needs another line."
  (or (string-suffix-p ":" (string-trim-right input))
      (string-suffix-p "\\" (string-trim-right input))
      (with-temp-buffer
        (set-syntax-table chief/mojo-mode-syntax-table)
        (insert input)
        (> (car (syntax-ppss (point-max))) 0))))

(defun chief/mojo--repl-assignment-input-p (input)
  "Return non-nil when INPUT contains a top-level assignment operator."
  (with-temp-buffer
    (set-syntax-table chief/mojo-mode-syntax-table)
    (insert input)
    (goto-char (point-min))
    (catch 'assignment
      (while (search-forward "=" nil t)
        (let* ((position (1- (point)))
               (state (save-excursion (syntax-ppss position)))
               (before (char-before position))
               (after (char-after (1+ position))))
          (when (and (zerop (car state))
                     (not (nth 3 state))
                     (not (nth 4 state))
                     (not (memq before '(?= ?! ?< ?>)))
                     (not (eq after ?=)))
            (throw 'assignment t))))
      nil)))

(defun chief/mojo--repl-statement-input-p (input)
  "Return non-nil when INPUT should execute without displaying a value."
  (let ((trimmed (string-trim-left input)))
    (or (string-empty-p trimmed)
        (string-match-p "\n" trimmed)
        (string-prefix-p ":" trimmed)
        (string-prefix-p "<" trimmed)
        (string-prefix-p "#" trimmed)
        (string-match-p
         (concat "\\`\\(?:"
                 (regexp-opt
                  '("alias" "assert" "break" "comptime" "continue" "def"
                    "del" "elif" "else" "fn" "for" "from" "if" "import"
                    "let" "match" "pass" "print" "raise" "return" "struct"
                    "trait" "try" "var" "while" "with" "yield"))
                 "\\)\\_>")
         trimmed)
        (chief/mojo--repl-assignment-input-p trimmed))))

(defun chief/mojo--repl-current-user-input ()
  "Return editable input after the managed REPL prompt."
  (let ((process (get-buffer-process (current-buffer))))
    (buffer-substring-no-properties
     (marker-position (process-mark process)) (point-max))))

(defun chief/mojo-repl-newline ()
  "Insert a newline into the current managed Mojo REPL input."
  (interactive)
  (goto-char (point-max))
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position) (line-end-position)))
         (indent (current-indentation)))
    (newline)
    (insert (make-string (+ indent (if (string-suffix-p ":" line) 4 0))
                         ?\s))))

(defun chief/mojo-repl-return ()
  "Evaluate current managed Mojo input, or continue an incomplete block."
  (interactive)
  (when chief/mojo--repl-busy
    (user-error "The Mojo REPL is still evaluating the previous input"))
  (goto-char (point-max))
  (let ((input (chief/mojo--repl-current-user-input)))
    (if (and (not (string-empty-p input))
             (chief/mojo--repl-incomplete-input-p input))
        (chief/mojo-repl-newline)
      (comint-send-input))))

(defun chief/mojo--repl-process-input (input)
  "Return native Mojo code used to evaluate visible frontend INPUT."
  (let ((trimmed (string-trim input)))
    (if (chief/mojo--repl-statement-input-p trimmed)
        trimmed
      (format "print(%s)" trimmed))))

(defun chief/mojo--repl-insert-prompt (process)
  "Insert the managed frontend prompt for PROCESS."
  (comint-output-filter process chief/mojo--repl-prompt))

(defun chief/mojo--repl-input-sender (process input)
  "Send visible INPUT to the native Mojo PROCESS with completion framing."
  (when chief/mojo--repl-busy
    (user-error "The Mojo REPL is still evaluating the previous input"))
  (let ((trimmed (string-trim-right input)))
    (if (string= (string-trim trimmed) ":quit")
        (process-send-string process ":quit\n")
      (cl-incf chief/mojo--repl-evaluation-counter)
      (setq chief/mojo--repl-busy t
            chief/mojo--repl-pending-output ""
            chief/mojo--repl-expected-marker
            (format "__CHIEF_MOJO_REPL_%s_%d__"
                    chief/mojo--repl-session-id
                    chief/mojo--repl-evaluation-counter))
      (cond
       ((string-empty-p (string-trim trimmed)))
       ((string-prefix-p ":" (string-trim-left trimmed))
        (process-send-string process (concat trimmed "\n")))
       (t
        (process-send-string
         process
         (concat (chief/mojo--repl-process-input trimmed) "\n\n"))))
      (process-send-string
       process
       (format "print(\"%s\")\n\n" chief/mojo--repl-expected-marker)))))

(defun chief/mojo--repl-process-filter (process output)
  "Convert native PROCESS OUTPUT into a clean managed REPL transcript."
  (when-let* ((buffer (process-buffer process)))
    (with-current-buffer buffer
      (if (not chief/mojo--repl-expected-marker)
          (comint-output-filter process output)
        (setq chief/mojo--repl-pending-output
              (concat chief/mojo--repl-pending-output output))
        (when (string-match-p
               (regexp-quote chief/mojo--repl-expected-marker)
               chief/mojo--repl-pending-output)
          (let* ((marker chief/mojo--repl-expected-marker)
                 (cleaned
                  (replace-regexp-in-string
                   (concat (regexp-quote marker) "\\(?:\\r?\\n\\)?")
                   "" chief/mojo--repl-pending-output t t)))
            (setq cleaned (string-trim-right
                           (string-replace "\r" "" cleaned) "[\n]+")
                  chief/mojo--repl-busy nil
                  chief/mojo--repl-expected-marker nil
                  chief/mojo--repl-pending-output "")
            (unless (string-empty-p cleaned)
              (setq cleaned (concat cleaned "\n")))
            (comint-output-filter
             process (concat cleaned chief/mojo--repl-prompt))))))))

(define-derived-mode chief/mojo-interactive-repl-mode
  comint-mode "Mojo-REPL"
  "Managed Mojo REPL frontend without native LLDB terminal prompts."
  :syntax-table chief/mojo-mode-syntax-table
  (setq-local font-lock-defaults '(chief/mojo-font-lock-keywords))
  (setq-local comint-prompt-regexp "^mojo\\[[^]\n]+\\]> ")
  (setq-local paragraph-start comint-prompt-regexp)
  (setq-local comint-use-prompt-regexp t)
  (setq-local comint-process-echoes nil)
  (setq-local comint-input-ignoredups t)
  (setq-local comint-input-sender #'chief/mojo--repl-input-sender)
  (setq-local chief/mojo--repl-configured-version
              chief/mojo--repl-integration-version)
  (local-set-key (kbd "RET") #'chief/mojo-repl-return)
  (local-set-key (kbd "<return>") #'chief/mojo-repl-return)
  (local-set-key (kbd "S-<return>") #'chief/mojo-repl-newline)
  (local-set-key (kbd "C-c C-j") #'chief/mojo-repl-newline))

(defun chief/mojo-ensure-repl-buffer ()
  "Return a live project-local managed Mojo REPL buffer."
  (let* ((root (chief/mojo-require-pixi-root))
         (name (chief/mojo-repl-buffer-name root))
         (buffer (get-buffer name)))
    (unless (and buffer
                 (process-live-p (get-buffer-process buffer))
                 (with-current-buffer buffer
                   (and (derived-mode-p 'chief/mojo-interactive-repl-mode)
                        (equal chief/mojo--repl-configured-version
                               chief/mojo--repl-integration-version))))
      (when (buffer-live-p buffer)
        (when-let* ((process (get-buffer-process buffer)))
          (delete-process process))
        (kill-buffer buffer))
      (setq buffer (get-buffer-create name))
      (let* ((default-directory root)
             (process-connection-type nil)
             (process-environment (copy-sequence process-environment))
             (command (chief/mojo-repl-command)))
        (setenv "TERM" "dumb")
        (setenv "NO_COLOR" "1")
        (apply #'make-comint-in-buffer
               "chief-mojo" buffer (car command) nil (cdr command)))
      (with-current-buffer buffer
        (chief/mojo-interactive-repl-mode)
        (setq-local default-directory root
                    chief/mojo--repl-session-id
                    (substring
                     (secure-hash 'sha1
                                  (format "%s%s%s" root (float-time) (random)))
                     0 16)
                    chief/mojo--repl-prompt
                    (format "mojo[%s]> "
                            (file-name-nondirectory
                             (directory-file-name root))))
        (let ((process (get-buffer-process buffer))
              (inhibit-read-only t))
          (set-process-filter process #'chief/mojo--repl-process-filter)
          (set-process-query-on-exit-flag process nil)
          (erase-buffer)
          (insert "Mojo REPL\n"
                  "RET evaluates; S-RET or C-c C-j inserts a newline.\n\n")
          (set-marker (process-mark process) (point))
          (chief/mojo--repl-insert-prompt process))))
    buffer))

(defun chief/mojo-start-repl ()
  "Start or switch to the current project's managed Mojo REPL."
  (interactive)
  (pop-to-buffer (chief/mojo-ensure-repl-buffer))
  (when (fboundp 'evil-insert-state)
    (evil-insert-state)))

(defun chief/mojo-restart-repl ()
  "Restart the current project's Mojo REPL."
  (interactive)
  (when-let* ((buffer (get-buffer (chief/mojo-repl-buffer-name))))
    (when-let* ((process (get-buffer-process buffer)))
      (delete-process process))
    (kill-buffer buffer))
  (chief/mojo-start-repl))

(defun chief/mojo-send-string (string)
  "Evaluate STRING through the managed Mojo REPL frontend."
  (let ((buffer (chief/mojo-ensure-repl-buffer)))
    (with-current-buffer buffer
      (when chief/mojo--repl-busy
        (user-error "The Mojo REPL is still evaluating the previous input"))
      (goto-char (point-max))
      (insert (string-trim-right string))
      (chief/mojo-repl-return))
    (display-buffer buffer)))

(defun chief/mojo-send-line ()
  "Send the current line to the Mojo REPL."
  (interactive)
  (chief/mojo-send-string
   (buffer-substring-no-properties
    (line-beginning-position) (line-end-position))))

(defun chief/mojo-send-region (start end)
  "Send the region from START to END to the Mojo REPL.
A region covering the entire buffer uses the REPL's file-loading command so
blank lines inside a module do not prematurely terminate its declarations."
  (interactive "r")
  (if (and (= start (point-min)) (= end (point-max)))
      (chief/mojo-send-buffer)
    (chief/mojo-send-string (buffer-substring-no-properties start end))))

(defun chief/mojo-send-buffer ()
  "Load the current file into the Mojo REPL.
Save a modified buffer first, then use the REPL's official `< PATH' command;
this preserves complete module structure and avoids replaying every source
line through the terminal editor."
  (interactive)
  (unless buffer-file-name
    (user-error "Save this Mojo buffer before loading it into the REPL"))
  (when (buffer-modified-p)
    (save-buffer))
  (when (string-match-p "[\n\r]" buffer-file-name)
    (user-error "The Mojo REPL cannot load a path containing a newline"))
  (chief/mojo-send-string (concat "< " (expand-file-name buffer-file-name))))

(defun chief/mojo-send-defun ()
  "Send the current Mojo declaration to the REPL."
  (interactive)
  (save-excursion
    (mark-defun)
    (chief/mojo-send-region (region-beginning) (region-end))
    (deactivate-mark)))

(defun chief/mojo-sdk-file (relative-path &optional root)
  "Return RELATIVE-PATH in the selected Mojo SDK under ROOT."
  (when-let* ((environment (chief/mojo-pixi-environment-root root)))
    (expand-file-name relative-path environment)))

(defun chief/mojo-dap-executable-name ()
  "Return the selected SDK's Mojo-capable LLDB DAP executable name."
  (let ((candidates
         (if (eq system-type 'windows-nt)
             '(("mojo-lldb-dap" . "Scripts/mojo-lldb-dap.exe")
               ("lldb-dap" . "Scripts/lldb-dap.exe"))
           '(("mojo-lldb-dap" . "bin/mojo-lldb-dap")
             ("lldb-dap" . "bin/lldb-dap")))))
    (cl-loop for (name . relative-path) in candidates
             for file = (chief/mojo-sdk-file relative-path)
             when (and file (file-executable-p file))
             return name)))

(defun chief/mojo-dap-available-p ()
  "Return non-nil when the selected Pixi SDK contains the Mojo DAP adapter."
  (and (chief/mojo-dap-executable-name) t))

(defun chief/mojo-dap-command ()
  "Return the Pixi-backed Mojo LLDB DAP adapter command."
  (let ((executable (or (chief/mojo-dap-executable-name)
                        (user-error
                         "The selected SDK has no Mojo-capable DAP adapter"))))
    (append (chief/mojo-pixi-command executable)
            '("--repl-mode" "variable"))))

(defun chief/mojo--lldb-plugin-path ()
  "Return the selected SDK's Mojo LLDB plugin path, when present."
  (let* ((environment (chief/mojo-pixi-environment-root))
         (library-directory (and environment (expand-file-name "lib" environment))))
    (when (file-directory-p library-directory)
      (car (directory-files library-directory t
                            "\\`\\(?:lib\\)?MojoLLDB\\.\\(?:dylib\\|so\\|dll\\)\\'")))))

(defun chief/mojo--lldb-init-commands ()
  "Return official Mojo LLDB initialization commands for DAP sessions."
  (let ((plugin (chief/mojo--lldb-plugin-path)))
    (vconcat
     (append
      (when plugin
        (list (format "?!plugin load %s" (shell-quote-argument plugin))))
      '("?settings set target.show-hex-variable-values-with-leading-zeroes false"
        "?settings set target.process.optimization-warnings false")))))

(defun chief/mojo-debug-entitlements-file ()
  "Return a macOS entitlements file suitable for Mojo debug binaries."
  (let ((file (expand-file-name "debug-entitlements.plist"
                                (chief/mojo-cache-directory))))
    (unless (file-exists-p file)
      (with-temp-file file
        (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" "
                "\"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n"
                "<plist version=\"1.0\"><dict>\n"
                "  <key>com.apple.security.get-task-allow</key><true/>\n"
                "</dict></plist>\n")))
    file))

(defun chief/mojo--dap-start (binary file root arguments)
  "Start a Mojo DAP session for BINARY built from FILE in ROOT with ARGUMENTS."
  (unless (chief/mojo-dap-available-p)
    (user-error "The selected Pixi SDK has no Mojo-capable DAP adapter"))
  (require 'dap-mode)
  (require 'dap-lldb)
  (dap-debug
   (list :type "lldb-vscode"
         :request "launch"
         :name (format "Mojo :: %s" (file-name-nondirectory file))
         :program binary
         :cwd root
         :args (vconcat arguments)
         :sourceLanguages ["mojo"]
         :dap-server-path (chief/mojo-dap-command)
         :initCommands (chief/mojo--lldb-init-commands)
         :customFrameFormat
         "${function.name-with-args}{${frame.is-artificial} [artificial]}"
         :enableSyntheticChildDebugging t
         :enableAutoVariableSummaries t
         :commandEscapePrefix ":"
         :timeout 300
         :stopOnEntry (if chief/mojo-dap-stop-on-entry t :json-false))))

(defun chief/mojo-dap-debug-current-file (&optional prompt-arguments always-prompt)
  "Build and debug a project Mojo executable through DAP.
The current file is preferred when it defines `main'.  With PROMPT-ARGUMENTS,
edit debuggee arguments.  ALWAYS-PROMPT forces entrypoint selection."
  (interactive (list current-prefix-arg nil))
  (let* ((root (chief/mojo-require-pixi-root))
         (file (chief/mojo-resolve-runnable-file always-prompt))
         (arguments
          (if prompt-arguments
              (chief/mojo-read-arguments
               "Mojo debuggee arguments: " chief/mojo-runtime-arguments)
            chief/mojo-runtime-arguments))
         (binary (chief/mojo-artifact-path "-debug" nil file))
         (build-command
          (append (chief/mojo-command "build")
                  '("--no-optimization" "--debug-level" "full")
                  chief/mojo-compiler-arguments
                  (chief/mojo-include-arguments)
                  (list file "-o" binary)))
         (commands
          (append
           (list build-command)
           (when (eq system-type 'darwin)
             (unless (executable-find "codesign")
               (user-error "Codesign is required for Mojo DAP debugging on macOS"))
             (list (list "codesign" "-s" "-" "-f" "--entitlements"
                         (chief/mojo-debug-entitlements-file) binary))))))
    (unless (chief/mojo-dap-available-p)
      (user-error "The selected Pixi SDK has no Mojo-capable DAP adapter"))
    (chief/mojo-save-file-buffer file)
    (chief/mojo--compile-sequence
     commands
     (format "*mojo debug build %s*" (file-name-nondirectory file))
     root
     (lambda () (chief/mojo--dap-start binary file root arguments)))))

(defun chief/mojo-dap-debug-project-file (&optional prompt-arguments)
  "Select, build, and debug a project Mojo executable through DAP.
With PROMPT-ARGUMENTS, edit the debuggee arguments."
  (interactive "P")
  (chief/mojo-dap-debug-current-file prompt-arguments t))

(defun chief/mojo--read-process-id ()
  "Prompt for and return a process ID for a Mojo DAP attach session."
  (let* ((lines (process-lines "ps" "-Ao" "pid=,comm="))
         (choice (completing-read "Attach Mojo debugger to process: "
                                  lines nil t)))
    (string-to-number (string-trim-left choice))))

(defun chief/mojo-dap-attach (pid)
  "Attach the Mojo LLDB DAP adapter to process PID."
  (interactive (list (chief/mojo--read-process-id)))
  (unless (chief/mojo-dap-available-p)
    (user-error "The selected Pixi SDK has no Mojo-capable DAP adapter"))
  (require 'dap-mode)
  (require 'dap-lldb)
  (dap-debug
   (list :type "lldb-vscode"
         :request "attach"
         :name (format "Mojo attach :: %d" pid)
         :pid pid
         :cwd (chief/mojo-require-pixi-root)
         :sourceLanguages ["mojo"]
         :dap-server-path (chief/mojo-dap-command)
         :initCommands (chief/mojo--lldb-init-commands)
         :enableSyntheticChildDebugging t
         :enableAutoVariableSummaries t
         :commandEscapePrefix ":"
         :timeout 300)))

(defun chief/mojo--start-debugger-cli (command label)
  "Start Mojo debugger CLI COMMAND in a project buffer named for LABEL."
  (let* ((root (chief/mojo-require-pixi-root))
         (buffer-name
          (format "*mojo-debug[%s:%s]*"
                  (file-name-nondirectory (directory-file-name root)) label))
         (buffer (get-buffer-create buffer-name)))
    (when (comint-check-proc buffer)
      (delete-process (get-buffer-process buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (let ((default-directory root))
      (apply #'make-comint-in-buffer
             "chief-mojo-debug" buffer (car command) nil (cdr command)))
    (with-current-buffer buffer
      (setq-local default-directory root)
      (setq-local comint-prompt-regexp "^(\\(?:lldb\\|cuda-gdb\\)) ")
      (setq-local comint-use-prompt-regexp t))
    (pop-to-buffer buffer)))

(defun chief/mojo-debug-current-file
    (&optional prompt-arguments always-prompt debugger-arguments)
  "Launch the SDK debugger CLI for a project Mojo executable.
The current file is preferred when it defines `main'.  With PROMPT-ARGUMENTS,
edit the debuggee arguments.  ALWAYS-PROMPT forces entrypoint selection.
DEBUGGER-ARGUMENTS are inserted immediately after `mojo debug'."
  (interactive (list current-prefix-arg nil nil))
  (let* ((file (chief/mojo-resolve-runnable-file always-prompt))
         (arguments
          (if prompt-arguments
              (chief/mojo-read-arguments
               "Mojo debuggee arguments: " chief/mojo-runtime-arguments)
            chief/mojo-runtime-arguments))
         (command
          (append (chief/mojo-command "debug")
                  debugger-arguments
                  chief/mojo-compiler-arguments
                  (chief/mojo-include-arguments)
                  (list file)
                  arguments)))
    (chief/mojo-save-file-buffer file)
    (chief/mojo--start-debugger-cli
     command (file-name-nondirectory file))))

(defun chief/mojo-debug-project-file (&optional prompt-arguments)
  "Select and debug a project Mojo executable in the debugger CLI.
With PROMPT-ARGUMENTS, edit debuggee arguments."
  (interactive "P")
  (chief/mojo-debug-current-file prompt-arguments t))

(defun chief/mojo-debug-gpu-current-file (&optional prompt-arguments)
  "Debug a project Mojo executable with the CLI CUDA-GDB backend.
With PROMPT-ARGUMENTS, edit debuggee arguments."
  (interactive "P")
  (chief/mojo-debug-current-file prompt-arguments nil '("--cuda-gdb")))

(defun chief/mojo-debug-binary (program &optional prompt-arguments)
  "Debug precompiled PROGRAM through the Mojo debugger CLI.
With PROMPT-ARGUMENTS, prompt for program arguments."
  (interactive
   (list (read-file-name "Precompiled Mojo program: " nil nil t)
         current-prefix-arg))
  (let ((arguments
         (if prompt-arguments
             (chief/mojo-read-arguments
              "Program arguments: " chief/mojo-runtime-arguments)
           chief/mojo-runtime-arguments)))
    (chief/mojo--start-debugger-cli
     (append (chief/mojo-command "debug") (list program) arguments)
     (file-name-nondirectory program))))

(defun chief/mojo-debug-cli-attach (pid)
  "Attach the Mojo debugger CLI to process PID."
  (interactive (list (chief/mojo--read-process-id)))
  (chief/mojo--start-debugger-cli
   (append (chief/mojo-command "debug")
           (list "--pid" (number-to-string pid)))
   (format "pid-%d" pid)))

(defun chief/mojo-debugger-cli ()
  "Start a bare Mojo debugger CLI without a target."
  (interactive)
  (chief/mojo--start-debugger-cli
   (chief/mojo-command "debug") "console"))

(defun chief/mojo--previous-code-line ()
  "Move to the previous nonblank, non-comment line and return point."
  (let (found)
    (while (and (not found) (zerop (forward-line -1)))
      (back-to-indentation)
      (unless (or (eolp) (looking-at-p "#"))
        (setq found (point))))
    found))

(defun chief/mojo--line-ends-block-p ()
  "Return non-nil when the current logical line ends in a block colon."
  (save-excursion
    (end-of-line)
    (skip-chars-backward " \t")
    (and (eq (char-before) ?:)
         (not (nth 8 (syntax-ppss (1- (point))))))))

(defun chief/mojo-calculate-indentation ()
  "Return the appropriate indentation column for the current Mojo line."
  (save-excursion
    (back-to-indentation)
    (let* ((line-start (point))
           (closing-p (looking-at-p "[])}]"))
           (ppss (syntax-ppss line-start))
           (opener (nth 1 ppss)))
      (cond
       ((and closing-p opener)
        (goto-char opener)
        (current-indentation))
       (opener
        (goto-char (1+ opener))
        (let ((open-line-end (line-end-position)))
          (skip-chars-forward " \t")
          (if (< (point) open-line-end)
              (current-column)
            (goto-char opener)
            (+ (current-indentation) chief/mojo-indent-offset))))
       ((chief/mojo--previous-code-line)
        (let ((previous-indent (current-indentation))
              (opens-block (chief/mojo--line-ends-block-p)))
          (goto-char line-start)
          (cond
           ((looking-at-p "\\(?:elif\\|else\\|except\\|finally\\|case\\)\\_>")
            (max 0 (- previous-indent chief/mojo-indent-offset)))
           (opens-block (+ previous-indent chief/mojo-indent-offset))
           (t previous-indent))))
       (t 0)))))

(defun chief/mojo-indent-line ()
  "Indent the current line as Mojo source."
  (interactive)
  (let* ((column (current-column))
         (indent (current-indentation))
         (offset (- column indent))
         (target (chief/mojo-calculate-indentation)))
    (indent-line-to target)
    (when (> offset 0)
      (move-to-column (+ target offset)))))

(defun chief/mojo-beginning-of-defun (&optional arg)
  "Move to the beginning of a Mojo declaration.
With ARG, repeat that many times; negative ARG moves forward."
  (let ((arg (or arg 1)))
    (if (< arg 0)
        (chief/mojo-end-of-defun (- arg))
      (dotimes (_ arg)
        (when (and (looking-at chief/mojo-definition-regexp)
                   (not (bobp)))
          (forward-char -1))
        (unless (re-search-backward chief/mojo-definition-regexp nil 'move)
          (goto-char (point-min)))
        (beginning-of-line)))
    t))

(defun chief/mojo-end-of-defun (&optional arg)
  "Move to the end of a Mojo declaration.
With ARG, repeat that many times; negative ARG moves backward."
  (let ((arg (or arg 1)))
    (if (< arg 0)
        (chief/mojo-beginning-of-defun (- arg))
      (dotimes (_ arg)
        (unless (looking-at chief/mojo-definition-regexp)
          (unless (re-search-backward chief/mojo-definition-regexp nil t)
            (re-search-forward chief/mojo-definition-regexp nil 'move)))
        (beginning-of-line)
        (let ((indent (current-indentation)))
          (forward-line 1)
          (while (and (not (eobp))
                      (or (looking-at-p "^[ \t]*\\(?:#.*\\)?$")
                          (> (current-indentation) indent)))
            (forward-line 1))
          (skip-chars-backward "\n\r"))))
    t))

(defun chief/mojo-completion-setup (&optional ready)
  "Configure Mojo completion, enabling its LSP CAPF only when READY.
Corfu remains active while analysis starts so its real auto-completion hooks
are installed; only the unsafe server-backed CAPF is deferred."
  (setq-local lsp-completion-enable t)
  (setq-local lsp-completion-enable-additional-text-edit t)
  (when (boundp 'corfu-auto)
    (setq-local corfu-auto t))
  (when (boundp 'corfu-auto-delay)
    (setq-local corfu-auto-delay 0.05))
  (when (boundp 'corfu-auto-prefix)
    (setq-local corfu-auto-prefix 1))
  (when (fboundp 'corfu-mode)
    (corfu-mode 1))
  (when (fboundp 'lsp-completion-mode)
    (lsp-completion-mode (if ready 1 -1))))

(defun chief/mojo-defer-semantic-features ()
  "Keep unsafe Mojo semantic requests disabled during initial analysis."
  (unless chief/mojo-lsp-ready
    (setq-local lsp-semantic-tokens-enable nil)
    (setq-local lsp-eldoc-enable-hover nil)
    (setq-local lsp-enable-symbol-highlighting nil)
    (setq-local lsp-signature-auto-activate nil)
    (chief/mojo-completion-setup nil)))

(defun chief/mojo-schedule-analysis-ready ()
  "Schedule a safe fallback readiness transition after LSP opens the file."
  (chief/mojo-defer-semantic-features)
  (when (timerp chief/mojo-analysis-ready-timer)
    (cancel-timer chief/mojo-analysis-ready-timer))
  (let ((buffer (current-buffer)))
    (setq-local
     chief/mojo-analysis-ready-timer
     (run-at-time
      chief/mojo-analysis-ready-delay nil
      (lambda ()
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (and (derived-mode-p 'chief/mojo-mode)
                       (bound-and-true-p lsp-mode)
                       (lsp-workspaces))
              (chief/mojo-mark-analysis-ready)))))))))

(defun chief/mojo-mark-analysis-ready ()
  "Enable completion and explicit Mojo semantic requests after startup."
  (when (timerp chief/mojo-analysis-ready-timer)
    (cancel-timer chief/mojo-analysis-ready-timer))
  (setq-local chief/mojo-analysis-ready-timer nil)
  (unless chief/mojo-lsp-ready
    (setq-local chief/mojo-lsp-ready t)
    (setq-local lsp-eldoc-enable-hover t)
    (setq-local lsp-enable-symbol-highlighting t)
    (setq-local lsp-signature-auto-activate
                '(:on-trigger-char :on-server-request))
    (chief/mojo-completion-setup t)
    (let ((callbacks (nreverse chief/mojo-pending-semantic-requests)))
      (setq-local chief/mojo-pending-semantic-requests nil)
      (dolist (callback callbacks)
        (funcall callback)))))

(defun chief/mojo-mark-diagnostics-ready ()
  "Enable request-on-activation features after Mojo publishes diagnostics."
  (chief/mojo-mark-analysis-ready)
  (setq-local chief/lsp-inlay-hints-deferred nil)
  (when (and (fboundp 'lsp--capability)
             (lsp--capability :semanticTokensProvider))
    (setq-local lsp-semantic-tokens-enable t)
    (when (fboundp 'lsp-semantic-tokens-mode)
      (lsp-semantic-tokens-mode 1)))
  (when (fboundp 'chief/lsp-sanitize-inlay-hints)
    (chief/lsp-sanitize-inlay-hints)))

(defun chief/mojo-reset-analysis-state ()
  "Reset readiness state when the Mojo language server disconnects."
  (when (timerp chief/mojo-analysis-ready-timer)
    (cancel-timer chief/mojo-analysis-ready-timer))
  (setq-local chief/mojo-analysis-ready-timer nil)
  (setq-local chief/mojo-pending-semantic-requests nil)
  (setq-local chief/mojo-lsp-ready nil)
  (setq-local chief/lsp-inlay-hints-deferred t)
  (setq-local lsp-semantic-tokens-enable nil)
  (setq-local lsp-eldoc-enable-hover nil)
  (setq-local lsp-enable-symbol-highlighting nil)
  (setq-local lsp-signature-auto-activate nil)
  (chief/mojo-completion-setup nil))

(defun chief/mojo-when-analysis-ready (callback)
  "Run CALLBACK once the Mojo server is safe for semantic requests."
  (if chief/mojo-lsp-ready
      (funcall callback)
    (push callback chief/mojo-pending-semantic-requests)
    (message "Mojo is analyzing this file; the request has been queued")))

(defun chief/mojo-show-hover-now ()
  "Request and display Mojo hover documentation immediately."
  (let ((buffer (current-buffer)))
    (lsp-request-async
     "textDocument/hover"
     (lsp--text-document-position-params)
     (lambda (hover)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (if-let* ((contents (and hover (lsp:hover-contents hover))))
               (lsp--display-contents contents)
             (message "No Mojo documentation at point")))))
     :error-handler
     (lambda (error)
       (message "Mojo hover failed: %s" error))
     :mode 'detached)))

(defun chief/mojo-show-hover ()
  "Show Mojo hover documentation after initial analysis completes."
  (interactive)
  (chief/mojo-when-analysis-ready #'chief/mojo-show-hover-now))

(defun chief/mojo--stdlib-builtin-source (symbol)
  "Return a fallback standard-library source file for built-in SYMBOL."
  (let* ((relative
          (cond
           ((member symbol '("Int" "UInt" "Int8" "Int16" "Int32" "Int64"
                             "UInt8" "UInt16" "UInt32" "UInt64"))
            "std/builtin/int.mojo")
           ((member symbol '("Float16" "Float32" "Float64" "Scalar" "SIMD"))
            "std/builtin/simd.mojo")
           ((string= symbol "AnyType") "std/builtin/anytype.mojo")
           ((string= symbol "DType") "std/builtin/dtype.mojo")
           ((string= symbol "Error") "std/builtin/error.mojo")
           ((string= symbol "String") "std/collections/string/string.mojo")
           ((string= symbol "List") "std/collections/list.mojo")
           ((string= symbol "Bool") "std/builtin/bool.mojo")))
         (file (and relative
                    (expand-file-name relative
                                      chief/mojo-stdlib-source-directory))))
    (and file (file-readable-p file) file)))

(defun chief/mojo--stdlib-definition-items (symbol)
  "Return Xref items for SYMBOL from official local standard-library sources."
  (when (and symbol
             (file-directory-p chief/mojo-stdlib-source-directory)
             (executable-find "rg"))
    (let* ((regexp
            (format
             "^[[:space:]]*(struct|trait|class|def|fn|alias)[[:space:]]+%s([^[:alnum:]_]|$)"
             (regexp-quote symbol)))
           (buffer (generate-new-buffer " *mojo stdlib definitions*"))
           items)
      (unwind-protect
          (when (zerop
                 (process-file "rg" nil buffer nil "--line-number" "--no-heading"
                               "--glob" "*.mojo" regexp
                               chief/mojo-stdlib-source-directory))
            (with-current-buffer buffer
              (goto-char (point-min))
              (while (re-search-forward
                      "^\\(.+\\):\\([0-9]+\\):\\(.*\\)$" nil t)
                (let ((file (match-string-no-properties 1))
                      (line (string-to-number (match-string-no-properties 2))))
                  (push (xref-make
                         (format "%s — %s" symbol
                                 (file-relative-name
                                  file chief/mojo-stdlib-source-directory))
                         (xref-make-file-location file line 0))
                        items)))))
        (kill-buffer buffer))
      (or (nreverse items)
          (when-let* ((file (chief/mojo--stdlib-builtin-source symbol)))
            (list (xref-make
                   (format "%s — %s" symbol
                           (file-relative-name
                            file chief/mojo-stdlib-source-directory))
                   (xref-make-file-location file 1 0))))))))

(defun chief/mojo-goto-definition-now ()
  "Jump to the current Mojo symbol's definition immediately.
When the server omits SDK locations, use the matching official local source."
  (let ((buffer (current-buffer))
        (source-window (selected-window))
        (marker (point-marker))
        (symbol (thing-at-point 'symbol t)))
    (lsp-request-async
     "textDocument/definition"
     (lsp--text-document-position-params)
     (lambda (locations)
       (unwind-protect
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (let ((items
                      (or (and locations
                               (lsp--locations-to-xref-items locations))
                          (chief/mojo--stdlib-definition-items symbol))))
                 (cond
                  ((null items)
                   (message "No Mojo definition found for %s"
                            (or symbol "symbol at point")))
                  ((not (and (window-live-p source-window)
                             (eq (window-buffer source-window) buffer)))
                   (message
                    "Mojo definition is available; run g d again in its source window"))
                  (t
                   (save-excursion
                     (goto-char marker)
                     (xref-push-marker-stack))
                   (with-selected-window source-window
                     (xref-pop-to-location (car items))))))))
         (set-marker marker nil)))
     :error-handler
     (lambda (error)
       (set-marker marker nil)
       (message "Mojo definition failed: %s" error))
     :mode 'detached)))

(defun chief/mojo-goto-definition ()
  "Jump to a Mojo definition after initial document analysis completes."
  (interactive)
  (chief/mojo-when-analysis-ready #'chief/mojo-goto-definition-now))

(defun chief/mojo-find-references ()
  "Show references after initial Mojo document analysis completes."
  (interactive)
  (chief/mojo-when-analysis-ready
   (lambda ()
     (chief/lsp-show-location-list-async
      "textDocument/references"
      (list :context '(:includeDeclaration t))
      nil t))))

(defun chief/mojo-show-signature ()
  "Show signature help after initial Mojo document analysis completes."
  (interactive)
  (chief/mojo-when-analysis-ready
   (lambda ()
     (when (fboundp 'lsp-signature-activate)
       (call-interactively #'lsp-signature-activate)))))

(defun chief/mojo-rename ()
  "Rename the Mojo symbol at point after initial analysis completes."
  (interactive)
  (chief/mojo-when-analysis-ready
   (lambda () (call-interactively #'lsp-rename))))

(defun chief/mojo-code-action ()
  "Execute a Mojo code action after initial analysis completes."
  (interactive)
  (chief/mojo-when-analysis-ready
   (lambda () (call-interactively #'lsp-execute-code-action))))

(defun chief/mojo-workspace-symbols ()
  "Search Mojo workspace symbols after initial analysis completes."
  (interactive)
  (chief/mojo-when-analysis-ready
   (lambda ()
     (unless (fboundp 'consult-lsp-symbols)
       (user-error "Consult-lsp is unavailable"))
     (call-interactively #'consult-lsp-symbols))))

(defun chief/mojo-toggle-inlay-hints ()
  "Toggle Mojo LSP inlay hints in the current buffer."
  (interactive)
  (unless chief/mojo-lsp-ready
    (user-error "Mojo is still analyzing this file"))
  (if (bound-and-true-p lsp-inlay-hints-mode)
      (lsp-inlay-hints-mode -1)
    (setq-local lsp-inlay-hint-enable t)
    (lsp-inlay-hints-mode 1)))

(defun chief/mojo-mode-setup ()
  "Configure a Mojo buffer for Pixi-backed development."
  (setq-local chief/lsp-root-function #'chief/mojo-project-root)
  (setq-local lsp-enabled-clients '(chief-mojo))
  (setq-local lsp-disabled-clients '(mojo))
  (setq-local lsp-semantic-tokens-enable nil)
  (setq-local lsp-eldoc-enable-hover nil)
  (setq-local lsp-enable-symbol-highlighting nil)
  (setq-local lsp-signature-auto-activate nil)
  (setq-local chief/mojo-lsp-ready nil)
  (setq-local chief/lsp-inlay-hints-deferred t)
  (setq-local chief/lsp-definition-function #'chief/mojo-goto-definition)
  (setq-local chief/lsp-references-function #'chief/mojo-find-references)
  (setq-local chief/lsp-hover-function #'chief/mojo-show-hover)
  (setq-local chief/lsp-signature-function #'chief/mojo-show-signature)
  ;; Run after shared LSP UI hooks so they cannot re-enable the Mojo CAPF
  ;; during the server's crash-prone initial analysis window.
  (add-hook 'lsp-mode-hook #'chief/mojo-defer-semantic-features t t)
  (add-hook 'lsp-configure-hook #'chief/mojo-defer-semantic-features t t)
  (add-hook 'lsp-after-open-hook #'chief/mojo-schedule-analysis-ready t t)
  (add-hook 'lsp-diagnostics-updated-hook
            #'chief/mojo-mark-diagnostics-ready t t)
  (add-hook 'lsp-unconfigure-hook #'chief/mojo-reset-analysis-state t t)
  (setq-local compile-command
              (when (and buffer-file-name (chief/mojo-pixi-root))
                (chief/mojo--shell-command
                 (if (chief/mojo-file-defines-main-p buffer-file-name)
                     (append (chief/mojo-command "run")
                             (chief/mojo-include-arguments)
                             (list (chief/mojo-current-file)))
                   (append (chief/mojo-command "doc")
                           (chief/mojo-include-arguments)
                           (list (chief/mojo-current-file)
                                 "-o"
                                 (chief/mojo-artifact-path
                                  "-check" ".json")))))))
  (chief/mojo-completion-setup nil)
  (chief/repl-configure
   :start #'chief/mojo-start-repl
   :restart #'chief/mojo-restart-repl
   :send-line #'chief/mojo-send-line
   :send-region #'chief/mojo-send-region
   :send-buffer #'chief/mojo-send-buffer
   :send-defun #'chief/mojo-send-defun
   :load-file #'chief/mojo-send-buffer)
  (when (and chief/mojo-format-on-save
             (chief/mojo-lsp-available-p))
    (chief/format-enable-on-save #'chief/mojo-format-buffer))
  (when (fboundp 'chief/lsp-managed-mode-setup)
    (chief/lsp-managed-mode-setup)))

;;;###autoload
(define-derived-mode chief/mojo-mode prog-mode "Mojo"
  "Major mode for Mojo 1.x source backed by the project Pixi SDK.
This mode intentionally avoids third-party Mojo modes and currently incomplete
Mojo Tree-sitter grammars."
  :syntax-table chief/mojo-mode-syntax-table
  (setq-local tab-width chief/mojo-indent-offset)
  (setq-local indent-tabs-mode nil)
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local parse-sexp-lookup-properties t)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local syntax-propertize-function python-syntax-propertize-function)
  (setq-local indent-line-function #'chief/mojo-indent-line)
  (setq-local beginning-of-defun-function #'chief/mojo-beginning-of-defun)
  (setq-local end-of-defun-function #'chief/mojo-end-of-defun)
  (setq-local imenu-generic-expression
              `(("Types" "^[ \t]*\\(?:class\\|struct\\|trait\\)[ \t]+\\([[:alpha:]_][[:alnum:]_]*\\)" 1)
                ("Functions" ,chief/mojo-definition-regexp 1)))
  (setq-local font-lock-defaults '(chief/mojo-font-lock-keywords)))

(add-to-list 'auto-mode-alist '("\\.mojo\\'" . chief/mojo-mode))
(add-to-list 'auto-mode-alist '("\\.🔥\\'" . chief/mojo-mode))
(add-hook 'chief/mojo-mode-hook #'chief/mojo-mode-setup)

(dolist (marker '("pixi.toml" "pixi.lock" "mojoproject.toml"))
  (add-to-list 'chief/project-root-markers marker t)
  (when (boundp 'project-vc-extra-root-markers)
    (add-to-list 'project-vc-extra-root-markers marker t)))

(with-eval-after-load 'core-lsp
  (add-to-list 'chief/lsp-managed-major-modes 'chief/mojo-mode)
  (add-to-list
   'chief/lsp-manual-install-specs
   '(:modes (chief/mojo-mode)
     :label "Mojo Pixi SDK"
     :install-function chief/mojo-install-project-tooling
     :install-predicate chief/mojo-project-tooling-installable-p
     :resume-after-install t
     :doc-url "https://mojolang.org/docs/manual/install/"
     :help "Installs the configured full Mojo SDK from Modular's stable channel into this Pixi workspace.")))

(with-eval-after-load 'dap-mode
  ;; LLDB emits one `module' event for every loaded image.  dap-mode has no
  ;; built-in handler and otherwise floods `*Messages*' during Mojo sessions.
  (cl-defmethod dap-handle-event ((event-type string) session params)
    (unless (string= event-type "module")
      (cl-call-next-method event-type session params))))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(chief/mojo-mode . "mojo"))
  (add-to-list 'lsp-language-id-configuration '("\\.mojo\\'" . "mojo"))
  (add-to-list 'lsp-language-id-configuration '("\\.🔥\\'" . "mojo"))
  (lsp-register-client
   (make-lsp-client
    :new-connection
    (lsp-stdio-connection #'chief/mojo-lsp-command
                          #'chief/mojo-lsp-available-p)
    :activation-fn (lsp-activate-on "mojo")
    :major-modes '(chief/mojo-mode)
    :library-folders-fn (lambda (_workspace)
                          (chief/mojo-include-directories))
    :priority 2
    :server-id 'chief-mojo)))

(chief/repl-setup-standard-local-leader 'chief/mojo-mode-map)
(chief/local-leader-def
  :keymaps 'chief/mojo-mode-map
  "c" '(:ignore t :which-key "mojo")
  "cc" #'chief/mojo-check-file
  "cC" #'chief/mojo-check-project
  "ca" #'chief/mojo-build-artifact
  "cb" #'chief/mojo-build-current-file
  "cB" #'chief/mojo-build-project-file
  "cd" #'chief/mojo-doc-current-file
  "cD" #'chief/mojo-doc-project
  "cf" #'chief/mojo-format-buffer
  "cF" #'chief/mojo-format-project
  "cp" #'chief/mojo-precompile-package
  "cv" #'chief/mojo-version
  "r" '(:ignore t :which-key "run")
  "rr" #'chief/mojo-run-current-file
  "rp" #'chief/mojo-run-project-file
  "rt" #'chief/mojo-run-pixi-task
  "rx" #'chief/mojo-run-arbitrary-command
  "t" '(:ignore t :which-key "test")
  "tt" #'chief/mojo-test-at-point
  "tf" #'chief/mojo-test-buffer
  "tp" #'chief/mojo-test-project
  "d" '(:ignore t :which-key "debug")
  "dd" #'chief/mojo-dap-debug-current-file
  "dp" #'chief/mojo-dap-debug-project-file
  "da" #'chief/mojo-dap-attach
  "dA" #'chief/mojo-debug-cli-attach
  "db" #'chief/mojo-debug-binary
  "dc" #'chief/mojo-debug-current-file
  "dC" #'chief/mojo-debug-project-file
  "dg" #'chief/mojo-debug-gpu-current-file
  "ds" #'chief/mojo-debugger-cli
  "p" '(:ignore t :which-key "pixi/project")
  "pd" #'chief/mojo-doctor
  "pe" #'chief/mojo-select-pixi-environment
  "pi" #'chief/mojo-init-project
  "pa" #'chief/mojo-install-project-tooling
  "pj" #'chief/mojo-start-jupyter
  "pJ" #'chief/mojo-install-jupyter
  "pn" #'chief/mojo-create-notebook
  "pt" #'chief/mojo-run-pixi-task
  "l" '(:ignore t :which-key "lsp")
  "la" #'chief/mojo-code-action
  "ld" #'chief/mojo-goto-definition
  "lh" #'chief/mojo-show-hover
  "li" #'chief/mojo-toggle-inlay-hints
  "lk" #'chief/mojo-show-signature
  "lr" #'chief/mojo-rename
  "lR" #'chief/mojo-find-references
  "ls" #'chief/mojo-workspace-symbols
  "lw" #'lsp-workspace-restart)

(provide 'lang-mojo)
;;; lang-mojo.el ends here
