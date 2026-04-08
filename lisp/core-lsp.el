;;; core-lsp.el --- LSP setup -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'core-projects)
(require 'lsp-mode)
(require 'project)
(require 'subr-x)
(require 'xref)

(setq lsp-use-plists nil)

(declare-function chief/large-buffer-p "core-ui" (&optional threshold))

(defvar chief/lsp-manual-install-history (make-hash-table :test #'equal)
  "Remember manual LSP install prompts already shown this session.")

(defvar chief/lsp-command-probe-cache (make-hash-table :test #'equal)
  "Cache of LSP command probe results keyed by command line.")

(defconst chief/lsp-disabled-auxiliary-clients
  '(semgrep-ls golangci-lint)
  "Auxiliary LSP clients disabled by default.

These clients are useful as secondary analyzers, but they should never
masquerade as the primary language server for a buffer.")

(defconst chief/lsp-managed-major-modes
  '(bash-ts-mode
    c-ts-mode
    c++-ts-mode
    clojure-mode
    csharp-ts-mode
    css-ts-mode
    cue-mode
    dart-mode
    elixir-mode
    elixir-ts-mode
    erlang-mode
    ess-r-mode
    fennel-mode
    fsharp-mode
    gfm-mode
    go-ts-mode
    haskell-mode
    hy-mode
    java-ts-mode
    js-ts-mode
    json-ts-mode
    kotlin-mode
    lua-mode
    lua-ts-mode
    markdown-mode
    nix-mode
    nim-mode
    nushell-mode
    php-mode
    protobuf-mode
    racket-mode
    reason-mode
    ruby-ts-mode
    rust-ts-mode
    swift-mode
    tuareg-mode
    typescript-ts-mode
    yaml-mode
    yaml-ts-mode
    zig-mode)
  "Major modes that should auto-start LSP support.")

(defconst chief/lsp-manual-install-specs
  '((:modes (python-base-mode python-mode python-ts-mode)
     :label "Python"
     :install-function chief/python-install-project-tooling
     :install-predicate chief/python-project-tooling-installable-p
     :doc-url "https://docs.astral.sh/ty/installation/"
     :help "This setup uses uv-managed ty and Ruff inside the project environment.")
    (:modes (elixir-mode elixir-ts-mode)
     :label "Elixir Expert"
     :doc-url "https://expert-lsp.org/docs/editors/"
     :help "Install `expert`, disable `elixir-ls`, and ensure Emacs can run `expert --stdio`.")
    (:modes (erlang-mode)
     :label "Erlang ELP"
     :doc-url "https://whatsapp.github.io/erlang-language-platform/docs/get-started/install/"
     :help "Install `elp` and ensure Emacs can run `elp server`.")
    (:modes (swift-mode swift-ts-mode)
     :label "Swift"
     :doc-url "https://www.swift.org/install/"
     :help "Install the Swift toolchain or Xcode so sourcekit-lsp is on PATH.")
    (:modes (dart-mode dart-ts-mode)
     :label "Dart"
     :doc-url "https://dart.dev/get-dart"
     :help "Install the Dart SDK or Flutter so `dart language-server --protocol=lsp` is available.")
    (:modes (tuareg-mode reason-mode caml-mode)
     :label "OCaml"
     :install-command ("opam" "install" "ocaml-lsp-server")
     :install-predicate (lambda () (executable-find "opam"))
     :doc-url "https://github.com/ocaml/ocaml-lsp?tab=readme-ov-file#installation")
    (:modes (nim-mode)
     :label "Nim"
     :install-command ("nimble" "install" "nimlsp")
     :install-predicate (lambda () (executable-find "nimble"))
     :doc-url "https://github.com/PMunch/nimlsp")
    (:modes (racket-mode)
     :label "Racket"
     :install-command ("raco" "pkg" "install" "racket-langserver")
     :install-predicate (lambda () (executable-find "raco"))
     :doc-url "https://github.com/jeapostrophe/racket-langserver")
    (:modes (ess-r-mode)
     :label "R"
     :install-command ("R" "--slave" "-e" "install.packages('languageserver')")
     :install-predicate (lambda () (executable-find "R"))
     :doc-url "https://github.com/REditorSupport/languageserver")
    (:modes (fennel-mode)
     :label "Fennel"
     :doc-url "https://git.sr.ht/~xerool/fennel-ls")
    (:modes (hy-mode)
     :label "Hy"
     :doc-url "https://pypi.org/project/hyuga/")
    (:modes (nix-mode nix-ts-mode)
     :label "Nix"
     :doc-url "https://github.com/oxalica/nil")
    (:modes (cue-mode)
     :label "CUE"
     :doc-url "https://cuelang.org/docs/reference/command/")
    (:modes (templ-ts-mode)
     :label "templ"
     :install-command ("go" "install" "github.com/a-h/templ/cmd/templ@latest")
     :install-predicate (lambda () (executable-find "go"))
     :doc-url "https://templ.guide/developer-tools/ide-support/"
     :help "The templ CLI provides both `templ lsp` and `templ fmt`.")
    (:modes (rust-ts-mode rust-mode)
     :label "Rust Analyzer"
     :install-command ("rustup" "component" "add" "rust-analyzer")
     :install-predicate (lambda () (executable-find "rustup"))
     :doc-url "https://rust-analyzer.github.io/book/installation.html"
     :help "Install rust-analyzer so Emacs can start the Rust LSP server directly.")
    (:modes (nushell-mode)
     :label "Nushell"
     :doc-url "https://www.nushell.sh/book/"
     :help "Install Nushell and ensure Emacs can run `nu --lsp` and `nu`."))
  "Manual installation metadata for languages without native `lsp-install-server' support.")

(defun chief/lsp-command-cache-key (command)
  "Return a stable cache key for COMMAND."
  (mapconcat #'identity command "\0"))

(defun chief/lsp-command-usable-p (command)
  "Return non-nil when COMMAND exits successfully."
  (let* ((key (chief/lsp-command-cache-key command))
         (cached (gethash key chief/lsp-command-probe-cache :missing)))
    (if (not (eq cached :missing))
        cached
      (let ((stderr-file (make-temp-file "chief-lsp-probe-")))
        (unwind-protect
            (let ((status (condition-case nil
                              (apply #'process-file
                                     (car command)
                                     nil
                                     (list nil stderr-file)
                                     nil
                                     (cdr command))
                            (error 255))))
              (puthash key (zerop status) chief/lsp-command-probe-cache)
              (zerop status))
          (when (file-exists-p stderr-file)
            (delete-file stderr-file)))))))

(defun chief/lsp-probed-executable-command (program args probe-args)
  "Return PROGRAM with ARGS when the corresponding PROBE-ARGS succeed."
  (when-let* ((path (executable-find program))
              (probe (cons path probe-args))
              ((chief/lsp-command-usable-p probe)))
    (cons path args)))

(defun chief/lsp-xcrun-tool-path (tool)
  "Return the resolved TOOL path via `xcrun -f', if available."
  (when-let* ((xcrun (executable-find "xcrun")))
    (ignore-errors
      (when-let* ((path (car (process-lines xcrun "-f" tool)))
                  (path (string-trim path))
                  ((file-executable-p path)))
        path))))

(defun chief/lsp-xcrun-command (tool args probe-args)
  "Return TOOL resolved through `xcrun -f' with ARGS when usable."
  (when-let* ((path (chief/lsp-xcrun-tool-path tool))
              (probe (cons path probe-args))
              ((chief/lsp-command-usable-p probe)))
    (cons path args)))

(defun chief/lsp-project-root ()
  "Return the current project root, or `default-directory' when unavailable."
  (chief/project-preferred-root
   (when (or (functionp chief/lsp-root-function)
             (and (symbolp chief/lsp-root-function)
                  (fboundp chief/lsp-root-function)))
     (funcall chief/lsp-root-function))
   (chief/lsp-language-root)
   (chief/project-current-root)
   default-directory))

(defvar-local chief/lsp-root-function nil
  "Buffer-local function used to resolve the preferred LSP workspace root.")

(defun chief/lsp-language-root ()
  "Return a language-aware workspace root for the current buffer, if any."
  (cond
   ((or (derived-mode-p 'go-mode 'go-ts-mode 'go-mod-ts-mode 'go-work-ts-mode 'templ-ts-mode)
        (and buffer-file-name (string-match-p "\\.templ\\'" buffer-file-name)))
    (when (fboundp 'chief/go-project-root)
      (chief/go-project-root)))
   ((derived-mode-p 'python-base-mode 'python-mode 'python-ts-mode)
    (when (fboundp 'chief/python-project-root)
      (chief/python-project-root)))
   ((or (derived-mode-p 'elixir-mode 'elixir-ts-mode 'heex-ts-mode)
        (and (derived-mode-p 'web-mode)
             (fboundp 'chief/elixir-template-buffer-p)
             (chief/elixir-template-buffer-p)))
    (when (fboundp 'chief/elixir-project-root)
      (chief/elixir-project-root)))
   ((derived-mode-p 'erlang-mode)
    (when (fboundp 'chief/erlang-project-root)
      (chief/erlang-project-root)))
   ((derived-mode-p 'js-mode 'js-ts-mode 'typescript-ts-mode 'tsx-ts-mode 'mhtml-mode)
    (when (fboundp 'chief/jsts-project-root)
      (chief/jsts-project-root)))
   ((derived-mode-p 'kotlin-mode 'java-mode 'java-ts-mode)
    (when (fboundp 'chief/jvm-project-root)
      (chief/jvm-project-root)))
   ((derived-mode-p 'tuareg-mode 'reason-mode 'caml-mode)
    (when (fboundp 'chief/ocaml-project-root)
      (chief/ocaml-project-root)))
   ((derived-mode-p 'rust-ts-mode)
    (when (fboundp 'chief/rust-project-root)
      (chief/rust-project-root)))
   ((derived-mode-p 'csharp-mode 'csharp-ts-mode 'fsharp-mode)
    (when (fboundp 'chief/dotnet-project-root)
      (chief/dotnet-project-root)))
   ((derived-mode-p 'swift-mode)
    (when (fboundp 'chief/swift-project-root)
      (chief/swift-project-root)))
   ((derived-mode-p 'zig-mode)
    (when (fboundp 'chief/zig-project-root)
      (chief/zig-project-root)))
   ((derived-mode-p 'nushell-mode)
    (when (fboundp 'chief/nushell-project-root)
      (chief/nushell-project-root)))))

(defun chief/lsp-prime-session-root ()
  "Seed `lsp-mode' with the preferred workspace root for the current buffer."
  (when-let* ((root (chief/lsp-project-root)))
    (require 'lsp-mode)
    (let* ((canonical (file-name-as-directory (lsp-f-canonical root)))
           (session (lsp-session)))
      (setq-local default-directory canonical)
      (setq-local lsp-auto-guess-root nil)
      (setq-local lsp-use-workspace-root-for-server-default-directory t)
      (cl-pushnew canonical (lsp-session-folders session) :test #'equal)
      canonical)))

(defun chief/lsp-suggest-project-root-a (orig-fn &rest args)
  "Prefer `chief/lsp-project-root' over `lsp-mode' root guessing."
  (or (chief/lsp-project-root)
      (apply orig-fn args)))

(advice-add 'lsp--suggest-project-root :around #'chief/lsp-suggest-project-root-a)

(defvar-local chief/lsp-definition-function nil
  "Buffer-local override used by `chief/goto-definition'.")

(defvar-local chief/lsp-declaration-function nil
  "Buffer-local override used by `chief/goto-declaration'.")

(defvar-local chief/lsp-references-function nil
  "Buffer-local override used by `chief/goto-references'.")

(defvar-local chief/lsp-implementation-function nil
  "Buffer-local override used by `chief/goto-implementation'.")

(defvar-local chief/lsp-hover-function nil
  "Buffer-local override used by `chief/show-hover-doc'.")

(defvar-local chief/lsp-signature-function nil
  "Buffer-local override used by `chief/show-signature-help'.")

(defun chief/lsp-request-error-message (err)
  "Return a readable error message for async LSP ERR."
  (cond
   ((lsp-json-error? err)
    (lsp:json-error-message err))
   ((and (consp err)
         (lsp-json-error? (car err)))
    (lsp:json-error-message (car err)))
   ((stringp err) err)
   (t
    (error-message-string err))))

(defun chief/lsp-call-local-override (fn)
  "Call FN when it is a callable buffer-local override."
  (when (or (functionp fn)
            (and (symbolp fn) (fboundp fn)))
    (call-interactively fn)
    t))

(defun chief/lsp-executable-command (name &rest args)
  "Return an executable command list for NAME and ARGS when available."
  (when-let* ((program (executable-find name)))
    (cons program args)))

(defun chief/lsp-disable-clients (&rest clients)
  "Disable CLIENTS for the current buffer without dropping shared defaults."
  (setq-local
   lsp-disabled-clients
   (cl-remove-duplicates
    (append clients lsp-disabled-clients)
    :test #'eq)))

(defun chief/lsp-apply-default-disabled-clients ()
  "Disable shared auxiliary LSP clients for the current buffer."
  (apply #'chief/lsp-disable-clients chief/lsp-disabled-auxiliary-clients))

(defun chief/lsp-primary-client-p (client)
  "Return non-nil when CLIENT is a primary language server candidate."
  (and client
       (not (lsp--client-add-on? client))
       (not (memq (lsp--client-server-id client)
                  chief/lsp-disabled-auxiliary-clients))))

(defun chief/lsp-managed-buffer-p ()
  "Return non-nil when the current buffer should prefer LSP navigation."
  (or chief/lsp-root-function
      (chief/lsp-manual-install-spec)
      (apply #'derived-mode-p chief/lsp-managed-major-modes)))

(defun chief/lsp-explain-inactive-server ()
  "Explain why LSP navigation is unavailable for the current buffer."
  (if-let* ((spec (chief/lsp-manual-install-spec))
            (label (plist-get spec :label))
            (doc-url (plist-get spec :doc-url))
            (help (plist-get spec :help)))
      (chief/lsp-show-install-docs label doc-url help)
    (message "LSP is not active for %s" major-mode)))

(defun chief/lsp-ensure-active-for-navigation ()
  "Ensure LSP is active before handling a navigation request.
Return non-nil when the current buffer should proceed with LSP navigation."
  (cond
   ((bound-and-true-p lsp-managed-mode) t)
   ((not (chief/lsp-managed-buffer-p)) nil)
   (t
    (chief/lsp-apply-default-disabled-clients)
    (chief/lsp-prime-session-root)
    (pcase (chief/lsp-maybe-offer-manual-install)
      ('manual
       (chief/lsp-explain-inactive-server)
       nil)
      (_
       (condition-case err
           (progn
             (lsp)
             (if (bound-and-true-p lsp-managed-mode)
                 t
               (chief/lsp-explain-inactive-server)
               nil))
         (error
          (message "Failed to start LSP: %s" (error-message-string err))
          nil)))))))

(defun chief/lsp-location-items (method &optional extra)
  "Return Xref items for LSP METHOD using EXTRA request parameters."
  (when (and (bound-and-true-p lsp-managed-mode)
             (fboundp 'lsp-request)
             (fboundp 'lsp--text-document-position-params)
             (fboundp 'lsp--locations-to-xref-items))
    (let* ((locations (lsp-request method
                                   (append (lsp--text-document-position-params) extra)))
           (items (and locations (lsp--locations-to-xref-items locations))))
      (unless (seq-empty-p items)
        items))))

(defun chief/lsp-request-locations-async (method success &optional extra not-found-message)
  "Request METHOD asynchronously and pass xref items to SUCCESS.
EXTRA is appended to the position params.  NOT-FOUND-MESSAGE overrides the
default echo-area text when no locations are returned."
  (unless (and (bound-and-true-p lsp-managed-mode)
               (fboundp 'lsp-request-async)
               (fboundp 'lsp--text-document-position-params)
               (fboundp 'lsp--locations-to-xref-items))
    (user-error "No LSP location backend is available here"))
  (let ((buffer (current-buffer))
        (params (append (lsp--text-document-position-params) extra))
        (symbol (thing-at-point 'symbol t)))
    (lsp-request-async
     method
     params
     (lambda (locations)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let ((items (and locations (lsp--locations-to-xref-items locations))))
             (if (seq-empty-p items)
                 (message "%s" (or not-found-message
                                   (format "Not found for: %s" (or symbol ""))))
               (funcall success items))))))
     :error-handler
     (lambda (err)
       (message "LSP %s failed: %s"
                method
                (chief/lsp-request-error-message err)))
     :mode 'detached)))

(defun chief/lsp-show-location-list-async (method &optional extra display-action references?)
  "Show METHOD results asynchronously via the xref UI.
EXTRA is appended to the request params.  DISPLAY-ACTION and REFERENCES?
match `lsp-show-xrefs'."
  (chief/lsp-request-locations-async
   method
   (lambda (items)
     (lsp-show-xrefs items display-action references?))
   extra))

(defun chief/lsp-jump-to-first-location (method &optional extra)
  "Jump directly to the first LSP location returned by METHOD and EXTRA."
  (when-let* ((items (chief/lsp-location-items method extra))
              (item (car items)))
    (xref-push-marker-stack)
    (xref-pop-to-location item)
    t))

(defun chief/lsp-jump-to-first-location-async (method &optional extra)
  "Jump asynchronously to the first LSP location returned by METHOD and EXTRA."
  (chief/lsp-request-locations-async
   method
   (lambda (items)
     (when-let* ((item (car items)))
       (xref-push-marker-stack)
       (xref-pop-to-location item)))
   extra))

(defun chief/lsp-goto-definition-direct ()
  "Jump directly to the first LSP definition at point."
  (interactive)
  (chief/lsp-jump-to-first-location-async "textDocument/definition"))

(defun chief/lsp-goto-declaration-direct ()
  "Jump directly to the first LSP declaration at point."
  (interactive)
  (chief/lsp-jump-to-first-location-async "textDocument/declaration"))

(defun chief/lsp-goto-implementation-direct ()
  "Jump directly to the first LSP implementation at point."
  (interactive)
  (chief/lsp-jump-to-first-location-async "textDocument/implementation"))

(defun chief/find-definitions ()
  "Show definitions for the symbol at point."
  (interactive)
  (if (chief/lsp-ensure-active-for-navigation)
      (chief/lsp-show-location-list-async "textDocument/definition")
    (user-error "No LSP definition backend is available here")))

(defun chief/find-definitions-other-window ()
  "Show definitions for the symbol at point in another window."
  (interactive)
  (if (chief/lsp-ensure-active-for-navigation)
      (chief/lsp-show-location-list-async "textDocument/definition" nil 'window)
    (user-error "No LSP definition backend is available here")))

(defun chief/find-references ()
  "Show references for the symbol at point."
  (interactive)
  (if (chief/lsp-ensure-active-for-navigation)
      (chief/lsp-show-location-list-async
       "textDocument/references"
       (list :context `(:includeDeclaration ,(lsp-json-bool (not lsp-references-exclude-declaration))))
       nil
       t)
    (user-error "No LSP references backend is available here")))

(defun chief/find-implementations ()
  "Show implementations for the symbol at point."
  (interactive)
  (cond
   ((chief/lsp-ensure-active-for-navigation)
    (chief/lsp-show-location-list-async
     "textDocument/implementation"
     nil
     nil
     t))
   (t
    (user-error "No implementation backend is available here"))))

(defun chief/goto-definition ()
  "Jump to the definition at point, preferring LSP/Xref over Evil tags."
  (interactive)
  (cond
   ((chief/lsp-call-local-override chief/lsp-definition-function))
   ((chief/lsp-ensure-active-for-navigation)
    (chief/lsp-goto-definition-direct))
   (t
    (user-error "No LSP definition backend is available here"))))

(defun chief/goto-declaration ()
  "Jump to the declaration at point, preferring LSP."
  (interactive)
  (cond
   ((chief/lsp-call-local-override chief/lsp-declaration-function))
   ((chief/lsp-ensure-active-for-navigation)
    (chief/lsp-goto-declaration-direct))
   (t
    (user-error "No LSP declaration backend is available here"))))

(defun chief/goto-references ()
  "Find references for the symbol at point."
  (interactive)
  (cond
   ((chief/lsp-call-local-override chief/lsp-references-function))
   ((chief/lsp-ensure-active-for-navigation)
    (chief/find-references))
   (t
    (user-error "No LSP references backend is available here"))))

(defun chief/goto-implementation ()
  "Jump to the implementation for the symbol at point."
  (interactive)
  (cond
   ((chief/lsp-call-local-override chief/lsp-implementation-function))
   ((chief/lsp-ensure-active-for-navigation)
    (chief/lsp-goto-implementation-direct))
   (t
    (user-error "No implementation backend is available here"))))

(defun chief/show-hover-doc ()
  "Show hover documentation for the symbol at point."
  (interactive)
  (cond
   ((chief/lsp-call-local-override chief/lsp-hover-function))
   ((and (chief/lsp-ensure-active-for-navigation)
         (fboundp 'lsp-request-async)
         (fboundp 'lsp--display-contents))
    (let ((buffer (current-buffer))
          (params (lsp--text-document-position-params)))
      (lsp-request-async
       "textDocument/hover"
       params
       (lambda (hover)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (if-let* ((contents (and hover (lsp:hover-contents hover))))
                 (lsp--display-contents contents)
               (message "No content at point.")))))
       :error-handler
       (lambda (err)
         (message "LSP hover failed: %s" (chief/lsp-request-error-message err)))
       :mode 'detached)))
   ((fboundp 'eldoc-doc-buffer)
    (eldoc-doc-buffer))
   (t
    (user-error "No documentation backend is available here"))))

(defun chief/show-signature-help ()
  "Show signature help for the call at point."
  (interactive)
  (cond
   ((chief/lsp-call-local-override chief/lsp-signature-function))
   ((and (chief/lsp-ensure-active-for-navigation)
         (fboundp 'lsp-signature-activate))
    (call-interactively #'lsp-signature-activate))
   (t
    (user-error "No signature help backend is available here"))))

(defun chief/lsp-ui-buffer-setup ()
  "Enable lightweight LSP UI features for the current buffer."
  (let ((lightweight-p
         (not (and (fboundp 'chief/large-buffer-p)
                   (chief/large-buffer-p)))))
    (setq-local lsp-enable-symbol-highlighting lightweight-p)
    (setq-local lsp-inlay-hint-enable lightweight-p))
  (eldoc-mode 1)
  (when (and (bound-and-true-p lsp-inlay-hint-enable)
             (fboundp 'lsp-inlay-hints-mode))
    (lsp-inlay-hints-mode 1))
  (when (fboundp 'chief/format-enable-on-save)
    (chief/format-enable-on-save))
  (when (fboundp 'yas-minor-mode)
    (yas-minor-mode 1)))

(with-eval-after-load 'evil
  (dolist (state '(normal motion visual))
    (evil-global-set-key state "gd" #'chief/goto-definition)
    (evil-global-set-key state "gD" #'chief/goto-declaration)
    (evil-global-set-key state "gr" #'chief/goto-references)
    (evil-global-set-key state "gI" #'chief/goto-implementation)
    (evil-global-set-key state "gK" #'chief/show-signature-help)
    (evil-global-set-key state "K" #'chief/show-hover-doc))
  (when (fboundp 'general-define-key)
    (general-define-key
     :states '(normal motion visual)
     :keymaps 'override
     "gd" #'chief/goto-definition
     "gD" #'chief/goto-declaration
     "gr" #'chief/goto-references
     "gI" #'chief/goto-implementation
     "gK" #'chief/show-signature-help
     "K" #'chief/show-hover-doc))
  (evil-global-set-key 'insert (kbd "C-S-SPC") #'completion-at-point)
  (evil-global-set-key 'insert (kbd "C-k") #'chief/show-signature-help))

(defun chief/lsp-ocamllsp-command ()
  "Return the command list for ocamllsp, preferring `dune tools which'."
  (let ((root (chief/lsp-project-root)))
    (or (when (and root (executable-find "dune"))
          (let ((default-directory root))
            (ignore-errors
              (when-let* ((output (car (process-lines "dune" "tools" "which" "ocamllsp")))
                          (path (string-trim output))
                          ((file-executable-p path)))
                (list path)))))
        (chief/lsp-executable-command "ocamllsp"))))

(defun chief/lsp-sourcekit-command ()
  "Return the command list for sourcekit-lsp."
  (or (chief/lsp-probed-executable-command "sourcekit-lsp" nil '("--help"))
      (chief/lsp-xcrun-command "sourcekit-lsp" nil '("--help"))))

(defun chief/lsp-kotlin-command ()
  "Return the command list for the Kotlin LSP used in the NeoVim setup."
  (chief/lsp-probed-executable-command "kotlin-lsp" '("--stdio") '("--help")))

(defun chief/lsp-dart-command ()
  "Return the command list for the Dart language server."
  (chief/lsp-probed-executable-command "dart" '("language-server" "--protocol=lsp") '("--version")))

(defun chief/lsp-cue-command ()
  "Return the command list for the CUE language server."
  (chief/lsp-probed-executable-command "cue" '("lsp") '("version")))

(defun chief/lsp-gopls-command ()
  "Return the command list for gopls."
  (chief/lsp-probed-executable-command "gopls" nil '("version")))

(defun chief/lsp-rust-analyzer-command ()
  "Return the command list for rust-analyzer when it is usable."
  (chief/lsp-probed-executable-command "rust-analyzer" nil '("--version")))

(defun chief/lsp-manual-install-spec ()
  "Return the manual-install spec for the current buffer, if any."
  (cl-find-if
   (lambda (spec)
     (apply #'derived-mode-p (plist-get spec :modes)))
   chief/lsp-manual-install-specs))

(defun chief/lsp-manual-install-key (label)
  "Return a session key for LABEL scoped to the current project."
  (format "%s:%s" label (chief/lsp-project-root)))

(defun chief/lsp-run-install-command (command label)
  "Run install COMMAND for LABEL in a compilation buffer."
  (let ((default-directory (chief/lsp-project-root))
        (compilation-read-command nil))
    (compilation-start
     (mapconcat #'shell-quote-argument command " ")
     'compilation-mode
     (lambda (_) (format "*install %s lsp*" (downcase label))))))

(defun chief/lsp-buffer-status ()
  "Return a plist describing current-buffer LSP support."
  (require 'lsp-mode)
  (lsp--require-packages)
  (let* ((clients (seq-filter #'chief/lsp-primary-client-p
                              (lsp--filter-clients #'lsp--supports-buffer?)))
         (installed (seq-filter #'lsp--server-binary-present? clients))
         (downloading (seq-filter #'lsp--client-download-in-progress? clients))
         (downloadable
          (seq-filter
           (lambda (client)
             (and (lsp--client-download-server-fn client)
                  (not (lsp--server-binary-present? client))
                  (not (lsp--client-download-in-progress? client))))
           clients)))
    (list :clients clients
          :installed installed
          :downloading downloading
          :downloadable downloadable)))

(defun chief/lsp-show-install-docs (label url help)
  "Show install guidance for LABEL using URL and HELP."
  (message
   "%s LSP install guidance: %s%s"
   label
   (or url "No documentation URL configured.")
   (if help
       (concat " " help)
     "")))

(defun chief/lsp-maybe-offer-manual-install ()
  "Offer a manual install path when the current buffer lacks an auto-installable LSP server.
Return one of `ready', `auto', `downloading', `manual', or nil."
  (when (and (not noninteractive)
             buffer-file-name
             (not (file-remote-p buffer-file-name)))
    (pcase-let* ((`(:clients ,clients :installed ,installed :downloading ,downloading :downloadable ,downloadable)
                  (chief/lsp-buffer-status)))
      (cond
       (installed 'ready)
       (downloading 'downloading)
       (downloadable 'auto)
       ((and clients (not (chief/lsp-manual-install-spec)))
        nil)
       (t
        (when-let* ((spec (chief/lsp-manual-install-spec))
                    (label (plist-get spec :label))
                    (key (chief/lsp-manual-install-key label))
                    ((not (gethash key chief/lsp-manual-install-history))))
          (puthash key t chief/lsp-manual-install-history)
          (let* ((install-function (plist-get spec :install-function))
                 (install-command (plist-get spec :install-command))
                 (install-predicate (plist-get spec :install-predicate))
                 (install-available-p
                  (cond
                   (install-predicate
                    (funcall install-predicate))
                   (install-command
                    (executable-find (car install-command)))
                   (install-function
                    (commandp install-function))))
                 (doc-url (plist-get spec :doc-url))
                 (help (plist-get spec :help))
                 (choices
                  (delq
                   nil
                   (list
                    (when (and install-available-p
                               (or install-function install-command))
                      '(?i "install" "Run the configured install action now."))
                    (when doc-url
                      '(?d "docs" "Show the installation documentation URL in the echo area."))
                    '(?s "skip" "Skip this for now."))))
                 (choice
                  (car
                   (read-multiple-choice
                    (format "%s LSP server is not installed for this buffer. Choose an action: "
                            label)
                    choices))))
            (setq-local lsp-warn-no-matched-clients nil)
            (pcase choice
              (?i
               (cond
                (install-function
                 (call-interactively install-function))
                (install-command
                 (chief/lsp-run-install-command install-command label)))
               'manual)
              (?d
               (chief/lsp-show-install-docs label doc-url help)
               'manual)
              (_ 'manual)))))))))

(defun chief/lsp-managed-mode-setup ()
  "Start or provision LSP support for the current buffer."
  (when (and buffer-file-name
             (not (file-remote-p buffer-file-name))
             (not (bound-and-true-p lsp-managed-mode)))
    (chief/lsp-apply-default-disabled-clients)
    (chief/lsp-prime-session-root)
    (pcase (chief/lsp-maybe-offer-manual-install)
      ((or 'ready 'auto 'downloading) (lsp-deferred))
      ('manual nil)
      (_ (lsp-deferred)))))

(use-package flycheck
  :hook (lsp-mode . flycheck-mode)
  :custom
  (flycheck-idle-change-delay 0.5)
  (flycheck-display-errors-delay 0.3))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . chief/lsp-ui-buffer-setup)
         (bash-ts-mode . chief/lsp-managed-mode-setup)
         (c-mode . chief/lsp-managed-mode-setup)
         (c-ts-mode . chief/lsp-managed-mode-setup)
         (c++-mode . chief/lsp-managed-mode-setup)
         (c++-ts-mode . chief/lsp-managed-mode-setup)
         (clojure-mode . chief/lsp-managed-mode-setup)
         (css-mode . chief/lsp-managed-mode-setup)
         (css-ts-mode . chief/lsp-managed-mode-setup)
         (csharp-mode . chief/lsp-managed-mode-setup)
         (csharp-ts-mode . chief/lsp-managed-mode-setup)
         (cue-mode . chief/lsp-managed-mode-setup)
         (dart-mode . chief/lsp-managed-mode-setup)
         (ess-r-mode . chief/lsp-managed-mode-setup)
         (fennel-mode . chief/lsp-managed-mode-setup)
         (fsharp-mode . chief/lsp-managed-mode-setup)
         (gfm-mode . chief/lsp-managed-mode-setup)
         (haskell-mode . chief/lsp-managed-mode-setup)
         (html-ts-mode . chief/lsp-managed-mode-setup)
         (hy-mode . chief/lsp-managed-mode-setup)
         (java-mode . chief/lsp-managed-mode-setup)
         (java-ts-mode . chief/lsp-managed-mode-setup)
         (js-json-mode . chief/lsp-managed-mode-setup)
         (json-ts-mode . chief/lsp-managed-mode-setup)
         (kotlin-mode . chief/lsp-managed-mode-setup)
         (lua-mode . chief/lsp-managed-mode-setup)
         (lua-ts-mode . chief/lsp-managed-mode-setup)
         (markdown-mode . chief/lsp-managed-mode-setup)
         (mhtml-mode . chief/lsp-managed-mode-setup)
         (nix-mode . chief/lsp-managed-mode-setup)
         (nim-mode . chief/lsp-managed-mode-setup)
         (php-mode . chief/lsp-managed-mode-setup)
         (protobuf-mode . chief/lsp-managed-mode-setup)
         (racket-mode . chief/lsp-managed-mode-setup)
         (reason-mode . chief/lsp-managed-mode-setup)
         (ruby-mode . chief/lsp-managed-mode-setup)
         (ruby-ts-mode . chief/lsp-managed-mode-setup)
         (rust-mode . chief/lsp-managed-mode-setup)
         (rust-ts-mode . chief/lsp-managed-mode-setup)
         (sh-mode . chief/lsp-managed-mode-setup)
         (swift-mode . chief/lsp-managed-mode-setup)
         (tuareg-mode . chief/lsp-managed-mode-setup)
         (yaml-mode . chief/lsp-managed-mode-setup)
         (yaml-ts-mode . chief/lsp-managed-mode-setup))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-suggest-server-download t)
  (lsp-completion-provider :none)
  (lsp-idle-delay 0.45)
  (lsp-log-io nil)
  (lsp-response-timeout 20)
  (lsp-auto-guess-root nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-folding nil)
  (lsp-enable-imenu t)
  (lsp-enable-snippet t)
  (lsp-enable-file-watchers t)
  (lsp-keep-workspace-alive nil)
  (lsp-eldoc-enable-hover t)
  (lsp-signature-auto-activate '(:on-trigger-char :on-server-request))
  (lsp-signature-render-documentation t)
  (lsp-signature-doc-lines 4)
  (lsp-symbol-highlighting-skip-current nil)
  (lsp-completion-enable-additional-text-edit t)
  (lsp-semantic-tokens-enable nil)
  (lsp-inlay-hint-enable t)
  (lsp-file-watch-threshold 5000)
  (lsp-diagnostics-provider :flycheck)
  (lsp-disabled-clients chief/lsp-disabled-auxiliary-clients)
  :config
  (require 'lsp-bash)
  (require 'lsp-clojure)
  (require 'lsp-clangd)
  (require 'lsp-css)
  (require 'lsp-elixir)
  (require 'lsp-erlang)
  (require 'lsp-fennel)
  (require 'lsp-html)
  (require 'lsp-hy)
  (require 'lsp-json)
  (require 'lsp-kotlin)
  (require 'lsp-lua)
  (require 'lsp-marksman)
  (require 'lsp-nim)
  (require 'lsp-nix)
  (require 'lsp-nushell)
  (require 'lsp-ocaml)
  (require 'lsp-php)
  (require 'lsp-r)
  (require 'lsp-racket)
  (require 'lsp-rust)
  (require 'lsp-yaml)
  (require 'lsp-completion)
  (dolist (directory '("[/\\\\]\\.git\\'"
                       "[/\\\\]\\.direnv\\'"
                       "[/\\\\]\\.venv\\'"
                       "[/\\\\]\\.elixir_ls\\'"
                       "[/\\\\]_build\\'"
                       "[/\\\\]deps\\'"
                       "[/\\\\]\\.mypy_cache\\'"
                       "[/\\\\]\\.ruff_cache\\'"
                       "[/\\\\]\\.pytest_cache\\'"
                       "[/\\\\]\\.cache\\'"
                       "[/\\\\]\\.dart_tool\\'"
                       "[/\\\\]\\.gradle\\'"
                       "[/\\\\]\\.zig-cache\\'"
                       "[/\\\\]\\.yarn\\'"
                       "[/\\\\]\\.pnpm-store\\'"
                       "[/\\\\]\\.npm\\'"
                       "[/\\\\]\\.turbo\\'"
                       "[/\\\\]\\.next\\'"
                       "[/\\\\]node_modules\\'"
                       "[/\\\\]dist\\'"
                       "[/\\\\]build\\'"
                       "[/\\\\]target\\'"
                       "[/\\\\]coverage\\'"
                       "[/\\\\]tmp\\'"
                       "[/\\\\]vendor\\'"))
    (add-to-list 'lsp-file-watch-ignored-directories directory))
  (setq lsp-completion-enable t)
  (setq lsp-completion-show-kind t)
  (setq lsp-completion-show-detail t)
  (setq lsp-clojure-server-command '("clojure-lsp"))
  (setq lsp-go-directory-filters
        ["-.git"
         "-.direnv"
         "-tmp"
         "-node_modules"
         "-vendor"
         "-dist"
         "-build"
         "-coverage"])
  (setq lsp-erlang-server 'erlang-language-platform)
  (setq lsp-erlang-elp-types-on-hover t)
  (setq lsp-erlang-elp-signature-help-enable t)
  (setq lsp-rust-server 'rust-analyzer)
  (when-let* ((command (chief/lsp-gopls-command)))
    (setq lsp-go-gopls-server-path (car command)))
  (when-let* ((command (chief/lsp-probed-executable-command "elp" '("server") '("version"))))
    (setq lsp-erlang-elp-server-command command))
  (when-let* ((command (chief/lsp-probed-executable-command "kotlin-language-server" nil '("--help"))))
    (setq lsp-clients-kotlin-server-executable (car command)))
  (when-let* ((command (chief/lsp-executable-command "lua-language-server")))
    (setq lsp-clients-lua-language-server-command command))
  (when-let* ((command (chief/lsp-ocamllsp-command)))
    (setq lsp-ocaml-lsp-server-command command))
  (when-let* ((command (chief/lsp-executable-command "nimlangserver")))
    (setq lsp-nim-langserver (car command)))
  (when-let* ((command (chief/lsp-executable-command "nimlsp")))
    (setq lsp-nim-lsp (car command)))
  (when-let* ((command (or (chief/lsp-executable-command "elixir-ls")
                           (chief/lsp-executable-command "language_server.sh"))))
    (setq lsp-elixir-server-command command))
  (if-let* ((command (chief/lsp-rust-analyzer-command)))
      (setq lsp-rust-analyzer-server-command command)
    (setq lsp-rust-analyzer-server-command nil))
  (when-let* ((command (chief/lsp-probed-executable-command "nu" '("--lsp") '("--version"))))
    (setq lsp-nushell-language-server-command command))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection #'chief/lsp-kotlin-command)
    :major-modes '(kotlin-mode)
    :priority 2
    :server-id 'kotlin-lsp-custom))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection #'chief/lsp-sourcekit-command)
    :major-modes '(swift-mode)
    :priority 1
    :server-id 'sourcekit-lsp))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection #'chief/lsp-dart-command)
    :major-modes '(dart-mode)
    :priority 1
    :server-id 'dart-lsp))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection #'chief/lsp-cue-command)
    :activation-fn (lsp-activate-on "cue")
    :priority 1
    :server-id 'cue-lsp))
  (chief/leader-def
    "l" '(:ignore t :which-key "lsp")
    "la" #'lsp-execute-code-action
    "ld" #'chief/find-definitions
    "lD" #'chief/find-definitions-other-window
    "lf" #'lsp-format-buffer
    "li" #'chief/find-implementations
    "lt" #'lsp-find-type-definition
    "lr" #'lsp-rename
    "lR" #'chief/find-references
    "lh" #'lsp-describe-thing-at-point
    "lk" #'chief/show-signature-help
    "lw" #'lsp-workspace-restart
    "ls" #'consult-lsp-symbols
    "le" #'flycheck-list-errors)
  )

(provide 'core-lsp)
;;; core-lsp.el ends here
