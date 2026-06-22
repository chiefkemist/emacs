;;; lang-polyglot.el --- Extra language coverage -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'compile)
(require 'comint)
(require 'core-projects)
(require 'core-repl)
(require 'json)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'xml)

(declare-function dap-debug "dap-mode" (debug-args))
(declare-function lsp-execute-code-action "lsp-mode" ())
(declare-function lsp-format-buffer "lsp-mode" ())
(declare-function lsp-organize-imports "lsp-mode" ())
(declare-function lsp-ocaml-find-alternate-file "lsp-ocaml" ())
(declare-function lsp-ocaml-infer-interface "lsp-ocaml" ())
(declare-function lsp-ocaml-type-enclosing "lsp-ocaml" ())

(defun chief/polyglot-project-root ()
  "Return the current project root or `default-directory'."
  (chief/project-preferred-root
   (chief/project-current-root)
   default-directory))

(defun chief/ocaml-project-root ()
  "Return the current OCaml project root."
  (chief/project-preferred-root
   '("dune-workspace")
   '("dune-project")
   (chief/polyglot-project-root)))

(defcustom chief/ocaml-dune-profile nil
  "Optional Dune profile passed to Dune commands."
  :type '(choice (const nil) string)
  :group 'chief)

(defun chief/polyglot-compile (command name &optional directory)
  "Run COMMAND in DIRECTORY using compilation buffer NAME."
  (let ((default-directory (or directory (chief/polyglot-project-root)))
        (compilation-read-command nil))
    (compilation-start
     (mapconcat #'shell-quote-argument command " ")
     'compilation-mode
     (lambda (_) name))))

(defun chief/polyglot-current-file (language)
  "Return the current buffer file for LANGUAGE as an absolute path."
  (unless buffer-file-name
    (user-error "This buffer is not visiting a %s file" language))
  (expand-file-name buffer-file-name))

(defun chief/polyglot-save-current-buffer ()
  "Save the current buffer when it is visiting a modified file."
  (when (and buffer-file-name (buffer-modified-p))
    (save-buffer)))

(defun chief/polyglot-command (program &rest args)
  "Return a command list starting with PROGRAM and ARGS."
  (unless (executable-find program)
    (user-error "%s is not available on PATH" program))
  (cons program args))

(defun chief/polyglot-completion-setup (&optional zero-prefix)
  "Enable Corfu/LSP completion for language buffers.
When ZERO-PREFIX is non-nil, popup completion may trigger immediately after
member-access punctuation with an empty textual prefix."
  (setq-local lsp-completion-enable t)
  (setq-local lsp-completion-enable-additional-text-edit t)
  (when (boundp 'corfu-auto)
    (setq-local corfu-auto t))
  (when (boundp 'corfu-auto-delay)
    (setq-local corfu-auto-delay 0.05))
  (when (and zero-prefix (boundp 'corfu-auto-prefix))
    (setq-local corfu-auto-prefix 0))
  (when (fboundp 'corfu-mode)
    (when (bound-and-true-p corfu-mode)
      (corfu-mode -1))
    (corfu-mode 1))
  (when (fboundp 'lsp-completion-mode)
    (lsp-completion-mode 1)))

(defun chief/polyglot-run-command-async (command directory buffer-name on-success)
  "Run COMMAND asynchronously in DIRECTORY.
Output goes to BUFFER-NAME.  ON-SUCCESS receives the buffer output after a zero
exit status."
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (setq buffer-read-only nil)
        (erase-buffer)
        (setq-local default-directory directory)))
    (let ((process
           (make-process
            :name buffer-name
            :buffer buffer
            :stderr buffer
            :command command
            :noquery t
            :sentinel
            (lambda (process _event)
              (when (memq (process-status process) '(exit signal))
                (let* ((status (process-exit-status process))
                       (success (and (eq (process-status process) 'exit)
                                     (zerop status)))
                       (output (when (buffer-live-p (process-buffer process))
                                 (with-current-buffer (process-buffer process)
                                   (buffer-string)))))
                  (if success
                      (funcall on-success output)
                    (when (buffer-live-p (process-buffer process))
                      (display-buffer (process-buffer process)))
                    (message "%s failed with exit status %s" buffer-name status))))))))
      (set-process-query-on-exit-flag process nil)
      process)))

(defun chief/polyglot-lldb-dap-debug (program name cwd source-language &optional args)
  "Start a DAP session for PROGRAM named NAME in CWD.
SOURCE-LANGUAGE is sent to LLDB/GDB when supported.  ARGS are program args."
  (require 'dap-mode)
  (let ((configuration
         (cond
          ((featurep 'dap-lldb)
           (list :type "lldb-vscode"
                 :request "launch"
                 :name name
                 :program program
                 :cwd cwd
                 :args (or args [])
                 :sourceLanguages (vector source-language)))
          ((featurep 'dap-gdb)
           (list :type "gdb"
                 :request "launch"
                 :name name
                 :program program
                 :target program
                 :cwd cwd
                 :args (or args [])
                 :sourceLanguages (vector source-language)))
          (t
           (user-error "No LLDB/GDB DAP adapter is configured")))))
    (dap-debug configuration)))

(defun chief/polyglot-directory-files-matching (directory regexp)
  "Return files in DIRECTORY whose basenames match REGEXP."
  (when (file-directory-p directory)
    (seq-filter
     (lambda (file)
       (string-match-p regexp (file-name-nondirectory file)))
     (directory-files directory t directory-files-no-dot-files-regexp t))))

(defun chief/polyglot-nearest-matching-file (regexp &optional start root)
  "Return the nearest file matching REGEXP between START and ROOT."
  (let ((dir (file-name-as-directory
              (expand-file-name
               (or start
                   (and buffer-file-name (file-name-directory buffer-file-name))
                   default-directory))))
        (limit (and root (file-name-as-directory (expand-file-name root)))))
    (catch 'found
      (while dir
        (when-let* ((files (chief/polyglot-directory-files-matching dir regexp)))
          (throw 'found (car files)))
        (if (or (equal dir limit)
                (equal dir "/"))
            (setq dir nil)
          (setq dir
                (file-name-directory
                 (directory-file-name dir))))))))

(defun chief/rust-project-root ()
  "Return the current Rust project root, if any."
  (chief/project-preferred-root
   '("Cargo.toml")
   (chief/polyglot-project-root)))

(defun chief/swift-project-root ()
  "Return the current Swift project root."
  (chief/project-preferred-root
   '("Package.swift")
   (chief/polyglot-project-root)))

(defun chief/rust-command (&rest args)
  "Return a cargo command list built from ARGS."
  (unless (executable-find "cargo")
    (user-error "cargo is not available on PATH"))
  (cons "cargo" args))

(defun chief/rust-build-project ()
  "Run `cargo build' for the current Rust project."
  (interactive)
  (chief/polyglot-compile (chief/rust-command "build") "*cargo build*" (chief/rust-project-root)))

(defun chief/rust-check-project ()
  "Run `cargo check' for the current Rust project."
  (interactive)
  (chief/polyglot-compile (chief/rust-command "check") "*cargo check*" (chief/rust-project-root)))

(defun chief/rust-test-project ()
  "Run `cargo test' for the current Rust project."
  (interactive)
  (chief/polyglot-compile (chief/rust-command "test") "*cargo test*" (chief/rust-project-root)))

(defun chief/rust-run-project ()
  "Run `cargo run' for the current Rust project."
  (interactive)
  (chief/polyglot-compile (chief/rust-command "run") "*cargo run*" (chief/rust-project-root)))

(defun chief/rust-format-project ()
  "Run `cargo fmt' for the current Rust project."
  (interactive)
  (chief/polyglot-compile (chief/rust-command "fmt") "*cargo fmt*" (chief/rust-project-root)))

(defun chief/ocaml-command (&rest args)
  "Return a Dune command list built from ARGS."
  (append (apply #'chief/polyglot-command "dune" args)
          (when chief/ocaml-dune-profile
            (list "--profile" chief/ocaml-dune-profile))))

(defun chief/ocaml-compile (command name &optional directory)
  "Run OCaml COMMAND in DIRECTORY using compilation buffer NAME."
  (chief/polyglot-compile command name (or directory (chief/ocaml-project-root))))

(defun chief/ocaml-build-project ()
  "Run `dune build' for the current OCaml project."
  (interactive)
  (chief/ocaml-compile (chief/ocaml-command "build") "*dune build*"))

(defun chief/ocaml-build-check ()
  "Run Dune's @check alias for the current OCaml project."
  (interactive)
  (chief/ocaml-compile (chief/ocaml-command "build" "@check") "*dune check*"))

(defun chief/ocaml-build-all ()
  "Run Dune's @all alias for the current OCaml project."
  (interactive)
  (chief/ocaml-compile (chief/ocaml-command "build" "@all") "*dune all*"))

(defun chief/ocaml-test-project ()
  "Run `dune runtest' for the current OCaml project."
  (interactive)
  (chief/ocaml-compile (chief/ocaml-command "runtest") "*dune runtest*"))

(defun chief/ocaml-test-current-dir ()
  "Run Dune tests for the current file's directory."
  (interactive)
  (let* ((file (chief/polyglot-current-file "OCaml"))
         (relative-dir (file-relative-name (file-name-directory file)
                                           (chief/ocaml-project-root))))
    (chief/ocaml-compile (chief/ocaml-command "runtest" relative-dir)
                         "*dune runtest dir*")))

(defun chief/ocaml-format-project ()
  "Run `dune fmt' for the current OCaml project."
  (interactive)
  (chief/ocaml-compile (chief/ocaml-command "fmt") "*dune fmt*"))

(defun chief/ocaml-format-buffer ()
  "Format the current OCaml buffer with ocamlformat when available."
  (interactive)
  (chief/polyglot-save-current-buffer)
  (let ((file (chief/polyglot-current-file "OCaml")))
    (if (executable-find "ocamlformat")
        (let ((stderr-file (make-temp-file "chief-ocamlformat-")))
          (unwind-protect
              (let ((status (call-process "ocamlformat" nil (list nil stderr-file) nil
                                          "--inplace" file)))
                (if (zerop status)
                    (revert-buffer t t t)
                  (error "%s"
                         (with-temp-buffer
                           (insert-file-contents stderr-file)
                           (string-trim (buffer-string))))))
            (when (file-exists-p stderr-file)
              (delete-file stderr-file))))
      (chief/ocaml-format-project))))

(defun chief/ocaml-clean-project ()
  "Run `dune clean' for the current OCaml project."
  (interactive)
  (chief/ocaml-compile (chief/ocaml-command "clean") "*dune clean*"))

(defun chief/ocaml-promote-project ()
  "Run `dune promote' for accepted expect-test output."
  (interactive)
  (chief/ocaml-compile (chief/ocaml-command "promote") "*dune promote*"))

(defun chief/ocaml-dune-files ()
  "Return dune files in the current OCaml project."
  (let ((root (chief/ocaml-project-root)))
    (when (file-directory-p root)
      (directory-files-recursively root "\\`dune\\'"))))

(defun chief/ocaml-dune-stanzas ()
  "Return executable/test stanza names from dune files."
  (let (items)
    (dolist (file (chief/ocaml-dune-files))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "(\\(executable\\|test\\)[[:space:]\n]+[^)]*?(name[[:space:]\n]+\\([^[:space:]\n)]+\\)" nil t)
          (push (list :kind (match-string 1)
                      :name (match-string 2)
                      :directory (file-name-directory file))
                items))
        (goto-char (point-min))
        (while (re-search-forward "(executables[[:space:]\n]+[^)]*?(names[[:space:]\n]+\\([^)]*\\))" nil t)
          (dolist (name (split-string (match-string 1) "[[:space:]\n]+" t))
            (push (list :kind "executable"
                        :name name
                        :directory (file-name-directory file))
                  items)))))
    (delete-dups (nreverse items))))

(defun chief/ocaml-read-dune-stanza (&optional kind prompt)
  "Read a Dune stanza, optionally restricted to KIND."
  (let* ((items (seq-filter (lambda (item)
                              (or (null kind)
                                  (string= (plist-get item :kind) kind)))
                            (chief/ocaml-dune-stanzas)))
         (choices (mapcar (lambda (item)
                            (cons (format "%s <%s> %s"
                                          (plist-get item :name)
                                          (plist-get item :kind)
                                          (file-relative-name (plist-get item :directory)
                                                              (chief/ocaml-project-root)))
                                  item))
                          items)))
    (unless choices
      (user-error "No Dune %s stanzas found" (or kind "executable/test")))
    (cdr (assoc (completing-read (or prompt "Dune target: ") choices nil t)
                choices))))

(defun chief/ocaml-exec-stanza (item)
  "Run Dune executable/test stanza ITEM."
  (let* ((name (plist-get item :name))
         (directory (plist-get item :directory))
         (relative-dir (string-remove-suffix
                        "/"
                        (file-relative-name directory (chief/ocaml-project-root))))
         (target (if (string= relative-dir ".")
                     (format "./%s.exe" name)
                   (format "./%s/%s.exe" relative-dir name))))
    (chief/ocaml-compile (chief/ocaml-command "exec" target)
                         (format "*dune exec %s*" name))))

(defun chief/ocaml-run-executable ()
  "Run a selected Dune executable stanza."
  (interactive)
  (chief/ocaml-exec-stanza
   (chief/ocaml-read-dune-stanza "executable" "Dune executable: ")))

(defun chief/ocaml-run-test-executable ()
  "Run a selected Dune test executable stanza."
  (interactive)
  (chief/ocaml-exec-stanza
   (chief/ocaml-read-dune-stanza "test" "Dune test executable: ")))

(defun chief/ocaml-lsp-type-enclosing ()
  "Show OCaml type enclosing information."
  (interactive)
  (if (fboundp 'lsp-ocaml-type-enclosing)
      (call-interactively #'lsp-ocaml-type-enclosing)
    (user-error "lsp-ocaml-type-enclosing is not available")))

(defun chief/ocaml-lsp-alternate-file ()
  "Switch between OCaml implementation and interface files."
  (interactive)
  (if (fboundp 'lsp-ocaml-find-alternate-file)
      (call-interactively #'lsp-ocaml-find-alternate-file)
    (user-error "lsp-ocaml-find-alternate-file is not available")))

(defun chief/ocaml-lsp-infer-interface ()
  "Infer an OCaml interface through ocamllsp."
  (interactive)
  (if (fboundp 'lsp-ocaml-infer-interface)
      (call-interactively #'lsp-ocaml-infer-interface)
    (user-error "lsp-ocaml-infer-interface is not available")))

(defun chief/swift-package-project-p ()
  "Return non-nil when the current project is a Swift package."
  (file-exists-p (expand-file-name "Package.swift" (chief/polyglot-project-root))))

(defun chief/swift-build-project ()
  "Run `swift build' for the current Swift project."
  (interactive)
  (unless (executable-find "swift")
    (user-error "swift is not available on PATH"))
  (chief/polyglot-compile '("swift" "build") "*swift build*" (chief/swift-project-root)))

(defun chief/swift-test-project ()
  "Run `swift test' for the current Swift project."
  (interactive)
  (unless (executable-find "swift")
    (user-error "swift is not available on PATH"))
  (chief/polyglot-compile '("swift" "test") "*swift test*" (chief/swift-project-root)))

(defun chief/swift-run-project ()
  "Run `swift run' for the current Swift project."
  (interactive)
  (unless (executable-find "swift")
    (user-error "swift is not available on PATH"))
  (chief/polyglot-compile '("swift" "run") "*swift run*" (chief/swift-project-root)))

(defun chief/lua-mode-setup ()
  "Configure REPL integration for Lua buffers."
  (chief/repl-configure
   :start #'lua-start-process
   :restart #'lua-restart-with-whole-file
   :send-line #'lua-send-current-line
   :send-region #'lua-send-region
   :send-buffer #'lua-send-buffer
   :send-defun #'lua-send-defun
   :load-file #'lua-send-buffer))

(defun chief/swift-mode-setup ()
  "Configure REPL integration for Swift buffers."
  (setq-local compile-command
              (if (chief/swift-package-project-p)
                  "swift test"
                "swift"))
  (setq-local chief/lsp-root-function #'chief/swift-project-root)
  (chief/repl-configure
   :start #'swift-mode:run-repl
   :restart #'swift-mode:run-repl
   :send-region #'swift-mode:send-region
   :send-buffer #'swift-mode:send-buffer
   :load-file #'swift-mode:send-buffer))

(defun chief/tuareg-send-line ()
  "Send the current line to the OCaml REPL."
  (interactive)
  (tuareg-eval-region (line-beginning-position) (line-end-position)))

(defun chief/tuareg-restart-repl ()
  "Restart the OCaml REPL."
  (interactive)
  (tuareg-kill-ocaml)
  (tuareg-run-ocaml))

(defun chief/tuareg-mode-setup ()
  "Configure REPL, LSP, and completion for OCaml-family buffers."
  (setq-local compile-command "dune build")
  (setq-local chief/lsp-root-function #'chief/ocaml-project-root)
  (chief/polyglot-completion-setup t)
  (when (require 'lsp-ocaml nil t)
    (setq-local lsp-enabled-clients '(ocaml-lsp-server))
    (setq-local lsp-disabled-clients '(ocaml-ls))
    (setq-local lsp-ocaml-markupkind 'markdown)
    (setq-local lsp-ocaml-enclosing-type-verbosity 1)
    (when-let* ((command (and (fboundp 'chief/lsp-ocamllsp-command)
                              (chief/lsp-ocamllsp-command))))
      (setq-local lsp-ocaml-lsp-server-command command)))
  (chief/repl-configure
   :start #'tuareg-run-ocaml
   :restart #'chief/tuareg-restart-repl
   :send-line #'chief/tuareg-send-line
   :send-region #'tuareg-eval-region
   :send-buffer #'tuareg-eval-buffer
   :send-defun #'tuareg-eval-phrase
   :load-file #'tuareg-eval-buffer))

(defun chief/ess-r-mode-setup ()
  "Configure REPL integration for ESS R buffers."
  (chief/repl-configure
   :start #'R
   :restart #'R
   :send-line #'ess-eval-line
   :send-region #'ess-eval-region
   :send-buffer #'ess-eval-buffer
   :load-file #'ess-load-file))

(defun chief/scheme-mode-setup ()
  "Configure REPL integration for Scheme buffers."
  (chief/repl-configure
   :start #'geiser
   :restart #'geiser-restart-repl
   :send-line #'geiser-eval-last-sexp
   :send-region #'geiser-eval-region
   :send-buffer #'geiser-eval-buffer
   :send-defun #'geiser-eval-definition))

(defun chief/racket-mode-setup ()
  "Configure REPL integration for Racket buffers."
  (chief/repl-configure
   :start #'racket-repl
   :restart #'racket-run
   :send-region #'racket-send-region
   :send-defun #'racket-send-definition
   :load-file #'racket-run))

(defun chief/ruby-repl-buffer-name ()
  "Return the current project's Ruby REPL buffer name."
  (format "*ruby[%s]*"
          (file-name-nondirectory
           (directory-file-name (chief/polyglot-project-root)))))

(defun chief/ruby-ensure-repl-buffer ()
  "Return a live Ruby REPL buffer for the current project."
  (let* ((buffer-name (chief/ruby-repl-buffer-name))
         (buffer (get-buffer-create buffer-name))
         (project-root (chief/polyglot-project-root))
         (program (or (executable-find "irb")
                      (executable-find "ruby")
                      (user-error "No Ruby REPL executable is available on PATH"))))
    (unless (comint-check-proc buffer)
      (let ((default-directory project-root))
        (make-comint-in-buffer "chief-ruby" buffer program nil))
      (with-current-buffer buffer
        (setq-local default-directory project-root)
        (setq-local comint-process-echoes nil)))
    buffer))

(defun chief/ruby-start-repl ()
  "Start or switch to the current project's Ruby REPL."
  (interactive)
  (pop-to-buffer (chief/ruby-ensure-repl-buffer)))

(defun chief/ruby-restart-repl ()
  "Restart the current project's Ruby REPL."
  (interactive)
  (when-let* ((buffer (get-buffer (chief/ruby-repl-buffer-name))))
    (when-let* ((process (get-buffer-process buffer)))
      (delete-process process))
    (kill-buffer buffer))
  (chief/ruby-start-repl))

(defun chief/ruby-send-string (string)
  "Send STRING to the current project's Ruby REPL."
  (let* ((buffer (chief/ruby-ensure-repl-buffer))
         (process (get-buffer-process buffer)))
    (comint-send-string process string)
    (unless (string-suffix-p "\n" string)
      (comint-send-string process "\n"))
    (display-buffer buffer)))

(defun chief/ruby-send-line ()
  "Send the current line to the current project's Ruby REPL."
  (interactive)
  (chief/ruby-send-string
   (buffer-substring-no-properties
    (line-beginning-position)
    (line-end-position))))

(defun chief/ruby-send-region (start end)
  "Send the region from START to END to the current project's Ruby REPL."
  (interactive "r")
  (chief/ruby-send-string (buffer-substring-no-properties start end)))

(defun chief/ruby-send-buffer ()
  "Send the current buffer to the current project's Ruby REPL."
  (interactive)
  (chief/ruby-send-region (point-min) (point-max)))

(defun chief/ruby-mode-setup ()
  "Configure REPL integration for Ruby buffers."
  (chief/repl-configure
   :start #'chief/ruby-start-repl
   :restart #'chief/ruby-restart-repl
   :send-line #'chief/ruby-send-line
   :send-region #'chief/ruby-send-region
   :send-buffer #'chief/ruby-send-buffer
   :load-file #'chief/ruby-send-buffer))

(defun chief/rust-mode-setup ()
  "Configure build integration for Rust buffers."
  (setq-local compile-command "cargo test")
  (setq-local chief/lsp-root-function #'chief/rust-project-root))

(autoload 'markdown-mode "markdown-mode" "Major mode for Markdown." t)
(autoload 'gfm-mode "markdown-mode" "Major mode for GitHub Flavored Markdown." t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(chief/safe-use-package markdown-mode
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-hide-markup nil))

(chief/safe-use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))

(chief/safe-use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode)
  :hook (lua-mode . chief/lua-mode-setup))

(chief/safe-use-package fennel-mode
  :mode ("\\.fnl\\'" . fennel-mode))

(chief/safe-use-package janet-mode
  :mode ("\\.janet\\'" . janet-mode))

(chief/safe-use-package nix-mode
  :mode ("\\.nix\\'" . nix-mode))

(chief/safe-use-package cue-mode
  :mode ("\\.cue\\'" . cue-mode))

(chief/safe-use-package geiser
  :defer t
  :hook (scheme-mode . chief/scheme-mode-setup))

(chief/safe-use-package racket-mode
  :mode ("\\.rkt\\'" . racket-mode)
  :mode ("\\.rktd\\'" . racket-mode)
  :hook (racket-mode . chief/racket-mode-setup)
  :config
  (add-hook 'racket-mode-hook #'racket-xp-mode))

(chief/safe-use-package geiser-racket
  :after geiser
  :defer t)

(chief/safe-use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :mode ("\\.lhs\\'" . haskell-mode))

(chief/safe-use-package hy-mode
  :mode ("\\.hy\\'" . hy-mode))

(chief/safe-use-package julia-mode
  :mode ("\\.jl\\'" . julia-mode))

(chief/safe-use-package php-mode
  :mode ("\\.php\\'" . php-mode))

(chief/safe-use-package tuareg
  :mode ("\\.ml\\'" . tuareg-mode)
  :mode ("\\.mli\\'" . tuareg-mode)
  :mode ("\\.mly\\'" . tuareg-menhir-mode)
  :mode ("\\.mll\\'" . tuareg-mode)
  :hook ((tuareg-mode . chief/tuareg-mode-setup)
         (reason-mode . chief/tuareg-mode-setup)))

(chief/safe-use-package reason-mode
  :mode ("\\.re\\'" . reason-mode)
  :mode ("\\.rei\\'" . reason-mode))

(chief/safe-use-package swift-mode
  :mode ("\\.swift\\'" . swift-mode)
  :hook (swift-mode . chief/swift-mode-setup))

(chief/safe-use-package protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode))

(chief/safe-use-package graphviz-dot-mode
  :mode ("\\.dot\\'" . graphviz-dot-mode)
  :mode ("\\.gv\\'" . graphviz-dot-mode))

(chief/safe-use-package plantuml-mode
  :mode ("\\.puml\\'" . plantuml-mode)
  :mode ("\\.plantuml\\'" . plantuml-mode))

(chief/safe-use-package ess
  :mode ("\\.R\\'" . ess-r-mode)
  :mode ("\\.r\\'" . ess-r-mode)
  :hook (ess-r-mode . chief/ess-r-mode-setup))

(add-to-list 'auto-mode-alist '("\\.scm\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.ss\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.sld\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.mojo\\'" . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\.🔥\\'" . python-ts-mode))

(add-hook 'ruby-mode-hook #'chief/ruby-mode-setup)
(when (fboundp 'rust-ts-mode)
  (add-hook 'rust-ts-mode-hook #'chief/rust-mode-setup))

(with-eval-after-load 'lua-mode
  (chief/repl-setup-standard-local-leader 'lua-mode-map))

(with-eval-after-load 'swift-mode
  (chief/repl-setup-standard-local-leader 'swift-mode-map)
  (chief/local-leader-def
    :keymaps 'swift-mode-map
    "c" '(:ignore t :which-key "compile")
    "cb" #'chief/swift-build-project
    "ct" #'chief/swift-test-project
    "cr" #'chief/swift-run-project))

(with-eval-after-load 'tuareg
  (chief/repl-setup-standard-local-leader 'tuareg-mode-map)
  (chief/local-leader-def
    :keymaps 'tuareg-mode-map
    "c" '(:ignore t :which-key "ocaml")
    "cb" #'chief/ocaml-build-project
    "cc" #'chief/ocaml-build-check
    "ca" #'chief/ocaml-build-all
    "ct" #'chief/ocaml-test-project
    "cT" #'chief/ocaml-test-current-dir
    "cf" #'chief/ocaml-format-buffer
    "cF" #'chief/ocaml-format-project
    "cl" #'chief/ocaml-clean-project
    "cp" #'chief/ocaml-promote-project
    "cr" #'chief/ocaml-run-executable
    "cR" #'chief/ocaml-run-test-executable
    "r" '(:ignore t :which-key "run")
    "rr" #'chief/ocaml-run-executable
    "rt" #'chief/ocaml-run-test-executable
    "t" '(:ignore t :which-key "test")
    "tt" #'chief/ocaml-test-project
    "td" #'chief/ocaml-test-current-dir
    "te" #'chief/ocaml-run-test-executable
    "l" '(:ignore t :which-key "lsp")
    "lt" #'chief/ocaml-lsp-type-enclosing
    "la" #'chief/ocaml-lsp-alternate-file
    "li" #'chief/ocaml-lsp-infer-interface
    "lc" #'lsp-execute-code-action))

(with-eval-after-load 'reason-mode
  (chief/repl-setup-standard-local-leader 'reason-mode-map)
  (chief/local-leader-def
    :keymaps 'reason-mode-map
    "c" '(:ignore t :which-key "ocaml")
    "cb" #'chief/ocaml-build-project
    "cc" #'chief/ocaml-build-check
    "ca" #'chief/ocaml-build-all
    "ct" #'chief/ocaml-test-project
    "cT" #'chief/ocaml-test-current-dir
    "cf" #'chief/ocaml-format-buffer
    "cF" #'chief/ocaml-format-project
    "cl" #'chief/ocaml-clean-project
    "cp" #'chief/ocaml-promote-project
    "cr" #'chief/ocaml-run-executable
    "cR" #'chief/ocaml-run-test-executable
    "r" '(:ignore t :which-key "run")
    "rr" #'chief/ocaml-run-executable
    "rt" #'chief/ocaml-run-test-executable
    "t" '(:ignore t :which-key "test")
    "tt" #'chief/ocaml-test-project
    "td" #'chief/ocaml-test-current-dir
    "te" #'chief/ocaml-run-test-executable
    "l" '(:ignore t :which-key "lsp")
    "lt" #'chief/ocaml-lsp-type-enclosing
    "la" #'chief/ocaml-lsp-alternate-file
    "li" #'chief/ocaml-lsp-infer-interface
    "lc" #'lsp-execute-code-action))

(with-eval-after-load 'scheme
  (chief/repl-setup-standard-local-leader 'scheme-mode-map))

(with-eval-after-load 'racket-mode
  (chief/repl-setup-standard-local-leader 'racket-mode-map))

(with-eval-after-load 'ess-r-mode
  (chief/repl-setup-standard-local-leader 'ess-r-mode-map))

(with-eval-after-load 'ruby-mode
  (chief/repl-setup-standard-local-leader 'ruby-mode-map))

(with-eval-after-load 'rust-ts-mode
  (chief/local-leader-def
    :keymaps 'rust-ts-mode-map
    "c" '(:ignore t :which-key "compile")
    "cb" #'chief/rust-build-project
    "cc" #'chief/rust-check-project
    "ct" #'chief/rust-test-project
    "cr" #'chief/rust-run-project
    "cf" #'chief/rust-format-project))

(chief/leader-def
  "m" '(:ignore t :which-key "major")
  "mm" #'describe-mode)

(provide 'lang-polyglot)
;;; lang-polyglot.el ends here
