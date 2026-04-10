;;; lang-polyglot.el --- Extra language coverage -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'compile)
(require 'comint)
(require 'core-projects)
(require 'project)
(require 'seq)
(require 'subr-x)

(defun chief/polyglot-project-root ()
  "Return the current project root or `default-directory'."
  (chief/project-preferred-root
   (chief/project-current-root)
   default-directory))

(defun chief/nim-project-root ()
  "Return the current Nim project root, if any."
  (or (when-let* ((file (chief/project-nearest-matching-file "\\.nimble\\'")))
        (chief/project-normalize-root (file-name-directory file)))
      (chief/project-preferred-root
       '(".git")
       (chief/polyglot-project-root))))

(defun chief/ocaml-project-root ()
  "Return the current OCaml project root."
  (chief/project-preferred-root
   '("dune-workspace")
   '("dune-project")
   (chief/polyglot-project-root)))

(defun chief/dotnet-project-root ()
  "Return the current C# or F# project root."
  (or (chief/project-nearest-regexp-root "\\.sln\\'")
      (chief/project-nearest-regexp-root "\\.[cf]sproj\\'")
      (chief/polyglot-project-root)))

(defun chief/polyglot-compile (command name &optional directory)
  "Run COMMAND in DIRECTORY using compilation buffer NAME."
  (let ((default-directory (or directory (chief/polyglot-project-root)))
        (compilation-read-command nil))
    (compilation-start
     (mapconcat #'shell-quote-argument command " ")
     'compilation-mode
     (lambda (_) name))))

(defun chief/nim-compile (command name &optional directory)
  "Run Nim COMMAND in DIRECTORY using compilation buffer NAME."
  (chief/polyglot-compile command name (or directory (chief/nim-project-root))))

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

(defun chief/polyglot-dotnet-target ()
  "Return the best dotnet build target for the current project."
  (let ((root (chief/polyglot-project-root)))
    (or (chief/polyglot-nearest-matching-file "\\.sln\\'" nil root)
        (chief/polyglot-nearest-matching-file "\\.csproj\\'" nil root)
        (chief/polyglot-nearest-matching-file "\\.fsproj\\'" nil root))))

(defun chief/polyglot-csharp-project-file ()
  "Return the nearest C# project file."
  (chief/polyglot-nearest-matching-file "\\.csproj\\'"))

(defun chief/polyglot-fsharp-project-file ()
  "Return the nearest F# project file."
  (chief/polyglot-nearest-matching-file "\\.fsproj\\'"))

(defun chief/polyglot-dotnet-command (verb &rest args)
  "Return a dotnet command using VERB and ARGS."
  (unless (executable-find "dotnet")
    (user-error "dotnet is not available on PATH"))
  (let ((target (chief/polyglot-dotnet-target)))
    (append (list "dotnet" verb)
            (when target (list target))
            args)))

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

(defun chief/csharp-build-project ()
  "Run `dotnet build' for the current C# project."
  (interactive)
  (chief/polyglot-compile (chief/polyglot-dotnet-command "build") "*dotnet build*"))

(defun chief/csharp-test-project ()
  "Run `dotnet test' for the current C# project."
  (interactive)
  (chief/polyglot-compile (chief/polyglot-dotnet-command "test") "*dotnet test*"))

(defun chief/csharp-run-project ()
  "Run `dotnet run' for the current C# project."
  (interactive)
  (let ((project (or (chief/polyglot-csharp-project-file)
                     (user-error "No .csproj found for this project"))))
    (chief/polyglot-compile (list "dotnet" "run" "--project" project) "*dotnet run*")))

(defun chief/csharp-restore-project ()
  "Run `dotnet restore' for the current C# project."
  (interactive)
  (chief/polyglot-compile (chief/polyglot-dotnet-command "restore") "*dotnet restore*"))

(defun chief/fsharp-build-project ()
  "Run `dotnet build' for the current F# project."
  (interactive)
  (chief/polyglot-compile (chief/polyglot-dotnet-command "build") "*dotnet build*"))

(defun chief/fsharp-test-project ()
  "Run `dotnet test' for the current F# project."
  (interactive)
  (chief/polyglot-compile (chief/polyglot-dotnet-command "test") "*dotnet test*"))

(defun chief/fsharp-run-project ()
  "Run `dotnet run' for the current F# project."
  (interactive)
  (let ((project (or (chief/polyglot-fsharp-project-file)
                     (user-error "No .fsproj found for this project"))))
    (chief/polyglot-compile (list "dotnet" "run" "--project" project) "*dotnet run*")))

(defun chief/fsharp-restore-project ()
  "Run `dotnet restore' for the current F# project."
  (interactive)
  (chief/polyglot-compile (chief/polyglot-dotnet-command "restore") "*dotnet restore*"))

(defun chief/ocaml-build-project ()
  "Run `dune build' for the current OCaml project."
  (interactive)
  (unless (executable-find "dune")
    (user-error "dune is not available on PATH"))
  (chief/polyglot-compile '("dune" "build") "*dune build*"))

(defun chief/ocaml-test-project ()
  "Run `dune runtest' for the current OCaml project."
  (interactive)
  (unless (executable-find "dune")
    (user-error "dune is not available on PATH"))
  (chief/polyglot-compile '("dune" "runtest") "*dune runtest*"))

(defun chief/ocaml-format-project ()
  "Run `dune fmt' for the current OCaml project."
  (interactive)
  (unless (executable-find "dune")
    (user-error "dune is not available on PATH"))
  (chief/polyglot-compile '("dune" "fmt") "*dune fmt*"))

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
  "Configure REPL integration for OCaml-family buffers."
  (setq-local compile-command "dune build")
  (setq-local chief/lsp-root-function #'chief/ocaml-project-root)
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

(defun chief/nim-build-buffer ()
  "Build the current Nim file."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a Nim file"))
  (save-buffer)
  (chief/nim-compile
   (list "nim" "c" (expand-file-name buffer-file-name))
   "*nim build*"))

(defun chief/nim-run-buffer ()
  "Run the current Nim file."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a Nim file"))
  (save-buffer)
  (chief/nim-compile
   (list "nim" "r" (expand-file-name buffer-file-name))
   "*nim run*"))

(defun chief/nim-check-buffer ()
  "Run `nim check' for the current Nim file."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a Nim file"))
  (save-buffer)
  (chief/nim-compile
   (list "nim" "check" (expand-file-name buffer-file-name))
   "*nim check*"))

(defun chief/nim-test-project ()
  "Run Nimble tests for the current project."
  (interactive)
  (unless (executable-find "nimble")
    (user-error "nimble is not available on PATH"))
  (chief/nim-compile '("nimble" "test") "*nimble test*"))

(defun chief/nim-mode-setup ()
  "Configure Nim buffers."
  (setq-local compile-command
              (if buffer-file-name
                  (format "nim check %s"
                          (shell-quote-argument (expand-file-name buffer-file-name)))
                "nimble test"))
  (setq-local chief/lsp-root-function #'chief/nim-project-root))

(defun chief/rust-mode-setup ()
  "Configure build integration for Rust buffers."
  (setq-local compile-command "cargo test")
  (setq-local chief/lsp-root-function #'chief/rust-project-root))

(defun chief/csharp-mode-setup ()
  "Configure build integration for C# buffers."
  (setq-local compile-command "dotnet build")
  (setq-local chief/lsp-root-function #'chief/dotnet-project-root))

(defun chief/fsharp-mode-setup ()
  "Configure build integration for F# buffers."
  (setq-local compile-command "dotnet build")
  (setq-local chief/lsp-root-function #'chief/dotnet-project-root))

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

(chief/safe-use-package web-mode
  :mode ("\\.razor\\'" . web-mode)
  :mode ("\\.cshtml\\'" . web-mode)
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t))

(chief/safe-use-package nim-mode
  :mode ("\\.nim\\'" . nim-mode)
  :mode ("\\.nims\\'" . nim-mode)
  :mode ("\\.nimble\\'" . nim-mode)
  :hook (nim-mode . chief/nim-mode-setup))

(chief/safe-use-package nix-mode
  :mode ("\\.nix\\'" . nix-mode))

(chief/safe-use-package cue-mode
  :mode ("\\.cue\\'" . cue-mode))

(chief/safe-use-package fsharp-mode
  :mode ("\\.fs[iylx]?\\'" . fsharp-mode)
  :hook (fsharp-mode . chief/fsharp-mode-setup))

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
(when (fboundp 'csharp-ts-mode)
  (add-hook 'csharp-ts-mode-hook #'chief/csharp-mode-setup))

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
    "c" '(:ignore t :which-key "compile")
    "cb" #'chief/ocaml-build-project
    "ct" #'chief/ocaml-test-project
    "cf" #'chief/ocaml-format-project))

(with-eval-after-load 'reason-mode
  (chief/repl-setup-standard-local-leader 'reason-mode-map)
  (chief/local-leader-def
    :keymaps 'reason-mode-map
    "c" '(:ignore t :which-key "compile")
    "cb" #'chief/ocaml-build-project
    "ct" #'chief/ocaml-test-project
    "cf" #'chief/ocaml-format-project))

(with-eval-after-load 'scheme
  (chief/repl-setup-standard-local-leader 'scheme-mode-map))

(with-eval-after-load 'racket-mode
  (chief/repl-setup-standard-local-leader 'racket-mode-map))

(with-eval-after-load 'ess-r-mode
  (chief/repl-setup-standard-local-leader 'ess-r-mode-map))

(with-eval-after-load 'ruby-mode
  (chief/repl-setup-standard-local-leader 'ruby-mode-map))

(with-eval-after-load 'nim-mode
  (chief/local-leader-def
    :keymaps 'nim-mode-map
    "c" '(:ignore t :which-key "compile")
    "cb" #'chief/nim-build-buffer
    "cr" #'chief/nim-run-buffer
    "cc" #'chief/nim-check-buffer
    "ct" #'chief/nim-test-project))

(with-eval-after-load 'rust-ts-mode
  (chief/local-leader-def
    :keymaps 'rust-ts-mode-map
    "c" '(:ignore t :which-key "compile")
    "cb" #'chief/rust-build-project
    "cc" #'chief/rust-check-project
    "ct" #'chief/rust-test-project
    "cr" #'chief/rust-run-project
    "cf" #'chief/rust-format-project))

(with-eval-after-load 'csharp-mode
  (chief/local-leader-def
    :keymaps 'csharp-ts-mode-map
    "c" '(:ignore t :which-key "compile")
    "cb" #'chief/csharp-build-project
    "ct" #'chief/csharp-test-project
    "cr" #'chief/csharp-run-project
    "cR" #'chief/csharp-restore-project))

(with-eval-after-load 'fsharp-mode
  (chief/local-leader-def
    :keymaps 'fsharp-mode-map
    "c" '(:ignore t :which-key "compile")
    "cb" #'chief/fsharp-build-project
    "ct" #'chief/fsharp-test-project
    "cr" #'chief/fsharp-run-project
    "cR" #'chief/fsharp-restore-project))

(chief/leader-def
  "m" '(:ignore t :which-key "major")
  "mm" #'describe-mode)

(provide 'lang-polyglot)
;;; lang-polyglot.el ends here
