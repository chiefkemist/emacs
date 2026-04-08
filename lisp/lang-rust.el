;;; lang-rust.el --- Rust, Cargo, and rust-analyzer support -*- lexical-binding: t; -*-

(require 'compile)
(require 'core-projects)
(require 'subr-x)

(defun chief/rust-crate-root ()
  "Return the nearest Rust crate root for the current buffer."
  (chief/project-preferred-root
   '("Cargo.toml")
   (chief/project-current-root)
   default-directory))

(defun chief/rust-manifest-path (&optional root)
  "Return the Cargo manifest path for ROOT, if it exists."
  (when-let* ((root (chief/project-normalize-root (or root (chief/rust-crate-root)))))
    (let ((manifest (expand-file-name "Cargo.toml" root)))
      (when (file-exists-p manifest)
        manifest))))

(defun chief/rust-workspace-manifest-p (manifest)
  "Return non-nil when MANIFEST defines a Cargo workspace."
  (when (and manifest (file-exists-p manifest))
    (with-temp-buffer
      (insert-file-contents manifest nil 0 4096)
      (re-search-forward "^[[:space:]]*\\[workspace\\][[:space:]]*$" nil t))))

(defun chief/rust-workspace-root ()
  "Return the outermost Cargo workspace root for the current buffer, if any."
  (let ((dir (chief/project-normalize-root
              (or (chief/rust-crate-root)
                  (chief/project-default-start-directory))))
        workspace-root)
    (while dir
      (let ((manifest (expand-file-name "Cargo.toml" dir)))
        (when (chief/rust-workspace-manifest-p manifest)
          (setq workspace-root dir)))
      (if (equal dir "/")
          (setq dir nil)
        (setq dir
              (file-name-directory
               (directory-file-name dir)))))
    workspace-root))

(defun chief/rust-project-root ()
  "Return the preferred Rust project root for the current buffer."
  (chief/project-preferred-root
   (chief/rust-workspace-root)
   (chief/rust-crate-root)
   (chief/project-current-root)
   default-directory))

(defun chief/rust-workspace-directory ()
  "Return the directory used for workspace-level Cargo commands."
  (or (chief/rust-workspace-root)
      (chief/rust-project-root)))

(defun chief/rust-default-directory ()
  "Return the directory used for current-package Cargo commands."
  (or (chief/rust-crate-root)
      (chief/rust-project-root)
      default-directory))

(defun chief/rust-command (&rest args)
  "Return a Cargo command list built from ARGS."
  (unless (executable-find "cargo")
    (user-error "cargo is not available on PATH"))
  (cons "cargo" args))

(defun chief/rust-compile (command name &optional directory)
  "Run Rust COMMAND in DIRECTORY using compilation buffer NAME."
  (let ((default-directory (or directory (chief/rust-project-root)))
        (compilation-read-command nil))
    (compilation-start
     (mapconcat #'shell-quote-argument command " ")
     'compilation-mode
     (lambda (_) name))))

(defun chief/rust-build-project ()
  "Run `cargo build' for the current Rust crate."
  (interactive)
  (chief/rust-compile (chief/rust-command "build") "*cargo build*" (chief/rust-default-directory)))

(defun chief/rust-build-workspace ()
  "Run `cargo build --workspace' for the current Rust workspace."
  (interactive)
  (chief/rust-compile (chief/rust-command "build" "--workspace")
                      "*cargo build workspace*"
                      (chief/rust-workspace-directory)))

(defun chief/rust-check-project ()
  "Run `cargo check' for the current Rust crate."
  (interactive)
  (chief/rust-compile (chief/rust-command "check") "*cargo check*" (chief/rust-default-directory)))

(defun chief/rust-check-workspace ()
  "Run `cargo check --workspace' for the current Rust workspace."
  (interactive)
  (chief/rust-compile (chief/rust-command "check" "--workspace")
                      "*cargo check workspace*"
                      (chief/rust-workspace-directory)))

(defun chief/rust-test-project ()
  "Run `cargo test' for the current Rust crate."
  (interactive)
  (chief/rust-compile (chief/rust-command "test") "*cargo test*" (chief/rust-default-directory)))

(defun chief/rust-test-workspace ()
  "Run `cargo test --workspace' for the current Rust workspace."
  (interactive)
  (chief/rust-compile (chief/rust-command "test" "--workspace")
                      "*cargo test workspace*"
                      (chief/rust-workspace-directory)))

(defun chief/rust-run-project ()
  "Run `cargo run' for the current Rust crate."
  (interactive)
  (chief/rust-compile (chief/rust-command "run") "*cargo run*" (chief/rust-default-directory)))

(defun chief/rust-format-project ()
  "Run `cargo fmt --all' for the current Rust project."
  (interactive)
  (chief/rust-compile (chief/rust-command "fmt" "--all")
                      "*cargo fmt*"
                      (chief/rust-workspace-directory)))

(defun chief/rust-lint-project ()
  "Run `cargo clippy' for the current Rust crate."
  (interactive)
  (chief/rust-compile (chief/rust-command "clippy" "--all-targets" "--all-features")
                      "*cargo clippy*"
                      (chief/rust-default-directory)))

(defun chief/rust-lint-workspace ()
  "Run `cargo clippy --workspace' for the current Rust workspace."
  (interactive)
  (chief/rust-compile (chief/rust-command "clippy" "--workspace" "--all-targets" "--all-features")
                      "*cargo clippy workspace*"
                      (chief/rust-workspace-directory)))

(defun chief/rust-run-runnable ()
  "Run the current rust-analyzer runnable."
  (interactive)
  (if (fboundp 'lsp-rust-analyzer-run)
      (call-interactively #'lsp-rust-analyzer-run)
    (user-error "rust-analyzer runnable support is not available")))

(defun chief/rust-debug-runnable ()
  "Debug the current rust-analyzer runnable with DAP."
  (interactive)
  (unless (fboundp 'lsp-rust-analyzer-debug)
    (user-error "rust-analyzer DAP support is not available"))
  (unless (or (featurep 'dap-lldb)
              (featurep 'dap-gdb))
    (user-error "No Rust-capable DAP adapter is configured"))
  (call-interactively #'lsp-rust-analyzer-debug))

(defun chief/rust-open-cargo-toml ()
  "Open the current Rust manifest."
  (interactive)
  (if-let* ((manifest (chief/rust-manifest-path (chief/rust-crate-root))))
      (find-file manifest)
    (user-error "No Cargo.toml found for this buffer")))

(defun chief/rust-mode-setup ()
  "Configure Rust buffers."
  (setq-local compile-command "cargo test")
  (setq-local chief/lsp-root-function #'chief/rust-project-root)
  (setq-local lsp-enabled-clients '(rust-analyzer))
  (when (require 'lsp-rust nil t)
    (if-let* ((command (and (fboundp 'chief/lsp-rust-analyzer-command)
                            (chief/lsp-rust-analyzer-command))))
        (progn
          (setq-local lsp-rust-server 'rust-analyzer)
          (setq-local lsp-rust-analyzer-server-command command)
          (setq-local lsp-rust-analyzer-cargo-watch-command "check")
          (setq-local lsp-rust-analyzer-cargo-watch-enable t)
          (setq-local lsp-rust-analyzer-cargo-target-dir t)
          (setq-local lsp-rust-analyzer-check-all-targets t)
          (setq-local lsp-rust-analyzer-cargo-run-build-scripts t)
          (setq-local lsp-rust-analyzer-cargo-auto-reload t)
          (setq-local lsp-rust-analyzer-proc-macro-enable t))))
  (when (fboundp 'chief/lsp-managed-mode-setup)
    (chief/lsp-managed-mode-setup)))

(when (fboundp 'rust-ts-mode)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  (add-hook 'rust-ts-mode-hook #'chief/rust-mode-setup))

(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode-hook #'chief/rust-mode-setup)
  (chief/local-leader-def
    :keymaps 'rust-mode-map
    "c" '(:ignore t :which-key "cargo")
    "cb" #'chief/rust-build-project
    "cB" #'chief/rust-build-workspace
    "cc" #'chief/rust-check-project
    "cC" #'chief/rust-check-workspace
    "ct" #'chief/rust-test-project
    "cT" #'chief/rust-test-workspace
    "cr" #'chief/rust-run-project
    "cf" #'chief/rust-format-project
    "cl" #'chief/rust-lint-project
    "cL" #'chief/rust-lint-workspace
    "d" '(:ignore t :which-key "debug")
    "dd" #'chief/rust-debug-runnable
    "dr" #'chief/rust-run-runnable
    "dm" #'chief/rust-open-cargo-toml))

(with-eval-after-load 'rust-ts-mode
  (chief/local-leader-def
    :keymaps 'rust-ts-mode-map
    "c" '(:ignore t :which-key "cargo")
    "cb" #'chief/rust-build-project
    "cB" #'chief/rust-build-workspace
    "cc" #'chief/rust-check-project
    "cC" #'chief/rust-check-workspace
    "ct" #'chief/rust-test-project
    "cT" #'chief/rust-test-workspace
    "cr" #'chief/rust-run-project
    "cf" #'chief/rust-format-project
    "cl" #'chief/rust-lint-project
    "cL" #'chief/rust-lint-workspace
    "d" '(:ignore t :which-key "debug")
    "dd" #'chief/rust-debug-runnable
    "dr" #'chief/rust-run-runnable
    "dm" #'chief/rust-open-cargo-toml)
  (with-eval-after-load 'lsp-rust
    (chief/local-leader-def
      :keymaps 'rust-ts-mode-map
      "da" #'lsp-rust-analyzer-status
      "de" #'lsp-rust-analyzer-expand-macro
      "do" #'lsp-rust-analyzer-open-external-docs)))

(provide 'lang-rust)
;;; lang-rust.el ends here
