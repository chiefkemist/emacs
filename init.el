;;; init.el --- Minimal modular Emacs config -*- lexical-binding: t; -*-

(defconst chief/lisp-directory
  (expand-file-name "lisp/" user-emacs-directory))

(defconst chief/etc-directory
  (expand-file-name "etc/" user-emacs-directory))

(defconst chief/var-directory
  (expand-file-name "var/" user-emacs-directory))

(dolist (directory (list chief/lisp-directory chief/etc-directory chief/var-directory))
  (make-directory directory t))

(add-to-list 'load-path chief/lisp-directory)

(defvar warning-suppress-types nil)
(defvar warning-suppress-log-types nil)
(add-to-list 'warning-suppress-types '(files missing-lexbind-cookie))
(add-to-list 'warning-suppress-log-types '(files missing-lexbind-cookie))

(put 'vc-follow-symlinks 'safe-local-variable
     (lambda (value)
       (memq value '(t nil ask))))
(put 'cider-clojure-cli-aliases 'safe-local-variable #'stringp)
(put 'cider-custom-cljs-repl-init-form 'safe-local-variable #'stringp)
(put 'cider-jack-in-nrepl-middlewares 'safe-local-variable
     (lambda (value)
       (and (listp value)
            (catch 'invalid
              (dolist (item value t)
                (unless (stringp item)
                  (throw 'invalid nil)))))))

(setq custom-file (expand-file-name "custom.el" chief/etc-directory))

(defvar chief/reloading-config nil
  "Non-nil while `chief/reload-config' is reloading config modules.")

(setq chief/config-modules
      '(core-bootstrap
        core-ui
        core-terminal
        core-fonts
        core-evil
        core-tools
        core-repl
        core-format
        core-structural
        core-completion
        core-projects
        core-themes
        core-org
        core-treesit
        core-lsp
        core-dap
        lang-dart
        lang-cljd
        lang-clojure
        lang-common-lisp
        lang-elixir
        lang-erlang
        lang-go
        lang-jsts
        lang-java
        lang-kotlin
        lang-nushell
        lang-python
        lang-polyglot
        lang-rust
        lang-zig
        core-org-babel))

(defun chief/module-file (feature)
  "Return the absolute source file path for FEATURE."
  (expand-file-name (format "%s.el" (symbol-name feature)) chief/lisp-directory))

(defun chief/load-config-modules ()
  "Load the configured Emacs modules in startup order.
When `chief/reloading-config' is non-nil, reload modules from disk."
  (dolist (feature chief/config-modules)
    (if chief/reloading-config
        (load-file (chief/module-file feature))
      (require feature))))

(defun chief/refresh-open-file-buffers ()
  "Reinitialize existing file-visiting buffers after a config reload."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and buffer-file-name
                 (file-exists-p buffer-file-name)
                 (not (minibufferp buffer)))
        (let ((inhibit-message t)
              (delayed-mode-hooks nil))
          (normal-mode t))))))

(defun chief/reload-config ()
  "Reload this Emacs configuration without restarting Emacs."
  (interactive)
  (let ((chief/reloading-config t)
        (gc-cons-threshold most-positive-fixnum)
        (load-prefer-newer t)
        (init-file (or user-init-file
                       (expand-file-name "init.el" user-emacs-directory)))
        (file-name-handler-alist
         (if (boundp 'chief/file-name-handler-alist)
             chief/file-name-handler-alist
           file-name-handler-alist)))
    (load-file init-file)
    (chief/refresh-open-file-buffers)
    (when (fboundp 'chief/apply-fonts)
      (chief/apply-fonts))
    (message "Reloaded Emacs config from %s" user-emacs-directory)))

(chief/load-config-modules)

(when (file-exists-p custom-file)
  (load custom-file nil t))

;;; init.el ends here
