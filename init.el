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

(setq custom-file (expand-file-name "custom.el" chief/etc-directory))

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
        lang-clojure
        lang-common-lisp
        lang-elixir
        lang-erlang
        lang-go
        lang-jsts
        lang-nushell
        lang-python
        lang-polyglot
        lang-rust
        lang-zig
        core-org-babel))

(defun chief/load-config-modules ()
  "Load the configured Emacs modules in startup order.
When `chief/reloading-config' is non-nil, reload modules from disk."
  (dolist (feature chief/config-modules)
    (if (bound-and-true-p chief/reloading-config)
        (load (symbol-name feature) nil 'nomessage)
      (require feature))))

(defun chief/reload-config ()
  "Reload this Emacs configuration without restarting Emacs."
  (interactive)
  (let ((chief/reloading-config t)
        (gc-cons-threshold most-positive-fixnum)
        (init-file (or user-init-file
                       (expand-file-name "init.el" user-emacs-directory)))
        (file-name-handler-alist
         (if (boundp 'chief/file-name-handler-alist)
             chief/file-name-handler-alist
           file-name-handler-alist)))
    (load-file init-file)
    (when (fboundp 'chief/apply-fonts)
      (chief/apply-fonts))
    (message "Reloaded Emacs config from %s" user-emacs-directory)))

(chief/load-config-modules)

(when (file-exists-p custom-file)
  (load custom-file nil t))

;;; init.el ends here
