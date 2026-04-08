;;; core-ui.el --- Core UI and editing defaults -*- lexical-binding: t; -*-

(setq inhibit-startup-screen t)
(setq ring-bell-function #'ignore)
(setq visible-bell nil)
(setq use-dialog-box nil)
(setq confirm-kill-emacs #'y-or-n-p)
(setq make-backup-files t)
(setq create-lockfiles nil)
(setq delete-by-moving-to-trash t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default fill-column 100)
(setq-default truncate-lines t)
(setq-default cursor-type 'bar)
(setq-default display-line-numbers-type 'relative)

(defcustom chief/large-buffer-threshold 400000
  "Approximate buffer size above which expensive UI helpers should stay disabled."
  :type 'integer
  :group 'chief)

(setq scroll-margin 10)
(setq scroll-conservatively 101)
(setq split-width-threshold 140)
(setq split-height-threshold nil)
(setq frame-title-format '("%b  [%f]"))
(setq sentence-end-double-space nil)
(setq bidi-inhibit-bpa t)
(setq bidi-paragraph-direction 'left-to-right)
(setq read-process-output-max (* 1024 1024))
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)
(setq inhibit-compacting-font-caches t)
(setq process-error-pause-time 0)
(setq history-delete-duplicates t)
(setq auto-revert-avoid-polling t)
(setq auto-revert-use-notify t)
(setq auto-revert-verbose nil)
(setq jit-lock-defer-time 0)
(setq jit-lock-stealth-time 1.25)
(setq jit-lock-stealth-nice 0.5)
(setq display-line-numbers-grow-only t)
(setq display-line-numbers-width-start t)
(setq display-line-numbers-current-absolute t)

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)

(defun chief/large-buffer-p (&optional threshold)
  "Return non-nil when the current buffer is larger than THRESHOLD."
  (> (buffer-size) (or threshold chief/large-buffer-threshold)))

(defun chief/enable-line-numbers-maybe ()
  "Enable line numbers in code/config buffers, but not in very large files."
  (unless (or (minibufferp)
              (chief/large-buffer-p))
    (display-line-numbers-mode 1)))

(defun chief/minibuffer-boost-gc ()
  "Reduce GC churn while the minibuffer is active."
  (setq gc-cons-threshold most-positive-fixnum))

(defun chief/minibuffer-restore-gc ()
  "Restore a sane GC threshold after leaving the minibuffer."
  (setq gc-cons-threshold (* 64 1024 1024)))

(blink-cursor-mode -1)
(global-so-long-mode 1)
(global-auto-revert-mode 1)
(save-place-mode 1)
(savehist-mode 1)
(recentf-mode 1)
(winner-mode 1)
(show-paren-mode 1)
(electric-pair-mode 1)
(delete-selection-mode 1)
(column-number-mode 1)

(setq recentf-max-saved-items 500)
(setq recentf-auto-cleanup 'never)
(setq recentf-save-file (expand-file-name "recentf.eld" chief/var-directory))
(setq savehist-file (expand-file-name "history" chief/var-directory))
(setq save-place-file (expand-file-name "places" chief/var-directory))
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring
        extended-command-history
        consult--grep-history
        consult--line-history))
(setq-default show-trailing-whitespace nil)

(dolist (hook '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                eat-mode-hook
                vterm-mode-hook
                org-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode -1))))

(add-hook 'prog-mode-hook #'chief/enable-line-numbers-maybe)
(add-hook 'conf-mode-hook #'chief/enable-line-numbers-maybe)
(add-hook 'minibuffer-setup-hook #'chief/minibuffer-boost-gc)
(add-hook 'minibuffer-exit-hook #'chief/minibuffer-restore-gc)

(use-package exec-path-from-shell
  :if (and (memq system-type '(darwin gnu/linux))
           (or (daemonp) (display-graphic-p)))
  :demand t
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (dolist (variable '("PATH"
                      "MANPATH"
                      "GOPATH"
                      "GOENV_ROOT"
                      "PYENV_ROOT"
                      "RBENV_ROOT"
                      "JAVA_HOME"
                      "DOTNET_ROOT"
                      "MODULAR_HOME"
                      "NIX_PATH"
                      "NIX_SSL_CERT_FILE"))
    (add-to-list 'exec-path-from-shell-variables variable))
  (exec-path-from-shell-initialize))

(use-package which-key
  :straight nil
  :defer 1
  :config
  (setq which-key-idle-delay 1.0)
  (setq which-key-max-description-length 40)
  (which-key-mode 1))

(provide 'core-ui)
;;; core-ui.el ends here
