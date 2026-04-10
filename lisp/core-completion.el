;;; core-completion.el --- Minibuffer and in-buffer completion -*- lexical-binding: t; -*-

(require 'project)
(declare-function chief/token-at-point "core-lsp")

(setq tab-always-indent 'complete)
(setq completion-cycle-threshold 3)
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

(defun chief/consult-current-buffer ()
  "Search the current buffer, matching the NeoVim Telescope current-buffer flow."
  (interactive)
  (call-interactively #'consult-line))

(defun chief/consult-project-buffers ()
  "Search across project buffers."
  (interactive)
  (consult-line-multi nil))

(defun chief/consult-project-symbol-at-point ()
  "Search the project for the symbol at point."
  (interactive)
  (consult-ripgrep nil (or (and (fboundp 'chief/token-at-point)
                                (chief/token-at-point))
                           (thing-at-point 'symbol t))))

(defun chief/consult-diagnostics ()
  "Show diagnostics using Consult when available."
  (interactive)
  (cond
   ((fboundp 'consult-lsp-diagnostics)
    (call-interactively #'consult-lsp-diagnostics))
   ((fboundp 'flycheck-list-errors)
    (call-interactively #'flycheck-list-errors))
   (t
    (user-error "No diagnostics command is available"))))

(defun chief/consult-todos ()
  "Search the current project for TODO-style markers."
  (interactive)
  (consult-ripgrep nil "TODO|FIXME|HACK|NOTE|REVIEW|BUG|PERF"))

(defun chief/consult-find-emacs-config ()
  "Search files inside the current Emacs configuration."
  (interactive)
  (let ((default-directory user-emacs-directory))
    (call-interactively #'consult-find)))

(defun chief/consult-find-neovim-config ()
  "Search files inside the reference NeoVim configuration."
  (interactive)
  (let ((default-directory (expand-file-name "~/.config/nvim/")))
    (call-interactively #'consult-find)))

(defun chief/consult-command-picker ()
  "Offer a Telescope-style picker of common search/navigation commands."
  (interactive)
  (let* ((choices
          `(("buffers" . consult-buffer)
            ("find files" . consult-find)
            ("project files" . project-find-file)
            ("recent files" . consult-recent-file)
            ("current buffer" . chief/consult-current-buffer)
            ("project buffers" . chief/consult-project-buffers)
            ("project ripgrep" . consult-ripgrep)
            ("grep word at point" . chief/consult-project-symbol-at-point)
            ("diagnostics" . chief/consult-diagnostics)
            ("imenu" . consult-imenu)
            ("multi imenu" . consult-imenu-multi)
            ("bookmarks" . consult-bookmark)
            ("themes" . consult-theme)
            ,@(when (fboundp 'consult-lsp-symbols)
                '(("lsp symbols" . consult-lsp-symbols)))))
         (selection
          (completing-read "Picker: " (mapcar #'car choices) nil t)))
    (when-let* ((command (cdr (assoc selection choices))))
      (call-interactively command))))

(use-package vertico
  :demand t
  :init
  (vertico-mode 1)
  :config
  (setq vertico-count 12)
  (setq vertico-resize nil)
  (setq vertico-cycle t))

(use-package vertico-directory
  :straight nil
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-repeat
  :straight nil
  :after vertico
  :config
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides
        '((file (styles basic partial-completion)))))

(use-package marginalia
  :after vertico
  :init
  (setq marginalia-annotators
        '(marginalia-annotators-light
          marginalia-annotators-heavy
          nil))
  (marginalia-mode 1))

(use-package consult
  :demand t
  :bind
  (("C-s" . consult-line)
   ("C-x b" . consult-buffer)
   ("M-y" . consult-yank-pop)
   ("C-c h" . consult-history)
   ("C-c p f" . consult-find)
   ("C-c p g" . consult-git-grep))
  :config
  (setq consult-preview-key "M-.")
  (setq register-preview-delay 0.5)
  (setq register-preview-function #'consult-register-format)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (chief/leader-def
    "b" '(:ignore t :which-key "buffers")
    "bb" #'consult-buffer
    "bd" #'consult-bookmark
    "bk" #'kill-current-buffer
    "f" '(:ignore t :which-key "files")
    "ff" #'consult-find
    "fe" #'chief/consult-find-emacs-config
    "fg" #'consult-git-grep
    "fn" #'chief/consult-find-neovim-config
    "fr" #'consult-recent-file
    "fs" #'save-buffer
    "p" '(:ignore t :which-key "project")
    "pb" #'consult-project-buffer
    "pf" #'project-find-file
    "pp" #'project-switch-project
    "pg" #'consult-git-grep
    "ps" #'consult-ripgrep
    "s" '(:ignore t :which-key "search")
    "sh" #'consult-history
    "sm" #'consult-mark
    "so" #'consult-outline
    "ss" #'chief/consult-command-picker
    "sg" #'consult-ripgrep
    "si" #'consult-imenu
    "sI" #'consult-imenu-multi
    "sl" #'consult-line
    "s/" #'chief/consult-project-buffers
    "sL" #'consult-line-multi
    "sr" #'vertico-repeat
    "sd" #'chief/consult-diagnostics
    "st" #'chief/consult-todos
    "sw" #'chief/consult-project-symbol-at-point
    "s." #'consult-recent-file))

(use-package embark
  :after consult
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :straight (:files (:defaults "extensions/*.el"))
  :demand t
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.12)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-preview-current nil)
  (corfu-quit-no-match 'separator)
  :bind
  (:map corfu-map
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("C-y" . corfu-insert)
        ("C-SPC" . corfu-complete)
        ("<tab>" . corfu-next)
        ("TAB" . corfu-next)
        ("S-TAB" . corfu-previous)
        ("<backtab>" . corfu-previous))
  :init
  (global-corfu-mode 1))

(use-package corfu-history
  :straight nil
  :after corfu
  :init
  (corfu-history-mode 1)
  :custom
  (savehist-additional-variables
   (append savehist-additional-variables '(corfu-history))))

(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.15))
  :init
  (corfu-popupinfo-mode 1))

(use-package yasnippet
  :commands (yas-minor-mode yas-insert-snippet yas-next-field-or-maybe-expand yas-prev-field)
  :hook ((prog-mode . yas-minor-mode)
         (conf-mode . yas-minor-mode)
         (text-mode . yas-minor-mode)
         (org-mode . yas-minor-mode))
  :config
  (chief/leader-def
    "is" #'yas-insert-snippet))

(use-package yasnippet-snippets
  :after yasnippet
  :config
  (when (fboundp 'yasnippet-snippets-initialize)
    (yasnippet-snippets-initialize)))

(use-package kind-icon
  :if (display-graphic-p)
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-use-icons nil)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :demand t
  :after corfu
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package consult-lsp
  :after (consult lsp-mode)
  :commands (consult-lsp-symbols consult-lsp-diagnostics)
  :config
  (chief/leader-def
    "ld" #'consult-lsp-diagnostics
    "ls" #'consult-lsp-symbols))

(with-eval-after-load 'evil
  (evil-global-set-key 'insert (kbd "C-@") #'completion-at-point)
  (evil-global-set-key 'insert (kbd "C-l") #'yas-next-field-or-maybe-expand)
  (evil-global-set-key 'insert (kbd "C-h") #'yas-prev-field))

(chief/leader-def
  "SPC" #'execute-extended-command
  "." #'embark-act
  "/" #'chief/consult-current-buffer
  "h" '(:ignore t :which-key "help")
  "hf" #'describe-function
  "hv" #'describe-variable
  "hk" #'describe-key
  "hr" #'chief/reload-config
  "q" '(:ignore t :which-key "quit")
  "qq" #'save-buffers-kill-terminal)

(provide 'core-completion)
;;; core-completion.el ends here
