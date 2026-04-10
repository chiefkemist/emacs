;;; core-evil.el --- Modal editing -*- lexical-binding: t; -*-

(require 'newcomment)

(defun chief/evil-comment-dwim ()
  "Comment or uncomment the active region, or the current line."
  (interactive)
  (comment-normalize-vars)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position)
                                 (line-end-position))))

(use-package general
  :demand t
  :config
  (general-override-mode 1)
  (general-create-definer chief/leader-def
    :keymaps 'override
    :states '(normal visual motion emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-create-definer chief/local-leader-def
    :states '(normal visual motion emacs)
    :prefix ","))

(use-package evil
  :demand t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-esc-delay 0.01)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  (general-define-key
   :states '(normal visual motion)
   "C-h" #'windmove-left
   "C-j" #'windmove-down
   "C-k" #'windmove-up
   "C-l" #'windmove-right)
  (general-define-key
   :states '(visual)
   "gc" #'chief/evil-comment-dwim)
  (general-define-key
   :states '(normal)
   "gcc" #'chief/evil-comment-dwim)
  (general-define-key
   :keymaps 'override
   "<escape>" #'keyboard-escape-quit)
  (chief/leader-def
    "w" '(:ignore t :which-key "windows")
    "wh" #'windmove-left
    "wj" #'windmove-down
    "wk" #'windmove-up
    "wl" #'windmove-right
    "wd" #'delete-window
    "wo" #'delete-other-windows
    "wv" #'split-window-right
    "ws" #'split-window-below))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(provide 'core-evil)
;;; core-evil.el ends here
