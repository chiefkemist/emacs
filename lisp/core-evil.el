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
  ;; Undo any old global C-h/j/k/l window bindings on reload.
  ;; Removing the old `general-define-key' form from the config file is not
  ;; enough, because reloading doesn't automatically scrub existing keymap
  ;; entries.
  (when (fboundp 'general-unbind)
    (general-unbind :states '(normal visual motion)
                    "C-h" "C-j" "C-k" "C-l"))
  ;; Keep Evil's default C-w window map intact; do not steal C-h/j/k/l.
  (general-define-key
   :states '(visual)
   "gc" #'chief/evil-comment-dwim)
  (general-define-key
   :states '(normal)
   "gcc" #'chief/evil-comment-dwim)
  ;; Match this config's C-s Consult search UI in Evil command states.
  ;; Evil normally uses C-r for redo, but redo remains available as :redo.
  (general-define-key
   :states '(normal visual motion)
   "C-r" #'chief/consult-line-backward)
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
