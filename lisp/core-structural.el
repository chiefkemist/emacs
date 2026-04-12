;;; core-structural.el --- Structured editing defaults -*- lexical-binding: t; -*-

(chief/safe-use-package paredit
  :hook ((clojure-mode . paredit-mode)
         (clojuredart-mode . paredit-mode)
         (clojurescript-mode . paredit-mode)
         (clojurec-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (scheme-mode . paredit-mode)
         (fennel-mode . paredit-mode)
         (janet-mode . paredit-mode)
         (cider-repl-mode . paredit-mode)))

;; Do not enable Paredit in SLY's MREPL.  Paredit's minor-mode keymap
;; overrides `sly-mrepl-return', making RET insert a newline instead of
;; submitting/evaluating input at the prompt.
(remove-hook 'sly-mrepl-mode-hook #'paredit-mode)

(provide 'core-structural)
;;; core-structural.el ends here
