;;; core-structural.el --- Structured editing defaults -*- lexical-binding: t; -*-

(chief/safe-use-package paredit
  :hook ((clojure-mode . paredit-mode)
         (clojurescript-mode . paredit-mode)
         (clojurec-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (scheme-mode . paredit-mode)
         (fennel-mode . paredit-mode)
         (janet-mode . paredit-mode)
         (cider-repl-mode . paredit-mode)
         (sly-mrepl-mode . paredit-mode)))

(provide 'core-structural)
;;; core-structural.el ends here
