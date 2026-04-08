;;; lang-common-lisp.el --- Common Lisp tooling -*- lexical-binding: t; -*-

(defun chief/sly-restart-dwim ()
  "Restart the current SLY session."
  (interactive)
  (when (fboundp 'sly-quit-lisp)
    (ignore-errors (sly-quit-lisp)))
  (call-interactively #'sly))

(defun chief/common-lisp-mode-setup ()
  "Configure REPL integration for Common Lisp buffers."
  (chief/repl-configure
   :start #'sly
   :restart #'chief/sly-restart-dwim
   :send-line #'sly-eval-last-expression
   :send-region #'sly-eval-region
   :send-buffer #'sly-eval-buffer
   :send-defun #'sly-eval-defun
   :load-file #'sly-load-file))

(use-package sly
  :commands (sly sly-connect)
  :init
  (setq inferior-lisp-program (or (executable-find "sbcl") "sbcl"))
  (setq sly-lisp-implementations
        `((sbcl (,(or (executable-find "sbcl") "sbcl")))))
  :hook (lisp-mode . chief/common-lisp-mode-setup)
  :config
  (chief/leader-def
    "x" '(:ignore t :which-key "common-lisp")
    "xx" #'sly
    "xc" #'sly-connect
    "xq" #'sly-quit-lisp)
  (chief/local-leader-def
    :keymaps 'lisp-mode-map
    "e" #'sly-eval-defun
    "c" #'sly-compile-defun
    "d" #'sly-documentation-lookup
    "r" #'sly-mrepl))

(chief/repl-setup-standard-local-leader 'lisp-mode-map)

(provide 'lang-common-lisp)
;;; lang-common-lisp.el ends here
