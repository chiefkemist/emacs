;;; early-init.el --- Early startup tuning -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message user-login-name)
(setq frame-inhibit-implied-resize t)
(setq native-comp-async-report-warnings-errors 'silent)
(setq native-comp-jit-compilation-deny-list
      '("/general\\.el\\'"))

(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

(defvar chief/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache (expand-file-name "eln-cache/" user-emacs-directory)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

(setq-default cursor-in-non-selected-windows nil)

(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq file-name-handler-alist
         (delete-dups (append file-name-handler-alist chief/file-name-handler-alist)))
   (setq gc-cons-threshold (* 64 1024 1024))
   (setq gc-cons-percentage 0.1)))

;;; early-init.el ends here
