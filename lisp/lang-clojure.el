;;; lang-clojure.el --- Clojure tooling -*- lexical-binding: t; -*-

(defcustom chief/babashka-command "bb"
  "Command used to start Babashka tooling."
  :type 'string
  :group 'chief)

(defcustom chief/nbb-command "nbb"
  "Command used to start nbb tooling."
  :type 'string
  :group 'chief)

(define-derived-mode chief-clojureclr-mode clojure-mode "ClojureCLR")
(define-derived-mode chief-jank-mode clojure-mode "Jank")
(define-derived-mode chief-joker-mode clojure-mode "Joker")
(define-derived-mode chief-babashka-mode clojure-mode "Babashka")

(defun chief/clojure-project-file-p (filename)
  "Return non-nil when FILENAME exists in the current project."
  (locate-dominating-file default-directory filename))

(defun chief/cider-jack-in-dwim ()
  "Pick an appropriate CIDER jack-in command for the current project."
  (interactive)
  (cond
   ((chief/clojure-project-file-p "nbb.edn")
    (call-interactively #'cider-jack-in-cljs))
   ((chief/clojure-project-file-p "bb.edn")
    (call-interactively #'cider-jack-in-clj))
   (t
    (call-interactively #'cider-jack-in))))

(defun chief/cider-connect-dwim ()
  "Pick an appropriate CIDER connect command for the current project."
  (interactive)
  (if (chief/clojure-project-file-p "nbb.edn")
      (call-interactively #'cider-connect-cljs)
    (call-interactively #'cider-connect-clj)))

(defun chief/start-babashka-nrepl ()
  "Start a Babashka nREPL server in a side buffer."
  (interactive)
  (async-shell-command
   (format "%s --nrepl-server" chief/babashka-command)
   "*babashka-nrepl*"))

(defun chief/start-nbb-nrepl ()
  "Start an nbb nREPL server in a side buffer."
  (interactive)
  (async-shell-command
   (format "%s nrepl-server" chief/nbb-command)
   "*nbb-nrepl*"))

(defun chief/cider-restart-dwim ()
  "Restart the current CIDER session."
  (interactive)
  (when (fboundp 'cider-quit)
    (cider-quit))
  (call-interactively #'chief/cider-jack-in-dwim))

(defun chief/clojure-mode-setup ()
  "Configure REPL integration for Clojure-family buffers."
  (chief/repl-configure
   :start #'chief/cider-jack-in-dwim
   :restart #'chief/cider-restart-dwim
   :send-line #'cider-eval-last-sexp
   :send-region #'cider-eval-region
   :send-buffer #'cider-load-buffer
   :send-defun #'cider-eval-defun-at-point
   :load-file #'cider-load-buffer))

(use-package clojure-mode
  :mode ("\\.clj\\'" . clojure-mode)
  :mode ("\\.cljc\\'" . clojure-mode)
  :mode ("\\.cljs\\'" . clojure-mode)
  :mode ("\\.edn\\'" . clojure-mode)
  :mode ("\\.cljr\\'" . chief-clojureclr-mode)
  :mode ("\\.jank\\'" . chief-jank-mode)
  :mode ("\\.joke\\'" . chief-joker-mode)
  :mode ("\\.bb\\'" . chief-babashka-mode)
  :config
  (setq clojure-mode-extra-font-locking t)
  (setq clojure-align-forms-automatically t)
  (add-hook 'clojure-mode-hook #'chief/clojure-mode-setup))

(use-package cider
  :after clojure-mode
  :commands (cider-jack-in
             cider-jack-in-clj
             cider-jack-in-cljs
             cider-connect-clj
             cider-connect-cljs)
  :custom
  (cider-save-file-on-load t)
  (cider-repl-display-help-banner nil)
  (cider-show-error-buffer 'except-in-repl)
  (cider-auto-select-error-buffer nil)
  (nrepl-log-messages nil)
  :config
  (chief/leader-def
    "c" '(:ignore t :which-key "clojure")
    "cj" #'chief/cider-jack-in-dwim
    "cc" #'chief/cider-connect-dwim
    "cb" #'chief/start-babashka-nrepl
    "cB" #'chief/start-nbb-nrepl
    "cq" #'cider-quit)
  (chief/local-leader-def
    :keymaps 'clojure-mode-map
    "e" #'cider-eval-defun-at-point
    "r" #'cider-ns-refresh
    "t" #'cider-test-run-ns-tests
    "j" #'chief/cider-jack-in-dwim))

(chief/repl-setup-standard-local-leader 'clojure-mode-map)

(provide 'lang-clojure)
;;; lang-clojure.el ends here
