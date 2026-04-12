;;; lang-common-lisp.el --- Common Lisp tooling -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defcustom chief/common-lisp-quicklisp-paths '("~/quicklisp" "~/.quicklisp")
  "Directories to search for Quicklisp distribution files."
  :type '(repeat directory)
  :group 'chief)

;; Doom declares this early so external SLY contrib packages can safely append
;; to it before SLY has been loaded.
(defvar sly-contribs '(sly-fancy))

(defun chief/sly-ensure-connected ()
  "Signal a clear error unless a SLY connection is available."
  (require 'sly)
  (unless (and (fboundp 'sly-connected-p)
               (sly-connected-p))
    (user-error "SLY is not connected; start it with M-x sly or , '")))

(defun chief/sly-wait-for-connection (&optional attempts delay)
  "Wait for a SLY connection for ATTEMPTS, sleeping DELAY seconds each time."
  (let ((attempts (or attempts 25))
        (delay (or delay 0.2)))
    (cl-loop repeat attempts
             when (and (fboundp 'sly-connected-p) (sly-connected-p))
             return t
             do (sleep-for delay))))

(defun chief/sly-open-repl ()
  "Open the SLY MREPL, starting SLY first if necessary.
This is a direct port of Doom's `+lisp/open-repl' idea."
  (interactive)
  (require 'sly)
  (if (sly-connected-p)
      (sly-mrepl)
    (sly nil nil t)
    (unless (chief/sly-wait-for-connection)
      (user-error "Failed to start SLY/Slynk"))
    (sly-mrepl)))

(defun chief/sly-ask ()
  "Start SLY with Doom's `Sly (ask)' behavior."
  (interactive)
  (let ((current-prefix-arg '-))
    (call-interactively #'sly)))

(defun chief/sly-restart-dwim ()
  "Restart the current SLY session."
  (interactive)
  (require 'sly)
  (cond
   ((and (fboundp 'sly-inferior-process)
         (sly-connected-p)
         (sly-inferior-process))
    (call-interactively #'sly-restart-inferior-lisp))
   (t
    (when (fboundp 'sly-quit-lisp)
      (ignore-errors (sly-quit-lisp)))
    (call-interactively #'sly))))

(defun chief/sly-edit-definition ()
  "Jump to the Common Lisp definition at point using SLY."
  (interactive)
  (chief/sly-ensure-connected)
  (call-interactively #'sly-edit-definition))

(defun chief/sly-edit-definition-other-window ()
  "Jump to the Common Lisp definition at point in another window using SLY."
  (interactive)
  (chief/sly-ensure-connected)
  (let ((name (or (sly-symbol-at-point t)
                  (sly-read-symbol-name "Symbol: "))))
    (sly-edit-definition name 'window)))

(defun chief/sly-edit-uses ()
  "Show uses/references for the Common Lisp symbol at point using SLY."
  (interactive)
  (chief/sly-ensure-connected)
  (call-interactively #'sly-edit-uses))

(defun chief/sly-who-specializes ()
  "Show methods specializing the Common Lisp symbol at point using SLY."
  (interactive)
  (chief/sly-ensure-connected)
  (call-interactively #'sly-who-specializes))

(defun chief/sly-describe-symbol ()
  "Show SLY documentation for the Common Lisp symbol at point."
  (interactive)
  (chief/sly-ensure-connected)
  (call-interactively #'sly-describe-symbol))

(defun chief/sly-show-arglist ()
  "Show SLY arglist/signature help for the call at point."
  (interactive)
  (chief/sly-ensure-connected)
  (call-interactively #'sly-show-arglist))

(defun chief/common-lisp-find-file-in-quicklisp ()
  "Find a file from the local Quicklisp distribution tree."
  (interactive)
  (let ((root (cl-loop for dir in chief/common-lisp-quicklisp-paths
                       for dists = (expand-file-name "dists/" dir)
                       when (file-directory-p dists)
                       return dists)))
    (unless root
      (user-error "Couldn't find Quicklisp under %S"
                  chief/common-lisp-quicklisp-paths))
    (find-file (read-file-name "Quicklisp file: " root))))

(defun chief/common-lisp-macrostep-expand ()
  "Expand a Common Lisp macro like Doom, falling back to SLY's expander."
  (interactive)
  (cond
   ((fboundp 'macrostep-expand)
    (call-interactively #'macrostep-expand))
   (t
    (call-interactively #'sly-macroexpand-1))))

(defun chief/common-lisp-eval-defun-dwim ()
  "Evaluate the current defun with Doom's SLY overlay command when available."
  (interactive)
  (if (fboundp 'sly-overlay-eval-defun)
      (call-interactively #'sly-overlay-eval-defun)
    (call-interactively #'sly-eval-defun)))

(defun chief/common-lisp-reload-project ()
  "Restart SLY and reload the current ASDF system, following Doom's module."
  (interactive)
  (require 'sly-asdf)
  (sly-restart-inferior-lisp)
  (cl-labels ((recurse (attempt)
                (sleep-for 1)
                (condition-case nil
                    (sly-eval "PONG")
                  (error (if (= 5 attempt)
                             (error "Failed to reload Lisp project in 5 attempts")
                           (recurse (1+ attempt)))))))
    (recurse 1)
    (sly-asdf-load-system
     (or (sly-asdf-find-current-system)
         (car sly-asdf-system-history)
         (user-error "Can't find a system to reload")))))

(defun chief/common-lisp-mode-setup ()
  "Configure REPL, evaluation, and navigation integration for Common Lisp."
  (chief/repl-configure
   :start #'chief/sly-open-repl
   :restart #'chief/sly-restart-dwim
   :send-line #'sly-eval-last-expression
   :send-region #'sly-eval-region
   :send-buffer #'sly-eval-buffer
   :send-defun #'sly-eval-defun
   :load-file #'sly-load-file)
  ;; Common Lisp uses SLY/Slynk for code intelligence, not LSP.  These overrides
  ;; make the global navigation/doc commands (`gd', `gD', `gr', `gI', `K',
  ;; `gK') dispatch to SLY in `lisp-mode'.
  (setq-local chief/lsp-definition-function #'chief/sly-edit-definition)
  (setq-local chief/lsp-definition-other-window-function #'chief/sly-edit-definition-other-window)
  (setq-local chief/lsp-declaration-function #'chief/sly-edit-definition)
  (setq-local chief/lsp-references-function #'chief/sly-edit-uses)
  (setq-local chief/lsp-implementation-function #'chief/sly-who-specializes)
  (setq-local chief/lsp-hover-function #'chief/sly-describe-symbol)
  (setq-local chief/lsp-signature-function #'chief/sly-show-arglist)
  (when (fboundp 'sly-editing-mode)
    (sly-editing-mode 1)))

(put 'lisp-mode 'chief/lsp-definition-function #'chief/sly-edit-definition)
(put 'lisp-mode 'chief/lsp-definition-other-window-function #'chief/sly-edit-definition-other-window)
(put 'lisp-mode 'chief/lsp-declaration-function #'chief/sly-edit-definition)
(put 'lisp-mode 'chief/lsp-references-function #'chief/sly-edit-uses)
(put 'lisp-mode 'chief/lsp-implementation-function #'chief/sly-who-specializes)
(put 'lisp-mode 'chief/lsp-hover-function #'chief/sly-describe-symbol)
(put 'lisp-mode 'chief/lsp-signature-function #'chief/sly-show-arglist)

(defun chief/sly-mrepl-setup ()
  "Keep SLY's interactive REPL keys usable.
Paredit's minor-mode keymap overrides `sly-mrepl-return', making RET insert
newlines instead of submitting/evaluating input.  Disable Paredit in the MREPL
while keeping it enabled for Common Lisp source buffers."
  (when (bound-and-true-p paredit-mode)
    (paredit-mode -1)))

(add-hook 'sly-mrepl-mode-hook #'chief/sly-mrepl-setup 90)

(with-eval-after-load 'sly-mrepl
  (define-key sly-mrepl-mode-map (kbd "RET") #'sly-mrepl-return)
  (define-key sly-mrepl-mode-map (kbd "C-m") #'sly-mrepl-return)
  (define-key sly-mrepl-mode-map (kbd "<return>") #'sly-mrepl-return)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq major-mode 'sly-mrepl-mode)
        (chief/sly-mrepl-setup)))))

(use-package sly
  :commands (sly sly-connect)
  :init
  (setq inferior-lisp-program (or (executable-find "sbcl") "sbcl"))
  (setq sly-lisp-implementations
        `((sbcl (,(or (executable-find "sbcl") "sbcl")))))
  :hook ((lisp-mode . sly-editing-mode)
         (lisp-mode . chief/common-lisp-mode-setup))
  :config
  (sly-setup)
  (setq sly-mrepl-history-file-name (expand-file-name "sly-mrepl-history"
                                                       chief/var-directory)
        sly-kill-without-query-p t
        sly-net-coding-system 'utf-8-unix
        ;; Like Doom: prefer precise, non-fuzzy SLY completion by default.
        sly-complete-symbol-function 'sly-simple-completions)
  (unless (file-exists-p sly-mrepl-history-file-name)
    (write-region "" nil sly-mrepl-history-file-name nil 'silent))

  ;; Ported from Doom's Common Lisp module: start SLY when a real Lisp source
  ;; buffer enables `sly-mode', and clean up when the last visible SLY buffer is
  ;; killed.
  (defun chief/common-lisp--cleanup-sly-maybe-h ()
    "Kill SLY processes and buffers when the last visible SLY buffer is killed."
    (unless (cl-loop for buf in (delq (current-buffer) (buffer-list))
                     if (and (buffer-local-value 'sly-mode buf)
                             (get-buffer-window buf))
                     return t)
      (dolist (conn (sly--purge-connections))
        (sly-quit-lisp-internal conn 'sly-quit-sentinel t))
      (let (kill-buffer-hook kill-buffer-query-functions)
        (mapc #'kill-buffer
              (cl-loop for buf in (delq (current-buffer) (buffer-list))
                       if (buffer-local-value 'sly-mode buf)
                       collect buf)))))

  (defun chief/common-lisp-init-sly-h ()
    "Attempt to auto-start SLY when opening a Common Lisp source buffer."
    (cond
     ((or (not (derived-mode-p 'lisp-mode))
          (string-prefix-p " " (buffer-name))
          (sly-connected-p)))
     ((executable-find (car (if (listp inferior-lisp-program)
                                inferior-lisp-program
                              (split-string inferior-lisp-program))))
      (let ((sly-auto-start 'always))
        (sly-auto-start)
        (add-hook 'kill-buffer-hook #'chief/common-lisp--cleanup-sly-maybe-h
                  nil t)))
     ((message "WARNING: Couldn't find `inferior-lisp-program' (%s)"
               inferior-lisp-program))))

  (add-hook 'sly-mode-hook #'chief/common-lisp-init-sly-h)

  (with-eval-after-load 'evil
    (dolist (state '(normal motion visual))
      (evil-define-key state lisp-mode-map (kbd "gb") #'sly-pop-find-definition-stack)
      (evil-define-key state sly-db-mode-map (kbd "gr") #'sly-db-restart-frame)
      (evil-define-key state sly-inspector-mode-map (kbd "gb") #'sly-inspector-pop)
      (evil-define-key state sly-inspector-mode-map (kbd "gr") #'sly-inspector-reinspect)
      (evil-define-key state sly-inspector-mode-map (kbd "gR") #'sly-inspector-fetch-all)
      (evil-define-key state sly-inspector-mode-map (kbd "K") #'sly-inspector-describe-inspectee)
      (evil-define-key state sly-xref-mode-map (kbd "gr") #'sly-recompile-xref)
      (evil-define-key state sly-xref-mode-map (kbd "gR") #'sly-recompile-all-xrefs)))

  (chief/leader-def
    "x" '(:ignore t :which-key "common-lisp")
    "xx" #'chief/sly-open-repl
    "xc" #'sly-connect
    "xq" #'sly-quit-lisp
    "xr" #'chief/sly-restart-dwim)
  (chief/local-leader-def
    :keymaps 'lisp-mode-map
    "'" #'sly
    ";" #'chief/sly-ask
    "m" #'chief/common-lisp-macrostep-expand
    "f" #'chief/common-lisp-find-file-in-quicklisp
    "c" '(:ignore t :which-key "compile")
    "cc" #'sly-compile-file
    "cC" #'sly-compile-and-load-file
    "cf" #'sly-compile-defun
    "cl" #'sly-load-file
    "cn" #'sly-remove-notes
    "cr" #'sly-compile-region
    "e" '(:ignore t :which-key "evaluate")
    "eb" #'sly-eval-buffer
    "ed" #'chief/common-lisp-eval-defun-dwim
    "ee" #'sly-eval-last-expression
    "eE" #'sly-eval-print-last-expression
    "ef" #'sly-eval-defun
    "eF" #'sly-undefine-function
    "er" #'sly-eval-region
    "g" '(:ignore t :which-key "goto/xref")
    "gb" #'sly-pop-find-definition-stack
    "gd" #'chief/sly-edit-definition
    "gD" #'chief/sly-edit-definition-other-window
    "ge" #'chief/sly-edit-uses
    "gn" #'sly-next-note
    "gN" #'sly-previous-note
    "gr" #'sly-who-references
    "gs" #'sly-stickers-next-sticker
    "gS" #'sly-stickers-prev-sticker
    "h" '(:ignore t :which-key "help")
    "h<" #'sly-who-calls
    "h>" #'sly-calls-who
    "h~" #'hyperspec-lookup-format
    "h#" #'hyperspec-lookup-reader-macro
    "ha" #'sly-apropos
    "hA" #'sly-apropos-all
    "hb" #'sly-who-binds
    "hd" #'sly-disassemble-symbol
    "hh" #'sly-describe-symbol
    "hH" #'sly-hyperspec-lookup
    "hm" #'sly-who-macroexpands
    "hp" #'sly-apropos-package
    "hr" #'sly-who-references
    "hs" #'sly-who-specializes
    "hS" #'sly-who-sets
    "r" '(:ignore t :which-key "repl")
    "rc" #'sly-mrepl-clear-repl
    "rl" #'sly-asdf-load-system
    "rq" #'sly-quit-lisp
    "rr" #'sly-restart-inferior-lisp
    "rR" #'chief/common-lisp-reload-project
    "rs" #'sly-mrepl-sync
    "s" '(:ignore t :which-key "stickers")
    "sb" #'sly-stickers-toggle-break-on-stickers
    "sc" #'sly-stickers-clear-defun-stickers
    "sC" #'sly-stickers-clear-buffer-stickers
    "sf" #'sly-stickers-fetch
    "sr" #'sly-stickers-replay
    "ss" #'sly-stickers-dwim
    "t" '(:ignore t :which-key "test")
    "ts" #'sly-asdf-test-system
    "T" '(:ignore t :which-key "trace")
    "Td" #'sly-trace-dialog
    "Tt" #'sly-toggle-trace-fdefinition
    "TT" #'sly-toggle-fancy-trace
    "Tu" #'sly-untrace-all))

;; Doom's Common Lisp module installs these SLY extensions.  Keep them optional
;; but register their contribs exactly as Doom does when they are available.
(chief/safe-use-package sly-repl-ansi-color
  :defer t
  :init
  (add-to-list 'sly-contribs 'sly-repl-ansi-color))

(chief/safe-use-package sly-asdf
  :defer t
  :init
  (add-to-list 'sly-contribs 'sly-asdf 'append))

(chief/safe-use-package sly-stepper
  :defer t
  :straight (:host github :repo "joaotavora/sly-stepper"
                   :files (:defaults "*.lisp" "*.asd"))
  :init
  (add-to-list 'sly-contribs 'sly-stepper 'append))

(chief/safe-use-package sly-overlay
  :defer t)

(chief/safe-use-package sly-macrostep
  :defer t)

(with-eval-after-load 'sly
  ;; On config reloads where SLY is already loaded, re-run setup after optional
  ;; Doom-style contribs have had a chance to append themselves.
  (sly-setup))

(provide 'lang-common-lisp)
;;; lang-common-lisp.el ends here
