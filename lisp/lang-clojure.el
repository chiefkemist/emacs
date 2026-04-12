;;; lang-clojure.el --- Clojure tooling -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'lang-java)
(require 'seq)
(require 'subr-x)

(defcustom chief/babashka-command "bb"
  "Command used to start Babashka tooling."
  :type 'string
  :group 'chief)

(defcustom chief/nbb-command "nbb"
  "Command used to start nbb tooling."
  :type 'string
  :group 'chief)

(defun chief/cider-ensure-jvm-environment (&rest _)
  "Force CIDER commands to use the configured JVM environment."
  (chief/jvm-activate-environment))

(defun chief/clojure-current-directory ()
  "Return the current Clojure buffer directory."
  (file-name-as-directory
   (expand-file-name
    (or (and buffer-file-name (file-name-directory buffer-file-name))
        default-directory))))

(defun chief/clojure-polylith-workspace-root (&optional directory)
  "Return the enclosing Polylith workspace root for DIRECTORY, if any."
  (when-let* ((root (locate-dominating-file (or directory (chief/clojure-current-directory))
                                            "workspace.edn")))
    (file-name-as-directory (expand-file-name root))))

(defun chief/clojure-polylith-related-project-root (&optional directory)
  "Return the best Polylith project root for DIRECTORY, if any.

Files inside `projects/<name>/` use that project directly. Files inside
`bases/<name>/` reuse `projects/<name>/` when it exists, so CIDER gets the
full application classpath instead of the isolated base classpath. All other
workspace files fall back to the workspace `:dev` alias."
  (when-let* ((workspace-root (chief/clojure-polylith-workspace-root directory))
              (relative-path (file-relative-name (or directory (chief/clojure-current-directory))
                                                 workspace-root)))
    (cond
     ((string-match "\\`projects/\\([^/]+\\)\\(?:/\\|\\'\\)" relative-path)
      (let ((project-root (expand-file-name (match-string 1 relative-path)
                                            (expand-file-name "projects" workspace-root))))
        (when (file-exists-p (expand-file-name "deps.edn" project-root))
          (file-name-as-directory project-root))))
     ((string-match "\\`bases/\\([^/]+\\)\\(?:/\\|\\'\\)" relative-path)
      (let ((project-root (expand-file-name (match-string 1 relative-path)
                                            (expand-file-name "projects" workspace-root))))
        (when (file-exists-p (expand-file-name "deps.edn" project-root))
          (file-name-as-directory project-root)))))))

(defun chief/clojure-file-contains-p (file regexp)
  "Return non-nil when FILE contains REGEXP."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (re-search-forward regexp nil t))))

(defun chief/clojure-cider-project-dir (&optional directory)
  "Return the best CIDER project directory for DIRECTORY."
  (let* ((directory (or directory (chief/clojure-current-directory)))
         (workspace-root (chief/clojure-polylith-workspace-root directory)))
    (or (chief/clojure-polylith-related-project-root directory)
        workspace-root
        (when (fboundp 'clojure-project-dir)
          (clojure-project-dir directory))
        directory)))

(defun chief/clojure-cider-cli-aliases (&optional directory)
  "Return any Clojure CLI aliases CIDER should use for DIRECTORY."
  (or cider-clojure-cli-aliases
      (let* ((directory (or directory (chief/clojure-current-directory)))
             (project-dir (chief/clojure-cider-project-dir directory))
             (workspace-root (chief/clojure-polylith-workspace-root directory)))
        (when (and workspace-root
                   (equal (file-name-as-directory project-dir)
                          (file-name-as-directory workspace-root)))
          ":dev"))))

(defun chief/clojure-cider-cljs-repl-type (&optional directory)
  "Return the default ClojureScript REPL type for DIRECTORY, if any."
  (or cider-default-cljs-repl
      (let* ((project-dir (chief/clojure-cider-project-dir directory))
             (shadow-cljs-edn (expand-file-name "shadow-cljs.edn" project-dir))
             (nbb-edn (expand-file-name "nbb.edn" project-dir))
             (app-edn (expand-file-name "app.edn" project-dir)))
        (cond
         ((file-exists-p shadow-cljs-edn) 'shadow)
         ((file-exists-p nbb-edn) 'nbb)
         ((chief/clojure-file-contains-p app-edn ":target[[:space:]
]*:nodejs") 'node)
         (t nil)))))

(defun chief/clojure-cider-preferred-project-type ()
  "Return the preferred CIDER project type from dir-locals, if any."
  (pcase cider-preferred-build-tool
    ('clojure-cli 'clojure-cli)
    ('lein 'lein)
    ('shadow-cljs 'shadow-cljs)
    ('babashka 'babashka)
    ('nbb 'nbb)
    ('basilisp 'basilisp)
    (_ nil)))

(defun chief/clojure-cider-clj-project-type (&optional directory)
  "Return the CIDER project type to use for Clojure in DIRECTORY."
  (or (chief/clojure-cider-preferred-project-type)
      (let* ((project-dir (chief/clojure-cider-project-dir directory))
             (project-clj (expand-file-name "project.clj" project-dir))
             (deps-edn (expand-file-name "deps.edn" project-dir))
             (bb-edn (expand-file-name "bb.edn" project-dir))
             (basilisp-edn (expand-file-name "basilisp.edn" project-dir)))
        (cond
         ((file-exists-p project-clj) 'lein)
         ((file-exists-p deps-edn) 'clojure-cli)
         ((file-exists-p bb-edn) 'babashka)
         ((file-exists-p basilisp-edn) 'basilisp)
         (t nil)))))

(defun chief/clojure-cider-cljs-project-type (&optional directory)
  "Return the CIDER project type to use for ClojureScript in DIRECTORY."
  (or (chief/clojure-cider-preferred-project-type)
      (let ((cljs-repl-type (chief/clojure-cider-cljs-repl-type directory)))
        (cond
         ((eq cljs-repl-type 'shadow) 'shadow-cljs)
         ((eq cljs-repl-type 'shadow-select) 'shadow-cljs)
         ((eq cljs-repl-type 'nbb) 'nbb)
         (t (chief/clojure-cider-clj-project-type directory))))))

(defun chief/clojure-cider-context (&optional directory)
  "Return the inferred CIDER context for DIRECTORY."
  (let* ((directory (or directory (chief/clojure-current-directory)))
         (project-dir (chief/clojure-cider-project-dir directory)))
    (list :directory directory
          :project-dir project-dir
          :clojure-cli-aliases (chief/clojure-cider-cli-aliases directory)
          :cljs-repl-type (chief/clojure-cider-cljs-repl-type directory)
          :clj-project-type (chief/clojure-cider-clj-project-type directory)
          :cljs-project-type (chief/clojure-cider-cljs-project-type directory))))

(defun chief/clojure-cider-apply-context (&optional directory)
  "Apply inferred CIDER defaults for DIRECTORY to the current buffer."
  (let* ((context (chief/clojure-cider-context directory))
         (project-dir (plist-get context :project-dir))
         (aliases (plist-get context :clojure-cli-aliases))
         (cljs-repl-type (plist-get context :cljs-repl-type)))
    (when project-dir
      (setq-local default-directory project-dir))
    (when aliases
      (setq-local cider-clojure-cli-aliases aliases))
    (when cljs-repl-type
      (setq-local cider-default-cljs-repl cljs-repl-type))
    context))

(defun chief/cider-prefix-params (&optional prefix)
  "Translate PREFIX into CIDER parameter plist form."
  (cond
   ((equal prefix '(4)) '(:edit-jack-in-command t))
   ((equal prefix '(16)) '(:do-prompt t))
   (t nil)))

(defun chief/cider-context-params (repl-kind &optional prefix directory)
  "Return a CIDER parameter plist for REPL-KIND in DIRECTORY.

REPL-KIND should be one of `clj', `cljs', or `clj&cljs'."
  (let* ((context (chief/clojure-cider-context directory))
         (params (chief/cider-prefix-params prefix))
         (project-dir (plist-get context :project-dir))
         (project-type (pcase repl-kind
                         ((or 'cljs 'clj&cljs) (plist-get context :cljs-project-type))
                         (_ (plist-get context :clj-project-type))))
         (cljs-repl-type (plist-get context :cljs-repl-type)))
    (setq params (plist-put params :project-dir project-dir))
    (when project-type
      (setq params (plist-put params :project-type project-type)))
    (when (and (memq repl-kind '(cljs clj&cljs)) cljs-repl-type)
      (setq params (plist-put params :cljs-repl-type cljs-repl-type)))
    params))

(defun chief/cider-call-with-context (command repl-kind &optional prefix)
  "Call COMMAND using the inferred CIDER context for REPL-KIND."
  (let* ((context (chief/clojure-cider-apply-context))
         (default-directory (plist-get context :project-dir))
         (cider-clojure-cli-aliases (or (plist-get context :clojure-cli-aliases)
                                        cider-clojure-cli-aliases))
         (cider-default-cljs-repl (or (plist-get context :cljs-repl-type)
                                      cider-default-cljs-repl)))
    (funcall command (chief/cider-context-params repl-kind prefix default-directory))))

(defun chief/cider-buffer-project-directory (&optional buffer)
  "Return the preferred CIDER project directory for BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (chief/clojure-cider-project-dir default-directory)))

(defun chief/cider-with-buffer-project-dir-a (orig-fn &rest args)
  "Run ORIG-FN with `default-directory' set to the buffer's CIDER project dir."
  (let ((default-directory (chief/cider-buffer-project-directory)))
    (apply orig-fn args)))

(defun chief/cider--clean-ns-list (namespaces)
  "Normalize NAMESPACES, removing nil and empty entries."
  (sort (seq-remove #'string-empty-p
                    (delq nil (copy-sequence namespaces)))
        #'string<))

(defun chief/cider-cached-ns-list (&optional repl)
  "Return namespace names cached in REPL, filtering out nils."
  (when-let* ((repl (or repl (cider-current-repl 'infer nil)))
              (cache (buffer-local-value 'cider-repl-ns-cache repl)))
    (chief/cider--clean-ns-list (nrepl-dict-keys cache))))

(defun chief/cider-discard-ns-list-error-buffer ()
  "Kill the transient `*cider-error*' buffer for suppressed `ns-list' failures."
  (when-let* ((buf (get-buffer "*cider-error*")))
    (with-current-buffer buf
      (when (save-excursion
              (goto-char (point-min))
              (re-search-forward "ns-list-error" nil t))
        (kill-buffer buf)))))

(defun chief/cider-safe-ns-list-a (orig-fn &rest args)
  "Avoid noisy cljs `ns-list' middleware failures.

Some cljs sessions can return `ns-list-error' due to upstream middleware
bugs. In that case, suppress the popup stacktrace and fall back to any cached
namespace list so opening buffers does not blow up the UI."
  (let ((cider-show-error-buffer nil)
        (cider-auto-select-error-buffer nil))
    (or (condition-case nil
            (let ((result (apply orig-fn args)))
              (chief/cider--clean-ns-list result))
          (error nil))
        (prog1 (chief/cider-cached-ns-list)
          (chief/cider-discard-ns-list-error-buffer))
        '())))

(defun chief/cider-jack-in-clj-dwim (&optional prefix)
  "Start the best Clojure CIDER session for the current buffer."
  (interactive "P")
  (chief/cider-call-with-context #'cider-jack-in-clj 'clj prefix))

(defun chief/cider-jack-in-cljs-dwim (&optional prefix)
  "Start the best ClojureScript CIDER session for the current buffer."
  (interactive "P")
  (chief/cider-call-with-context #'cider-jack-in-cljs 'cljs prefix))

(defun chief/cider-jack-in-clj&cljs-dwim (&optional prefix)
  "Start paired Clojure and ClojureScript CIDER sessions for the current buffer."
  (interactive "P")
  (chief/cider-call-with-context #'cider-jack-in-clj&cljs 'clj&cljs prefix))

(defun chief/cider-connect-clj-dwim (&optional prefix)
  "Connect to the best Clojure nREPL for the current buffer."
  (interactive "P")
  (chief/cider-call-with-context #'cider-connect-clj 'clj prefix))

(defun chief/cider-connect-cljs-dwim (&optional prefix)
  "Connect to the best ClojureScript nREPL for the current buffer."
  (interactive "P")
  (chief/cider-call-with-context #'cider-connect-cljs 'cljs prefix))

(defun chief/cider-connect-clj&cljs-dwim (&optional prefix)
  "Connect to paired Clojure and ClojureScript nREPLs for the current buffer."
  (interactive "P")
  (chief/cider-call-with-context #'cider-connect-clj&cljs 'clj&cljs prefix))

(defun chief/cider-jack-in-dwim (&optional prefix)
  "Start the best CIDER session for the current buffer."
  (interactive "P")
  (cond
   ((derived-mode-p 'clojurescript-mode)
    (chief/cider-jack-in-cljs-dwim prefix))
   ((derived-mode-p 'clojurec-mode)
    (chief/cider-jack-in-clj&cljs-dwim prefix))
   (t
    (chief/cider-jack-in-clj-dwim prefix))))

(defun chief/cider-connect-dwim (&optional prefix)
  "Connect to the best CIDER session for the current buffer."
  (interactive "P")
  (cond
   ((derived-mode-p 'clojurescript-mode)
    (chief/cider-connect-cljs-dwim prefix))
   ((derived-mode-p 'clojurec-mode)
    (chief/cider-connect-clj&cljs-dwim prefix))
   (t
    (chief/cider-connect-clj-dwim prefix))))

(defun chief/cider-restart-dwim ()
  "Restart the current CIDER session, or jack in when disconnected."
  (interactive)
  (if (fboundp 'cider-connected-p)
      (if (cider-connected-p)
          (cider-restart)
        (chief/cider-jack-in-dwim))
    (chief/cider-jack-in-dwim)))

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

(defun chief/clojure-mode-setup ()
  "Configure REPL integration for Clojure-family buffers."
  (chief/clojure-cider-apply-context)
  (setq-local chief/lsp-root-function
              (lambda ()
                (or (chief/clojure-polylith-workspace-root)
                    (chief/clojure-cider-project-dir))))
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
  :mode ("\\.cljc\\'" . clojurec-mode)
  :mode ("\\.cljs\\'" . clojurescript-mode)
  :mode ("\\.edn\\'" . edn-mode)
  :config
  (setq clojure-mode-extra-font-locking t)
  (setq clojure-align-forms-automatically t)
  (dolist (map (list clojure-mode-map clojurescript-mode-map clojurec-mode-map))
    (define-key map (kbd "C-c C-l") #'cider-load-buffer)
    (define-key map (kbd "C-c C-k") #'cider-load-buffer)
    (define-key map (kbd "C-x C-e") #'cider-eval-last-sexp))
  (dolist (hook '(clojure-mode-hook
                  clojurescript-mode-hook
                  clojurec-mode-hook))
    (add-hook hook #'chief/clojure-mode-setup)))

(use-package cider
  :after clojure-mode
  :commands (cider-jack-in
             cider-jack-in-clj
             cider-jack-in-cljs
             cider-jack-in-clj&cljs
             cider-connect-clj
             cider-connect-cljs
             cider-connect-clj&cljs)
  :custom
  (cider-save-file-on-load t)
  (cider-repl-display-help-banner nil)
  (cider-show-error-buffer 'except-in-repl)
  (cider-auto-select-error-buffer nil)
  (nrepl-log-messages nil)
  :config
  (dolist (command '(cider-jack-in
                     cider-jack-in-clj
                     cider-jack-in-cljs
                     cider-jack-in-clj&cljs
                     cider-connect-clj
                     cider-connect-cljs
                     cider-connect-clj&cljs))
    (advice-add command :before #'chief/cider-ensure-jvm-environment))
  (dolist (command '(cider-load-buffer
                     cider-load-file
                     cider-eval-last-sexp
                     cider-eval-defun-at-point
                     cider-eval-region
                     cider-eval-sexp-at-point
                     cider-pprint-eval-last-sexp
                     cider-pprint-eval-defun-at-point))
    (advice-add command :around #'chief/cider-with-buffer-project-dir-a))
  (advice-add 'cider-sync-request:ns-list :around #'chief/cider-safe-ns-list-a)
  (add-to-list 'cider-stacktrace-suppressed-errors "ns-list-error")
  (define-key cider-mode-map (kbd "C-c C-l") #'cider-load-buffer)
  (define-key cider-mode-map (kbd "C-c C-k") #'cider-load-buffer)
  (define-key cider-mode-map (kbd "C-x C-e") #'cider-eval-last-sexp)
  (chief/leader-def
    "c" '(:ignore t :which-key "clojure")
    "cj" #'chief/cider-jack-in-dwim
    "ck" #'chief/cider-jack-in-clj-dwim
    "cK" #'chief/cider-jack-in-cljs-dwim
    "cJ" #'chief/cider-jack-in-clj&cljs-dwim
    "cc" #'chief/cider-connect-dwim
    "cC" #'chief/cider-connect-cljs-dwim
    "cb" #'chief/start-babashka-nrepl
    "cB" #'chief/start-nbb-nrepl
    "cq" #'cider-quit)
  (chief/local-leader-def
    :keymaps '(clojure-mode-map clojurescript-mode-map clojurec-mode-map)
    "e" #'cider-eval-defun-at-point
    "r" #'cider-ns-refresh
    "t" #'cider-test-run-ns-tests
    "j" #'chief/cider-jack-in-dwim
    "k" #'chief/cider-jack-in-clj-dwim
    "K" #'chief/cider-jack-in-cljs-dwim
    "J" #'chief/cider-jack-in-clj&cljs-dwim))

(chief/repl-setup-standard-local-leader 'clojure-mode-map)
(chief/repl-setup-standard-local-leader 'clojurescript-mode-map)
(chief/repl-setup-standard-local-leader 'clojurec-mode-map)

(provide 'lang-clojure)
;;; lang-clojure.el ends here
