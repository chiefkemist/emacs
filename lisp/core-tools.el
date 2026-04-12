;;; core-tools.el --- Daily tooling -*- lexical-binding: t; -*-

(declare-function chief/large-buffer-p "core-ui" (&optional threshold))
(autoload 'agent-shell-openai-start-codex "agent-shell-openai" nil t)
(autoload 'agent-shell-google-start-gemini "agent-shell-google" nil t)
(autoload 'agent-shell-cursor-start-agent "agent-shell-cursor" nil t)
(autoload 'agent-shell-pi-start-agent "agent-shell-pi" nil t)
(autoload 'agent-shell-opencode-start-agent "agent-shell-opencode" nil t)

(use-package envrc
  :defer 1
  :config
  (envrc-global-mode))

(use-package xterm-color
  :defer t)

(defcustom chief/magit-delta-executable "delta"
  "Executable used for delta-powered Git diff views."
  :type 'string
  :group 'chief)

(defcustom chief/magit-delta-arguments
  '("--paging=never")
  "Arguments passed to `chief/magit-delta-executable'."
  :type '(repeat string)
  :group 'chief)

(defcustom chief/magit-difftastic-executables
  '("difft" "difftastic")
  "Executable candidates used for difftastic-powered Git diff views."
  :type '(repeat string)
  :group 'chief)

(defvar-local chief/git-external-diff-command nil
  "The command used to populate the current external diff buffer.")

(defvar-local chief/git-external-diff-process-environment nil
  "Extra environment entries used for the current external diff buffer.")

(defvar-local chief/git-external-diff-postprocess-function nil
  "Optional post-processing function for the current external diff buffer.")

(defvar-local chief/git-external-diff-root nil
  "Git repository root used for the current external diff buffer.")

(define-derived-mode chief/git-external-diff-mode special-mode "Git-External-Diff"
  "Major mode for external Git diff viewers such as delta and difftastic."
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function #'chief/git-external-diff-revert))

(defun chief/git-external-diff-revert (&rest _ignore)
  "Re-run the command used to build the current external diff buffer."
  (unless chief/git-external-diff-command
    (user-error "This buffer is not backed by an external Git diff command"))
  (chief/git-external-diff--run
   (current-buffer)
   chief/git-external-diff-command
   chief/git-external-diff-root
   chief/git-external-diff-process-environment
   chief/git-external-diff-postprocess-function))

(defun chief/git-external-diff--resolve-executable (&rest candidates)
  "Return the first installed executable from CANDIDATES."
  (or (catch 'match
        (dolist (candidate candidates)
          (when (and candidate (executable-find candidate))
            (throw 'match candidate))))
      (user-error "Missing executable. Tried: %s"
                  (mapconcat #'identity (delq nil candidates) ", "))))

(defun chief/magit--toplevel ()
  "Return the current Git repository root or raise a user error."
  (or (magit-toplevel)
      (user-error "Not inside a Git repository")))

(defun chief/magit--dwim-diff-arg ()
  "Return a sensible Git diff argument for the current context."
  (or (when current-prefix-arg
        (magit-diff-read-range-or-commit "Range/Commit"))
      (pcase (ignore-errors (magit-diff--dwim))
        ('unmerged (user-error "Difftastic/Delta diff for unmerged changes is not implemented"))
        ('unstaged nil)
        ('staged "--cached")
        (`(commit . ,value) (format "%s^..%s" value value))
        (`(stash . ,_) (user-error "Difftastic/Delta diff for stashes is not implemented"))
        ((and range (pred stringp)) range)
        (_ nil))
      (magit-diff-read-range-or-commit "Range/Commit")))

(defun chief/magit--read-show-revision ()
  "Return the most relevant revision at point, prompting when needed."
  (or (and (not current-prefix-arg)
           (or (magit-thing-at-point 'git-revision t)
               (magit-branch-or-commit-at-point)))
      (magit-read-branch-or-commit "Revision")))

(defun chief/magit--read-log-arg ()
  "Return a useful revision or range for patch log viewers."
  (or (when (not current-prefix-arg)
        (if-let* ((rev (magit-thing-at-point 'git-revision t)))
            (format "%s^..%s" rev rev)
          (magit-branch-or-commit-at-point)))
      (magit-read-range-or-commit "Log revision/range")))

(defun chief/git-external-diff--max-line-width ()
  "Return the width of the longest line in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((width 0))
      (while (not (eobp))
        (end-of-line)
        (setq width (max width (current-column)))
        (forward-line 1))
      width)))

(defun chief/git-external-diff--display-buffer (buffer)
  "Display BUFFER, preferring a wide side window for side-by-side diffs."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let* ((width (max 80 (chief/git-external-diff--max-line-width)))
           (window
            (display-buffer
             buffer
             (if (> 80 (- (frame-width) width))
                 '(display-buffer-at-bottom (window-height . 0.45))
               `(display-buffer-pop-up-window
                 (window-width . ,(min width (frame-width))))))))
      (when (window-live-p window)
        (select-window window)))))

(defun chief/git-external-diff--finalize-buffer (buffer success postprocess)
  "Finalize BUFFER after an external diff command.
SUCCESS indicates whether the process exited successfully.
POSTPROCESS, when non-nil, transforms the buffer contents before display."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (when (and success postprocess)
        (funcall postprocess))
      (ansi-color-apply-on-region (point-min) (point-max))
      (goto-char (point-min))
      (chief/git-external-diff-mode)
      (unless success
        (message "External diff command exited unsuccessfully; showing raw output"))))
  (chief/git-external-diff--display-buffer buffer))

(defun chief/git-external-diff--run (buffer command directory &optional environment postprocess)
  "Populate BUFFER by running COMMAND in DIRECTORY.
ENVIRONMENT is a list of extra environment variables.
POSTPROCESS is applied to the resulting buffer on success."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq-local chief/git-external-diff-command command
                  chief/git-external-diff-root directory
                  chief/git-external-diff-process-environment environment
                  chief/git-external-diff-postprocess-function postprocess
                  default-directory directory)))
  (let ((process-environment (append environment process-environment)))
    (make-process
     :name (buffer-name buffer)
     :buffer buffer
     :command command
     :noquery t
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (chief/git-external-diff--finalize-buffer
          (process-buffer proc)
          (and (eq (process-status proc) 'exit)
               (zerop (process-exit-status proc)))
          postprocess))))))

(defun chief/magit-delta--make-args ()
  "Return the delta arguments used for Magit and external delta views."
  (let ((args chief/magit-delta-arguments))
    (unless (member "--color-only" args)
      (setq args (append args '("--color-only"))))
    args))

(defun chief/magit-delta--colorize-buffer ()
  "Apply ANSI colors in the current buffer, preferring `xterm-color'."
  (let ((inhibit-read-only t)
        (buffer-read-only nil))
    (if (fboundp 'xterm-color-colorize-buffer)
        (xterm-color-colorize-buffer 'use-overlays)
      (ansi-color-apply-on-region (point-min) (point-max)))))

(defun chief/magit-delta--hide-plus-minus-markers ()
  "Hide leading +/- markers inside diff hunks.
This lets delta's styling carry the visual weight instead."
  (save-excursion
    (goto-char (point-min))
    (let ((in-hunk nil))
      (while (re-search-forward "^\\(diff\\|@@\\|+\\|-\\)" nil t)
        (cond
         ((string-equal (match-string 0) "diff")
          (setq in-hunk nil))
         ((string-equal (match-string 0) "@@")
          (setq in-hunk t))
         (in-hunk
          (add-text-properties (match-beginning 0) (match-end 0)
                               '(display " "))))))))

(defun chief/magit-delta--wash-diffs ()
  "Re-render the current Magit diff through delta."
  (when (executable-find chief/magit-delta-executable)
    (let ((inhibit-read-only t))
      (apply #'call-process-region
             (point-min)
             (point-max)
             chief/magit-delta-executable
             t
             t
             nil
             (chief/magit-delta--make-args))
      (chief/magit-delta--colorize-buffer)
      (chief/magit-delta--hide-plus-minus-markers))))

(defun chief/magit-enable-native-delta-h ()
  "Use delta to render native Magit diff/revision/log buffers."
  (when (executable-find chief/magit-delta-executable)
    (setq-local magit-diff-refine-hunk nil)
    (setq-local face-remapping-alist
                (append
                 (seq-remove
                  (lambda (entry)
                    (memq (car-safe entry)
                          '(magit-diff-context-highlight
                            magit-diff-added
                            magit-diff-added-highlight
                            magit-diff-removed
                            magit-diff-removed-highlight)))
                  face-remapping-alist)
                 '((magit-diff-context-highlight . default)
                   (magit-diff-added . default)
                   (magit-diff-added-highlight . default)
                   (magit-diff-removed . default)
                   (magit-diff-removed-highlight . default))))
    (add-hook 'magit-diff-wash-diffs-hook #'chief/magit-delta--wash-diffs nil t)))

(defun chief/git-external-diff--apply-delta ()
  "Render the current raw Git diff buffer through delta."
  (let ((delta (chief/git-external-diff--resolve-executable chief/magit-delta-executable)))
    (apply #'call-process-region
           (point-min)
           (point-max)
           delta
           t
           t
           nil
           (chief/magit-delta--make-args))))

(defun chief/magit-delta-diff (&optional arg)
  "Show a delta-rendered Git diff using Magit's DWIM rules."
  (interactive)
  (let* ((root (chief/magit--toplevel))
         (arg (or arg (chief/magit--dwim-diff-arg)))
         (name (format "*git delta diff%s*"
                       (if arg (format " %s" arg) "")))
         (command (append '("git" "--no-pager" "diff" "--no-ext-diff" "--color=always")
                          (when arg (list arg)))))
    (chief/git-external-diff--run
     (get-buffer-create name)
     command
     root
     nil
     #'chief/git-external-diff--apply-delta)))

(defun chief/magit-delta-show (rev)
  "Show REV through a delta-rendered `git show'."
  (interactive (list (chief/magit--read-show-revision)))
  (let ((root (chief/magit--toplevel)))
    (chief/git-external-diff--run
     (get-buffer-create (format "*git delta show %s*" rev))
     (list "git" "--no-pager" "show" "--no-ext-diff" "--color=always" rev)
     root
     nil
     #'chief/git-external-diff--apply-delta)))

(defun chief/magit-delta-log (&optional arg)
  "Show `git log -p` through delta for ARG or the thing at point."
  (interactive)
  (let* ((root (chief/magit--toplevel))
         (arg (or arg (chief/magit--read-log-arg)))
         (name (format "*git delta log%s*"
                       (if arg (format " %s" arg) "")))
         (command (append '("git" "--no-pager" "log" "-p" "--stat" "--decorate"
                             "--no-ext-diff" "--color=always")
                          (when arg (list arg)))))
    (chief/git-external-diff--run
     (get-buffer-create name)
     command
     root
     nil
     #'chief/git-external-diff--apply-delta)))

(defun chief/magit-difftastic-diff (&optional arg)
  "Show a difftastic-rendered Git diff using Magit's DWIM rules."
  (interactive)
  (let* ((root (chief/magit--toplevel))
         (arg (or arg (chief/magit--dwim-diff-arg)))
         (difft (apply #'chief/git-external-diff--resolve-executable
                       chief/magit-difftastic-executables))
         (name (format "*git difftastic diff%s*"
                       (if arg (format " %s" arg) "")))
         (environment (list (format "GIT_EXTERNAL_DIFF=%s --width=%d"
                                    difft
                                    (frame-width))))
         (command (append '("git" "--no-pager" "diff")
                          (when arg (list arg)))))
    (chief/git-external-diff--run
     (get-buffer-create name)
     command
     root
     environment)))

(defun chief/magit-difftastic-show (rev)
  "Show REV through difftastic via `git show --ext-diff'."
  (interactive (list (chief/magit--read-show-revision)))
  (let* ((root (chief/magit--toplevel))
         (difft (apply #'chief/git-external-diff--resolve-executable
                       chief/magit-difftastic-executables))
         (environment (list (format "GIT_EXTERNAL_DIFF=%s --width=%d"
                                    difft
                                    (frame-width)))))
    (chief/git-external-diff--run
     (get-buffer-create (format "*git difftastic show %s*" rev))
     (list "git" "--no-pager" "show" "--ext-diff" rev)
     root
     environment)))

(defun chief/magit-difftastic-log (&optional arg)
  "Show `git log -p` through difftastic for ARG or the thing at point."
  (interactive)
  (let* ((root (chief/magit--toplevel))
         (arg (or arg (chief/magit--read-log-arg)))
         (difft (apply #'chief/git-external-diff--resolve-executable
                       chief/magit-difftastic-executables))
         (name (format "*git difftastic log%s*"
                       (if arg (format " %s" arg) "")))
         (environment (list (format "GIT_EXTERNAL_DIFF=%s --width=%d"
                                    difft
                                    (frame-width))))
         (command (append '("git" "--no-pager" "log" "-p" "--decorate" "--color=always"
                             "--ext-diff")
                          (when arg (list arg)))))
    (chief/git-external-diff--run
     (get-buffer-create name)
     command
     root
     environment)))

(use-package magit
  :commands (magit-status magit-blame-addition magit-log magit-log-current magit-log-buffer-file)
  :init
  (chief/leader-def
    "g" '(:ignore t :which-key "git")
    "gg" #'magit-status
    "gs" #'magit-status
    "gb" #'magit-blame-addition
    "gl" #'magit-log
    "gL" #'magit-log-current
    "gf" #'magit-log-buffer-file
    "gd" '(:ignore t :which-key "diff viewers")
    "gdd" #'chief/magit-delta-diff
    "gds" #'chief/magit-delta-show
    "gdl" #'chief/magit-delta-log
    "gdt" #'chief/magit-difftastic-diff
    "gdT" #'chief/magit-difftastic-show
    "gdL" #'chief/magit-difftastic-log)
  :config
  (require 'ansi-color)
  (require 'seq)
  (require 'transient)
  ;; `evil-collection-init' does not reliably switch Magit buffers into an
  ;; Evil state before the first status buffer is created, which leaves Magit in
  ;; Emacs state and steals `SPC' from our leader map. Re-initialize the Magit
  ;; integrations explicitly so leader-based buffer switching still works inside
  ;; Magit.
  (when (fboundp 'evil-collection-init)
    (evil-collection-init '(magit magit-section)))
  (setq magit-diff-refine-hunk 'all
        magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
        magit-bury-buffer-function #'magit-mode-quit-window
        transient-display-buffer-action
        '(display-buffer-below-selected
          (dedicated . t)
          (inhibit-same-window . t))
        transient-show-during-minibuffer-read t)
  (add-hook 'magit-diff-mode-hook #'chief/magit-enable-native-delta-h)
  (add-hook 'magit-revision-mode-hook #'chief/magit-enable-native-delta-h)
  (add-hook 'magit-log-mode-hook #'chief/magit-enable-native-delta-h)
  (add-hook 'magit-status-mode-hook #'chief/magit-enable-native-delta-h)
  (transient-define-prefix chief/magit-external-diff-dispatch ()
    "Open alternate Git diff viewers for the current repository."
    ["External diff viewers"
     ("d" "Delta diff (dwim)" chief/magit-delta-diff)
     ("s" "Delta show revision" chief/magit-delta-show)
     ("l" "Delta log patches" chief/magit-delta-log)
     ("t" "Difftastic diff (dwim)" chief/magit-difftastic-diff)
     ("T" "Difftastic show revision" chief/magit-difftastic-show)
     ("L" "Difftastic log patches" chief/magit-difftastic-log)])
  (transient-append-suffix 'magit-dispatch "!"
    '("#" "External diff viewers" chief/magit-external-diff-dispatch))
  (define-key magit-mode-map (kbd "#") #'chief/magit-external-diff-dispatch))

(use-package diff-hl
  :hook ((prog-mode . chief/enable-diff-hl-maybe)
         (conf-mode . chief/enable-diff-hl-maybe)
         (text-mode . chief/enable-diff-hl-maybe)
         (dired-mode . diff-hl-dired-mode-unless-remote)
         (diff-hl-mode . (lambda ()
                           (unless (display-graphic-p)
                             (diff-hl-margin-mode 1)))))
  :preface
  (defun chief/enable-diff-hl-maybe ()
    "Enable `diff-hl-mode' only where it stays lightweight."
    (when (and buffer-file-name
               (not (file-remote-p buffer-file-name))
               (not (and (fboundp 'chief/large-buffer-p)
                         (chief/large-buffer-p))))
      (diff-hl-mode 1)))
  :config
  (setq diff-hl-draw-borders nil)
  (setq diff-hl-side 'left)
  (diff-hl-flydiff-mode 1)
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

(use-package hl-todo
  :hook ((prog-mode . hl-todo-mode)
         (conf-mode . hl-todo-mode)
         (text-mode . hl-todo-mode))
  :config
  (setq hl-todo-keyword-faces
        '(("TODO" . "#ffb86c")
          ("FIXME" . "#ff6b6b")
          ("HACK" . "#c792ea")
          ("NOTE" . "#8be9fd")
          ("REVIEW" . "#ffd866")
          ("PERF" . "#7ee787")
          ("BUG" . "#ff5555"))))

(use-package agent-shell
  :commands (agent-shell)
  :config
  (when (require 'agent-shell-openai nil t)
    (when (fboundp 'agent-shell-openai-make-codex-config)
      (setq agent-shell-preferred-agent-config
            (agent-shell-openai-make-codex-config))))
  (chief/leader-def
    "a" '(:ignore t :which-key "agent")
    "aa" #'agent-shell
    "ac" #'agent-shell-openai-start-codex
    "ag" #'agent-shell-google-start-gemini
    "ao" #'agent-shell-opencode-start-agent
    "ar" #'agent-shell-cursor-start-agent
    "ap" #'agent-shell-pi-start-agent))

(provide 'core-tools)
;;; core-tools.el ends here
