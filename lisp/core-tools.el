;;; core-tools.el --- Daily tooling -*- lexical-binding: t; -*-

(declare-function chief/large-buffer-p "core-ui" (&optional threshold))

(use-package envrc
  :defer 1
  :config
  (envrc-global-mode))

(use-package magit
  :commands (magit-status magit-blame-addition)
  :config
  (chief/leader-def
    "g" '(:ignore t :which-key "git")
    "gs" #'magit-status
    "gb" #'magit-blame-addition))

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
  :commands (agent-shell agent-shell-openai-start-codex)
  :config
  (when (require 'agent-shell-openai nil t)
    (when (fboundp 'agent-shell-openai-make-codex-config)
      (setq agent-shell-preferred-agent-config
            (agent-shell-openai-make-codex-config))))
  (chief/leader-def
    "a" '(:ignore t :which-key "agent")
    "aa" #'agent-shell
    "ac" #'agent-shell-openai-start-codex))

(provide 'core-tools)
;;; core-tools.el ends here
