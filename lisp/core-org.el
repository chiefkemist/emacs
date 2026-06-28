;;; core-org.el --- Org mode setup -*- lexical-binding: t; -*-

(require 'subr-x)

(defun chief/org--block-spec-parts (spec)
  "Return normalized Org block SPEC and its block name."
  (let* ((trimmed (string-trim spec))
         (normalized (replace-regexp-in-string
                      "\\`\\(?:#\\+\\)?begin_" "" trimmed)))
    (unless (string-match-p "\\S-" normalized)
      (user-error "Org block name is required"))
    (list normalized (car (split-string normalized "[ \t]+" t)))))

(defun chief/org-insert-block (spec)
  "Insert an Org begin/end block from SPEC.
Example SPEC: \"src rust :results output\".  When the region is active, wrap it
inside the inserted block."
  (interactive (list (read-string "Org block: " "src ")))
  (let* ((parts (chief/org--block-spec-parts spec))
         (normalized (car parts))
         (block-name (cadr parts)))
    (if (use-region-p)
        (let ((body (delete-and-extract-region (region-beginning) (region-end))))
          (insert "#+begin_" normalized "\n"
                  body
                  (if (string-suffix-p "\n" body) "" "\n")
                  "#+end_" block-name "\n"))
      (insert "#+begin_" normalized "\n")
      (save-excursion
        (insert "\n#+end_" block-name "\n")))))

(use-package org
  :straight nil
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-directory (expand-file-name "~/org/"))
  (setq org-agenda-files
        (list org-directory
              (expand-file-name "inbox.org" org-directory)
              (expand-file-name "projects.org" org-directory)
              (expand-file-name "notes.org" org-directory)))
  (setq org-startup-indented t)
  (setq org-hide-emphasis-markers t)
  (setq org-ellipsis " …")
  (setq org-log-done 'time)
  (setq org-return-follows-link t)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file+headline "inbox.org" "Tasks")
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
          ("n" "Note" entry
           (file+headline "notes.org" "Notes")
           "* %U %?\n")
          ("p" "Project task" entry
           (file+headline "projects.org" "Projects")
           "* TODO %?\nSCHEDULED: %^t\n")))
  (add-hook 'org-mode-hook #'visual-line-mode)
  (chief/leader-def
    "o" '(:ignore t :which-key "org")
    "oa" #'org-agenda
    "oc" #'org-capture
    "od" #'org-babel-detangle
    "ol" #'org-store-link)
  (chief/local-leader-def
    :keymaps 'org-mode-map
    "b" '(:ignore t :which-key "babel")
    "bi" #'chief/org-insert-block
    "be" #'org-babel-execute-src-block
    "bn" #'org-babel-next-src-block
    "bp" #'org-babel-previous-src-block
    "bt" #'org-babel-tangle
    "t" #'org-todo
    "s" #'org-schedule
    "d" #'org-deadline
    "p" #'org-priority))

(use-package org-modern
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

(use-package evil-org
  :after (evil org)
  :hook ((org-mode . evil-org-mode)
         (org-agenda-mode . evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(provide 'core-org)
;;; core-org.el ends here
