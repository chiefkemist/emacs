;;; core-terminal.el --- Terminal-specific behavior -*- lexical-binding: t; -*-

(require 'subr-x)

(declare-function evil-insert-state "evil-states" ())
(declare-function evil-normal-state "evil-states" ())
(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function ghostel-copy-mode "ghostel" ())
(declare-function ghostel-copy-mode-exit "ghostel" ())
(declare-function ghostel-send-next-key "ghostel" ())
(declare-function ghostel-yank "ghostel" ())
(defvar evil-state)
(defvar ghostel--copy-mode-active)
(defvar ghostel-copy-mode-map)
(defvar ghostel-mode-map)

(defun chief/terminal-frame-p (&optional frame)
  "Return non-nil when FRAME is a terminal frame."
  (not (display-graphic-p frame)))

(defun chief/tty-clipboard-programs ()
  "Return clipboard programs available for the current system, if any."
  (cond
   ((and (eq system-type 'darwin)
         (executable-find "pbcopy")
         (executable-find "pbpaste"))
    '(:copy ("pbcopy")
      :paste ("pbpaste")))
   ((and (executable-find "wl-copy")
         (executable-find "wl-paste"))
    '(:copy ("wl-copy")
      :paste ("wl-paste" "--no-newline")))
   ((executable-find "xclip")
    '(:copy ("xclip" "-selection" "clipboard")
      :paste ("xclip" "-selection" "clipboard" "-o")))
   (t nil)))

(defun chief/tty-copy-to-clipboard (text &optional _push)
  "Copy TEXT to the terminal clipboard helper, if configured."
  (when-let* ((programs (chief/tty-clipboard-programs)))
    (pcase-let ((`(,copy-program . ,copy-args) (plist-get programs :copy)))
      (with-temp-buffer
        (insert text)
        (apply #'call-process-region
               (point-min)
               (point-max)
               copy-program
               nil
               nil
               nil
               copy-args)))))

(defun chief/tty-paste-from-clipboard ()
  "Read text from the terminal clipboard helper, if configured."
  (when-let* ((programs (chief/tty-clipboard-programs))
              (paste-command (plist-get programs :paste)))
    (string-trim-right
     (with-temp-buffer
       (apply #'call-process
              (car paste-command)
              nil
              t
              nil
              (cdr paste-command))
       (buffer-string)))))

(defun chief/terminal-setup (&optional frame)
  "Configure terminal behavior for FRAME."
  (when (and (not noninteractive)
             (chief/terminal-frame-p frame))
    (with-selected-frame (or frame (selected-frame))
      (xterm-mouse-mode 1)
      (mouse-wheel-mode 1)
      (setq select-enable-clipboard t
            select-enable-primary t
            save-interprogram-paste-before-kill t)
      (when (boundp 'tty-select-active-regions)
        (setq tty-select-active-regions t))
      (when (chief/tty-clipboard-programs)
        (setq interprogram-cut-function #'chief/tty-copy-to-clipboard
              interprogram-paste-function #'chief/tty-paste-from-clipboard)))))

(add-hook 'tty-setup-hook #'chief/terminal-setup)
(add-hook 'after-make-frame-functions #'chief/terminal-setup)

(defun chief/ghostel-command-state-setup ()
  "Make a Ghostel buffer safe for Emacs/Evil command-state interaction.
Ghostel's copy mode freezes terminal output, installs a navigation-friendly
keymap, restores ordinary Emacs mouse selection, and prevents Evil commands
from accidentally editing the rendered terminal contents."
  (when (and (derived-mode-p 'ghostel-mode)
             (not (bound-and-true-p ghostel--copy-mode-active)))
    (ghostel-copy-mode)))

(defun chief/ghostel-insert-state-setup ()
  "Return a Ghostel buffer to live terminal input in Evil insert state."
  (when (and (derived-mode-p 'ghostel-mode)
             (bound-and-true-p ghostel--copy-mode-active))
    (ghostel-copy-mode-exit)))

(defun chief/ghostel-sync-evil-state (&rest _)
  "Keep Ghostel copy mode and Evil command/insert states synchronized."
  (when (and (derived-mode-p 'ghostel-mode)
             (bound-and-true-p evil-local-mode))
    (if (bound-and-true-p ghostel--copy-mode-active)
        (when (eq evil-state 'insert)
          (evil-normal-state))
      (unless (eq evil-state 'insert)
        (evil-insert-state)))))

(defun chief/ghostel-send-next-key ()
  "Resume terminal input and send Ghostel's next key literally.
This is the Doom-style escape hatch for keys normally reserved by Emacs."
  (interactive)
  (when (bound-and-true-p ghostel--copy-mode-active)
    (ghostel-copy-mode-exit))
  (ghostel-send-next-key))

(defun chief/ghostel-yank ()
  "Resume terminal input and paste the latest Emacs kill into Ghostel."
  (interactive)
  (when (bound-and-true-p ghostel--copy-mode-active)
    (ghostel-copy-mode-exit))
  (ghostel-yank))

(defun chief/ghostel-mode-setup (&optional preserve-state)
  "Apply Doom-style modal terminal behavior to the current Ghostel buffer.
Insert state belongs to the terminal.  Normal or Emacs state enters Ghostel's
read-only copy mode, where Evil, Emacs prefix keys, and mouse selection work.
When PRESERVE-STATE is non-nil, do not force an already-live buffer to insert
state."
  (setq-local confirm-kill-processes nil)
  (when (boundp 'evil-move-cursor-back)
    (setq-local evil-move-cursor-back nil))
  (add-hook 'evil-normal-state-entry-hook
            #'chief/ghostel-command-state-setup nil t)
  (add-hook 'evil-emacs-state-entry-hook
            #'chief/ghostel-command-state-setup nil t)
  (add-hook 'evil-insert-state-entry-hook
            #'chief/ghostel-insert-state-setup nil t)
  (when (and (not preserve-state)
             (bound-and-true-p evil-local-mode)
             (fboundp 'evil-insert-state))
    (evil-insert-state)))

(defun chief/ghostel-repair-buffer-state ()
  "Apply terminal setup and synchronize an already-live Ghostel buffer."
  (chief/ghostel-mode-setup t)
  (pcase evil-state
    ('insert (chief/ghostel-insert-state-setup))
    ((or 'normal 'motion 'visual 'emacs)
     (chief/ghostel-command-state-setup))))

(use-package ghostel
  :straight (ghostel
             :type git
             :host github
             :repo "dakra/ghostel")
  :commands (ghostel ghostel-project ghostel-other)
  :hook (ghostel-mode . chief/ghostel-mode-setup)
  :config
  ;; Doom's vterm integration reserves C-q as a one-shot literal-key escape
  ;; hatch and starts terminal buffers in Evil insert state.  Ghostel's copy
  ;; mode supplies the matching safe command state, including ordinary Emacs
  ;; mouse selection instead of forwarding clicks to a terminal application.
  (define-key ghostel-mode-map (kbd "C-q") #'chief/ghostel-send-next-key)
  (define-key ghostel-copy-mode-map (kbd "C-q") #'chief/ghostel-send-next-key)
  (define-key ghostel-mode-map (kbd "C-c C-k") #'kill-current-buffer)
  (define-key ghostel-copy-mode-map (kbd "C-c C-k") #'kill-current-buffer)
  ;; Unlike Ghostel's default, Doom's vterm leaves ordinary mouse clicks and
  ;; drag selection with Emacs.  Keep terminal-specific wheel scrolling, but do
  ;; not let terminal applications swallow window selection or mouse regions.
  (dolist (key '("<down-mouse-1>" "<mouse-1>" "<drag-mouse-1>"
                 "<down-mouse-2>" "<mouse-2>" "<drag-mouse-2>"
                 "<down-mouse-3>" "<mouse-3>" "<drag-mouse-3>"))
    (define-key ghostel-mode-map (kbd key) nil))
  (advice-add #'ghostel-copy-mode :after #'chief/ghostel-sync-evil-state)
  (advice-add #'ghostel-copy-mode-exit :after #'chief/ghostel-sync-evil-state)
  (with-eval-after-load 'evil
    (evil-set-initial-state 'ghostel-mode 'insert)
    (evil-define-key 'insert ghostel-mode-map
      (kbd "<escape>") #'evil-normal-state
      (kbd "C-q") #'chief/ghostel-send-next-key)
    (evil-define-key 'normal ghostel-copy-mode-map
      (kbd "q") #'quit-window
      (kbd "p") #'chief/ghostel-yank
      (kbd "P") #'chief/ghostel-yank))
  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'ghostel-mode)
        (chief/ghostel-repair-buffer-state)))))

(provide 'core-terminal)
;;; core-terminal.el ends here
