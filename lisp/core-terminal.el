;;; core-terminal.el --- Terminal-specific behavior -*- lexical-binding: t; -*-

(require 'project)
(require 'subr-x)

(defcustom chief/floating-terminal-height 0.32
  "Height of the popup terminal window."
  :type 'float
  :group 'chief)

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

(defun chief/floating-terminal-buffer-name ()
  "Return the project-aware Eat buffer name for the current context."
  (if-let* ((project (project-current nil)))
      (project-prefixed-buffer-name "eat")
    (if (boundp 'eat-buffer-name)
        eat-buffer-name
      "*eat*")))

(defun chief/floating-terminal-window ()
  "Return the live window showing the current popup terminal, if any."
  (when-let* ((buffer (get-buffer (chief/floating-terminal-buffer-name))))
    (get-buffer-window buffer t)))

(defun chief/floating-terminal-display (buffer _alist)
  "Display BUFFER in a reusable popup terminal window."
  (display-buffer-in-side-window
   buffer
   `((side . bottom)
     (slot . -1)
     (window-height . ,chief/floating-terminal-height)
     (preserve-size . (t . t))
     (window-parameters . ((no-delete-other-windows . t)
                           (no-other-window . t))))))

(defun chief/toggle-floating-terminal (&optional arg)
  "Toggle a project-aware popup terminal.
With prefix ARG, create or switch to a numbered Eat session."
  (interactive "P")
  (require 'eat)
  (if-let* ((window (chief/floating-terminal-window)))
      (delete-window window)
    (let ((display-buffer-overriding-action
           (cons #'chief/floating-terminal-display nil)))
      (if (project-current nil)
          (eat-project arg)
        (eat nil arg)))))

(defun chief/open-floating-terminal-here (&optional arg)
  "Open a popup Eat terminal rooted at the current directory."
  (interactive "P")
  (require 'eat)
  (let ((display-buffer-overriding-action
         (cons #'chief/floating-terminal-display nil)))
    (eat nil arg)))

(add-hook 'tty-setup-hook #'chief/terminal-setup)
(add-hook 'after-make-frame-functions #'chief/terminal-setup)

(use-package eat
  :commands (eat eat-project)
  :hook (eat-mode . (lambda ()
                      (setq-local mode-line-format nil)
                      (setq-local truncate-lines t)
                      (display-line-numbers-mode -1)))
  :config
  (setq eat-kill-buffer-on-exit nil)
  (chief/leader-def
    "'" #'chief/toggle-floating-terminal
    "ot" #'chief/open-floating-terminal-here))

(provide 'core-terminal)
;;; core-terminal.el ends here
