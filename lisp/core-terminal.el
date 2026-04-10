;;; core-terminal.el --- Terminal-specific behavior -*- lexical-binding: t; -*-

(require 'subr-x)

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

(use-package ghostel
  :straight (ghostel
             :type git
             :host github
             :repo "dakra/ghostel"))

(provide 'core-terminal)
;;; core-terminal.el ends here
