;;; core-repl.el --- Shared REPL helpers -*- lexical-binding: t; -*-

(defvar-local chief/repl-start-function nil
  "Function used to start or switch to the language REPL for the current buffer.")

(defvar-local chief/repl-restart-function nil
  "Function used to restart the language REPL for the current buffer.")

(defvar-local chief/repl-send-line-function nil
  "Function used to send the current line to the language REPL.")

(defvar-local chief/repl-send-region-function nil
  "Function used to send the active region to the language REPL.")

(defvar-local chief/repl-send-buffer-function nil
  "Function used to send the current buffer to the language REPL.")

(defvar-local chief/repl-send-defun-function nil
  "Function used to send the current defun or block to the language REPL.")

(defvar-local chief/repl-load-file-function nil
  "Function used to load the current file into the language REPL.")

(defun chief/repl--callable-p (fn)
  "Return non-nil when FN can be called."
  (cond
   ((null fn) nil)
   ((and (symbolp fn) (fboundp fn)) t)
   ((functionp fn) t)
   (t nil)))

(defun chief/repl--resolve (fn)
  "Return FN when it is callable, otherwise nil."
  (when (chief/repl--callable-p fn)
    fn))

(defun chief/repl-configure (&rest plist)
  "Configure the current buffer's REPL integration from PLIST."
  (setq-local chief/repl-start-function (plist-get plist :start))
  (setq-local chief/repl-restart-function (plist-get plist :restart))
  (setq-local chief/repl-send-line-function (plist-get plist :send-line))
  (setq-local chief/repl-send-region-function (plist-get plist :send-region))
  (setq-local chief/repl-send-buffer-function (plist-get plist :send-buffer))
  (setq-local chief/repl-send-defun-function (plist-get plist :send-defun))
  (setq-local chief/repl-load-file-function (plist-get plist :load-file)))

(defun chief/repl-current-line-bounds ()
  "Return the bounds of the current logical line."
  (cons (line-beginning-position) (line-end-position)))

(defun chief/repl-start ()
  "Start or switch to the current buffer's REPL."
  (interactive)
  (if-let* ((fn (chief/repl--resolve chief/repl-start-function)))
      (call-interactively fn)
    (user-error "No REPL starter is configured for %s" major-mode)))

(defun chief/repl-restart ()
  "Restart the current buffer's REPL."
  (interactive)
  (if-let* ((fn (chief/repl--resolve chief/repl-restart-function)))
      (call-interactively fn)
    (user-error "No REPL restart command is configured for %s" major-mode)))

(defun chief/repl-send-line ()
  "Send the current line to the current buffer's REPL."
  (interactive)
  (cond
   ((chief/repl--resolve chief/repl-send-line-function)
    (call-interactively chief/repl-send-line-function))
   ((chief/repl--resolve chief/repl-send-region-function)
    (pcase-let ((`(,start . ,end) (chief/repl-current-line-bounds)))
      (funcall chief/repl-send-region-function start end)))
   (t
    (user-error "No line send command is configured for %s" major-mode))))

(defun chief/repl-send-region (start end)
  "Send the active region from START to END to the current buffer's REPL."
  (interactive "r")
  (if-let* ((fn (chief/repl--resolve chief/repl-send-region-function)))
      (funcall fn start end)
    (user-error "No region send command is configured for %s" major-mode)))

(defun chief/repl-send-buffer ()
  "Send the current buffer to the current buffer's REPL."
  (interactive)
  (cond
   ((chief/repl--resolve chief/repl-send-buffer-function)
    (call-interactively chief/repl-send-buffer-function))
   ((chief/repl--resolve chief/repl-send-region-function)
    (funcall chief/repl-send-region-function (point-min) (point-max)))
   (t
    (user-error "No buffer send command is configured for %s" major-mode))))

(defun chief/repl-send-defun ()
  "Send the current defun or block to the current buffer's REPL."
  (interactive)
  (cond
   ((chief/repl--resolve chief/repl-send-defun-function)
    (call-interactively chief/repl-send-defun-function))
   ((chief/repl--resolve chief/repl-send-region-function)
    (save-excursion
      (mark-defun)
      (funcall chief/repl-send-region-function (region-beginning) (region-end))
      (deactivate-mark)))
   (t
    (user-error "No defun send command is configured for %s" major-mode))))

(defun chief/repl-load-file ()
  "Load the current file into the current buffer's REPL."
  (interactive)
  (cond
   ((chief/repl--resolve chief/repl-load-file-function)
    (call-interactively chief/repl-load-file-function))
   ((chief/repl--resolve chief/repl-send-buffer-function)
    (call-interactively chief/repl-send-buffer-function))
   (t
    (user-error "No file load command is configured for %s" major-mode))))

(defun chief/repl-setup-standard-local-leader (keymap)
  "Install standard local leader REPL bindings into KEYMAP."
  (chief/local-leader-def
    :keymaps keymap
    "s" '(:ignore t :which-key "send/shell")
    "ss" #'chief/repl-start
    "sr" #'chief/repl-restart
    "sl" #'chief/repl-send-line
    "se" #'chief/repl-send-region
    "sb" #'chief/repl-send-buffer
    "sd" #'chief/repl-send-defun
    "sf" #'chief/repl-load-file))

(provide 'core-repl)
;;; core-repl.el ends here
