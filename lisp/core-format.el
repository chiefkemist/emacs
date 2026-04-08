;;; core-format.el --- Shared formatting helpers -*- lexical-binding: t; -*-

(defcustom chief/format-on-save t
  "When non-nil, format buffers automatically before saving when supported."
  :type 'boolean
  :group 'chief)

(defvar-local chief/format-buffer-function nil
  "Buffer-local formatter function used by `chief/format-buffer'.")

(defvar-local chief/format-on-save-enabled t
  "Whether `chief/format-before-save' should format the current buffer.")

(defun chief/format-function-callable-p (fn)
  "Return non-nil when FN is a callable formatter."
  (cond
   ((null fn) nil)
   ((and (symbolp fn) (fboundp fn)) t)
   ((functionp fn) t)
   (t nil)))

(defun chief/format-lsp-available-p ()
  "Return non-nil when the current buffer can be formatted via LSP."
  (and (bound-and-true-p lsp-managed-mode)
       (fboundp 'lsp-format-buffer)
       (or (not (fboundp 'lsp-feature?))
           (ignore-errors
             (lsp-feature? "textDocument/formatting")))))

(defun chief/format-buffer ()
  "Format the current buffer using an explicit formatter or LSP."
  (interactive)
  (cond
   ((chief/format-function-callable-p chief/format-buffer-function)
    (call-interactively chief/format-buffer-function))
   ((chief/format-lsp-available-p)
    (call-interactively #'lsp-format-buffer))
   (t
    (user-error "No formatter is configured for %s" major-mode))))

(defun chief/format-before-save ()
  "Format the current buffer before save when supported."
  (when (and chief/format-on-save
             chief/format-on-save-enabled
             buffer-file-name
             (not (file-remote-p buffer-file-name))
             (or (chief/format-function-callable-p chief/format-buffer-function)
                 (chief/format-lsp-available-p)))
    (chief/format-buffer)))

(define-minor-mode chief/format-on-save-mode
  "Minor mode that formats the current buffer before save."
  :lighter nil
  (if chief/format-on-save-mode
      (add-hook 'before-save-hook #'chief/format-before-save nil t)
    (remove-hook 'before-save-hook #'chief/format-before-save t)))

(defun chief/format-enable-on-save (&optional formatter)
  "Enable format-on-save for the current buffer, using FORMATTER when provided."
  (when formatter
    (setq-local chief/format-buffer-function formatter))
  (chief/format-on-save-mode 1))

(defun chief/format-disable-on-save ()
  "Disable format-on-save for the current buffer."
  (interactive)
  (setq-local chief/format-on-save-enabled nil)
  (chief/format-on-save-mode -1))

(provide 'core-format)
;;; core-format.el ends here
