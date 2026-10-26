;;; lang-sharplsp-razor.el --- Project-scoped Razor C# and markup support -*- lexical-binding: t; -*-

(require 'sgml-mode)
(defvar-local chief/sharplsp-razor-html-buffer nil)

(defun chief/sharplsp-razor-format ()
  "Indent Razor markup and embedded code without passing it to a C# formatter."
  (interactive)
  ;; web-mode-buffer-indent indexes current-time as a list; Emacs 32 may
  ;; return (TICKS . HZ). Use the same indentation work without its debug timer.
  (atomic-change-group
    (save-excursion
      (indent-region (point-min) (point-max))
      (delete-trailing-whitespace))))

(defun chief/sharplsp-razor-kill-projection ()
  (when (buffer-live-p chief/sharplsp-razor-html-buffer)
    (kill-buffer chief/sharplsp-razor-html-buffer)))

(defun chief/sharplsp-razor-html-completion ()
  "Complete HTML tags/attributes against the compiler's Razor HTML projection.
The real Razor source is never replaced by its masked HTML view."
  (when (and (chief/sharplsp-razor-p) (chief/sharplsp-workspace))
    (let* ((source (current-buffer))
           (position (chief/sharplsp-point-position (point)))
           (projection (chief/sharplsp-rpc
                        "sharplsp/razor/htmlProjection"
                        (list :textDocument (lsp--text-document-identifier))))
           (text (chief/sharplsp-field projection "text")))
      (when (stringp text)
        (unless (buffer-live-p chief/sharplsp-razor-html-buffer)
          (setq chief/sharplsp-razor-html-buffer (generate-new-buffer " *Razor HTML*"))
          (with-current-buffer chief/sharplsp-razor-html-buffer (delay-mode-hooks (html-mode))))
        (let ((buffer chief/sharplsp-razor-html-buffer) completion start end)
          (with-current-buffer buffer
            (erase-buffer) (insert text) (set-buffer-modified-p nil)
            (goto-char (chief/sharplsp-position-point position))
            (setq completion (html-mode--complete-at-point))
            (when completion
              (setq start (chief/sharplsp-point-position (nth 0 completion))
                    end (chief/sharplsp-point-position (nth 1 completion)))))
          (when completion
            (list (with-current-buffer source (chief/sharplsp-position-point start))
                  (with-current-buffer source (chief/sharplsp-position-point end))
                  (lambda (string predicate action)
                    (when (buffer-live-p buffer)
                      (with-current-buffer buffer
                        (complete-with-action action (nth 2 completion) string predicate))))
                  :exclusive 'no)))))))

(defun chief/sharplsp-razor-setup-a (function &rest args)
  "Use SharpLsp for explicitly opted-in Razor files; leave all other web files alone."
  (if-let* (((chief/sharplsp-razor-p)) (root (chief/sharplsp-preferred-root)))
      (progn
        (setq-local compile-command "dotnet build" web-mode-engine "razor"
                    lsp-language-id-configuration
                    (cons '("\\.\\(?:razor\\|cshtml\\)\\'" . "razor")
                          lsp-language-id-configuration)
                    chief/lsp-definition-function #'chief/csharp-goto-definition
                    chief/lsp-definition-other-window-function #'chief/csharp-goto-definition-other-window)
        (chief/sharplsp-setup root)
        (chief/polyglot-completion-setup t)
        (chief/format-enable-on-save #'chief/sharplsp-razor-format)
        (add-hook 'completion-at-point-functions #'chief/sharplsp-razor-html-completion 20 t)
        (add-hook 'after-save-hook #'chief/sharplsp-hot-reload-after-save nil t)
        (add-hook 'kill-buffer-hook #'chief/sharplsp-razor-kill-projection nil t))
    (apply function args)))

(advice-add 'chief/dotnet-razor-mode-setup :around #'chief/sharplsp-razor-setup-a)

(provide 'lang-sharplsp-razor)
;;; lang-sharplsp-razor.el ends here
