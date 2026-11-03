;;; core-backup-test.el --- Backup and LSP rename regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'core-bootstrap)
(require 'lsp-mode)

(ert-deftest chief/backups-do-not-notify-lsp-of-file-renames ()
  "Saving JS/TS must create backups without triggering LSP import rewrites."
  (dolist (extension '(".js" ".jsx" ".ts" ".tsx"))
    (let* ((root (make-temp-file "chief-backup-" t))
           (default-directory (file-name-as-directory root))
           (source (expand-file-name (concat "index" extension) root))
           (backup-directory (expand-file-name "backups/" root))
           (backup-directory-alist `(("." . ,backup-directory)))
           (backup-enable-predicate (lambda (_file) t))
           (version-control 'never)
           (original "import { assets } from './assets.js'\n")
           requests)
      (unwind-protect
          (progn
            (make-directory backup-directory)
            (write-region original nil source nil 'silent)
            ;; Exercise lsp-mode's real rename-file advice with a mock server.
            (cl-letf (((symbol-function 'lsp--send-will-rename-files-p)
                       (lambda (path) (equal path source)))
                      ((symbol-function 'lsp-request)
                       (lambda (method &rest _)
                         (push method requests)
                         nil)))
              (with-temp-buffer
                (insert-file-contents source t)
                (goto-char (point-max))
                (insert "// saved\n")
                (save-buffer)))
            (should-not requests)
            (should (file-exists-p source))
            (should (equal (with-temp-buffer
                             (insert-file-contents source)
                             (buffer-string))
                           (concat original "// saved\n")))
            (should (equal (with-temp-buffer
                             (insert-file-contents (make-backup-file-name source))
                             (buffer-string))
                           original)))
        (delete-directory root t)))))

(ert-deftest chief/real-file-renames-still-notify-lsp ()
  "Preventing backup renames must not disable real LSP file-move support."
  (let* ((root (make-temp-file "chief-rename-" t))
         (source (expand-file-name "before.ts" root))
         (destination (expand-file-name "after.ts" root))
         requests)
    (unwind-protect
        (progn
          (write-region "export const value = 1\n" nil source nil 'silent)
          (cl-letf (((symbol-function 'lsp--send-will-rename-files-p)
                     (lambda (path) (equal path source)))
                    ((symbol-function 'lsp-request)
                     (lambda (method &rest _)
                       (push method requests)
                       nil)))
            (rename-file source destination))
          (should (equal requests '("workspace/willRenameFiles")))
          (should-not (file-exists-p source))
          (should (file-exists-p destination)))
      (delete-directory root t))))

(provide 'core-backup-test)
;;; core-backup-test.el ends here
