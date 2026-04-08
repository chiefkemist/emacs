;;; core-bootstrap.el --- Package bootstrap -*- lexical-binding: t; -*-

(require 'cl-lib)

(defgroup chief nil
  "Chiefkemist Emacs configuration."
  :group 'convenience)

(defcustom chief/bootstrap-packages t
  "When non-nil, bootstrap straight.el automatically."
  :type 'boolean
  :group 'chief)

(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups/" chief/var-directory))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" chief/var-directory) t)))
(setq lock-file-name-transforms
      `((".*" ,(expand-file-name "locks/" chief/var-directory) t)))

(dolist (directory '("backups/" "auto-save/" "locks/"))
  (make-directory (expand-file-name directory chief/var-directory) t))

(setq straight-base-dir chief/var-directory)
(setq straight-use-package-by-default t)
(setq straight-check-for-modifications '(check-on-save find-when-checking))
(setq straight-cache-autoloads t)

(when chief/bootstrap-packages
  (defvar bootstrap-version)
  (let* ((bootstrap-file
          (expand-file-name
           "straight/repos/straight.el/bootstrap.el"
           straight-base-dir))
         (bootstrap-version 7))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent
           'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(when chief/bootstrap-packages
  (straight-use-package 'use-package))

(require 'use-package)
(setq use-package-always-defer t)
(setq use-package-expand-minimally t)

(defmacro chief/safe-use-package (name &rest args)
  "Initialize package NAME like `use-package', but downgrade failures to warnings."
  (declare (indent defun))
  `(condition-case err
       (use-package ,name ,@args)
     (error
      (display-warning
       'chief
       (format "Failed to initialize package %s: %s"
               ',name
               (error-message-string err))
       :warning))))

(provide 'core-bootstrap)
;;; core-bootstrap.el ends here
