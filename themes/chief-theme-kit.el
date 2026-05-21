;;; chief-theme-kit.el --- Reusable custom theme helpers -*- lexical-binding: t; -*-

;; Generic helpers for local custom themes.  This file intentionally contains no
;; color choices and no face-to-color policy.  Theme files own those decisions.

(defun chief-theme-kit-color (palette key &optional fallback)
  "Return KEY from PALETTE, or FALLBACK when provided."
  (let ((cell (assq key palette)))
    (cond
     (cell (cdr cell))
     ((not (null fallback)) fallback)
     (t (error "Palette is missing required color key: %S" key)))))

(defmacro chief-theme-kit-with-colors (palette keys &rest body)
  "Bind KEYS from PALETTE while evaluating BODY.
Each entry in KEYS is either a symbol, or (SYMBOL FALLBACK)."
  (declare (indent 2))
  `(let ,(mapcar (lambda (entry)
                   (if (consp entry)
                       `(,(car entry)
                         (chief-theme-kit-color ,palette ',(car entry) ,(cadr entry)))
                     `(,entry (chief-theme-kit-color ,palette ',entry))))
                 keys)
     ,@body))

(defun chief-theme-kit-apply (theme faces &optional variables)
  "Apply FACES and optional VARIABLES to THEME."
  (apply #'custom-theme-set-faces theme faces)
  (when variables
    (apply #'custom-theme-set-variables theme variables)))

(provide 'chief-theme-kit)

;;; chief-theme-kit.el ends here
