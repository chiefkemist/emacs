;;; vercel-dark-theme.el --- Vercel dark theme -*- lexical-binding: t; -*-

;; Ported from Nathan Brodin's Zed Vercel theme:
;; https://github.com/NathanBrodin/zed-vercel-theme

(deftheme vercel-dark
  "A dark Emacs port of the Vercel theme for Zed.")

(let* ((theme-file (or load-file-name
                       buffer-file-name
                       (and (fboundp 'custom-theme--load-path)
                            (locate-file "vercel-dark-theme.el"
                                         (custom-theme--load-path)
                                         '("" "c")))))
       (theme-directory (and theme-file (file-name-directory theme-file)))
       (kit-file (and theme-directory
                      (expand-file-name "chief-theme-kit.el" theme-directory)))
       (support-file (and theme-directory
                          (expand-file-name "vercel-theme-support.el" theme-directory))))
  (unless (and kit-file (file-readable-p kit-file))
    (error "Unable to locate chief-theme-kit.el for vercel-dark"))
  (unless (and support-file (file-readable-p support-file))
    (error "Unable to locate vercel-theme-support.el for vercel-dark"))
  (load kit-file nil t)
  (load support-file nil t))

(declare-function vercel-theme-apply "vercel-theme-support" (theme palette))

(defconst vercel-dark-theme-palette
  '((bg . "#0a0a0a")
    (surface . "#000000")
    (surface-variant . "#111111")
    (element . "#1f1f1f")
    (hover . "#1a1a1a")
    (border . "#2e2e2e")
    (fg . "#ededed")
    (muted . "#a1a1a1")
    (line-number . "#878787")
    (ignored . "#777777")
    (black . "#000000")
    (bright-black . "#676767")
    (red . "#f75f8f")
    (green . "#62c073")
    (yellow . "#ff9907")
    (blue . "#52a8ff")
    (magenta . "#c472fb")
    (cyan . "#1da9b0")
    (white . "#a1a1a1")
    (bright-white . "#ededed")
    ;; Solid blends of Zed's 20% alpha colors over #0a0a0a.
    (selection . "#182a3b")
    (error-bg . "#391b25")
    (success-bg . "#1c2e1f")
    (warning-bg . "#3b2709")
    (hint-bg . "#282828")
    (inverse-fg . "#000000")
    (inlay-bg . "#000000")
    (subtle-bg . "#111111"))
  "Palette for `vercel-dark'.")

(vercel-theme-apply 'vercel-dark vercel-dark-theme-palette)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'vercel-dark)

;;; vercel-dark-theme.el ends here
