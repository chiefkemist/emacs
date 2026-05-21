;;; vercel-light-theme.el --- Vercel light theme -*- lexical-binding: t; -*-

;; Ported from Nathan Brodin's Zed Vercel theme:
;; https://github.com/NathanBrodin/zed-vercel-theme

(deftheme vercel-light
  "A light Emacs port of the Vercel theme for Zed.")

(let* ((theme-file (or load-file-name
                       buffer-file-name
                       (and (fboundp 'custom-theme--load-path)
                            (locate-file "vercel-light-theme.el"
                                         (custom-theme--load-path)
                                         '("" "c")))))
       (theme-directory (and theme-file (file-name-directory theme-file)))
       (kit-file (and theme-directory
                      (expand-file-name "chief-theme-kit.el" theme-directory)))
       (support-file (and theme-directory
                          (expand-file-name "vercel-theme-support.el" theme-directory))))
  (unless (and kit-file (file-readable-p kit-file))
    (error "Unable to locate chief-theme-kit.el for vercel-light"))
  (unless (and support-file (file-readable-p support-file))
    (error "Unable to locate vercel-theme-support.el for vercel-light"))
  (load kit-file nil t)
  (load support-file nil t))

(declare-function vercel-theme-apply "vercel-theme-support" (theme palette))

(defconst vercel-light-theme-palette
  '((bg . "#ffffff")
    (surface . "#ffffff")
    (surface-variant . "#f5f5f5")
    (element . "#ebebeb")
    (hover . "#e5e5e5")
    (border . "#ebebeb")
    (fg . "#171717")
    (muted . "#666666")
    (line-number . "#a8a8a8")
    (ignored . "#bbbbbb")
    (black . "#171717")
    (bright-black . "#666666")
    (red . "#bd2864")
    (green . "#297a3a")
    (yellow . "#a35200")
    (blue . "#0068d6")
    (magenta . "#7c00c7")
    (cyan . "#449999")
    (white . "#666666")
    (bright-white . "#666666")
    ;; Solid blends of Zed's 20% alpha colors over #ffffff.
    (selection . "#cce1f7")
    (error-bg . "#f2d4e0")
    (success-bg . "#d4e4d8")
    (warning-bg . "#eddccc")
    (hint-bg . "#e0e0e0")
    (inverse-fg . "#ffffff")
    (inlay-bg . "#f5f5f5")
    (subtle-bg . "#f5f5f5"))
  "Palette for `vercel-light'.")

(vercel-theme-apply 'vercel-light vercel-light-theme-palette)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'vercel-light)

;;; vercel-light-theme.el ends here
