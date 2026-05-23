;;; blue-theme.el --- Neovim blue theme -*- lexical-binding: t; -*-

;; Ported from Neovim's blue colorscheme:
;; https://github.com/neovim/neovim/blob/master/runtime/colors/blue.vim

(deftheme blue
  "A blue-background Emacs port of Neovim's blue colorscheme.")

(let* ((theme-file (or load-file-name
                       buffer-file-name
                       (and (fboundp 'custom-theme--load-path)
                            (locate-file "blue-theme.el"
                                         (custom-theme--load-path)
                                         '("" "c")))))
       (theme-directory (and theme-file (file-name-directory theme-file)))
       (kit-file (and theme-directory
                      (expand-file-name "chief-theme-kit.el" theme-directory)))
       (support-file (and theme-directory
                          (expand-file-name "blue-theme-support.el" theme-directory))))
  (unless (and kit-file (file-readable-p kit-file))
    (error "Unable to locate chief-theme-kit.el for blue"))
  (unless (and support-file (file-readable-p support-file))
    (error "Unable to locate blue-theme-support.el for blue"))
  (load kit-file nil t)
  (load support-file nil t))

(declare-function blue-theme-apply "blue-theme-support" (theme palette))

(defconst blue-theme-palette
  '((bg . "#000087")
    (fg . "#ffd700")
    (surface . "#000087")
    (surface-variant . "#0f3f77")
    (element . "#1f4f87")
    (hover . "#005faf")
    (border . "#008787")
    (color-column . "#870087")
    (cursor . "#00ff00")
    (cursor-fg . "#000000")
    (cursor-im . "#ffd700")
    (comment . "#878787")
    (conceal . "#008787")
    (constant . "#5fffff")
    (directory . "#5fffff")
    (error . "#ff7f50")
    (error-bg . "#000087")
    (error-message-bg . "#d70000")
    (fold-column . "#008787")
    (folded-fg . "#000087")
    (folded-bg . "#878700")
    (identifier . "#bcbcbc")
    (ignored . "#878787")
    (inc-search-fg . "#d787d7")
    (inc-search-bg . "#000000")
    (label . "#ffd700")
    (line-number . "#5fffff")
    (mode-message-fg . "#000087")
    (mode-message-bg . "#00ff00")
    (nontext . "#d787d7")
    (menu-fg . "#ffffff")
    (menu-bg . "#008787")
    (menu-match-fg . "#ffd700")
    (menu-selection-fg . "#008787")
    (menu-selection-bg . "#ffffff")
    (preproc . "#00ff00")
    (quickfix-fg . "#000000")
    (quickfix-bg . "#d787d7")
    (search-fg . "#ffd700")
    (search-bg . "#000000")
    (sign-column . "#008787")
    (special . "#d787d7")
    (status-fg . "#000087")
    (status-bg . "#5fffff")
    (status-inactive-bg . "#008787")
    (title . "#d787d7")
    (titlebar-bg . "#1f4f87")
    (titlebar-nc-bg . "#0f3f77")
    (titlebar-nc-fg . "#878787")
    (toolbar-bg . "#005faf")
    (type . "#ffa500")
    (visual-fg . "#ffffff")
    (visual-bg . "#008787")
    (visual-nos-fg . "#008787")
    (visual-nos-bg . "#ffffff")
    (warning . "#d787d7")
    (wild-menu-fg . "#000087")
    (wild-menu-bg . "#ffd700")
    (diff-add-bg . "#5f875f")
    (diff-change-bg . "#5f87af")
    (diff-delete-bg . "#af5faf")
    (diff-text-bg . "#c6c6c6")
    (inlay-bg . "#0f3f77")
    (inverse-fg . "#000000")
    ;; Neovim blue terminal palette from g:terminal_ansi_colors.
    (ansi-black . "#000000")
    (ansi-red . "#cd0000")
    (ansi-green . "#00cd00")
    (ansi-yellow . "#cdcd00")
    (ansi-blue . "#0000ee")
    (ansi-magenta . "#cd00cd")
    (ansi-cyan . "#00cdcd")
    (ansi-white . "#e5e5e5")
    (ansi-bright-black . "#7f7f7f")
    (ansi-bright-red . "#ff0000")
    (ansi-bright-green . "#00ff00")
    (ansi-bright-yellow . "#ffff00")
    (ansi-bright-blue . "#5c5cff")
    (ansi-bright-magenta . "#ff00ff")
    (ansi-bright-cyan . "#00ffff")
    (ansi-bright-white . "#ffffff"))
  "Palette for `blue'.")

(blue-theme-apply 'blue blue-theme-palette)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'blue)

;;; blue-theme.el ends here
