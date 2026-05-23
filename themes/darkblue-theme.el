;;; darkblue-theme.el --- Neovim darkblue theme -*- lexical-binding: t; -*-

;; Ported from Neovim's darkblue colorscheme:
;; https://github.com/neovim/neovim/blob/master/runtime/colors/darkblue.vim

(deftheme darkblue
  "A dark Emacs port of Neovim's darkblue colorscheme.")

(let* ((theme-file (or load-file-name
                       buffer-file-name
                       (and (fboundp 'custom-theme--load-path)
                            (locate-file "darkblue-theme.el"
                                         (custom-theme--load-path)
                                         '("" "c")))))
       (theme-directory (and theme-file (file-name-directory theme-file)))
       (kit-file (and theme-directory
                      (expand-file-name "chief-theme-kit.el" theme-directory)))
       (support-file (and theme-directory
                          (expand-file-name "darkblue-theme-support.el" theme-directory))))
  (unless (and kit-file (file-readable-p kit-file))
    (error "Unable to locate chief-theme-kit.el for darkblue"))
  (unless (and support-file (file-readable-p support-file))
    (error "Unable to locate darkblue-theme-support.el for darkblue"))
  (load kit-file nil t)
  (load support-file nil t))

(declare-function darkblue-theme-apply "darkblue-theme-support" (theme palette))

(defconst darkblue-theme-palette
  '((bg . "#000040")
    (fg . "#c0c0c0")
    (surface . "#000040")
    (surface-variant . "#2c2c56")
    (element . "#3c3c66")
    (hover . "#666666")
    (border . "#808080")
    (color-column . "#8b0000")
    (cursor . "#ffff60")
    (cursor-fg . "#000000")
    (cursor-im . "#ffff60")
    (comment . "#80a0ff")
    (conceal . "#008b8b")
    (constant . "#ffa0a0")
    (directory . "#008b8b")
    (error . "#ffa0a0")
    (error-bg . "#000040")
    (error-message-bg . "#8b0000")
    (fold-column . "#808080")
    (folded-fg . "#808080")
    (folded-bg . "#000040")
    (identifier . "#90fff0")
    (ignored . "#808080")
    (inc-search-fg . "#ffffff")
    (inc-search-bg . "#0030ff")
    (label . "#ffff60")
    (line-number . "#90f020")
    (mode-message-fg . "#90fff0")
    (mode-message-bg . "#000040")
    (nontext . "#0030ff")
    (menu-fg . "#ffffff")
    (menu-bg . "#0030ff")
    (menu-match-fg . "#ff80ff")
    (menu-selection-fg . "#0030ff")
    (menu-selection-bg . "#ffffff")
    (preproc . "#ff80ff")
    (quickfix-fg . "#000000")
    (quickfix-bg . "#ff80ff")
    (search-fg . "#90fff0")
    (search-bg . "#0030ff")
    (sign-column . "#808080")
    (special . "#ffa500")
    (status-fg . "#000040")
    (status-bg . "#c0c0c0")
    (status-inactive-bg . "#808080")
    (title . "#ff00ff")
    (titlebar-bg . "#3c3c66")
    (titlebar-nc-bg . "#2c2c56")
    (titlebar-nc-fg . "#808080")
    (toolbar-bg . "#0030ff")
    (type . "#90f020")
    ;; Neovim marks Visual as reverse; these values approximate the displayed swap.
    (visual-fg . "#ffffff")
    (visual-bg . "#8080ff")
    (visual-nos-fg . "#c0c0c0")
    (visual-nos-bg . "#8080ff")
    (warning . "#ff0000")
    (wild-menu-fg . "#ffff60")
    (wild-menu-bg . "#000000")
    (diff-add-bg . "#5f875f")
    (diff-change-bg . "#5f87af")
    (diff-delete-bg . "#af5faf")
    (diff-text-bg . "#c6c6c6")
    (inlay-bg . "#2c2c56")
    (inverse-fg . "#000000")
    ;; Neovim darkblue terminal palette from g:terminal_ansi_colors.
    (ansi-black . "#000000")
    (ansi-red . "#8b0000")
    (ansi-green . "#90f020")
    (ansi-yellow . "#ffa500")
    (ansi-blue . "#00008b")
    (ansi-magenta . "#8b008b")
    (ansi-cyan . "#008b8b")
    (ansi-white . "#c0c0c0")
    (ansi-bright-black . "#808080")
    (ansi-bright-red . "#ffa0a0")
    (ansi-bright-green . "#90f020")
    (ansi-bright-yellow . "#ffff60")
    (ansi-bright-blue . "#0030ff")
    (ansi-bright-magenta . "#ff00ff")
    (ansi-bright-cyan . "#90fff0")
    (ansi-bright-white . "#ffffff"))
  "Palette for `darkblue'.")

(darkblue-theme-apply 'darkblue darkblue-theme-palette)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'darkblue)

;;; darkblue-theme.el ends here
