;;; core-themes.el --- Theme collection and commands -*- lexical-binding: t; -*-

(require 'seq)

(defvar chief/theme-directory
  (expand-file-name "themes/" user-emacs-directory)
  "Directory containing local custom themes.")

(defun chief/ensure-theme-load-path ()
  "Ensure local custom themes are discoverable."
  (when (file-directory-p chief/theme-directory)
    (add-to-list 'custom-theme-load-path chief/theme-directory)))

(chief/ensure-theme-load-path)

(defvar chief/default-theme 'doom-tokyo-night)
(defvar chief/theme-preferences
  '(doom-tokyo-night
    vercel-dark
    vercel-light
    blue
    darkblue
    kanagawa-wave
    gruvbox-dark-medium
    gruber-darker
    catppuccin-macchiato
    everforest-hard-dark
    one-dark
    material
    material-deep-ocean
    monokai
    base16-tokyodark-terminal
    base16-everforest-dark-medium
    base16-gruvbox-dark-medium
    base16-material-darker
    doom-moonlight
    doom-material-dark
    doom-monokai-pro
    ef-symbiosis
    ef-night
    modus-vivendi-tinted))

(defun chief/ordered-themes ()
  "Return preferred themes in the order they are available."
  (chief/ensure-theme-load-path)
  (let ((available (custom-available-themes)))
    (or (seq-filter (lambda (theme) (memq theme available))
                    chief/theme-preferences)
        available)))

(defun chief/local-theme-file (theme)
  "Return local source file for THEME, when it exists."
  (let ((file (expand-file-name
               (format "%s-theme.el" (symbol-name theme))
               chief/theme-directory)))
    (when (file-readable-p file)
      file)))

(defun chief/load-local-theme-source-around (orig theme &optional no-confirm no-enable)
  "Load local THEME source files freshly before enabling them."
  (if-let* ((local-file (chief/local-theme-file theme)))
      (progn
        (load-file local-file)
        (unless no-enable
          (enable-theme theme))
        t)
    (funcall orig theme no-confirm no-enable)))

(advice-add 'load-theme :around #'chief/load-local-theme-source-around)

(defun chief/theme-apply-face-fixes (&optional frame)
  "Apply compatibility face overrides after theme loads.
This avoids a known Gnus face inheritance cycle exposed when packages such as
`templ-ts-mode' require `css-mode', which pulls in `eww' and `gnus'."
  (with-selected-frame (or frame (selected-frame))
    (ignore-errors
      (set-face-attribute 'gnus-group-mail-1-empty nil :inherit 'default)
      (set-face-attribute 'gnus-group-mail-1 nil :inherit 'gnus-group-mail-1-empty :weight 'bold)
      (set-face-attribute 'gnus-group-news-low-empty nil :inherit 'gnus-group-mail-1-empty :weight 'normal)
      (set-face-attribute 'gnus-group-news-low nil :inherit 'gnus-group-news-low-empty :weight 'bold))))

(defun chief/load-theme (theme)
  "Disable active themes and load THEME."
  (interactive
   (progn
     (chief/ensure-theme-load-path)
     (list
      (intern
       (completing-read
        "Load theme: "
        (mapcar #'symbol-name (custom-available-themes))
        nil
        t)))))
  (chief/ensure-theme-load-path)
  (let* ((available (custom-available-themes))
         (target (cond
                  ((memq theme available) theme)
                  ((eq theme chief/default-theme) (car (chief/ordered-themes)))
                  (t (user-error "Theme is not available: %s" theme)))))
    (unless target
      (user-error "No themes are currently available"))
    (mapc #'disable-theme custom-enabled-themes)
    (if-let* ((local-file (chief/local-theme-file target)))
        (progn
          ;; `consult-theme' and `load-theme' may reuse an already-loaded Custom
          ;; theme definition.  Local themes are under active development, so load
          ;; their source file directly and then enable the freshly evaluated
          ;; theme settings.
          (load-file local-file)
          (enable-theme target))
      (unless (load-theme target t)
        (user-error "Failed to load theme: %s" target)))
    (chief/theme-apply-face-fixes)
    (message "Theme: %s bg=%s fg=%s"
             target
             (face-background 'default nil t)
             (face-foreground 'default nil t))))

(defun chief/cycle-theme ()
  "Cycle through `chief/theme-preferences'."
  (interactive)
  (let* ((current (car custom-enabled-themes))
         (themes (chief/ordered-themes))
         (next (or (cadr (member current themes))
                   (car themes))))
    (chief/load-theme next)
    (message "Theme: %s" next)))

(defun chief/theme-status ()
  "Report configured themes that are available versus still missing."
  (interactive)
  (chief/ensure-theme-load-path)
  (let* ((available (custom-available-themes))
         (present (seq-filter (lambda (theme) (memq theme available))
                              chief/theme-preferences))
         (missing (seq-remove (lambda (theme) (memq theme available))
                              chief/theme-preferences)))
    (message "Available preferred themes: %s | Missing preferred themes: %s"
             (mapconcat #'symbol-name present ", ")
             (if missing
                 (mapconcat #'symbol-name missing ", ")
               "none"))))

(chief/safe-use-package doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (doom-themes-org-config))

(chief/safe-use-package ef-themes
  :defer t)

(chief/safe-use-package gruber-darker-theme
  :defer t)

(chief/safe-use-package gruvbox-theme
  :defer t
  :custom
  (gruvbox-contrast 'medium))

(chief/safe-use-package catppuccin-theme
  :defer t
  :straight
  (catppuccin-theme
   :type git
   :host github
   :repo "catppuccin/emacs"
   :files ("*.el")))

(chief/safe-use-package kanagawa-themes
  :defer t
  :straight
  (kanagawa-themes
   :type git
   :host github
   :repo "Fabiokleis/kanagawa-emacs"
   :local-repo "kanagawa-emacs"
   :files ("*.el"))
  :custom
  (kanagawa-themes-org-height nil))

(chief/safe-use-package everforest-theme
  :defer t
  :straight
  (everforest-theme
   :type git
   :host github
   :repo "Theory-of-Everything/everforest-emacs"
   :files ("*.el")))

(chief/safe-use-package material-theme
  :defer t)

(chief/safe-use-package monokai-theme
  :defer t)

(chief/safe-use-package one-themes
  :defer t)

(chief/safe-use-package base16-theme
  :defer t)

(chief/safe-use-package modus-themes
  :straight nil
  :defer t)

(add-hook 'after-make-frame-functions #'chief/theme-apply-face-fixes)

(chief/load-theme chief/default-theme)

(chief/leader-def
  "t" '(:ignore t :which-key "toggles")
  "tt" #'chief/load-theme
  "tT" #'consult-theme
  "tc" #'chief/cycle-theme
  "ts" #'chief/theme-status)

(provide 'core-themes)
;;; core-themes.el ends here
