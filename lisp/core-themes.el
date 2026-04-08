;;; core-themes.el --- Theme collection and commands -*- lexical-binding: t; -*-

(require 'seq)

(defvar chief/default-theme 'doom-tokyo-night)
(defvar chief/theme-preferences
  '(doom-tokyo-night
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
  (let ((available (custom-available-themes)))
    (or (seq-filter (lambda (theme) (memq theme available))
                    chief/theme-preferences)
        available)))

(defun chief/load-theme (theme)
  "Disable active themes and load THEME."
  (interactive
   (list
    (intern
     (completing-read
      "Load theme: "
      (mapcar #'symbol-name (custom-available-themes))
      nil
      t))))
  (let* ((available (custom-available-themes))
         (target (if (memq theme available)
                     theme
                   (car (chief/ordered-themes)))))
    (unless target
      (user-error "No themes are currently available"))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme target t)))

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

(chief/load-theme chief/default-theme)

(chief/leader-def
  "t" '(:ignore t :which-key "toggles")
  "tt" #'consult-theme
  "tc" #'chief/cycle-theme
  "ts" #'chief/theme-status)

(provide 'core-themes)
;;; core-themes.el ends here
