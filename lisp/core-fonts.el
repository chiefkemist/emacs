;;; core-fonts.el --- Font selection and face tuning -*- lexical-binding: t; -*-

(require 'seq)

(defgroup chief-fonts nil
  "Font preferences for the Chiefkemist Emacs configuration."
  :group 'chief)

(defcustom chief/fixed-pitch-font-candidates
  '("Maple Mono Normal NL"
    "JetBrainsMonoNL Nerd Font Mono"
    "JetBrainsMono Nerd Font Mono"
    "Iosevka Nerd Font Mono"
    "BlexMono Nerd Font Mono"
    "CaskaydiaCove Nerd Font Mono")
  "Preferred fixed-pitch font families, in order."
  :type '(repeat string)
  :group 'chief-fonts)

(defcustom chief/variable-pitch-font-candidates
  '("Inter"
    "SF Pro Text"
    "Helvetica Neue"
    "Arial")
  "Preferred variable-pitch font families, in order."
  :type '(repeat string)
  :group 'chief-fonts)

(defcustom chief/fixed-pitch-font-height 150
  "Default height for the fixed-pitch coding font."
  :type 'integer
  :group 'chief-fonts)

(defcustom chief/variable-pitch-font-height 155
  "Default height for the variable-pitch UI font."
  :type 'integer
  :group 'chief-fonts)

(defun chief/font-available-p (family)
  "Return non-nil when FAMILY is available."
  (and (display-graphic-p)
       (find-font (font-spec :family family))))

(defun chief/first-available-font (families)
  "Return the first available font family in FAMILIES."
  (seq-find #'chief/font-available-p families))

(defun chief/apply-fonts ()
  "Apply the preferred font preset for this Emacs session."
  (when (display-graphic-p)
    (when-let* ((fixed (chief/first-available-font chief/fixed-pitch-font-candidates)))
      (set-face-attribute 'default nil :family fixed :height chief/fixed-pitch-font-height :weight 'regular)
      (set-face-attribute 'fixed-pitch nil :family fixed :height 1.0))
    (when-let* ((variable (chief/first-available-font chief/variable-pitch-font-candidates)))
      (set-face-attribute 'variable-pitch nil :family variable :height chief/variable-pitch-font-height :weight 'regular))
    (setq-default line-spacing 0.12)))

(add-hook 'emacs-startup-hook #'chief/apply-fonts)
(add-hook 'server-after-make-frame-hook #'chief/apply-fonts)

(provide 'core-fonts)
;;; core-fonts.el ends here
