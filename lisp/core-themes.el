;;; core-themes.el --- Theme collection and commands -*- lexical-binding: t; -*-

(require 'seq)
(require 'color)

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

(defconst chief/theme-gnus-face-specs
  '((gnus-group-mail-1-empty ((t (:inherit default))))
    (gnus-group-mail-1 ((t (:inherit gnus-group-mail-1-empty :weight bold))))
    (gnus-group-news-low-empty ((t (:inherit gnus-group-mail-1-empty :weight normal))))
    (gnus-group-news-low ((t (:inherit gnus-group-news-low-empty :weight bold)))))
  "Acyclic Gnus face specs used to prevent theme inheritance cycles.")

(defun chief/theme-patch-theme-settings (theme)
  "Patch THEME settings that would break child-frame face recalculation."
  (when-let* ((settings (get theme 'theme-settings)))
    (let ((gnus-faces (mapcar #'car chief/theme-gnus-face-specs)))
      (put theme 'theme-settings
           (append
            (mapcar (lambda (face-spec)
                      `(theme-face ,(car face-spec) ,theme ,(cadr face-spec)))
                    chief/theme-gnus-face-specs)
            (seq-remove
             (lambda (setting)
               (and (eq (car-safe setting) 'theme-face)
                    (memq (cadr setting) gnus-faces)))
             settings))))))

(defun chief/theme-patch-theme-before-enable (theme)
  "Patch THEME before `enable-theme' recalculates faces."
  (chief/theme-patch-theme-settings theme))

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
(advice-add 'enable-theme :before #'chief/theme-patch-theme-before-enable)

(defun chief/theme-apply-face-fixes-after-load (&rest _)
  "Re-apply face compatibility fixes after a theme is loaded."
  (chief/theme-apply-face-fixes))

(advice-add 'load-theme :after #'chief/theme-apply-face-fixes-after-load)
(advice-add 'enable-theme :after #'chief/theme-apply-face-fixes-after-load)
(advice-add 'disable-theme :after #'chief/theme-apply-face-fixes-after-load)

(defun chief/theme--hex-rgb (color)
  "Return normalized RGB floats for hex COLOR without display color remapping."
  (when (and (stringp color)
             (string-match-p "\\`#[[:xdigit:]]+\\'" color))
    (let* ((hex (substring color 1))
           (len (length hex)))
      (when (memq len '(3 6 12))
        (let* ((digits (/ len 3))
               (max-value (float (1- (expt 16 digits))))
               (component (lambda (index)
                            (/ (string-to-number
                                (substring hex (* index digits) (* (1+ index) digits))
                                16)
                               max-value))))
          (list (funcall component 0)
                (funcall component 1)
                (funcall component 2)))))))

(defun chief/theme--rgb (color)
  "Return normalized RGB floats for COLOR."
  (or (chief/theme--hex-rgb color)
      (color-name-to-rgb color)))

(defun chief/theme--usable-color (color)
  "Return COLOR if it names a concrete color in the current display."
  (and (stringp color)
       (not (string-prefix-p "unspecified" color))
       (ignore-errors (chief/theme--rgb color))
       color))

(defun chief/theme--first-color (&rest colors)
  "Return the first concrete color in COLORS."
  (seq-find #'chief/theme--usable-color colors))

(defun chief/theme--face-spec-plist (entry)
  "Return the property list from one face-spec ENTRY."
  (cond
   ((and (listp entry) (keywordp (car entry))) entry)
   ((and (consp entry) (listp (cdr entry)) (keywordp (cadr entry))) (cdr entry))
   ((and (consp entry) (listp (cadr entry))) (cadr entry))))

(defun chief/theme--face-spec-color (spec attribute)
  "Return ATTRIBUTE color from face SPEC when concrete."
  (catch 'found
    (dolist (entry spec)
      (when-let* ((plist (chief/theme--face-spec-plist entry))
                  (value (plist-get plist attribute))
                  (color (chief/theme--usable-color
                          (if (symbolp value) (symbol-name value) value))))
        (throw 'found color)))))

(defun chief/theme--theme-face-color (face attribute)
  "Return FACE ATTRIBUTE from enabled theme settings, if present."
  (catch 'found
    (dolist (theme custom-enabled-themes)
      (dolist (setting (get theme 'theme-settings))
        (when (and (eq (car-safe setting) 'theme-face)
                   (eq (cadr setting) face))
          (when-let* ((color (chief/theme--face-spec-color (nth 3 setting) attribute)))
            (throw 'found color)))))))

(defun chief/theme--face-background (face &optional frame)
  "Return FACE background on FRAME when it is concrete."
  (when (facep face)
    (chief/theme--first-color
     (face-background face frame t)
     (chief/theme--theme-face-color face :background))))

(defun chief/theme--face-foreground (face &optional frame)
  "Return FACE foreground on FRAME when it is concrete."
  (when (facep face)
    (chief/theme--first-color
     (face-foreground face frame t)
     (chief/theme--theme-face-color face :foreground))))

(defun chief/theme--blend (color-a color-b alpha)
  "Blend COLOR-A into COLOR-B by ALPHA and return a hex color."
  (pcase-let* ((`(,ar ,ag ,ab) (chief/theme--rgb color-a))
               (`(,br ,bg ,bb) (chief/theme--rgb color-b))
               (mix (lambda (a b) (+ (* alpha a) (* (- 1 alpha) b)))))
    (color-rgb-to-hex (funcall mix ar br)
                      (funcall mix ag bg)
                      (funcall mix ab bb)
                      2)))

(defun chief/theme--luminance (color)
  "Return perceived luminance of COLOR."
  (pcase-let ((`(,r ,g ,b) (chief/theme--rgb color)))
    (+ (* 0.299 r) (* 0.587 g) (* 0.114 b))))

(defun chief/theme--shade-away-from-background (color amount)
  "Lighten dark COLOR or darken light COLOR by AMOUNT."
  (chief/theme--blend (if (> (chief/theme--luminance color) 0.5) "black" "white")
                      color
                      amount))

(defun chief/theme--frame-background (&optional frame)
  "Return the current theme's effective frame background."
  (let ((frame (or frame (selected-frame))))
    (chief/theme--first-color
     (chief/theme--face-background 'default frame)
     (frame-parameter frame 'background-color)
     (if (eq (frame-parameter frame 'background-mode) 'light) "white" "black"))))

(defun chief/theme--frame-foreground (&optional frame)
  "Return the current theme's effective frame foreground."
  (let ((frame (or frame (selected-frame))))
    (chief/theme--first-color
     (chief/theme--face-foreground 'default frame)
     (frame-parameter frame 'foreground-color)
     (if (eq (frame-parameter frame 'background-mode) 'light) "black" "white"))))

(defun chief/theme--different-background (face base &optional frame)
  "Return FACE background when it is concrete and different from BASE."
  (when-let* ((color (chief/theme--face-background face frame)))
    (unless (string-equal (downcase color) (downcase base))
      color)))

(defun chief/theme--completion-face-specs (&optional frame)
  "Return Corfu face specs derived from the active theme's visible palette.
This intentionally does not name or special-case any theme.  It samples the
current frame's effective default/hl-line/comment colors and derives popup colors
from those, so an installed theme tomorrow is handled the same way as today's
installed themes."
  (let* ((frame (or frame (selected-frame)))
         (bg (chief/theme--frame-background frame))
         (fg (chief/theme--frame-foreground frame))
         (popup-bg (chief/theme--shade-away-from-background bg 0.05))
         (current-bg (or (chief/theme--different-background 'hl-line bg frame)
                         (chief/theme--shade-away-from-background bg 0.16)))
         (border (chief/theme--shade-away-from-background bg 0.24))
         (annotation-fg (chief/theme--first-color
                         (chief/theme--face-foreground 'font-lock-comment-face frame)
                         (chief/theme--face-foreground 'shadow frame)
                         (chief/theme--blend fg bg 0.55)))
         (bar-bg (chief/theme--shade-away-from-background bg 0.35)))
    `((corfu-default ((t (:background ,popup-bg :foreground ,fg))))
      (corfu-current ((t (:background ,current-bg :foreground ,fg :extend t))))
      (corfu-border ((t (:background ,border))))
      (corfu-bar ((t (:background ,bar-bg))))
      (corfu-annotations ((t (:foreground ,annotation-fg))))
      (corfu-popupinfo ((t (:background ,popup-bg :foreground ,fg)))))))

(defun chief/theme-apply-completion-face-fixes (&optional frame)
  "Make Corfu colors follow whatever theme is currently active."
  (let ((face-specs (chief/theme--completion-face-specs frame)))
    ;; Store concrete colors computed from the active frame/theme so newly-created
    ;; Corfu child frames do not reuse stale package defaults.  These specs are
    ;; recomputed after theme enable/disable/load, so this is generic without
    ;; being tied to any installed theme or theme family.
    (dolist (face-spec face-specs)
      (put (car face-spec) 'theme-face `((user ,(cadr face-spec)))))
    ;; Update already-created/live frames immediately where the faces exist.
    (with-selected-frame (or frame (selected-frame))
      (dolist (face-spec face-specs)
        (when (facep (car face-spec))
          (apply #'set-face-attribute
                 (car face-spec)
                 nil
                 (cadr (car (cadr face-spec)))))))))

(defun chief/theme-apply-face-fixes (&optional frame)
  "Apply compatibility face overrides after theme loads.
This avoids a known Gnus face inheritance cycle exposed when packages such as
Corfu create child frames.  The fix must be installed as `user' theme specs, not
only as frame attributes, because new child frames recalculate faces from theme
specs before `after-make-frame-functions' can run."
  ;; Some theme/Gnus combinations define `gnus-group-news-low-empty' and
  ;; `gnus-group-news-low' in terms of each other.  Replace those specs directly
  ;; so every future frame, including Corfu child frames, sees a safe face graph.
  ;; Using `custom-theme-set-faces' here can itself trigger face recalculation
  ;; while the cyclic theme specs are still present.
  (dolist (face-spec chief/theme-gnus-face-specs)
    (put (car face-spec) 'theme-face `((user ,(cadr face-spec)))))
  ;; Also update the selected/live frame immediately when the faces are already
  ;; defined.  Undefined faces are harmless here; the user-theme specs above are
  ;; what matters for later Gnus loads and later child frames.
  (with-selected-frame (or frame (selected-frame))
    (ignore-errors
      (set-face-attribute 'gnus-group-mail-1-empty nil :inherit 'default)
      (set-face-attribute 'gnus-group-mail-1 nil :inherit 'gnus-group-mail-1-empty :weight 'bold)
      (set-face-attribute 'gnus-group-news-low-empty nil :inherit 'gnus-group-mail-1-empty :weight 'normal)
      (set-face-attribute 'gnus-group-news-low nil :inherit 'gnus-group-news-low-empty :weight 'bold)))
  (chief/theme-apply-completion-face-fixes frame))

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
(with-eval-after-load 'gnus-group
  (chief/theme-apply-face-fixes))
(with-eval-after-load 'corfu
  (chief/theme-apply-face-fixes))
(with-eval-after-load 'corfu-popupinfo
  (chief/theme-apply-face-fixes))

(chief/load-theme chief/default-theme)

(chief/leader-def
  "t" '(:ignore t :which-key "toggles")
  "tt" #'chief/load-theme
  "tT" #'consult-theme
  "tc" #'chief/cycle-theme
  "ts" #'chief/theme-status)

(provide 'core-themes)
;;; core-themes.el ends here
