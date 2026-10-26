;;; core-terminal.el --- Terminal-specific behavior -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(declare-function chief--macos-option-layout "chief-macos-option-keys" ())
(declare-function evil-insert-state "evil-states" ())
(declare-function evil-normal-state "evil-states" ())
(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function ghostel-copy-mode "ghostel" ())
(declare-function ghostel-emacs-mode "ghostel" ())
(declare-function ghostel-readonly-exit "ghostel" ())
(declare-function ghostel-send-next-key "ghostel" ())
(declare-function ghostel-yank "ghostel" ())
(defvar evil-state)
(defvar ghostel--input-mode)
(defvar ghostel-readonly-mode-map)
(defvar ghostel-mode-map)
(defvar global-kkp-mode)

(defun chief/terminal-frame-p (&optional frame)
  "Return non-nil when FRAME is a terminal frame."
  (not (display-graphic-p frame)))

(defcustom chief/tty-option-as-meta (eq system-type 'darwin)
  "Interpret macOS Option-generated characters as Meta in terminal frames.
Use the current native keyboard layout, not a hard-coded US layout.
GUI frames, bracketed paste, quoted insertion, and input methods keep their
ordinary character input.  No terminal-app settings are changed."
  :type 'boolean
  :group 'chief)

(defvar chief/tty-option-layout nil)
(defvar chief/tty-option-translations nil)
(defvar chief/tty-option-module-file nil
  "Native keyboard-layout module loaded in this Emacs process.")

(defun chief/tty-reset-option-translations ()
  "Remove only the Option translations installed by this configuration."
  (interactive)
  (dolist (entry chief/tty-option-translations)
    (when (eq (lookup-key key-translation-map (vector (nth 0 entry)))
              (nth 2 entry))
      (define-key key-translation-map (vector (nth 0 entry)) (nth 1 entry))))
  (setq chief/tty-option-translations nil
        chief/tty-option-layout nil))

(defun chief/tty-option-module-header ()
  "Find the module header supplied by this Emacs installation."
  (or
   (catch 'header
     (dolist (root (delq nil (list data-directory invocation-directory
                                  installation-directory source-directory)))
       (let ((directory (file-name-as-directory root)))
         (while directory
           (dolist (relative '("include/emacs-module.h" "src/emacs-module.h"
                               "Resources/include/emacs-module.h"))
             (let ((file (expand-file-name relative directory)))
               (when (file-readable-p file)
                 (throw 'header file))))
           (let ((parent (file-name-directory (directory-file-name directory))))
             (setq directory (unless (equal parent directory) parent)))))))
   (user-error "Cannot find emacs-module.h in this Emacs installation")))

(defun chief/tty-build-option-module (source destination)
  "Build SOURCE into DESTINATION using already-installed Apple tools."
  (let ((header (chief/tty-option-module-header))
        (architecture
         (cond ((string-match-p "\\`\\(?:aarch64\\|arm64\\)-" system-configuration) "arm64")
               ((string-prefix-p "x86_64-" system-configuration) "x86_64")
               (t (user-error "Unsupported native module architecture: %s"
                              system-configuration)))))
    ;; Querying the selected tools does not invoke Apple's installer stubs.
    (with-temp-buffer
      (unless (and (file-executable-p "/usr/bin/xcode-select")
                   (file-executable-p "/usr/bin/xcrun")
                   (eq 0 (call-process "/usr/bin/xcode-select" nil t nil "-p"))
                   (file-directory-p (string-trim (buffer-string)))
                   (cl-some (lambda (relative)
                              (file-executable-p
                               (expand-file-name relative (string-trim (buffer-string)))))
                            '("usr/bin/clang"
                              "Toolchains/XcodeDefault.xctoolchain/usr/bin/clang")))
        (user-error "Building the keyboard module requires existing Apple developer tools; none will be installed")))
    (make-directory (file-name-directory destination) t)
    (let ((temporary (make-temp-file
                      (expand-file-name ".option-module-" (file-name-directory destination))
                      nil module-file-suffix)))
      (unwind-protect
          (with-temp-buffer
            (unless (eq 0 (call-process
                           "/usr/bin/xcrun" nil (list t t) nil
                           "--sdk" "macosx" "clang" "-std=c11" "-O2"
                           "-Wall" "-Wextra" "-Werror" "-Wno-deprecated-declarations"
                           "-fvisibility=hidden" "-dynamiclib" "-arch" architecture
                           "-I" (file-name-directory header)
                           "-framework" "Carbon" "-framework" "CoreFoundation"
                           "-o" temporary source))
              (error "Native keyboard module build failed: %s"
                     (string-trim (buffer-string))))
            ;; Never overwrite a loaded library in place or leave partial builds.
            (rename-file temporary destination t))
        (when (file-exists-p temporary)
          (delete-file temporary))))))

(defun chief/tty-load-option-module ()
  "Load the native keyboard reader, building a cached module if needed."
  (unless (and (fboundp 'module-load) module-file-suffix)
    (user-error "This Emacs build does not support native modules"))
  (let* ((source (expand-file-name "modules/macos-option-keys.c" user-emacs-directory))
         (fingerprint (with-temp-buffer
                        (set-buffer-multibyte nil)
                        (insert-file-contents-literally source)
                        (substring (secure-hash 'sha256 (current-buffer)) 0 16)))
         (file (expand-file-name
                (format "chief-macos-option-keys-%s-%d-%s%s"
                        system-configuration emacs-major-version fingerprint module-file-suffix)
                (expand-file-name "native/" chief/var-directory))))
    (unless (and (equal chief/tty-option-module-file file)
                 (fboundp 'chief--macos-option-layout))
      (unless (file-exists-p file)
        (chief/tty-build-option-module source file))
      (load file nil t t)
      (setq chief/tty-option-module-file file))))

(defun chief/tty-read-option-layout ()
  "Query the macOS keyboard layout through the cached native Emacs module."
  (unless (eq system-type 'darwin)
    (user-error "Native Option-key layout discovery requires macOS"))
  (chief/tty-load-option-module)
  (chief--macos-option-layout))

(defun chief/tty-refresh-option-translations ()
  "Read the native macOS layout and restore GUI-like Option keys in TTYs.
Only unambiguous, printable Option characters are translated.  macOS dead
keys that produce no input are excluded rather than guessed."
  (interactive)
  (let* ((data (chief/tty-read-option-layout))
         (layout (alist-get 'layout data))
         (keys (alist-get 'keys data)))
    (unless (and (stringp layout) (listp keys))
      (error "Invalid native Option-key layout response"))
    (dolist (pair keys)
      (unless (and (listp pair) (= (length pair) 2)
                   (characterp (nth 0 pair))
                   (integerp (nth 1 pair)) (<= 32 (nth 1 pair) 126))
        (error "Invalid native Option-key character mapping: %S" pair)))
    (chief/tty-reset-option-translations)
    (dolist (pair keys)
      (let* ((character (nth 0 pair))
             (base (nth 1 pair))
             (key (vector character))
             (previous (lookup-key key-translation-map key))
             (meta (vector (event-convert-list (list 'meta base))))
             (translation
              (lambda (&optional _prompt)
                (if (and chief/tty-option-as-meta
                         (chief/terminal-frame-p)
                         (not current-input-method))
                    meta
                  key))))
        ;; Respect pre-existing translations instead of overwriting them.
        (unless previous
          (define-key key-translation-map key translation)
          (push (list character previous translation) chief/tty-option-translations))))
    (setq chief/tty-option-layout layout)))

(defun chief/tty-clipboard-programs ()
  "Return clipboard programs available for the current system, if any."
  (cond
   ((and (eq system-type 'darwin)
         (executable-find "pbcopy")
         (executable-find "pbpaste"))
    '(:copy ("pbcopy")
      :paste ("pbpaste")))
   ((and (executable-find "wl-copy")
         (executable-find "wl-paste"))
    '(:copy ("wl-copy")
      :paste ("wl-paste" "--no-newline")))
   ((executable-find "xclip")
    '(:copy ("xclip" "-selection" "clipboard")
      :paste ("xclip" "-selection" "clipboard" "-o")))
   (t nil)))

(defun chief/tty-copy-to-clipboard (text &optional _push)
  "Copy TEXT to the terminal clipboard helper, if configured."
  (when-let* ((programs (chief/tty-clipboard-programs)))
    (pcase-let ((`(,copy-program . ,copy-args) (plist-get programs :copy)))
      (with-temp-buffer
        (insert text)
        (apply #'call-process-region
               (point-min)
               (point-max)
               copy-program
               nil
               nil
               nil
               copy-args)))))

(defun chief/tty-paste-from-clipboard ()
  "Read text from the terminal clipboard helper, if configured."
  (when-let* ((programs (chief/tty-clipboard-programs))
              (paste-command (plist-get programs :paste)))
    (string-trim-right
     (with-temp-buffer
       (apply #'call-process
              (car paste-command)
              nil
              t
              nil
              (cdr paste-command))
       (buffer-string)))))

(defun chief/tty-decode-control-space (&optional _prompt)
  "Recognize legacy C-SPC without stealing insert-state completion."
  (if (and (bound-and-true-p evil-local-mode)
           (or (memq evil-state '(normal visual motion emacs))
               ;; Custom states such as Treemacs inherit motion bindings.
               (bound-and-true-p evil-motion-state-local-minor-mode)))
      (kbd "C-SPC")
    [0]))

(defun chief/tty-enable-extended-keys (enable &optional terminal)
  "Run ENABLE for TERMINAL after its initial device replies have drained."
  (let ((terminal (or terminal (frame-terminal)))
        (attempts 20))
    (when (and (not noninteractive)
               (terminal-live-p terminal)
               (not (display-graphic-p terminal))
               (not (terminal-parameter terminal 'chief/keyboard-setup-timer)))
      (cl-labels
          ((activate ()
             (when (terminal-live-p terminal)
               (set-terminal-parameter terminal 'chief/keyboard-setup-timer nil)
               (when (and global-kkp-mode (eq terminal (frame-terminal)))
                 ;; Native xterm and KKP replies share CSI ?.  Preserve the
                 ;; native handler and queued keys instead of racing them.
                 (let ((queued (input-pending-p)))
                   (if (or queued
                           (functionp (lookup-key input-decode-map "\e[?")))
                       (when (or queued (> attempts 0))
                         (unless queued (cl-decf attempts))
                         (set-terminal-parameter
                          terminal 'chief/keyboard-setup-timer
                          (run-at-time 0.1 nil #'activate)))
                     (funcall enable terminal)))))))
        ;; An idle timer can remain pending in server-created TTY frames
        ;; until the first real keypress; unreported keys cannot wake it.
        (set-terminal-parameter terminal 'chief/keyboard-setup-timer
                                (run-at-time 0.1 nil #'activate))))))

(defun chief/terminal-setup (&optional frame)
  "Configure terminal behavior for FRAME."
  (when (and (not noninteractive)
             (chief/terminal-frame-p frame))
    (with-selected-frame (or frame (selected-frame))
      (when (and chief/tty-option-as-meta
                 (eq system-type 'darwin)
                 (null chief/tty-option-layout))
        (condition-case err
            (chief/tty-refresh-option-translations)
          (error
           (setq chief/tty-option-layout 'unavailable)
           (display-warning 'chief (error-message-string err)))))
      (define-key input-decode-map [0] #'chief/tty-decode-control-space)
      (xterm-mouse-mode 1)
      (mouse-wheel-mode 1)
      (setq select-enable-clipboard t
            select-enable-primary t
            save-interprogram-paste-before-kill t)
      (when (boundp 'tty-select-active-regions)
        (setq tty-select-active-regions t))
      (when (chief/tty-clipboard-programs)
        (setq interprogram-cut-function #'chief/tty-copy-to-clipboard
              interprogram-paste-function #'chief/tty-paste-from-clipboard)))))

(add-hook 'tty-setup-hook #'chief/terminal-setup)
(add-hook 'after-make-frame-functions #'chief/terminal-setup)

;; Some terminals do not transmit C-. or C-; in their legacy keyboard mode.
;; Negotiate the supported protocol inside Emacs; do not change app settings.
(use-package kkp
  :straight (kkp :type git :host github :repo "benotn/kkp")
  :demand t
  :init
  (setq kkp-restore-legacy-keys-around-subprocesses t)
  :config
  (advice-add #'kkp-enable-in-terminal :around #'chief/tty-enable-extended-keys)
  (unless noninteractive
    (global-kkp-mode 1)))

(defun chief/ghostel-readonly-p ()
  "Return non-nil when Ghostel is in copy or Emacs read-only mode."
  (and (boundp 'ghostel--input-mode)
       (memq ghostel--input-mode '(copy emacs))))

(defun chief/ghostel-command-state-setup ()
  "Make a Ghostel buffer safe for Emacs/Evil command-state interaction.
Ghostel's copy mode freezes terminal output, installs a navigation-friendly
keymap, restores ordinary Emacs mouse selection, and prevents Evil commands
from accidentally editing the rendered terminal contents."
  (when (and (derived-mode-p 'ghostel-mode)
             (not (chief/ghostel-readonly-p)))
    (ghostel-copy-mode)))

(defun chief/ghostel-insert-state-setup ()
  "Return a Ghostel buffer to live terminal input in Evil insert state."
  (when (and (derived-mode-p 'ghostel-mode)
             (chief/ghostel-readonly-p))
    (ghostel-readonly-exit)))

(defun chief/ghostel-sync-evil-state (&rest _)
  "Keep Ghostel copy mode and Evil command/insert states synchronized."
  (when (and (derived-mode-p 'ghostel-mode)
             (bound-and-true-p evil-local-mode))
    (if (chief/ghostel-readonly-p)
        (when (eq evil-state 'insert)
          (evil-normal-state))
      (unless (eq evil-state 'insert)
        (evil-insert-state)))))

(defun chief/ghostel-send-next-key ()
  "Resume terminal input and send Ghostel's next key literally.
This is the Doom-style escape hatch for keys normally reserved by Emacs."
  (interactive)
  (when (chief/ghostel-readonly-p)
    (ghostel-readonly-exit))
  (ghostel-send-next-key))

(defun chief/ghostel-yank ()
  "Resume terminal input and paste the latest Emacs kill into Ghostel."
  (interactive)
  (when (chief/ghostel-readonly-p)
    (ghostel-readonly-exit))
  (ghostel-yank))

(defun chief/ghostel-mode-setup (&optional preserve-state)
  "Apply Doom-style modal terminal behavior to the current Ghostel buffer.
Insert state belongs to the terminal.  Normal or Emacs state enters Ghostel's
read-only copy mode, where Evil, Emacs prefix keys, and mouse selection work.
When PRESERVE-STATE is non-nil, do not force an already-live buffer to insert
state."
  (setq-local confirm-kill-processes nil)
  ;; Normal-state letters must navigate rather than exit copy mode and get
  ;; forwarded to the shell by Ghostel's optional fast-exit keymap.
  (setq-local ghostel-readonly-fast-exit nil)
  (when (boundp 'evil-move-cursor-back)
    (setq-local evil-move-cursor-back nil))
  (add-hook 'evil-normal-state-entry-hook
            #'chief/ghostel-command-state-setup nil t)
  (add-hook 'evil-emacs-state-entry-hook
            #'chief/ghostel-command-state-setup nil t)
  (add-hook 'evil-insert-state-entry-hook
            #'chief/ghostel-insert-state-setup nil t)
  (when (and (not preserve-state)
             (bound-and-true-p evil-local-mode)
             (fboundp 'evil-insert-state))
    (evil-insert-state)))

(defun chief/ghostel-repair-buffer-state ()
  "Apply terminal setup and synchronize an already-live Ghostel buffer."
  (chief/ghostel-mode-setup t)
  (pcase evil-state
    ('insert (chief/ghostel-insert-state-setup))
    ((or 'normal 'motion 'visual 'emacs)
     (chief/ghostel-command-state-setup))))

(use-package ghostel
  :straight (ghostel
             :type git
             :host github
             :repo "dakra/ghostel")
  :commands (ghostel ghostel-project ghostel-other)
  :init
  ;; Native binaries survive package rebuilds without living in tracked files.
  (setq ghostel-module-directory
        (expand-file-name "ghostel/" chief/var-directory))
  :hook (ghostel-mode . chief/ghostel-mode-setup)
  :config
  ;; Doom's vterm integration reserves C-q as a one-shot literal-key escape
  ;; hatch and starts terminal buffers in Evil insert state.  Ghostel's copy
  ;; mode supplies the matching safe command state, including ordinary Emacs
  ;; mouse selection instead of forwarding clicks to a terminal application.
  (define-key ghostel-mode-map (kbd "C-q") #'chief/ghostel-send-next-key)
  (define-key ghostel-readonly-mode-map (kbd "C-q") #'chief/ghostel-send-next-key)
  (define-key ghostel-mode-map (kbd "C-c C-k") #'kill-current-buffer)
  (define-key ghostel-readonly-mode-map (kbd "C-c C-k") #'kill-current-buffer)
  ;; Unlike Ghostel's default, Doom's vterm leaves ordinary mouse clicks and
  ;; drag selection with Emacs.  Keep terminal-specific wheel scrolling, but do
  ;; not let terminal applications swallow window selection or mouse regions.
  (dolist (key '("<down-mouse-1>" "<mouse-1>" "<drag-mouse-1>"
                 "<down-mouse-2>" "<mouse-2>" "<drag-mouse-2>"
                 "<down-mouse-3>" "<mouse-3>" "<drag-mouse-3>"))
    (define-key ghostel-mode-map (kbd key) nil))
  (advice-add #'ghostel-copy-mode :after #'chief/ghostel-sync-evil-state)
  (advice-add #'ghostel-emacs-mode :after #'chief/ghostel-sync-evil-state)
  (advice-add #'ghostel-readonly-exit :after #'chief/ghostel-sync-evil-state)
  (with-eval-after-load 'evil
    (evil-set-initial-state 'ghostel-mode 'insert)
    (evil-define-key 'insert ghostel-mode-map
      (kbd "<escape>") #'evil-normal-state
      (kbd "C-q") #'chief/ghostel-send-next-key)
    (evil-define-key 'normal ghostel-readonly-mode-map
      (kbd "q") #'quit-window
      (kbd "p") #'chief/ghostel-yank
      (kbd "P") #'chief/ghostel-yank))
  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'ghostel-mode)
        (chief/ghostel-repair-buffer-state)))))

(provide 'core-terminal)
;;; core-terminal.el ends here
