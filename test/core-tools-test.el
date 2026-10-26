;;; core-tools-test.el --- Tooling update regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'core-tools)
(require 'core-terminal)
(require 'core-projects)
(require 'core-treesit)
(require 'core-themes)
(require 'lang-polyglot)

(ert-deftest chief/pilish-installed-with-leader-bindings ()
  "Pilish commands are reachable before its package has been loaded."
  (let ((map (evil-get-auxiliary-keymap general-override-mode-map 'normal)))
    (should (eq (lookup-key map (kbd "SPC a a")) 'pilish))
    (should (eq (lookup-key map (kbd "SPC a p")) 'pilish))
    (should (eq (lookup-key map (kbd "SPC a t")) 'pilish-toggle))
    (should (eq (lookup-key map (kbd "SPC a s")) 'pilish-session-browser)))
  (should (commandp 'pi))
  (should (commandp 'pilish))
  (should (require 'pilish nil t))
  (should (eq pilish-project-trust-policy 'default)))

(ert-deftest chief/pilish-markdown-grammar-supports-tables ()
  "The installed Markdown grammar must support Pilish's rendering queries."
  (require 'pilish-grammars)
  (should-not (pilish--missing-essential-grammars))
  (should (pilish--markdown-grammar-compatible-p)))

(ert-deftest chief/ghostel-updated-api-loads ()
  "The terminal integration uses the current Ghostel read-only API."
  (should (require 'ghostel nil t))
  (should (boundp 'ghostel-readonly-mode-map))
  (should (fboundp 'ghostel-readonly-exit))
  (should
   (eq (lookup-key ghostel-readonly-mode-map (kbd "C-q"))
       'chief/ghostel-send-next-key))
  (should
   (file-in-directory-p ghostel-module-directory chief/var-directory)))

(ert-deftest chief/ghostel-command-and-input-state-transitions ()
  "Evil command and input states use Ghostel's new input-mode variable."
  (require 'ghostel)
  (with-temp-buffer
    (setq major-mode 'ghostel-mode)
    (setq-local ghostel--input-mode 'semi-char)
    (cl-letf (((symbol-function 'ghostel-copy-mode)
               (lambda () (setq-local ghostel--input-mode 'copy)))
              ((symbol-function 'ghostel-readonly-exit)
               (lambda () (setq-local ghostel--input-mode 'semi-char))))
      (chief/ghostel-command-state-setup)
      (should (eq ghostel--input-mode 'copy))
      (should (chief/ghostel-readonly-p))
      (chief/ghostel-insert-state-setup)
      (should (eq ghostel--input-mode 'semi-char))
      (should-not (chief/ghostel-readonly-p))
      (setq-local ghostel--input-mode 'emacs)
      (chief/ghostel-insert-state-setup)
      (should (eq ghostel--input-mode 'semi-char)))))

(ert-deftest chief/treemacs-return-works-in-evil ()
  "Terminal RET and GUI Return must both open the selected tree node."
  (require 'treemacs)
  (with-temp-buffer
    (setq-local treemacs--in-this-buffer t)
    (treemacs-mode)
    (evil-local-mode 1)
    (should (eq (key-binding (kbd "RET")) #'treemacs-RET-action))
    (should (eq (key-binding [return]) #'treemacs-RET-action))
    (should (keymapp (key-binding (kbd "C-SPC"))))
    (should (equal (chief/tty-decode-control-space) (kbd "C-SPC")))))

(defun chief/test-with-treemacs-index-file (function)
  "Call FUNCTION with a temporary text file, cleaning up its buffer afterward."
  (require 'treemacs-tags)
  (let ((file (make-temp-file "chief-treemacs-index-" nil ".txt" "No tags here.\n"))
        (text-mode-hook nil)
        (find-file-hook nil))
    (unwind-protect
        (save-window-excursion
          (funcall function file))
      (when-let* ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest chief/treemacs-no-tags-does-not-leak-uninitialized-buffer ()
  "A failed tag lookup must not prevent setup hooks on the actual file visit."
  (chief/test-with-treemacs-index-file
   (lambda (file)
     (let* ((hooks-run 0)
            (text-mode-hook (list (lambda () (cl-incf hooks-run)))))
       (cl-letf (((symbol-function 'imenu--make-index-alist)
                  (lambda (&rest _) (signal 'imenu-unavailable '("No tags")))))
         (should-not (treemacs--get-imenu-index file)))
       (should (= hooks-run 0))
       (should-not (find-buffer-visiting file))
       (find-file-noselect file)
       (should (= hooks-run 1))))))

(ert-deftest chief/treemacs-tag-lookup-preserves-existing-buffers ()
  "Tag lookup must leave already visited files and unsaved changes alone."
  (chief/test-with-treemacs-index-file
   (lambda (file)
     (let ((buffer (find-file-noselect file)))
       (cl-letf (((symbol-function 'imenu--make-index-alist)
                  (lambda (&rest _) (signal 'imenu-unavailable '("No tags")))))
         (dolist (modified '(nil t))
           (with-current-buffer buffer (set-buffer-modified-p modified))
           (should-not (treemacs--get-imenu-index file))
           (should (eq buffer (find-buffer-visiting file)))
           (should (eq modified (buffer-modified-p buffer)))))))))

(ert-deftest chief/treemacs-tag-cleanup-preserves-newly-used-buffers ()
  "Cleanup must not discard a new buffer that was modified or displayed."
  (dolist (use '(modify display))
    (chief/test-with-treemacs-index-file
     (lambda (file)
       (cl-letf (((symbol-function 'imenu--make-index-alist)
                  (lambda (&rest _)
                    (if (eq use 'modify)
                        (insert "Unsaved text")
                      (switch-to-buffer (current-buffer)))
                    (signal 'imenu-unavailable '("No tags")))))
         (should-not (treemacs--get-imenu-index file)))
       (should (buffer-live-p (find-buffer-visiting file)))))))

(ert-deftest chief/terminal-embark-has-legacy-minibuffer-shortcuts ()
  "Legacy terminals need an unambiguous alternative to modified punctuation."
  (should (eq (lookup-key global-map (kbd "C-c .")) #'embark-act))
  (should (eq (lookup-key global-map (kbd "C-c ;")) #'embark-dwim)))

(ert-deftest chief/treesit-remaps-only-the-selected-language ()
  "Opening an unrelated file must not probe every installed grammar."
  (require 'treesit-auto)
  (let ((recipe (make-treesit-auto-recipe
                 :lang 'python :ts-mode 'python-ts-mode :remap 'python-mode))
        (probed nil))
    (cl-letf (((symbol-function 'treesit-auto--get-mode-recipe)
               (lambda (mode) (and (eq mode 'python-mode) recipe)))
              ((symbol-function 'treesit-auto--ready-p)
               (lambda (mode) (push mode probed) t))
              ((symbol-function 'treesit-auto--build-major-mode-remap-alist)
               (lambda () (ert-fail "Unrelated grammars were probed"))))
      (with-temp-buffer
        (setq-local major-mode-remap-alist nil)
        (chief/treesit-set-major-remap nil)
        (chief/treesit-set-major-remap 'vbnet-mode)
        (should-not probed)
        (should-not major-mode-remap-alist)
        (chief/treesit-set-major-remap 'python-mode)
        (should (equal probed '(python-mode)))
        (should (eq (alist-get 'python-mode major-mode-remap-alist)
                    'python-ts-mode))))))

(ert-deftest chief/treesit-remapping-preserves-user-overrides-and-fallbacks ()
  "Explicit overrides and unavailable grammars must retain their behavior."
  (require 'treesit-auto)
  (let ((recipe (make-treesit-auto-recipe
                 :lang 'python :ts-mode 'python-ts-mode :remap 'python-mode))
        (ready t))
    (cl-letf (((symbol-function 'treesit-auto--get-mode-recipe) (lambda (_) recipe))
              ((symbol-function 'treesit-auto--ready-p) (lambda (_) ready)))
      (with-temp-buffer
        (let ((original '((python-mode . nil))))
          (setq-local major-mode-remap-alist original)
          (chief/treesit-set-major-remap 'python-mode)
          (should-not (alist-get 'python-mode major-mode-remap-alist))
          (should (equal original '((python-mode . nil))))))
      (setq ready nil)
      (with-temp-buffer
        (let ((major-mode-remap-alist nil))
          (chief/treesit-set-major-remap 'python-mode)
          (should-not major-mode-remap-alist))))))

(ert-deftest chief/treesit-php-keeps-working-without-embedded-parsers ()
  "Do not enter a half-initialized PHP mode when PHPDoc is unavailable."
  (skip-unless (>= emacs-major-version 32))
  (require 'treesit-auto)
  (let ((recipe (make-treesit-auto-recipe
                 :lang 'php :ts-mode 'php-ts-mode :remap 'php-mode))
        (available nil)
        (checked nil))
    (cl-letf (((symbol-function 'treesit-auto--get-mode-recipe) (lambda (_) recipe))
              ((symbol-function 'treesit-auto--ready-p) (lambda (_) t))
              ((symbol-function 'treesit-ready-p)
               (lambda (languages &optional _quiet)
                 (setq checked languages)
                 available)))
      (with-temp-buffer
        (setq-local major-mode-remap-alist nil)
        (chief/treesit-set-major-remap 'php-mode)
        (should-not major-mode-remap-alist)
        (should (equal checked '(php phpdoc html javascript jsdoc css)))
        (setq available t)
        (chief/treesit-set-major-remap 'php-mode)
        (should (eq (alist-get 'php-mode major-mode-remap-alist) 'php-ts-mode))))))

(ert-deftest chief/treesit-native-php-entry-point-also-falls-back ()
  "Native PHP file associations must use the same complete-parser check."
  (let ((available nil)
        (selected nil))
    (cl-letf (((symbol-function 'chief/treesit-php-ready-p) (lambda () available))
              ((symbol-function 'php-mode) (lambda () (setq selected 'classic))))
      (chief/treesit-php-maybe-a (lambda () (setq selected 'treesit)))
      (should (eq selected 'classic))
      (setq available t)
      (chief/treesit-php-maybe-a (lambda () (setq selected 'treesit)))
      (should (eq selected 'treesit)))))

(ert-deftest chief/racket-keeps-its-configured-mode-after-geiser ()
  "Geiser's autoloads must not bypass Racket's configured helpers."
  (let ((auto-mode-alist
         (copy-tree '(("\\.rkt\\'" . scheme-mode)
                      ("\\.rkt\\'" . racket-mode)
                      ("\\.scm\\'" . scheme-mode)))))
    (chief/racket-preserve-file-association)
    (should (eq (assoc-default "test.rkt" auto-mode-alist #'string-match-p)
                'racket-mode))
    (should (eq (assoc-default "test.scm" auto-mode-alist #'string-match-p)
                'scheme-mode))))

(ert-deftest chief/theme-inheritance-is-sanitized-before-face-application ()
  "Reversed Gnus inheritance must not fail before enable-theme can patch it."
  (let* ((arguments
          '(test-theme
            (gnus-group-news-6-empty
             ((t (:inherit gnus-group-news-6 :foreground "#abcdef" :weight bold))))
            (font-lock-comment-face ((t (:inherit shadow))))
            (gnus-group-news-7-empty
             ((t :inherit gnus-group-news-7 :foreground "#123456")))
            (org-hide ((t (:inherit org-indent :foreground "#000000"))))))
         (result (chief/theme-acyclic-gnus-face-arguments arguments))
         (attributes (cadr (car (cadr (cadr result))))))
    (should (eq (car result) 'test-theme))
    (should (eq (plist-get attributes :inherit) 'default))
    (should (equal (plist-get attributes :foreground) "#abcdef"))
    (should (eq (plist-get attributes :weight) 'bold))
    (should (equal (nth 2 result) (nth 2 arguments)))
    (let ((flat-attributes (cadr (car (cadr (nth 3 result))))))
      (should (eq (plist-get flat-attributes :inherit) 'default))
      (should (equal (plist-get flat-attributes :foreground) "#123456")))
    (should (eq (plist-get (cadr (car (cadr (nth 4 result)))) :inherit) 'default))
    (should (eq (plist-get (cadr (car (cadr (cadr arguments)))) :inherit)
                'gnus-group-news-6))))

(ert-deftest chief/theme-user-overlays-preserve-underlying-theme-settings ()
  "Compatibility overrides must not delete the theme colors beneath them."
  (let* ((face (make-symbol "chief-test-face"))
         (underlying '(test-theme ((t (:foreground "#abcdef")))))
         (old (list (list 'user '((t (:inherit shadow)))) underlying)))
    (put face 'theme-face old)
    (chief/theme--store-user-face-spec face '((t (:inherit default))))
    (should (equal (assq 'user (get face 'theme-face))
                   '(user ((t (:inherit default))))))
    (should (eq (assq 'test-theme (get face 'theme-face)) underlying))
    (should (equal (car old) '(user ((t (:inherit shadow))))))
    (chief/theme--apply-user-face-specs
     (list (list face '((t (:background "#121212"))))))
    (let ((attributes (cadr (car (cadr (assq 'user (get face 'theme-face)))))))
      (should (eq (plist-get attributes :inherit) 'default))
      (should (equal (plist-get attributes :background) "#121212")))
    (should (eq (assq 'test-theme (get face 'theme-face)) underlying))))

(provide 'core-tools-test)
;;; core-tools-test.el ends here
