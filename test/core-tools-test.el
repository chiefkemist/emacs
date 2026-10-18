;;; core-tools-test.el --- Tooling update regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'core-tools)
(require 'core-terminal)

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

(provide 'core-tools-test)
;;; core-tools-test.el ends here
