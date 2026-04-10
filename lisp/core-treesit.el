;;; core-treesit.el --- Tree-sitter setup -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'project)
(require 'subr-x)

(setq treesit-font-lock-level 3)

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
        (clojure "https://github.com/sogaiu/tree-sitter-clojure")
        (cue "https://github.com/eonpatapon/tree-sitter-cue")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (dart "https://github.com/UserNobody14/tree-sitter-dart")
        (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
        (erlang "https://github.com/WhatsApp/tree-sitter-erlang")
        (fennel "https://github.com/alexmozaidze/tree-sitter-fennel")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (heex "https://github.com/phoenixframework/tree-sitter-heex")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (janet-simple "https://github.com/sogaiu/tree-sitter-janet-simple")
        (java "https://github.com/tree-sitter/tree-sitter-java")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (kotlin "https://github.com/fwcd/tree-sitter-kotlin")
        (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (nix "https://github.com/nix-community/tree-sitter-nix")
        (nim "https://github.com/alaviss/tree-sitter-nim")
        (nu "https://github.com/nushell/tree-sitter-nu")
        (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml" nil "grammars/ocaml")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (r "https://github.com/r-lib/tree-sitter-r")
        (racket "https://github.com/6cdh/tree-sitter-racket")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (templ "https://github.com/vrischmann/tree-sitter-templ")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (zig "https://github.com/maxxnino/tree-sitter-zig")))

(defvar chief/treesit-install-prompt-history (make-hash-table :test #'equal)
  "Remember tree-sitter install prompts already shown this session.")

(defvar chief/treesit-manual-guidance-history (make-hash-table :test #'equal)
  "Remember one-shot tree-sitter manual guidance messages shown this session.")

(defconst chief/treesit-manual-install-specs
  '((swift
     :doc-url "https://github.com/alex-pinkus/tree-sitter-swift"
     :help "The maintained Swift grammar omits generated parser sources like parser.c, so Emacs cannot auto-build it with treesit-install-language-grammar.")))

(defconst chief/treesit-fallback-language-alist
  '((csharp-ts-mode . c-sharp)
    (cue-mode . cue)
    (dart-mode . dart)
    (elixir-mode . elixir)
    (elixir-ts-mode . elixir)
    (erlang-mode . erlang)
    (ess-r-mode . r)
    (fennel-mode . fennel)
    (go-mode . go)
    (go-ts-mode . go)
    (heex-ts-mode . heex)
    (janet-mode . janet-simple)
    (kotlin-mode . kotlin)
    (nim-mode . nim)
    (nushell-mode . nu)
    (python-mode . python)
    (python-ts-mode . python)
    (racket-mode . racket)
    (reason-mode . ocaml)
    (rust-ts-mode . rust)
    (swift-mode . swift)
    (templ-ts-mode . templ)
    (tuareg-mode . ocaml))
  "Fallback map from major modes to tree-sitter language symbols.")

(defun chief/treesit-manual-install-spec (lang)
  "Return manual tree-sitter install guidance for LANG, if any."
  (alist-get lang chief/treesit-manual-install-specs))

(defun chief/treesit-manual-guidance-key (lang)
  "Return a session key for LANG manual guidance."
  (format "%s:%s"
          lang
          (or (when-let* ((project (project-current nil)))
                (project-root project))
              default-directory)))

(defun chief/treesit-maybe-show-manual-guidance (lang)
  "Show one-shot manual guidance for LANG when auto-install is disabled."
  (when-let* ((spec (chief/treesit-manual-install-spec lang))
              (key (chief/treesit-manual-guidance-key lang))
              ((not (gethash key chief/treesit-manual-guidance-history))))
    (puthash key t chief/treesit-manual-guidance-history)
    (message
     "Tree-sitter auto-install for %s is disabled. %s %s"
     (symbol-name lang)
     (plist-get spec :help)
     (or (plist-get spec :doc-url) ""))))

(defun chief/treesit-buffer-language-from-file ()
  "Infer a tree-sitter language from `buffer-file-name' when possible."
  (when-let* ((file buffer-file-name))
    (cond
     ((string-match-p "\\.\\(heex\\|leex\\|surface\\)\\'" file) 'heex)
     ((string-match-p "\\.nu\\'" file) 'nu)
     ((string-match-p "\\.templ\\'" file) 'templ)
     (t nil))))

(defun chief/treesit-buffer-language ()
  "Return the tree-sitter language symbol for the current buffer."
  (or (when (featurep 'treesit-auto)
        (when-let* ((recipe (ignore-errors (treesit-auto--get-mode-recipe))))
          (treesit-auto-recipe-lang recipe)))
      (chief/treesit-buffer-language-from-file)
      (alist-get major-mode chief/treesit-fallback-language-alist)))

(defun chief/treesit-source-alist ()
  "Return the merged tree-sitter source alist for the current session."
  (if (featurep 'treesit-auto)
      (treesit-auto--build-treesit-source-alist)
    treesit-language-source-alist))

(defun chief/treesit-ts-mode-available-p ()
  "Return non-nil when a tree-sitter major mode is available for this buffer."
  (when (featurep 'treesit-auto)
    (when-let* ((recipe (ignore-errors (treesit-auto--get-mode-recipe))))
      (fboundp (treesit-auto-recipe-ts-mode recipe)))))

(defun chief/treesit-auto-handles-buffer-p ()
  "Return non-nil when `treesit-auto' already handles this buffer's prompt."
  (and (featurep 'treesit-auto)
       treesit-auto-install
       (when-let* ((recipe (ignore-errors (treesit-auto--get-mode-recipe))))
         (fboundp (treesit-auto-recipe-ts-mode recipe)))))

(defun chief/treesit--prompt-key (lang)
  "Return a session key for LANG in the current project or directory."
  (format "%s:%s"
          lang
          (or (when-let* ((project (project-current nil)))
                (project-root project))
              default-directory)))

(defun chief/treesit-maybe-offer-install ()
  "Offer to install a missing tree-sitter grammar for the current file."
  (when (and (not noninteractive)
             buffer-file-name
             (not (file-remote-p buffer-file-name))
             (fboundp 'treesit-install-language-grammar)
             (not (chief/treesit-auto-handles-buffer-p)))
    (when-let* ((lang (chief/treesit-buffer-language))
                (key (chief/treesit--prompt-key lang))
                ((not (gethash key chief/treesit-install-prompt-history)))
                ((not (treesit-ready-p lang t))))
      (cond
       ((chief/treesit-manual-install-spec lang)
        (chief/treesit-maybe-show-manual-guidance lang))
       ((alist-get lang (chief/treesit-source-alist))
        (puthash key t chief/treesit-install-prompt-history)
        (let ((treesit-language-source-alist (chief/treesit-source-alist)))
          (when (y-or-n-p
                 (format "Tree-sitter grammar for %s is missing. Install it now? "
                         (symbol-name lang)))
            (condition-case err
                (progn
                  (treesit-install-language-grammar lang)
                  (if (chief/treesit-ts-mode-available-p)
                      (normal-mode)
                    (message
                     "Installed the %s grammar. This setup does not yet have a dedicated %s tree-sitter major mode, so the current major mode stays active."
                     lang
                     lang)))
              (error
               (remhash key chief/treesit-install-prompt-history)
               (message
                "Tree-sitter install for %s failed: %s"
                (symbol-name lang)
                (error-message-string err)))))))))))

(use-package treesit-auto
  :defer 1
  :config
  (setq treesit-auto-install nil)
  (global-treesit-auto-mode 1)
  (add-hook 'find-file-hook #'chief/treesit-maybe-offer-install))

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode)
         (clojuredart-mode . rainbow-delimiters-mode)
         (clojurescript-mode . rainbow-delimiters-mode)
         (clojurec-mode . rainbow-delimiters-mode)
         (cider-repl-mode . rainbow-delimiters-mode)
         (fennel-mode . rainbow-delimiters-mode)
         (hy-mode . rainbow-delimiters-mode)
         (janet-mode . rainbow-delimiters-mode)
         (lisp-mode . rainbow-delimiters-mode)
         (scheme-mode . rainbow-delimiters-mode)
         (racket-mode . rainbow-delimiters-mode)
         (sly-mrepl-mode . rainbow-delimiters-mode)))

(chief/leader-def
  "ti" #'treesit-install-language-grammar
  "te" #'treesit-explore-mode
  "tn" #'treesit-inspect-mode)

(provide 'core-treesit)
;;; core-treesit.el ends here
