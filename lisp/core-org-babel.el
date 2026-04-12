;;; core-org-babel.el --- Org Babel language execution -*- lexical-binding: t; -*-

(require 'ob)
(require 'org)
(require 'subr-x)

(defun chief/org-babel-execution-directory (params)
  "Return the execution directory derived from PARAMS."
  (file-name-as-directory
   (expand-file-name
    (or (cdr (assq :dir params))
        default-directory))))

(defun chief/org-babel-write-temp-file (body extension params prefix)
  "Write BODY to a temporary file with EXTENSION for PARAMS using PREFIX."
  (let* ((directory (chief/org-babel-execution-directory params))
         (temporary-file-directory directory)
         (file (make-temp-file (expand-file-name prefix directory) nil extension)))
    (with-temp-file file
      (insert body))
    file))

(defun chief/org-babel-make-temp-directory (params prefix)
  "Create a temporary directory for PARAMS using PREFIX."
  (let* ((directory (chief/org-babel-execution-directory params))
         (temporary-file-directory directory))
    (make-temp-file (expand-file-name prefix directory) t)))

(defun chief/org-babel-run-command (command params)
  "Run COMMAND for Babel PARAMS and return its trimmed output."
  (let ((default-directory (chief/org-babel-execution-directory params))
        (stdout (generate-new-buffer " *chief-org-babel-stdout*"))
        (stderr-file (make-temp-file "chief-org-babel-stderr-")))
    (unwind-protect
        (let ((status (apply #'call-process
                             (car command)
                             nil
                             (list stdout stderr-file)
                             nil
                             (cdr command))))
          (if (zerop status)
              (with-current-buffer stdout
                (string-trim-right (buffer-string)))
            (error "%s"
                   (with-temp-buffer
                     (insert-file-contents stderr-file)
                     (string-trim (buffer-string))))))
      (kill-buffer stdout)
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

(defun chief/org-babel-format-result (result params)
  "Format RESULT according to Babel PARAMS."
  (let ((result-params (cdr (assq :result-params params))))
    (org-babel-result-cond result-params
      result
      (condition-case nil
          (org-babel-read result)
        (error result)))))

(defun chief/org-babel-execute-command-language (body params extension builder)
  "Execute BODY according to PARAMS using EXTENSION and command BUILDER."
  (let ((file (chief/org-babel-write-temp-file body extension params "chief-org-")))
    (unwind-protect
        (chief/org-babel-format-result
         (chief/org-babel-run-command (funcall builder file params) params)
         params)
      (when (file-exists-p file)
        (delete-file file)))))

(defun chief/org-babel-jsts-runtime (params)
  "Return the JS/TS runtime requested by PARAMS."
  (let ((runtime (cdr (assq :runtime params))))
    (if runtime
        (intern (format "%s" runtime))
      (let ((default-directory (chief/org-babel-execution-directory params))
            (chief/jsts-runtime nil))
        (if (fboundp 'chief/jsts-current-runtime)
            (or (chief/jsts-current-runtime) 'node)
          'node)))))

(defun chief/org-babel-execute-jsts (body params language)
  "Execute JavaScript or TypeScript BODY for PARAMS and LANGUAGE."
  (unless (fboundp 'chief/jsts-script-command)
    (user-error "lang-jsts.el is not loaded"))
  (let* ((runtime (chief/org-babel-jsts-runtime params))
         (extension (pcase language
                      ('tsx ".tsx")
                      ('typescript ".ts")
                      (_ ".js")))
         (file (chief/org-babel-write-temp-file body extension params "chief-org-jsts-")))
    (unwind-protect
        (chief/org-babel-format-result
         (chief/jsts-run-file file runtime language params)
         params)
      (when (file-exists-p file)
        (delete-file file)))))

(defmacro chief/org-babel-define-command-backend (language extension &rest command-form)
  "Define a simple command-backed Org Babel LANGUAGE.
EXTENSION is the temp file extension and COMMAND-FORM must return a command list."
  (declare (indent 2))
  `(defun ,(intern (format "org-babel-execute:%s" language)) (body params)
     ,(format "Execute a %s source block with Org Babel." language)
     (chief/org-babel-execute-command-language
      body
      params
      ,extension
      (lambda (file _params)
        ,@command-form))))

(setq org-babel-python-command (or (executable-find "python3") "python3"))
(setq org-babel-clojure-backend 'cider)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (R . t)
   (clojure . t)
   (dot . t)
   (haskell . t)
   (java . t)
   (js . t)
   (julia . t)
   (lisp . t)
   (lua . t)
   (ocaml . t)
   (plantuml . t)
   (python . t)
   (ruby . t)
   (scheme . t)
   (shell . t)
   (sql . t)))

(dolist (entry '(("javascript" . js)
                 ("js" . js)
                 ("typescript" . typescript-ts)
                 ("ts" . typescript-ts)
                 ("tsx" . tsx-ts)
                 ("rust" . rust-ts)
                 ("csharp" . csharp-ts)
                 ("cs" . csharp-ts)
                 ("fsharp" . fsharp)
                 ("fsx" . fsharp)
                 ("go" . go-ts)
                 ("golang" . go-ts)
                 ("swift" . swift)
                 ("kotlin" . kotlin)
                 ("dart" . dart)
                 ("elixir" . elixir)
                 ("erlang" . erlang)
                 ("nim" . nim)
                 ("nushell" . nushell)
                 ("nu" . nushell)
                 ("fennel" . fennel)
                 ("janet" . janet)
                 ("templ" . templ-ts)
                 ("racket" . racket)
                 ("babashka" . clojure)
                 ("cljd" . clojure)
                 ("nbb" . clojure)
                 ("zig" . zig)))
  (add-to-list 'org-src-lang-modes entry))

(defalias 'org-babel-execute:javascript
  (lambda (body params)
    (chief/org-babel-execute-jsts body params 'javascript)))

(defalias 'org-babel-execute:js
  (lambda (body params)
    (chief/org-babel-execute-jsts body params 'javascript)))

(defalias 'org-babel-execute:typescript
  (lambda (body params)
    (chief/org-babel-execute-jsts body params 'typescript)))

(defalias 'org-babel-execute:ts
  (lambda (body params)
    (chief/org-babel-execute-jsts body params 'typescript)))

(defalias 'org-babel-execute:tsx
  (lambda (body params)
    (chief/org-babel-execute-jsts body params 'tsx)))

(chief/org-babel-define-command-backend swift ".swift"
  (list (or (executable-find "swift")
            (user-error "Swift is not available on PATH"))
        file))

(defun org-babel-execute:rust (body params)
  "Execute a Rust source block with Org Babel."
  (let* ((file (chief/org-babel-write-temp-file body ".rs" params "chief-org-rust-"))
         (binary (make-temp-file (expand-file-name "chief-org-rust-bin-" (chief/org-babel-execution-directory params)))))
    (unwind-protect
        (progn
          (when (file-exists-p binary)
            (delete-file binary))
          (chief/org-babel-run-command
           (list (or (executable-find "rustc")
                     (user-error "rustc is not available on PATH"))
                 file
                 "-o"
                 binary)
           params)
          (chief/org-babel-format-result
           (chief/org-babel-run-command (list binary) params)
           params))
      (when (file-exists-p file)
        (delete-file file))
      (when (file-exists-p binary)
        (delete-file binary)))))

(chief/org-babel-define-command-backend go ".go"
  (list (or (executable-find "go")
            (user-error "Go is not available on PATH"))
        "run"
        file))

(defalias 'org-babel-execute:golang #'org-babel-execute:go)

(chief/org-babel-define-command-backend kotlin ".kts"
  (list (or (executable-find "kotlinc")
            (user-error "kotlinc is not available on PATH"))
        "-script"
        file))

(defun org-babel-execute:csharp (body params)
  "Execute a C# source block with Org Babel."
  (unless (executable-find "dotnet")
    (user-error "dotnet is not available on PATH"))
  (let* ((dir (chief/org-babel-make-temp-directory params "chief-org-csharp-"))
         (program-file (expand-file-name "Program.cs" dir))
         (project-file (expand-file-name "chief-org-csharp.csproj" dir)))
    (unwind-protect
        (progn
          (with-temp-file program-file
            (insert body))
          (with-temp-file project-file
            (insert "<Project Sdk=\"Microsoft.NET.Sdk\">\n"
                    "  <PropertyGroup>\n"
                    "    <OutputType>Exe</OutputType>\n"
                    "    <TargetFramework>net10.0</TargetFramework>\n"
                    "    <ImplicitUsings>enable</ImplicitUsings>\n"
                    "    <Nullable>enable</Nullable>\n"
                    "  </PropertyGroup>\n"
                    "</Project>\n"))
          (chief/org-babel-format-result
           (chief/org-babel-run-command
            (list "dotnet" "run" "--project" project-file)
            params)
           params))
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(chief/org-babel-define-command-backend fsharp ".fsx"
  (list (or (executable-find "dotnet")
            (user-error "dotnet is not available on PATH"))
        "fsi"
        "--exec"
        file))

(chief/org-babel-define-command-backend dart ".dart"
  (list (or (executable-find "dart")
            (user-error "Dart is not available on PATH"))
        "run"
        file))

(chief/org-babel-define-command-backend elixir ".exs"
  (list (or (executable-find "elixir")
            (user-error "Elixir is not available on PATH"))
        file))

(defun org-babel-execute:erlang (body params)
  "Execute an Erlang source block with Org Babel."
  (let* ((erl (or (executable-find "erl")
                  (user-error "erl is not available on PATH")))
         (form (replace-regexp-in-string "\\.[[:space:]\n\r]*\\'" "" (string-trim body)))
         (command (list erl "-noshell" "-eval" (concat form ", halt()."))))
    (chief/org-babel-format-result
     (chief/org-babel-run-command command params)
     params)))

(chief/org-babel-define-command-backend nim ".nim"
  (list (or (executable-find "nim")
            (user-error "Nim is not available on PATH"))
        "r"
        file))

(chief/org-babel-define-command-backend nushell ".nu"
  (list (or (executable-find "nu")
            (user-error "Nushell is not available on PATH"))
        file))

(defalias 'org-babel-execute:nu #'org-babel-execute:nushell)

(chief/org-babel-define-command-backend fennel ".fnl"
  (list (or (executable-find "fennel")
            (user-error "Fennel is not available on PATH"))
        file))

(chief/org-babel-define-command-backend janet ".janet"
  (list (or (executable-find "janet")
            (user-error "Janet is not available on PATH"))
        file))

(chief/org-babel-define-command-backend racket ".rkt"
  (list (or (executable-find "racket")
            (user-error "Racket is not available on PATH"))
        file))

(chief/org-babel-define-command-backend babashka ".clj"
  (list (or (executable-find "bb")
            (user-error "Babashka is not available on PATH"))
        file))

(chief/org-babel-define-command-backend nbb ".cljs"
  (list (or (executable-find "nbb")
            (user-error "nbb is not available on PATH"))
        file))

(chief/org-babel-define-command-backend templ ".templ"
  (list (or (executable-find "templ")
            (user-error "templ is not available on PATH"))
        "generate"
        "-f"
        file
        "-stdout"))

(chief/org-babel-define-command-backend zig ".zig"
  (list (or (executable-find "zig")
            (user-error "zig is not available on PATH"))
        "run"
        file))

(provide 'core-org-babel)
;;; core-org-babel.el ends here
