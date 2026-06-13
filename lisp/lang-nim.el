;;; lang-nim.el --- Nim and Nimble integration -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'compile)
(require 'comint)
(require 'core-projects)
(require 'core-repl)
(require 'json)
(require 'seq)
(require 'subr-x)

(declare-function dap-debug "dap-mode" (debug-args))

(defun chief/nim-project-root ()
  "Return the current Nim project root, if any."
  (or (when-let* ((file (chief/project-nearest-matching-file "\\.nimble\\'")))
        (chief/project-normalize-root (file-name-directory file)))
      (chief/project-preferred-root
       '(".git")
       (chief/project-current-root)
       default-directory)))

(defcustom chief/nim-run-args nil
  "Extra program arguments appended when running a single Nim main module."
  :type '(repeat string)
  :group 'chief)

(defcustom chief/nim-test-args nil
  "Extra arguments appended to Nim unittest executables."
  :type '(repeat string)
  :group 'chief)

(defcustom chief/nimble-run-args nil
  "Extra program arguments appended after `--' for `nimble run'."
  :type '(repeat string)
  :group 'chief)

(defcustom chief/nimble-test-args nil
  "Extra arguments appended to `nimble test'."
  :type '(repeat string)
  :group 'chief)

(defcustom chief/nimble-build-args nil
  "Extra arguments appended to `nimble build'."
  :type '(repeat string)
  :group 'chief)

(defcustom chief/nim-compile-switches '("--hints:off" "--colors:off" "--filenames:abs")
  "Default switches added to Nim compiler commands before project files."
  :type '(repeat string)
  :group 'chief)

(defcustom chief/nim-default-backend "c"
  "Default Nim backend used by backend-aware helpers."
  :type '(choice (const "c") (const "cpp") (const "js") (const "objc") string)
  :group 'chief)

(defcustom chief/nim-backends '("c" "cpp" "c++" "js" "javascript" "objc" "objective-c")
  "Nim backends offered by run/test/doc helpers.
The compiler command for C++ is `cpp'; user input like `c++' is normalized to
that backend."
  :type '(repeat string)
  :group 'chief)

(defcustom chief/nim-compile-commands '("c" "compile" "cc" "cpp" "js" "objc" "compileToC" "compileToCpp" "compileToOC")
  "Nim compiler commands offered by build helpers.
`c' and `compile' are Nim's default C build commands; `cc'/`compileToC',
`cpp'/`compileToCpp', `js', and `objc'/`compileToOC' expose the documented
backend commands."
  :type '(repeat string)
  :group 'chief)

(defcustom chief/nim-secret-extra-switches nil
  "Extra switches passed to `nim secret' REPL sessions."
  :type '(repeat string)
  :group 'chief)

(defcustom chief/nim-secret-use-package-src-path t
  "When non-nil, add the Nimble package srcDir to `nim secret' search paths."
  :type 'boolean
  :group 'chief)

(defcustom chief/nim-secret-display-buffer-after-send t
  "When non-nil, display the Nim REPL buffer after sending code to it."
  :type 'boolean
  :group 'chief)

(defcustom chief/nim-secret-auto-restart t
  "When non-nil, restart `nim secret' after an abnormal runtime-error exit.
The built-in Nim REPL terminates on unhandled VM/runtime exceptions; this keeps
the Emacs REPL buffer usable by starting a fresh process after those crashes."
  :type 'boolean
  :group 'chief)

(defcustom chief/nim-secret-replay-persistent-inputs t
  "When non-nil, replay simple imports and declarations after REPL auto-restart."
  :type 'boolean
  :group 'chief)

(defcustom chief/nim-secret-direct-input-protection 'dangerous
  "How to protect code typed directly at the `nim secret' comint prompt.
`dangerous' wraps assertions and raises in try/except before sending them.
`all' wraps every non-declaration input, but plain expression values will no
longer use Nim's native typed REPL printer.  nil sends direct input unchanged."
  :type '(choice (const :tag "Disabled" nil)
                 (const :tag "Assertions/raises only" dangerous)
                 (const :tag "All non-declaration input" all))
  :group 'chief)

(defcustom chief/nim-lsp-prefer-nimlangserver t
  "When non-nil, prefer `nimlangserver' over `nimlsp' when both are available."
  :type 'boolean
  :group 'chief)

(defcustom chief/nim-lsp-use-nim-check t
  "When non-nil, ask `nimlangserver' to use `nim check' for linting."
  :type 'boolean
  :group 'chief)

(defcustom chief/nim-lsp-check-on-save t
  "When non-nil, ask `nimlangserver' to check files when they are saved."
  :type 'boolean
  :group 'chief)

(defcustom chief/nim-lsp-auto-restart t
  "When non-nil, ask `nimlangserver' to restart crashed `nimsuggest' processes."
  :type 'boolean
  :group 'chief)

(defcustom chief/nim-flycheck-enable t
  "When non-nil, use compiler-backed `nim check' diagnostics in Nim buffers."
  :type 'boolean
  :group 'chief)

(defvar-local chief/nim-secret-deliberate-stop nil
  "Non-nil when the current `nim secret' process is expected to stop.")

(defvar-local chief/nim-secret-persistent-inputs nil
  "Simple top-level inputs replayed after an automatic Nim REPL restart.")

(defvar-local chief/nim-secret-replaying-inputs nil
  "Non-nil while replaying inputs into a restarted Nim REPL.")

(defvar-local chief/nim-secret-hide-continuation-prompts nil
  "Non-nil while hidden safe-wrapper prompts should be suppressed.")

(defun chief/nim--current-file (language)
  "Return the current buffer file for LANGUAGE as an absolute path."
  (unless buffer-file-name
    (user-error "This buffer is not visiting a %s file" language))
  (expand-file-name buffer-file-name))

(defun chief/nim--save-current-buffer ()
  "Save the current buffer when it is visiting a modified file."
  (when (and buffer-file-name (buffer-modified-p))
    (save-buffer)))

(defun chief/nim--command (program &rest args)
  "Return a command list starting with PROGRAM and ARGS."
  (unless (executable-find program)
    (user-error "%s is not available on PATH" program))
  (cons program args))

(defun chief/nim--first-executable-file (files)
  "Return the first executable regular file in FILES."
  (seq-find (lambda (file)
              (and (stringp file)
                   (file-regular-p file)
                   (file-executable-p file)))
            files))

(defun chief/nim-find-executable (program &rest extra-globs)
  "Find PROGRAM in PATH, ~/.nimble/bin, or EXTRA-GLOBS."
  (or (executable-find program)
      (chief/nim--first-executable-file
       (list (expand-file-name (concat ".nimble/bin/" program) "~")))
      (chief/nim--first-executable-file
       (apply #'append (mapcar #'file-expand-wildcards extra-globs)))))

(defun chief/nim-executable-usable-p (program &rest args)
  "Return non-nil when PROGRAM exits successfully with ARGS."
  (and program
       (file-executable-p program)
       (with-temp-buffer
         (zerop (condition-case nil
                    (apply #'call-process program nil t nil args)
                  (error 255))))))

(defun chief/nim-nimsuggest-executable ()
  "Return the preferred `nimsuggest' executable."
  (chief/nim-find-executable "nimsuggest"))

(defun chief/nimlsp-executable ()
  "Return the preferred `nimlsp' executable."
  (chief/nim-find-executable "nimlsp"))

(defun chief/nimlangserver-executable ()
  "Return the preferred `nimlangserver' executable.
In addition to PATH and ~/.nimble/bin, this checks Nimble's package cache; some
Nimble installs leave a built server there without linking it into ~/.nimble/bin."
  (seq-find
   (lambda (program)
     (chief/nim-executable-usable-p program "--version"))
   (delete-dups
    (delq nil
          (append
           (list (executable-find "nimlangserver")
                 (expand-file-name ".nimble/bin/nimlangserver" "~")
                 (expand-file-name ".nimble/pkgcache/githubcom_nimlanglangserver/nimlangserver" "~"))
           (file-expand-wildcards
            (expand-file-name ".nimble/pkgs2/nimlangserver-*/nimlangserver" "~")))))))

(defun chief/nim-configure-lsp ()
  "Configure Nim LSP clients for the current buffer."
  (when (require 'lsp-nim nil t)
    (when-let* ((nimsuggest (chief/nim-nimsuggest-executable)))
      (setq lsp-nim-nimsuggest-path nimsuggest))
    (let ((nimlangserver (chief/nimlangserver-executable))
          (nimlsp (chief/nimlsp-executable)))
      (when nimlangserver
        (setq lsp-nim-langserver nimlangserver))
      (when nimlsp
        (setq lsp-nim-lsp nimlsp))
      (setq-local lsp-enabled-clients
                  (cond
                   ((and chief/nim-lsp-prefer-nimlangserver nimlangserver)
                    '(nimlangserver))
                   (nimlsp '(nimlsp))
                   (nimlangserver '(nimlangserver))
                   (t '(nimlangserver nimlsp)))))
    ;; Nim LSP diagnostics are inconsistent across `nimlsp' and `nimlangserver'
    ;; versions, so editor error highlighting uses the compiler-backed Flycheck
    ;; checker below while LSP remains responsible for navigation/completion.
    (setq-local lsp-diagnostics-provider :none)
    (setq-local lsp-nim-auto-check-file t)
    (setq-local lsp-nim-auto-check-project t)
    (setq-local lsp-nim-timeout 120000)))

(with-eval-after-load 'lsp-mode
  (require 'lsp-nim nil t)
  (lsp-register-custom-settings
   '(("nim.useNimCheck" chief/nim-lsp-use-nim-check t)
     ("nim.checkOnSave" chief/nim-lsp-check-on-save t)
     ("nim.autoRestart" chief/nim-lsp-auto-restart t))))

(defun chief/nim--completion-setup (&optional zero-prefix)
  "Enable Corfu/LSP completion for Nim buffers.
When ZERO-PREFIX is non-nil, popup completion may trigger immediately after
member-access punctuation with an empty textual prefix."
  (setq-local lsp-completion-enable t)
  (setq-local lsp-completion-enable-additional-text-edit t)
  (when (boundp 'corfu-auto)
    (setq-local corfu-auto t))
  (when (boundp 'corfu-auto-delay)
    (setq-local corfu-auto-delay 0.05))
  (when (and zero-prefix (boundp 'corfu-auto-prefix))
    (setq-local corfu-auto-prefix 0))
  (when (fboundp 'corfu-mode)
    (when (bound-and-true-p corfu-mode)
      (corfu-mode -1))
    (corfu-mode 1))
  (when (fboundp 'lsp-completion-mode)
    (lsp-completion-mode 1)))

(defun chief/nim--run-command-async (command directory buffer-name on-success)
  "Run COMMAND asynchronously in DIRECTORY.
Output goes to BUFFER-NAME.  ON-SUCCESS receives the buffer output after a zero
exit status."
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (setq buffer-read-only nil)
        (erase-buffer)
        (setq-local default-directory directory)))
    (let ((process
           (make-process
            :name buffer-name
            :buffer buffer
            :stderr buffer
            :command command
            :noquery t
            :sentinel
            (lambda (process _event)
              (when (memq (process-status process) '(exit signal))
                (let* ((status (process-exit-status process))
                       (success (and (eq (process-status process) 'exit)
                                     (zerop status)))
                       (output (when (buffer-live-p (process-buffer process))
                                 (with-current-buffer (process-buffer process)
                                   (buffer-string)))))
                  (if success
                      (funcall on-success output)
                    (when (buffer-live-p (process-buffer process))
                      (display-buffer (process-buffer process)))
                    (message "%s failed with exit status %s" buffer-name status))))))))
      (set-process-query-on-exit-flag process nil)
      process)))

(defun chief/nim--lldb-dap-debug (program name cwd source-language &optional args)
  "Start a DAP session for PROGRAM named NAME in CWD.
SOURCE-LANGUAGE is sent to LLDB/GDB when supported.  ARGS are program args."
  (require 'dap-mode)
  (let ((configuration
         (cond
          ((featurep 'dap-lldb)
           (list :type "lldb-vscode"
                 :request "launch"
                 :name name
                 :program program
                 :cwd cwd
                 :args (or args [])
                 :sourceLanguages (vector source-language)))
          ((featurep 'dap-gdb)
           (list :type "gdb"
                 :request "launch"
                 :name name
                 :program program
                 :target program
                 :cwd cwd
                 :args (or args [])
                 :sourceLanguages (vector source-language)))
          (t
           (user-error "No LLDB/GDB DAP adapter is configured")))))
    (dap-debug configuration)))

(defun chief/nim-shell-quote-argument (argument)
  "Quote Nim shell ARGUMENT without making compiler switches unreadable.
Emacs' generic `shell-quote-argument' escapes colons, which is correct but makes
Nim switches like `--hints:off' look broken in compilation buffers."
  (if (string-match-p "\\`[[:alnum:]_./:@%+=,-]+\\'" argument)
      argument
    (shell-quote-argument argument)))

(defun chief/nim-shell-command (command)
  "Return a shell-safe, readable string for Nim COMMAND."
  (mapconcat #'chief/nim-shell-quote-argument command " "))

(defun chief/nimble-compile (command name &optional directory)
  "Run Nimble COMMAND in DIRECTORY using compilation buffer NAME.
Nimble can report package build failures while still exiting successfully, so
this wrapper treats visible Nimble error output as a failed compilation while
showing the clean Nimble command in the compilation buffer."
  (let* ((default-directory (or directory (chief/nim-project-root)))
         (compilation-read-command nil)
         (display-command (chief/nim-shell-command command))
         (error-filter
          "BEGIN { bad = 0 } /(^|[[:space:]])Error:|Build failed|Tests failed/ { bad = 1 } { print } END { exit bad }")
         (script
          (format "set +e; %s 2>&1 | awk %s; statuses=(${PIPESTATUS[@]}); if [ ${statuses[1]} -ne 0 ]; then exit 1; fi; exit ${statuses[0]}"
                  display-command
                  (shell-quote-argument error-filter)))
         (wrapper-command (format "bash -lc %s" (shell-quote-argument script)))
         (process-environment (append '("NO_COLOR=1" "CLICOLOR=0")
                                      process-environment))
         (buffer (compilation-start wrapper-command 'compilation-mode (lambda (_) name))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (when (search-forward wrapper-command nil t)
            (replace-match display-command t t)))))
    buffer))

(defun chief/nim-compile (command name &optional directory)
  "Run Nim COMMAND in DIRECTORY using compilation buffer NAME.
Force non-interactive/no-color output so compilation buffers remain parseable."
  (let ((process-environment (append '("NO_COLOR=1" "CLICOLOR=0")
                                     process-environment)))
    (if (string= (file-name-nondirectory (car command)) "nimble")
        (chief/nimble-compile command name directory)
      (let ((default-directory (or directory (chief/nim-project-root)))
            (compilation-read-command nil))
        (compilation-start
         (chief/nim-shell-command command)
         'compilation-mode
         (lambda (_) name))))))

(defconst chief/nim-required-compile-switches
  '("--hints:off" "--colors:off" "--filenames:abs")
  "Nim switches required for editor-friendly, parseable compilation output.")

(defun chief/nim-effective-compile-switches ()
  "Return Nim compiler switches, forcing editor-required output flags.
This also repairs already-running sessions where `chief/nim-compile-switches'
was bound before newer defaults were loaded.  `--listFullPaths:on' is stripped
because current Nim uses `--filenames:abs' for absolute diagnostic paths."
  (append
   (seq-remove
    (lambda (arg)
      (or (string-prefix-p "--hints:" arg)
          (string-prefix-p "--colors:" arg)
          (string-prefix-p "--filenames:" arg)
          (string-prefix-p "--listFullPaths:" arg)))
    chief/nim-compile-switches)
   chief/nim-required-compile-switches))

(defun chief/nim-command (subcommand &rest args)
  "Return a Nim command list for SUBCOMMAND and ARGS.
Configured compiler switches are inserted before filenames/program args so they
are interpreted by the compiler rather than by `nim c -r' executables."
  (append (chief/nim--command "nim" subcommand)
          (chief/nim-effective-compile-switches)
          args))

(defun chief/nim-normalize-backend (backend)
  "Normalize BACKEND to Nim's backend names."
  (let ((backend (downcase (string-trim (or backend chief/nim-default-backend)))))
    (pcase backend
      ((or "c++" "cplusplus" "cxx") "cpp")
      ((or "javascript" "node") "js")
      ((or "objective-c" "objectivec" "objective_c" "oc") "objc")
      ("cc" "c")
      (_ backend))))

(defun chief/nim-normalize-compile-command (backend-or-command)
  "Normalize BACKEND-OR-COMMAND to a Nim compile command."
  (let* ((raw (string-trim (or backend-or-command chief/nim-default-backend)))
         (command (downcase raw)))
    (pcase command
      ((or "c++" "cplusplus" "cxx") "cpp")
      ((or "javascript" "node") "js")
      ((or "objective-c" "objectivec" "objective_c" "oc") "objc")
      ("compiletoc" "compileToC")
      ("compiletocpp" "compileToCpp")
      ("compiletooc" "compileToOC")
      (_ command))))

(defun chief/nim-backend-switch (backend)
  "Return Nim's `--backend' switch for BACKEND."
  (format "--backend:%s" (chief/nim-normalize-backend backend)))

(defun chief/nim-read-backend (&optional prompt)
  "Read a Nim backend name with PROMPT."
  (chief/nim-normalize-backend
   (completing-read (or prompt "Nim backend: ")
                    chief/nim-backends nil t nil nil chief/nim-default-backend)))

(defun chief/nim-read-compile-command (&optional prompt)
  "Read a Nim compile command/backend with PROMPT."
  (chief/nim-normalize-compile-command
   (completing-read (or prompt "Nim compiler command: ")
                    chief/nim-compile-commands nil t nil nil chief/nim-default-backend)))

(defun chief/nim-command-runs-with-backend (backend file &rest args)
  "Return a `nim r --backend:BACKEND FILE ARGS...' command.
Nim's `r' command is documented as using `--backend:c|cpp|js|objc', which is
what allows running with C++, JavaScript, or Objective-C instead of default C."
  (append (chief/nim-command "r" (chief/nim-backend-switch backend) file)
          args))

(defun chief/nim-command-compiles-with-backend (backend file &rest args)
  "Return a `nim BACKEND FILE ARGS...' compile command."
  (append (chief/nim-command (chief/nim-normalize-compile-command backend) file)
          args))

(defun chief/nim-command-checks-with-backend (backend file &rest args)
  "Return a backend-aware `nim check' command."
  (append (chief/nim-command "check" (chief/nim-backend-switch backend) file)
          args))

(defun chief/nim-command-docs-with-backend (backend file &rest args)
  "Return a backend-aware `nim doc' command."
  (append (chief/nim-command "doc" (chief/nim-backend-switch backend) file)
          args))

(defun chief/nimble-file-command-supported-p (command)
  "Return non-nil when Nimble has a file compiler COMMAND."
  (member command '("c" "compile" "cc" "cpp" "js")))

(defun chief/nimble-current-file-command-with-backend (backend file)
  "Return a command to compile FILE inside a Nimble package using BACKEND.
Nimble supports file commands such as `c', `cc', `cpp', and `js'.  It does not
provide an `objc' file command in the installed Nimble, so Objective-C falls
back to direct `nim objc' from the package root, where config.nims/nimble.paths
are still loaded by Nim."
  (let ((command (chief/nim-normalize-compile-command backend)))
    (if (chief/nimble-file-command-supported-p command)
        (chief/nimble-command command file)
      (chief/nim-command-compiles-with-backend command file))))

(defun chief/nimble-command (subcommand &rest args)
  "Return a Nimble command list for SUBCOMMAND and ARGS.
Pass `--noColor' as a Nimble option before SUBCOMMAND, and pass the compiler
switches to Nimble subcommands that forward options to Nim."
  (append (chief/nim--command "nimble" "--noColor" subcommand)
          (when (member subcommand '("build" "run" "test" "c" "compile" "cc" "cpp" "objc" "js" "doc" "doc2" "jsondoc"))
            (chief/nim-effective-compile-switches))
          args))

(defconst chief/nim-compilation-error-regexp
  (rx line-start
      (* space)
      (? (or "..." "Nim Output") (+ space))
      (group (+? anything) (or ".nimble" ".nims" ".nim"))
      "("
      (group (+ digit))
      ","
      (* space)
      (group (+ digit))
      ")"
      (+ space)
      (or "Error" "Warning" "Hint")
      ":")
  "Compilation regexp matching direct Nim and Nimble-wrapped diagnostics.")

(add-to-list 'compilation-error-regexp-alist-alist
             `(chief-nim ,chief/nim-compilation-error-regexp 1 2 3))
(add-to-list 'compilation-error-regexp-alist 'chief-nim)

(defun chief/nim-current-file ()
  "Return the current Nim file."
  (chief/nim--current-file "Nim"))

(defun chief/nimble-project-file ()
  "Return the nearest Nimble project file, if any."
  (chief/project-nearest-matching-file "\\.nimble\\'"))

(defun chief/nimble-project-p ()
  "Return non-nil when the current buffer belongs to a Nimble project."
  (not (null (chief/nimble-project-file))))

(defun chief/nim-read-extra-args (prompt)
  "Read shell-like arguments using PROMPT and return a list of strings."
  (let ((input (string-trim (read-shell-command prompt))))
    (if (string-empty-p input)
        nil
      (split-string-and-unquote input))))

(defun chief/nimble-package-info (&optional root)
  "Return Nimble package metadata for ROOT as an alist.
The data is read through `nimble dump --json' so srcDir, bin, backend, and
entryPoints match Nimble's own interpretation of the .nimble file."
  (let* ((root (file-name-as-directory (expand-file-name (or root (chief/nim-project-root)))))
         (project-file (or (car (chief/project-directory-files-matching root "\\.nimble\\'"))
                           (chief/nimble-project-file))))
    (when (and project-file (executable-find "nimble"))
      (let ((default-directory root)
            (process-environment (append '("NO_COLOR=1" "CLICOLOR=0")
                                         process-environment)))
        (with-temp-buffer
          (let ((status (call-process "nimble" nil t nil "--noColor" "dump" "--json")))
            (when (zerop status)
              (condition-case nil
                  (let ((json-object-type 'alist)
                        (json-array-type 'list)
                        (json-key-type 'symbol))
                    (json-read-from-string (buffer-string)))
                (error nil)))))))))

(defun chief/nimble-package-field (field &optional root)
  "Return FIELD from Nimble package metadata at ROOT."
  (cdr (assq field (chief/nimble-package-info root))))

(defun chief/nimble-package-src-dir (&optional root)
  "Return the Nimble source directory for ROOT, or nil when it is the root."
  (let ((src-dir (chief/nimble-package-field 'srcDir root)))
    (when (and (stringp src-dir) (not (string-empty-p src-dir)))
      src-dir)))

(defun chief/nimble-package-entry-points (&optional root)
  "Return Nimble entry points for ROOT."
  (seq-filter #'stringp (or (chief/nimble-package-field 'entryPoints root) nil)))

(defun chief/nimble-package-binary-entries (&optional root)
  "Return Nimble binary entries for ROOT as (RUN-NAME . SOURCE) pairs.
SOURCE is nil for ordinary `bin' entries because Nimble resolves those through
srcDir.  For `namedBin' entries, SOURCE is the source path when available."
  (let* ((bin (chief/nimble-package-field 'bin root))
         (named (chief/nimble-package-field 'namedBin root))
         (entries (mapcar (lambda (name) (cons name nil))
                          (seq-filter #'stringp (or bin nil)))))
    (dolist (entry named)
      (cond
       ((stringp entry)
        (push (cons entry nil) entries))
       ((consp entry)
        (push (cons (format "%s" (car entry))
                    (and (stringp (cdr entry)) (cdr entry)))
              entries))))
    (delete-dups (nreverse entries))))

(defun chief/nimble-package-binaries (&optional root)
  "Return Nimble binary names for ROOT."
  (mapcar #'car (chief/nimble-package-binary-entries root)))

(defun chief/nim--relative-file-equal-p (root a b)
  "Return non-nil when A and B name the same file relative to ROOT."
  (let ((left (expand-file-name a root))
        (right (expand-file-name b root)))
    (or (string= (file-truename left) (file-truename right))
        (string= (file-name-sans-extension (file-truename left))
                 (file-name-sans-extension (file-truename right))))))

(defun chief/nimble-binary-candidate-files (binary source &optional root)
  "Return source-file candidates for Nimble BINARY and optional SOURCE.
Prefer `.nim' source paths before extensionless names so an existing built
binary in the package root is never mistaken for source."
  (let* ((src-dir (chief/nimble-package-src-dir root))
         (entry-points (chief/nimble-package-entry-points root))
         candidates)
    (cl-labels ((add (candidate)
                  (when (and candidate (not (member candidate candidates)))
                    (setq candidates (append candidates (list candidate))))))
      (dolist (entry entry-points)
        (when (string= (file-name-base entry) binary)
          (add entry)))
      (when source
        (if (string-suffix-p ".nim" source)
            (add source)
          (progn
            (add (concat source ".nim"))
            (add source))))
      (when src-dir
        (add (concat (file-name-as-directory src-dir) binary ".nim"))
        (add (concat (file-name-as-directory src-dir) binary)))
      (add (concat binary ".nim"))
      (add binary))
    candidates))

(defun chief/nimble-current-binary (&optional root)
  "Return the Nimble binary associated with the current buffer, if any."
  (when (and buffer-file-name (chief/nimble-project-p))
    (let* ((root (file-name-as-directory (expand-file-name (or root (chief/nim-project-root)))))
           (relative-file (file-relative-name (expand-file-name buffer-file-name) root)))
      (cl-loop for (binary . source) in (chief/nimble-package-binary-entries root)
               when (seq-some (lambda (candidate)
                                (chief/nim--relative-file-equal-p root relative-file candidate))
                              (chief/nimble-binary-candidate-files binary source root))
               return binary))))

(defun chief/nimble-binary-source-file (binary &optional root)
  "Return the source file for Nimble BINARY in ROOT."
  (unless (and (stringp binary) (not (string-empty-p binary)))
    (user-error "This Nimble package does not define any binaries"))
  (let* ((root (file-name-as-directory (expand-file-name (or root (chief/nim-project-root)))))
         (source (cdr (assoc binary (chief/nimble-package-binary-entries root)))))
    (or
     (cl-loop for candidate in (chief/nimble-binary-candidate-files binary source root)
              for absolute = (expand-file-name candidate root)
              when (file-exists-p absolute)
              return absolute)
     (and buffer-file-name
          (string= binary (or (chief/nimble-current-binary root) ""))
          (expand-file-name buffer-file-name))
     (user-error "Could not resolve source file for Nimble binary %s" binary))))

(defun chief/nimble-read-binary (&optional prompt root)
  "Read a Nimble binary name from ROOT."
  (let ((binaries (chief/nimble-package-binaries root)))
    (unless binaries
      (user-error "This Nimble package does not define any binaries"))
    (completing-read (or prompt "Nimble binary: ") binaries nil t
                     (or (chief/nimble-current-binary root) (car binaries)))))

(defun chief/nimble-run-binary-name (&optional root)
  "Return the best binary name for `nimble run', prompting when ambiguous."
  (let ((current (chief/nimble-current-binary root))
        (binaries (chief/nimble-package-binaries root)))
    (or current
        (cond
         ((null binaries) nil)
         ((= (length binaries) 1) nil)
         (t (chief/nimble-read-binary "Run Nimble binary: " root))))))

(defun chief/nim-test-file-p (&optional file)
  "Return non-nil when FILE names a Nim test file by convention."
  (let* ((file (or file buffer-file-name))
         (root (and file (chief/nim-project-root)))
         (relative (and file root (file-relative-name (expand-file-name file) root))))
    (and relative
         (string-match-p "\\(?:\\`\\|/\\)tests/\\(?:.*/\\)?t[^/]*\\.nim\\'" relative))))

(defun chief/nim-project-test-files (&optional root)
  "Return Nim test files under ROOT/tests whose basenames start with t."
  (let ((tests-dir (expand-file-name "tests" (or root (chief/nim-project-root)))))
    (when (file-directory-p tests-dir)
      (directory-files-recursively tests-dir "\\(?:\\`\\|/\\)t[^/]*\\.nim\\'"))))

(defun chief/nim-library-test-candidates (&optional file root)
  "Return test files that plausibly exercise library FILE."
  (let* ((file (or file (chief/nim-current-file)))
         (root (file-name-as-directory (expand-file-name (or root (chief/nim-project-root)))))
         (base (file-name-base file))
         (relative (file-name-sans-extension
                    (file-relative-name (expand-file-name file) root)))
         (src-dir (chief/nimble-package-src-dir root))
         (src-relative (if (and src-dir (string-prefix-p (file-name-as-directory src-dir) relative))
                           (substring relative (length (file-name-as-directory src-dir)))
                         relative))
         (flattened (replace-regexp-in-string "[/\\]" "_" src-relative))
         (tests (chief/nim-project-test-files root))
         (preferred (list (format "t%s.nim" base)
                          (format "t%s.nim" flattened))))
    (append
     (seq-filter (lambda (test)
                   (member (file-name-nondirectory test) preferred))
                 tests)
     (seq-filter (lambda (test)
                   (and (not (member (file-name-nondirectory test) preferred))
                        (string-match-p (regexp-quote base)
                                        (file-name-base test))))
                 tests))))

(defun chief/nim-read-library-test-file (&optional file root)
  "Read or infer the Nim test file for library FILE."
  (let ((candidates (delete-dups (chief/nim-library-test-candidates file root))))
    (cond
     ((null candidates)
      (user-error "No tests/t*.nim file found for this library module"))
     ((= (length candidates) 1)
      (car candidates))
     (t
      (completing-read "Nim library test file: " candidates nil t (car candidates))))))

(defun chief/nim-source-path-switches (&optional root)
  "Return Nim `--path' switches needed for package-local tests."
  (when-let* ((src-dir (chief/nimble-package-src-dir root)))
    (list (format "--path:%s" src-dir))))

(defun chief/nim-flycheck-compiler-switches ()
  "Return compiler switches for Flycheck's `nim check' invocation."
  (append (chief/nim-effective-compile-switches)
          (chief/nim-source-path-switches (chief/nim-project-root))))

(defun chief/nim-flycheck-working-directory (_checker)
  "Return the working directory for Nim Flycheck diagnostics."
  (when-let* ((root (chief/nim-project-root)))
    (and (file-directory-p root)
         (file-name-as-directory (expand-file-name root)))))

(defun chief/nim-select-flycheck-checker ()
  "Prefer compiler-backed Nim diagnostics in Nim buffers."
  (when (and chief/nim-flycheck-enable
             (derived-mode-p 'nim-mode)
             (boundp 'flycheck-checker)
             (fboundp 'flycheck-valid-checker-p)
             (flycheck-valid-checker-p 'chief-nim-check))
    (setq-local flycheck-checker 'chief-nim-check)))

(with-eval-after-load 'flycheck
  (flycheck-define-checker chief-nim-check
    "Check Nim buffers with `nim check'."
    :command ("nim" "check"
              (eval (chief/nim-flycheck-compiler-switches))
              (source-inplace ".nim"))
    :error-patterns
    ((error line-start (file-name) "(" line ", " column ") "
            (or "Error" "Fatal") ": " (message) line-end)
     (warning line-start (file-name) "(" line ", " column ") Warning: "
              (message) line-end)
     (info line-start (file-name) "(" line ", " column ") Hint: "
           (message) line-end))
    :modes (nim-mode)
    :predicate (lambda ()
                 (and chief/nim-flycheck-enable
                      buffer-file-name
                      (string-match-p "\\.nim\\'" buffer-file-name)))
    :working-directory chief/nim-flycheck-working-directory)
  (add-to-list 'flycheck-checkers 'chief-nim-check)
  (add-hook 'flycheck-mode-hook #'chief/nim-select-flycheck-checker t)
  (add-hook 'lsp-mode-hook #'chief/nim-select-flycheck-checker t))

(defun chief/nim-secret-command (&optional root)
  "Return the command used to start a `nim secret' REPL for ROOT."
  (append (chief/nim--command "nim" "secret")
          (chief/nim-effective-compile-switches)
          (when chief/nim-secret-use-package-src-path
            (chief/nim-source-path-switches root))
          chief/nim-secret-extra-switches))

(defun chief/nim-secret-buffer-name (&optional root)
  "Return the `nim secret' REPL buffer name for ROOT."
  (format "*nim-secret[%s]*"
          (file-name-nondirectory
           (directory-file-name
            (or root (chief/nim-project-root) default-directory)))))

(defun chief/nim-secret-quit-input-p (input)
  "Return non-nil when INPUT intentionally exits the Nim REPL."
  (string-match-p "\\`\\s-*\\(?:quit\\|exit\\)\\s-*(" input))

(defun chief/nim-secret-persistent-input-p (input)
  "Return non-nil when INPUT should be replayed after REPL restart.
This is deliberately conservative: it records simple imports/declarations that
recreate REPL context, not arbitrary effectful expressions."
  (let ((text (string-trim (substring-no-properties input))))
    (and (not (string-empty-p text))
         (not (chief/nim-secret-quit-input-p text))
         (or (string-match-p "\\`\\s-*#" text)
             (string-match-p
              "\\`\\s-*\\(?:import\\|from\\|include\\|let\\|var\\|const\\|type\\|proc\\|func\\|method\\|iterator\\|converter\\|template\\|macro\\|using\\)\\_>"
              text)
             (string-match-p
              "\\`\\s-*\\(?:when\\|if\\|case\\|for\\|while\\|try\\|block\\|static\\|elif\\|else\\|except\\|finally\\|of\\)\\_>"
              text)
             (string-match-p "[:=]\\s-*\\'" text)))))

(defun chief/nim-secret-dangerous-input-p (input)
  "Return non-nil when direct INPUT should be guarded before sending."
  (string-match-p "\\`\\s-*\\(?:assert\\|doAssert\\|raise\\)\\_>"
                  (substring-no-properties input)))

(defun chief/nim-secret-continuation-input-p ()
  "Return non-nil when point is submitting from a Nim continuation prompt."
  (and (boundp 'comint-last-input-start)
       (markerp comint-last-input-start)
       (save-excursion
         (goto-char comint-last-input-start)
         (forward-line 0)
         (looking-at-p "\\.\\.\\. "))))

(defun chief/nim-secret-protect-input-p (input)
  "Return non-nil when direct REPL INPUT should be try/except wrapped."
  (and chief/nim-secret-direct-input-protection
       (not (chief/nim-secret-continuation-input-p))
       (not (chief/nim-secret-persistent-input-p input))
       (not (chief/nim-secret-quit-input-p input))
       (pcase chief/nim-secret-direct-input-protection
         ('all t)
         ('dangerous (chief/nim-secret-dangerous-input-p input))
         (_ nil))))

(defun chief/nim-secret-record-input (input)
  "Record persistent REPL INPUT for crash recovery."
  (when (and chief/nim-secret-replay-persistent-inputs
             (not chief/nim-secret-replaying-inputs)
             (chief/nim-secret-persistent-input-p input))
    (setq chief/nim-secret-persistent-inputs
          (append chief/nim-secret-persistent-inputs (list input)))))

(defun chief/nim-secret-normalize-output (output)
  "Return Nim REPL OUTPUT with prompt runs split onto readable lines."
  (let ((text output))
    (while (string-match "\\(\\(?:>>>\\|\\.\\.\\.\\) \\)\\(\\(?:>>>\\|\\.\\.\\.\\) \\)" text)
      (setq text (replace-match "\\1\n\\2" t nil text)))
    (replace-regexp-in-string
     "\\([^ \t\n]\\)\\(\\(?:>>>\\|\\.\\.\\.\\) \\)"
     "\\1\n\\2"
     text t)))

(defun chief/nim-secret-preoutput-filter (output)
  "Format process OUTPUT before inserting it into a Nim REPL buffer."
  (let ((text output))
    (when chief/nim-secret-hide-continuation-prompts
      (setq text (replace-regexp-in-string "\\(?:\\.\\.\\. \\)+" "" text))
      (when (and (not (string-empty-p text))
                 (not (string-prefix-p "\n" text))
                 (not (string-prefix-p ">>> " text))
                 (save-excursion
                   (goto-char (point-max))
                   (forward-line 0)
                   (looking-at-p comint-prompt-regexp)))
        (setq text (concat "\n" text)))
      (when (string-match-p ">>> " text)
        (setq chief/nim-secret-hide-continuation-prompts nil)))
    (chief/nim-secret-normalize-output text)))

(defun chief/nim-secret-strip-input-face (&optional start end)
  "Remove Comint's one-face input highlighting between START and END.
This lets Nim font-lock color keywords, strings, numbers, and types in the REPL
instead of painting every submitted input line with `comint-highlight-input'."
  (let ((start (or start (point-min)))
        (end (or end (point-max))))
    (cond
     ((fboundp 'font-lock--remove-face-from-text-property)
      (font-lock--remove-face-from-text-property
       start end 'font-lock-face 'comint-highlight-input))
     (t
      (let ((pos start) next face)
        (while (< pos end)
          (setq next (or (next-single-property-change pos 'font-lock-face nil end)
                         end)
                face (get-text-property pos 'font-lock-face))
          (cond
           ((eq face 'comint-highlight-input)
            (remove-text-properties pos next '(font-lock-face nil)))
           ((and (listp face) (memq 'comint-highlight-input face))
            (put-text-property pos next 'font-lock-face
                               (delq 'comint-highlight-input (copy-sequence face)))))
          (setq pos next)))))))

(defun chief/nim-secret-font-lock-setup ()
  "Enable Nim syntax highlighting inside the `nim secret' comint buffer."
  (when (require 'nim-mode nil t)
    ;; `comint-mode' installs empty font-lock defaults.  Replace them with
    ;; nim-mode's syntax table/propertizer/font-lock rules, then restart
    ;; font-lock so already-open REPL buffers don't keep the stale comint setup.
    (when (bound-and-true-p font-lock-mode)
      (font-lock-mode -1))
    (when (boundp 'nim-mode-syntax-table)
      (set-syntax-table nim-mode-syntax-table))
    (setq-local comint-highlight-input nil)
    (when (boundp 'nim-syntax-propertize-function)
      (setq-local syntax-propertize-function nim-syntax-propertize-function))
    (if (fboundp 'nim--set-font-lock-keywords)
        (nim--set-font-lock-keywords 'nim-mode)
      (when (boundp 'nim-font-lock-keywords)
        (setq-local font-lock-defaults '(nim-font-lock-keywords nil t))))
    (setq-local font-lock-multiline t)
    (chief/nim-secret-strip-input-face)
    (font-lock-mode 1)
    (font-lock-flush (point-min) (point-max))
    (when (fboundp 'font-lock-ensure)
      (font-lock-ensure (point-min) (point-max)))))

(define-derived-mode chief/nim-secret-mode comint-mode "Nim-Secret"
  "Comint mode for `nim secret' with Nim syntax highlighting."
  (setq-local comint-input-sender #'chief/nim-secret-input-sender)
  (setq-local comint-use-prompt-regexp t)
  (setq-local comint-prompt-regexp "^\\(?:>>>\\|\\.\\.\\.\\) ")
  (setq-local paragraph-start comint-prompt-regexp)
  (setq-local comint-process-echoes nil)
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (setq-local indent-tabs-mode nil)
  (chief/nim-secret-font-lock-setup))

(defun chief/nim-secret-input-sender (process input)
  "Send direct comint INPUT to PROCESS, guarding common fatal forms."
  (let* ((protected (chief/nim-secret-protect-input-p input))
         (payload (if protected
                      (chief/nim-secret-safe-wrapper input)
                    input)))
    (when protected
      (setq chief/nim-secret-hide-continuation-prompts t))
    (chief/nim-secret-record-input input)
    (comint-send-string process (chief/nim-secret-prepare-input payload))))

(defun chief/nim-secret--configure-buffer (buffer root)
  "Configure BUFFER as a `nim secret' comint buffer rooted at ROOT."
  (with-current-buffer buffer
    (let ((persistent-inputs chief/nim-secret-persistent-inputs))
      (unless (derived-mode-p 'chief/nim-secret-mode)
        (chief/nim-secret-mode))
      (setq-local chief/nim-secret-persistent-inputs persistent-inputs))
    (setq-local default-directory root)
    (setq-local chief/nim-secret-deliberate-stop nil)
    (setq-local comint-process-echoes nil)
    (setq-local comint-input-sender #'chief/nim-secret-input-sender)
    (setq-local comint-use-prompt-regexp t)
    (setq-local comint-prompt-regexp "^\\(?:>>>\\|\\.\\.\\.\\) ")
    (setq-local paragraph-start comint-prompt-regexp)
    (setq-local comint-scroll-to-bottom-on-input t)
    (setq-local comint-scroll-to-bottom-on-output t)
    (setq-local truncate-lines nil)
    (setq-local truncate-partial-width-windows nil)
    (setq-local word-wrap t)
    (setq-local line-move-visual t)
    (add-hook 'comint-preoutput-filter-functions
              #'chief/nim-secret-preoutput-filter nil t)
    (chief/nim-secret-font-lock-setup)
    (when (fboundp 'visual-line-mode)
      (visual-line-mode 1))
    (when-let* ((process (get-buffer-process buffer)))
      (set-process-sentinel process #'chief/nim-secret-process-sentinel))
    (when (fboundp 'ansi-color-for-comint-mode-on)
      (ansi-color-for-comint-mode-on))))

(defun chief/nim-secret--start-process (buffer root)
  "Start a `nim secret' process in BUFFER for ROOT."
  (let ((command (chief/nim-secret-command root)))
    (let ((default-directory root)
          (process-connection-type t)
          (process-environment (append '("NO_COLOR=1" "CLICOLOR=0")
                                       process-environment)))
      (apply #'make-comint-in-buffer
             "chief-nim-secret"
             buffer
             (car command)
             nil
             (cdr command))))
  (chief/nim-secret--configure-buffer buffer root))

(defun chief/nim-secret--replay-inputs (buffer inputs)
  "Replay persistent INPUTS into restarted Nim REPL BUFFER."
  (when (and chief/nim-secret-replay-persistent-inputs inputs)
    (with-current-buffer buffer
      (setq chief/nim-secret-replaying-inputs t)
      (unwind-protect
          (when-let* ((process (get-buffer-process buffer)))
            (dolist (input inputs)
              (when (process-live-p process)
                (chief/nim-secret-wait-for-prompt buffer 5.0)
                (comint-send-string process (chief/nim-secret-prepare-input input)))))
        (setq chief/nim-secret-replaying-inputs nil)))))

(defun chief/nim-secret--restart-buffer (buffer root inputs)
  "Restart `nim secret' in BUFFER for ROOT and replay INPUTS."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n[nim-secret restarted after runtime error]\n"))
      (setq chief/nim-secret-persistent-inputs inputs))
    (chief/nim-secret--start-process buffer root)
    (chief/nim-secret--replay-inputs buffer inputs)))

(defun chief/nim-secret-process-sentinel (process event)
  "Handle `nim secret' PROCESS state changes described by EVENT."
  (when (fboundp 'comint-sentinel)
    (comint-sentinel process event))
  (when-let* ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and chief/nim-secret-auto-restart
                   (not chief/nim-secret-deliberate-stop)
                   (not chief/nim-secret-replaying-inputs)
                   (memq (process-status process) '(exit signal))
                   (not (and (eq (process-status process) 'exit)
                             (zerop (process-exit-status process)))))
          (let ((root default-directory)
                (inputs chief/nim-secret-persistent-inputs))
            (run-at-time 0.25 nil
                         #'chief/nim-secret--restart-buffer
                         buffer root inputs)))))))

(defun chief/nim-secret-ensure-repl-buffer ()
  "Return a live `nim secret' REPL buffer for the current Nim project."
  (let* ((root (file-name-as-directory
                (expand-file-name (or (chief/nim-project-root) default-directory))))
         (buffer (get-buffer-create (chief/nim-secret-buffer-name root))))
    (if (comint-check-proc buffer)
        ;; Re-apply display/process settings after config reloads, without
        ;; forcing users to kill an already-running REPL.
        (chief/nim-secret--configure-buffer buffer root)
      (chief/nim-secret--start-process buffer root))
    buffer))

(defun chief/nim-secret-at-prompt-p (&optional buffer)
  "Return non-nil when BUFFER appears to be waiting at a Nim REPL prompt."
  (let ((buffer (or buffer (chief/nim-secret-ensure-repl-buffer))))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (forward-line 0)
        (looking-at-p comint-prompt-regexp)))))

(defun chief/nim-secret-wait-for-prompt (&optional buffer timeout)
  "Wait until BUFFER reaches a Nim REPL prompt, up to TIMEOUT seconds."
  (let* ((buffer (or buffer (chief/nim-secret-ensure-repl-buffer)))
         (process (get-buffer-process buffer))
         (deadline (+ (float-time) (or timeout 5.0))))
    (while (and process
                (process-live-p process)
                (< (float-time) deadline)
                (not (chief/nim-secret-at-prompt-p buffer)))
      (accept-process-output process 0.05))
    (chief/nim-secret-at-prompt-p buffer)))

(defun chief/nim-secret-start-repl ()
  "Start or switch to the current project's built-in Nim REPL.
This uses Nim's hidden/interactive compiler command, `nim secret'."
  (interactive)
  (pop-to-buffer (chief/nim-secret-ensure-repl-buffer)))

(defun chief/nim-secret-restart-repl ()
  "Restart the current project's `nim secret' REPL."
  (interactive)
  (when-let* ((buffer (get-buffer (chief/nim-secret-buffer-name))))
    (with-current-buffer buffer
      (setq chief/nim-secret-deliberate-stop t))
    (when-let* ((process (get-buffer-process buffer)))
      (delete-process process))
    (kill-buffer buffer))
  (chief/nim-secret-start-repl))

(defun chief/nim-secret-quit-repl ()
  "Ask the current Nim REPL to quit with `quit()'."
  (interactive)
  (let ((buffer (chief/nim-secret-ensure-repl-buffer)))
    (with-current-buffer buffer
      (setq chief/nim-secret-deliberate-stop t))
    (chief/nim-secret-send-string "quit()" t)))

(defun chief/nim-secret-interrupt-repl ()
  "Interrupt the current Nim REPL process."
  (interactive)
  (let* ((buffer (chief/nim-secret-ensure-repl-buffer))
         (process (get-buffer-process buffer)))
    (unless process
      (user-error "No Nim REPL process is running"))
    (interrupt-process process)
    (display-buffer buffer)))

(defun chief/nim-secret-clear-repl ()
  "Clear the current Nim REPL buffer."
  (interactive)
  (let ((buffer (chief/nim-secret-ensure-repl-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (display-buffer buffer)))

(defun chief/nim-secret-prepare-input (string)
  "Return STRING prepared for submission to `nim secret'.
Multi-line forms receive an extra blank line so the REPL leaves continuation
mode after indented blocks such as proc/func/template definitions."
  (let ((text (string-trim-right (substring-no-properties string))))
    (cond
     ((string-empty-p text) "\n")
     ((string-match-p "\n" text) (concat text "\n\n"))
     (t (concat text "\n")))))

(defun chief/nim-secret-send-string (string &optional no-record hide-continuation-prompts)
  "Send STRING to the current project's `nim secret' REPL.
When NO-RECORD is non-nil, do not record STRING for crash-recovery replay.
When HIDE-CONTINUATION-PROMPTS is non-nil, suppress hidden wrapper `...'
prompts produced by the process."
  (let* ((buffer (chief/nim-secret-ensure-repl-buffer))
         (process (get-buffer-process buffer)))
    (unless process
      (user-error "No Nim REPL process is running"))
    (chief/nim-secret-wait-for-prompt buffer)
    (with-current-buffer buffer
      (when hide-continuation-prompts
        (setq chief/nim-secret-hide-continuation-prompts t))
      (unless no-record
        (chief/nim-secret-record-input string)))
    (comint-send-string process (chief/nim-secret-prepare-input string))
    (when chief/nim-secret-display-buffer-after-send
      (display-buffer buffer))))

(defun chief/nim-secret-send-line ()
  "Send the current line to the Nim REPL."
  (interactive)
  (chief/nim-secret-send-string
   (buffer-substring-no-properties
    (line-beginning-position)
    (line-end-position))))

(defun chief/nim-secret-send-region (start end)
  "Send the region from START to END to the Nim REPL."
  (interactive "r")
  (chief/nim-secret-send-string
   (buffer-substring-no-properties start end)))

(defun chief/nim-secret-indent-code (string)
  "Indent STRING by two spaces for insertion into a Nim block."
  (mapconcat (lambda (line)
               (if (string-empty-p line)
                   ""
                 (concat "  " line)))
             (split-string (substring-no-properties string) "\n")
             "\n"))

(defun chief/nim-secret-safe-wrapper (string)
  "Wrap STRING in a Nim try/except block for safer REPL evaluation.
This catches runtime `Defect' and `CatchableError' failures so assertions and
ordinary exceptions do not kill `nim secret'.  Because Nim block scopes are
real scopes, definitions and `let'/`var' bindings made inside this wrapper do
not become top-level REPL bindings; use the normal send commands for code that
should persist in the REPL session."
  (let ((body (string-trim-right (substring-no-properties string))))
    (if (string-empty-p body)
        "discard"
      (format "try:\n%s\nexcept Defect as e:\n  echo \"REPL Defect: \", e.name\n  echo \"  \", e.msg\nexcept CatchableError as e:\n  echo \"REPL Error: \", e.name\n  echo \"  \", e.msg"
              (chief/nim-secret-indent-code body)))))

(defun chief/nim-secret-send-string-safely (string)
  "Send STRING to the Nim REPL wrapped in exception handlers.
Use this for calls, assertions, and experiments that may raise.  Use normal
send commands for persistent declarations/imports."
  (let ((buffer (chief/nim-secret-ensure-repl-buffer)))
    (with-current-buffer buffer
      (setq chief/nim-secret-hide-continuation-prompts t))
    (chief/nim-secret-send-string
     (chief/nim-secret-safe-wrapper string)
     t
     t)))

(defun chief/nim-secret-send-line-safely ()
  "Send the current line to the Nim REPL with exception handling."
  (interactive)
  (chief/nim-secret-send-string-safely
   (buffer-substring-no-properties
    (line-beginning-position)
    (line-end-position))))

(defun chief/nim-secret-send-region-safely (start end)
  "Send region START to END to the Nim REPL with exception handling."
  (interactive "r")
  (chief/nim-secret-send-string-safely
   (buffer-substring-no-properties start end)))

(defun chief/nim-secret-top-level-bounds ()
  "Return bounds for the current Nim top-level block."
  (or
   (save-mark-and-excursion
     (condition-case nil
         (progn
           (mark-defun)
           (cons (region-beginning) (region-end)))
       (error nil)))
   (save-excursion
     (let (start end)
       (unless (bolp)
         (beginning-of-line))
       (unless (looking-at-p "\\S-")
         (re-search-backward "^\\S-" nil t))
       (setq start (point))
       (forward-line 1)
       (if (re-search-forward "^\\S-" nil t)
           (progn
             (beginning-of-line)
             (setq end (point)))
         (setq end (point-max)))
       (cons start end)))))

(defun chief/nim-secret-send-defun ()
  "Send the current Nim top-level block to the Nim REPL."
  (interactive)
  (pcase-let ((`(,start . ,end) (chief/nim-secret-top-level-bounds)))
    (chief/nim-secret-send-region start end)))

(defun chief/nim-secret-send-buffer ()
  "Send the current buffer to the Nim REPL."
  (interactive)
  (chief/nim-secret-send-region (point-min) (point-max)))

(defun chief/nim-secret-load-file ()
  "Load the current Nim file into the REPL by sending the buffer contents.
This preserves unsaved edits and avoids modifying project files.  Use
`chief/nim-secret-include-current-file' to include the on-disk file instead."
  (interactive)
  (chief/nim-secret-send-buffer))

(defun chief/nim-secret-include-current-file ()
  "Include the current on-disk Nim file in the REPL."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a Nim file"))
  (chief/nim-secret-send-string
   (format "include %S" (expand-file-name buffer-file-name))))

(defun chief/nim-module-import-path (&optional file root)
  "Return the Nim import path for FILE relative to ROOT/srcDir when possible."
  (let* ((file (expand-file-name (or file (chief/nim-current-file))))
         (root (file-name-as-directory (expand-file-name (or root (chief/nim-project-root)))))
         (src-dir (chief/nimble-package-src-dir root))
         (src-root (and src-dir (file-name-as-directory (expand-file-name src-dir root))))
         (relative (if (and src-root (string-prefix-p src-root file))
                       (file-relative-name file src-root)
                     (file-name-nondirectory file))))
    (replace-regexp-in-string
     "\\\\" "/"
     (file-name-sans-extension relative))))

(defun chief/nim-secret-import-current-module ()
  "Import the current Nim module into the REPL."
  (interactive)
  (chief/nim-secret-send-string
   (format "import %s" (chief/nim-module-import-path))))

(defun chief/nim-cache-directory ()
  "Return the directory used for transient Nim build artifacts."
  (let ((directory (expand-file-name
                    "nim/"
                    (if (boundp 'chief/var-directory)
                        chief/var-directory
                      user-emacs-directory))))
    (make-directory directory t)
    directory))

(defun chief/nim-artifact-path (&optional suffix)
  "Return a stable transient executable path for the current Nim file."
  (let* ((file (chief/nim-current-file))
         (hash (substring (secure-hash 'sha1 (expand-file-name file)) 0 12))
         (extension (if (eq system-type 'windows-nt) ".exe" "")))
    (expand-file-name
     (format "%s-%s%s%s"
             (file-name-base file)
             hash
             (or suffix "")
             extension)
     (chief/nim-cache-directory))))

(defun chief/nim-build-buffer ()
  "Build the current Nim file."
  (interactive)
  (chief/nim--save-current-buffer)
  (chief/nim-compile
   (chief/nim-command "c" (chief/nim-current-file))
   "*nim build*"))

(defun chief/nim-build-buffer-with-backend (backend)
  "Build the current Nim file with compiler command/backend BACKEND."
  (interactive (list (chief/nim-read-compile-command "Build with Nim command/backend: ")))
  (chief/nim--save-current-buffer)
  (let ((command (chief/nim-normalize-compile-command backend)))
    (chief/nim-compile
     (chief/nim-command-compiles-with-backend command (chief/nim-current-file))
     (format "*nim %s build*" command))))

(defun chief/nim-build-current-target-with-backend (backend)
  "Build the current Nim target with compiler command/backend BACKEND."
  (interactive (list (chief/nim-read-compile-command "Build target with Nim command/backend: ")))
  (if (chief/nimble-project-p)
      (chief/nimble-compile-current-file-with-backend backend)
    (chief/nim-build-buffer-with-backend backend)))

(defun chief/nim-build-release-buffer ()
  "Build the current Nim file with release optimizations."
  (interactive)
  (chief/nim--save-current-buffer)
  (chief/nim-compile
   (chief/nim-command "c" "-d:release" (chief/nim-current-file))
   "*nim release build*"))

(defun chief/nim-build-current-target ()
  "Build the current Nim target.
Inside a Nimble package, build through `nimble build'.  Outside Nimble, compile
this file as a standalone Nim main module."
  (interactive)
  (if (chief/nimble-project-p)
      (chief/nimble-build-project)
    (chief/nim-build-buffer)))

(defun chief/nim-run-buffer (&optional prompt-args)
  "Run the current file as a standalone Nim main module.
This intentionally uses the canonical `nim c -r file' form, not a Nimble
package run.  With PROMPT-ARGS, read program arguments interactively."
  (interactive "P")
  (chief/nim--save-current-buffer)
  (let ((args (if prompt-args
                  (chief/nim-read-extra-args "Program args: ")
                chief/nim-run-args)))
    (chief/nim-compile
     (append (chief/nim-command "c" "-r" (chief/nim-current-file)) args)
     "*nim run module*")))

(defun chief/nim-run-buffer-with-backend (backend &optional prompt-args)
  "Run the current Nim file with BACKEND.
Uses Nim's documented `nim r --backend:BACKEND file' form, so C++, JavaScript,
and Objective-C runs are available in addition to default C."
  (interactive (list (chief/nim-read-backend "Run with Nim backend: ") current-prefix-arg))
  (chief/nim--save-current-buffer)
  (let* ((backend (chief/nim-normalize-backend backend))
         (args (if prompt-args
                   (chief/nim-read-extra-args "Program args: ")
                 chief/nim-run-args)))
    (chief/nim-compile
     (apply #'chief/nim-command-runs-with-backend
            backend
            (chief/nim-current-file)
            args)
     (format "*nim run %s*" backend))))

(defun chief/nim-run-nimble-binary-with-backend (backend &optional prompt-args binary)
  "Run a Nimble package BINARY with Nim BACKEND.
Nimble package `run' itself uses the backend declared by the .nimble file; this
helper resolves the package binary source and runs it through `nim r
--backend:BACKEND' from the package root so alternate backends are possible
without editing the .nimble file."
  (interactive
   (list (chief/nim-read-backend "Run package with Nim backend: ")
         current-prefix-arg
         nil))
  (chief/nim--save-current-buffer)
  (let* ((root (chief/nim-project-root))
         (backend (chief/nim-normalize-backend backend))
         (binary (or binary
                     (chief/nimble-current-binary root)
                     (let ((binaries (chief/nimble-package-binaries root)))
                       (cond
                        ((= (length binaries) 1) (car binaries))
                        ((> (length binaries) 1) (chief/nimble-read-binary "Run Nimble binary: " root))
                        (t nil)))))
         (source-file (chief/nimble-binary-source-file binary root))
         (path-switches (chief/nim-source-path-switches root))
         (args (if prompt-args
                   (chief/nim-read-extra-args "Program args: ")
                 chief/nimble-run-args))
         (command (append (apply #'chief/nim-command
                                 (append (list "r" (chief/nim-backend-switch backend))
                                         path-switches
                                         (list source-file)))
                          args)))
    (chief/nim-compile command
                       (format "*nim run %s %s*" backend binary)
                       root)))

(defun chief/nim-run-current-target-with-backend (backend &optional prompt-args)
  "Run the current Nim target with BACKEND.
For Nimble packages this runs the selected package binary source from the
package root with `nim r --backend:BACKEND'.  For standalone files it runs the
current file the same way."
  (interactive (list (chief/nim-read-backend "Run target with Nim backend: ") current-prefix-arg))
  (if (chief/nimble-project-p)
      (chief/nim-run-nimble-binary-with-backend backend prompt-args)
    (chief/nim-run-buffer-with-backend backend prompt-args)))

(defun chief/nim-run-current-target (&optional prompt-args)
  "Run the current Nim target.
Inside a Nimble package, use `nimble run' so package binaries, srcDir, Nimble
paths, and package hooks are respected.  Outside Nimble, run this file as a
standalone main module.  With PROMPT-ARGS, read program arguments."
  (interactive "P")
  (if (chief/nimble-project-p)
      (chief/nimble-run-project prompt-args)
    (chief/nim-run-buffer prompt-args)))

(defun chief/nim-check-buffer ()
  "Run `nim check' for the current Nim file."
  (interactive)
  (chief/nim--save-current-buffer)
  (chief/nim-compile
   (chief/nim-command "check" (chief/nim-current-file))
   "*nim check*"))

(defun chief/nim-check-buffer-with-backend (backend)
  "Run `nim check' for the current Nim file with BACKEND."
  (interactive (list (chief/nim-read-backend "Check with Nim backend: ")))
  (chief/nim--save-current-buffer)
  (let ((backend (chief/nim-normalize-backend backend)))
    (chief/nim-compile
     (chief/nim-command-checks-with-backend backend (chief/nim-current-file))
     (format "*nim check %s*" backend))))

(defun chief/nim-doc-buffer ()
  "Generate HTML docs for the current Nim file."
  (interactive)
  (chief/nim--save-current-buffer)
  (chief/nim-compile
   (chief/nim-command "doc" (chief/nim-current-file))
   "*nim doc*"))

(defun chief/nim-doc-buffer-with-backend (backend)
  "Generate HTML docs for the current Nim file with BACKEND."
  (interactive (list (chief/nim-read-backend "Document with Nim backend: ")))
  (chief/nim--save-current-buffer)
  (let ((backend (chief/nim-normalize-backend backend)))
    (chief/nim-compile
     (chief/nim-command-docs-with-backend backend (chief/nim-current-file))
     (format "*nim doc %s*" backend))))

(defun chief/nim-js-build-buffer ()
  "Compile the current Nim file to JavaScript."
  (interactive)
  (chief/nim--save-current-buffer)
  (chief/nim-compile
   (chief/nim-command "js" (chief/nim-current-file))
   "*nim js*"))

(defun chief/nim-cpp-build-buffer ()
  "Compile the current Nim file with the C++ backend."
  (interactive)
  (chief/nim--save-current-buffer)
  (chief/nim-compile
   (chief/nim-command "cpp" (chief/nim-current-file))
   "*nim cpp*"))

(defun chief/nim-objc-build-buffer ()
  "Compile the current Nim file with the Objective-C backend."
  (interactive)
  (chief/nim--save-current-buffer)
  (chief/nim-compile
   (chief/nim-command "objc" (chief/nim-current-file))
   "*nim objc*"))

(defun chief/nim-doc2-buffer ()
  "Generate docs for the current Nim file with `nim doc2'."
  (interactive)
  (chief/nim--save-current-buffer)
  (chief/nim-compile
   (chief/nim-command "doc2" (chief/nim-current-file))
   "*nim doc2*"))

(defun chief/nim-jsondoc-buffer ()
  "Generate JSON docs for the current Nim file."
  (interactive)
  (chief/nim--save-current-buffer)
  (chief/nim-compile
   (chief/nim-command "jsondoc" (chief/nim-current-file))
   "*nim jsondoc*"))

(defun chief/nim-dump-buffer ()
  "Run `nim dump' for the current Nim file."
  (interactive)
  (chief/nim--save-current-buffer)
  (chief/nim-compile
   (chief/nim-command "dump" (chief/nim-current-file))
   "*nim dump*"))

(defun chief/nim-eval (code)
  "Evaluate Nim CODE through the compiler."
  (interactive "sNim eval: ")
  (chief/nim-compile
   (chief/nim-command "r" (format "--eval:%s" code))
   "*nim eval*"))

(defconst chief/nim-test-declaration-regexp
  "^[ \t]*test[ \t]+\\([\"`]\\)\\([^\"`]+\\)\\1[ \t]*:"
  "Regexp matching Nim unittest `test' declarations.")

(defconst chief/nim-suite-declaration-regexp
  "^[ \t]*suite[ \t]+\\([\"`]\\)\\([^\"`]+\\)\\1[ \t]*:"
  "Regexp matching Nim unittest `suite' declarations.")

(defun chief/nim-suite-before-position (position)
  "Return the nearest enclosing suite name before POSITION, if any."
  (let ((test-indent (save-excursion
                       (goto-char position)
                       (current-indentation))))
    (save-excursion
      (goto-char position)
      (catch 'suite
        (while (re-search-backward chief/nim-suite-declaration-regexp nil t)
          (when (< (current-indentation) test-indent)
            (throw 'suite (match-string 2))))))))

(defun chief/nim-buffer-test-items ()
  "Return plist entries for Nim unittest tests in the current buffer."
  (let (items)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward chief/nim-test-declaration-regexp nil t)
        (unless (nth 8 (save-excursion (syntax-ppss (match-beginning 0))))
          (let* ((name (match-string 2))
                 (position (match-beginning 0))
                 (suite (chief/nim-suite-before-position position))
                 (filter (if suite (format "%s::%s" suite name) name)))
            (push (list :name name :suite suite :filter filter :position position) items)))))
    (nreverse items)))

(defun chief/nim-test-item-at-point ()
  "Return the Nim test item at point, or nil."
  (let ((position (point))
        (items (chief/nim-buffer-test-items)))
    (cl-loop for item in items
             for next in (append (cdr items) (list nil))
             for start = (plist-get item :position)
             for end = (or (plist-get next :position) (point-max))
             when (and (<= start position) (< position end))
             return item)))

(defun chief/nim-read-test-item ()
  "Return the Nim test item at point, or prompt for one."
  (let* ((items (chief/nim-buffer-test-items))
         (at-point (chief/nim-test-item-at-point)))
    (unless items
      (user-error "No Nim unittest tests found in this file"))
    (or at-point
        (cdr (assoc (completing-read "Nim test: "
                                     (mapcar (lambda (item)
                                               (cons (plist-get item :filter) item))
                                             items)
                                     nil t)
                    (mapcar (lambda (item)
                              (cons (plist-get item :filter) item))
                            items))))))

(defun chief/nim-test-buffer (&optional prompt-args)
  "Run the current Nim file as a unittest/self-test executable.
This is the file-level form used for a main module or a test file:
`nim c -r file [test filters...]'.  With PROMPT-ARGS, read unittest filters or
other program arguments interactively."
  (interactive "P")
  (chief/nim--save-current-buffer)
  (let ((args (if prompt-args
                  (chief/nim-read-extra-args "Nim test args: ")
                chief/nim-test-args)))
    (chief/nim-compile
     (append (chief/nim-command "c" "-r" (chief/nim-current-file)) args)
     "*nim test module*")))

(defun chief/nim-test-buffer-with-backend (backend &optional prompt-args)
  "Run the current Nim unittest/self-test file with BACKEND."
  (interactive (list (chief/nim-read-backend "Test with Nim backend: ") current-prefix-arg))
  (chief/nim--save-current-buffer)
  (let* ((backend (chief/nim-normalize-backend backend))
         (args (if prompt-args
                   (chief/nim-read-extra-args "Nim test args: ")
                 chief/nim-test-args)))
    (chief/nim-compile
     (apply #'chief/nim-command-runs-with-backend
            backend
            (chief/nim-current-file)
            args)
     (format "*nim test %s*" backend))))

(defun chief/nim-test-main-module (&optional prompt-args)
  "Test the current Nim main module by compiling and running it."
  (interactive "P")
  (chief/nim-test-buffer prompt-args))

(defun chief/nim-test-at-point (&optional prompt-args)
  "Run the Nim unittest test at point, prompting when needed.
With PROMPT-ARGS, read additional unittest executable arguments before the
selected test filter."
  (interactive "P")
  (chief/nim--save-current-buffer)
  (let* ((item (chief/nim-read-test-item))
         (filter (plist-get item :filter))
         (args (if prompt-args
                   (chief/nim-read-extra-args "Extra Nim test args: ")
                 chief/nim-test-args)))
    (chief/nim-compile
     (append (chief/nim-command "c" "-r" (chief/nim-current-file))
             args
             (list filter))
     (format "*nim test %s*" filter))))

(defun chief/nim-test-at-point-with-backend (backend &optional prompt-args)
  "Run the Nim unittest test at point with BACKEND."
  (interactive (list (chief/nim-read-backend "Test at point with Nim backend: ") current-prefix-arg))
  (chief/nim--save-current-buffer)
  (let* ((backend (chief/nim-normalize-backend backend))
         (item (chief/nim-read-test-item))
         (filter (plist-get item :filter))
         (args (if prompt-args
                   (chief/nim-read-extra-args "Extra Nim test args: ")
                 chief/nim-test-args)))
    (chief/nim-compile
     (append (apply #'chief/nim-command-runs-with-backend
                    backend
                    (chief/nim-current-file)
                    args)
             (list filter))
     (format "*nim test %s %s*" backend filter))))

(defun chief/nim-test-library-module (&optional prompt-args)
  "Run the test file that exercises the current Nim library module.
This uses the Nim convention of `tests/t*.nim' files and adds the Nimble srcDir
as `--path:srcDir' when the package declares one."
  (interactive "P")
  (chief/nim--save-current-buffer)
  (let* ((root (chief/nim-project-root))
         (test-file (chief/nim-read-library-test-file (chief/nim-current-file) root))
         (path-switches (chief/nim-source-path-switches root))
         (args (if prompt-args
                   (chief/nim-read-extra-args "Nim library test args: ")
                 chief/nim-test-args))
         (command (append (apply #'chief/nim-command
                                 (append (list "c" "-r")
                                         path-switches
                                         (list test-file)))
                          args)))
    (chief/nim-compile command "*nim test library*" root)))

(defun chief/nim-test-library-module-with-backend (backend &optional prompt-args)
  "Run the test file for the current Nim library module with BACKEND."
  (interactive (list (chief/nim-read-backend "Test library with Nim backend: ") current-prefix-arg))
  (chief/nim--save-current-buffer)
  (let* ((root (chief/nim-project-root))
         (backend (chief/nim-normalize-backend backend))
         (test-file (chief/nim-read-library-test-file (chief/nim-current-file) root))
         (path-switches (chief/nim-source-path-switches root))
         (args (if prompt-args
                   (chief/nim-read-extra-args "Nim library test args: ")
                 chief/nim-test-args))
         (command (append (apply #'chief/nim-command
                                 (append (list "r" (chief/nim-backend-switch backend))
                                         path-switches
                                         (list test-file)))
                          args)))
    (chief/nim-compile command (format "*nim test library %s*" backend) root)))

(defun chief/nimble-test-project (&optional prompt-args)
  "Run Nimble tests for the current project."
  (interactive "P")
  (chief/nim--save-current-buffer)
  (let ((args (if prompt-args
                  (chief/nim-read-extra-args "Nimble test args: ")
                chief/nimble-test-args)))
    (chief/nim-compile (append (chief/nimble-command "test") args)
                       "*nimble test*"
                       (chief/nim-project-root))))

(defun chief/nim-test-current-target (&optional prompt-args)
  "Test the current Nim target using the correct layer.
- A buffer containing `std/unittest' tests runs the test at point/file.
- A Nimble package buffer without file-local tests runs `nimble test'.
- A non-Nimble library module with a matching `tests/t*.nim' file runs that test file.
- A standalone file is compiled and run as a self-test/main module."
  (interactive "P")
  (cond
   ((chief/nim-buffer-test-items)
    (chief/nim-test-at-point prompt-args))
   ((chief/nim-test-file-p)
    (chief/nim-test-buffer prompt-args))
   ((chief/nimble-project-p)
    (chief/nimble-test-project prompt-args))
   ((chief/nim-library-test-candidates)
    (chief/nim-test-library-module prompt-args))
   (t
    (chief/nim-test-buffer prompt-args))))

(defun chief/nim-test-current-target-with-backend (backend &optional prompt-args)
  "Test the current Nim target with BACKEND where file-level testing applies.
For package-level suites, Nimble's `test' command uses the package's configured
backend, so this helper only overrides backend for file/test-at-point/library
module forms."
  (interactive (list (chief/nim-read-backend "Test target with Nim backend: ") current-prefix-arg))
  (cond
   ((chief/nim-buffer-test-items)
    (chief/nim-test-at-point-with-backend backend prompt-args))
   ((chief/nim-test-file-p)
    (chief/nim-test-buffer-with-backend backend prompt-args))
   ((and (not (chief/nimble-project-p)) (chief/nim-library-test-candidates))
    (chief/nim-test-library-module-with-backend backend prompt-args))
   ((chief/nimble-project-p)
    (chief/nimble-test-project prompt-args))
   (t
    (chief/nim-test-buffer-with-backend backend prompt-args))))

(defun chief/nimble-build-project (&optional prompt-args)
  "Run Nimble build for the current project."
  (interactive "P")
  (chief/nim--save-current-buffer)
  (let ((args (if prompt-args
                  (chief/nim-read-extra-args "Nimble build args: ")
                chief/nimble-build-args)))
    (chief/nim-compile (append (chief/nimble-command "build") args)
                       "*nimble build*"
                       (chief/nim-project-root))))

(defun chief/nimble-build-binary (binary &optional prompt-args)
  "Run `nimble build' for a specific BINARY."
  (interactive
   (list (chief/nimble-read-binary "Build Nimble binary: ") current-prefix-arg))
  (chief/nim--save-current-buffer)
  (let ((args (if prompt-args
                  (chief/nim-read-extra-args "Nimble build args: ")
                chief/nimble-build-args)))
    (chief/nim-compile (append (chief/nimble-command "build") args (list binary))
                       (format "*nimble build %s*" binary)
                       (chief/nim-project-root))))

(defun chief/nimble-run-project (&optional prompt-args)
  "Run the current Nimble package with `nimble run'.
When the package has multiple binaries, the binary associated with the current
file is selected, otherwise the command prompts.  Program args are placed after
`--' so Nimble passes them to the executable."
  (interactive "P")
  (chief/nim--save-current-buffer)
  (let* ((binary (chief/nimble-run-binary-name))
         (args (if prompt-args
                   (chief/nim-read-extra-args "Program args: ")
                 chief/nimble-run-args))
         (command (append (chief/nimble-command "run")
                          (when binary (list binary))
                          (when args (cons "--" args)))))
    (chief/nim-compile command "*nimble run*" (chief/nim-project-root))))

(defun chief/nimble-run-binary (binary &optional prompt-args)
  "Run a specific Nimble BINARY."
  (interactive
   (list (chief/nimble-read-binary "Run Nimble binary: ") current-prefix-arg))
  (chief/nim--save-current-buffer)
  (let* ((args (if prompt-args
                   (chief/nim-read-extra-args "Program args: ")
                 chief/nimble-run-args))
         (command (append (chief/nimble-command "run")
                          (list binary)
                          (when args (cons "--" args)))))
    (chief/nim-compile command (format "*nimble run %s*" binary) (chief/nim-project-root))))

(defun chief/nimble-check-project ()
  "Run `nimble check' to validate the current package structure."
  (interactive)
  (chief/nim--save-current-buffer)
  (chief/nim-compile (chief/nimble-command "check") "*nimble check*" (chief/nim-project-root)))

(defun chief/nimble-compile-current-file ()
  "Compile the current file through `nimble c file'."
  (interactive)
  (chief/nim--save-current-buffer)
  (let* ((root (chief/nim-project-root))
         (file (file-relative-name (chief/nim-current-file) root)))
    (chief/nim-compile (chief/nimble-command "c" file)
                       "*nimble c*"
                       root)))

(defun chief/nimble-compile-current-file-with-backend (backend)
  "Compile the current package file with Nimble/Nim compiler BACKEND."
  (interactive (list (chief/nim-read-compile-command "Compile package file with: ")))
  (chief/nim--save-current-buffer)
  (let* ((root (chief/nim-project-root))
         (command (chief/nim-normalize-compile-command backend))
         (file (file-relative-name (chief/nim-current-file) root)))
    (chief/nim-compile (chief/nimble-current-file-command-with-backend command file)
                       (format "*nimble/nim %s*" command)
                       root)))

(defun chief/nimble-cpp-current-file ()
  "Compile the current file through `nimble cpp file'."
  (interactive)
  (chief/nimble-compile-current-file-with-backend "cpp"))

(defun chief/nimble-js-current-file ()
  "Compile the current file to JavaScript through `nimble js file'."
  (interactive)
  (chief/nim--save-current-buffer)
  (let* ((root (chief/nim-project-root))
         (file (file-relative-name (chief/nim-current-file) root)))
    (chief/nim-compile (chief/nimble-command "js" file)
                       "*nimble js*"
                       root)))

(defun chief/nimble-doc-current-file ()
  "Generate docs for the current file through `nimble doc file'."
  (interactive)
  (chief/nim--save-current-buffer)
  (let* ((root (chief/nim-project-root))
         (file (file-relative-name (chief/nim-current-file) root)))
    (chief/nim-compile (chief/nimble-command "doc" file)
                       "*nimble doc*"
                       root)))

(defun chief/nimble-doc2-current-file ()
  "Generate docs for the current file through `nimble doc2 file'."
  (interactive)
  (chief/nim--save-current-buffer)
  (let* ((root (chief/nim-project-root))
         (file (file-relative-name (chief/nim-current-file) root)))
    (chief/nim-compile (chief/nimble-command "doc2" file)
                       "*nimble doc2*"
                       root)))

(defun chief/nimble-clean-project ()
  "Run `nimble clean' for the current project."
  (interactive)
  (chief/nim-compile (chief/nimble-command "clean") "*nimble clean*" (chief/nim-project-root)))

(defun chief/nimble-dump-project ()
  "Run `nimble dump --json' for the current project."
  (interactive)
  (chief/nim-compile (chief/nimble-command "dump" "--json") "*nimble dump*" (chief/nim-project-root)))

(defun chief/nimble-deps-project (&optional tree)
  "Run `nimble deps' for the current project.
With TREE, include `--tree'."
  (interactive "P")
  (chief/nim-compile (append (chief/nimble-command "deps")
                             (when tree (list "--tree")))
                     "*nimble deps*"
                     (chief/nim-project-root)))

(defun chief/nimble-lock-project ()
  "Run `nimble lock' for the current project."
  (interactive)
  (chief/nim-compile (chief/nimble-command "lock") "*nimble lock*" (chief/nim-project-root)))

(defun chief/nimble-sync-project ()
  "Run `nimble sync' for the current project."
  (interactive)
  (chief/nim-compile (chief/nimble-command "sync") "*nimble sync*" (chief/nim-project-root)))

(defun chief/nimble-install-deps ()
  "Install dependencies for the current package with `nimble install --depsOnly'."
  (interactive)
  (chief/nim-compile (chief/nimble-command "install" "--depsOnly")
                     "*nimble install deps*"
                     (chief/nim-project-root)))

(defun chief/nimble-setup-project ()
  "Run `nimble setup' for the current project."
  (interactive)
  (chief/nim-compile (chief/nimble-command "setup") "*nimble setup*" (chief/nim-project-root)))

(defun chief/nimble-show-tasks ()
  "List Nimble tasks for the current project."
  (interactive)
  (chief/nim-compile (chief/nimble-command "tasks") "*nimble tasks*" (chief/nim-project-root)))

(defun chief/nimble-task-names ()
  "Return task names declared in the nearest .nimble file."
  (when-let* ((file (chief/project-nearest-matching-file "\\.nimble\\'")))
    (with-temp-buffer
      (insert-file-contents file)
      (let (tasks)
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*task[ \t]+\\([[:alnum:]_]+\\)[ \t]*," nil t)
          (push (match-string 1) tasks))
        (delete-dups (nreverse tasks))))))

(defun chief/nimble-run-task (task &optional prompt-args)
  "Run Nimble TASK from the current project.
With PROMPT-ARGS, read task run arguments.  For task compiler flags, use the
raw Nimble command helper because Nimble task flags are positional."
  (interactive
   (list (completing-read "Nimble task: "
                          (or (chief/nimble-task-names)
                              '("build" "test" "run"))
                          nil t)
         current-prefix-arg))
  (let ((args (and prompt-args (chief/nim-read-extra-args "Nimble task args: "))))
    (chief/nim-compile (append (chief/nimble-command task) args)
                       (format "*nimble %s*" task)
                       (chief/nim-project-root))))

(defun chief/nim-run-arbitrary-command (command)
  "Run an arbitrary Nim COMMAND in a compilation buffer."
  (interactive (list (read-shell-command "Nim command: " "nim ")))
  (let ((argv (split-string-and-unquote command)))
    (unless (and argv (string= (car argv) "nim"))
      (setq argv (cons "nim" argv)))
    (chief/nim-compile argv "*nim command*" (chief/nim-project-root))))

(defun chief/nimble-run-arbitrary-command (command)
  "Run an arbitrary Nimble COMMAND in a compilation buffer."
  (interactive (list (read-shell-command "Nimble command: " "nimble --noColor ")))
  (let ((argv (split-string-and-unquote command)))
    (unless (and argv (string= (car argv) "nimble"))
      (setq argv (append '("nimble" "--noColor") argv)))
    (chief/nim-compile argv "*nimble command*" (chief/nim-project-root))))

(defun chief/nim-format-buffer ()
  "Format the current Nim buffer with nimpretty when available."
  (interactive)
  (chief/nim--save-current-buffer)
  (unless (executable-find "nimpretty")
    (user-error "nimpretty is not available on PATH"))
  (let ((file (chief/nim-current-file))
        (stderr-file (make-temp-file "chief-nimpretty-")))
    (unwind-protect
        (let ((status (call-process "nimpretty" nil (list nil stderr-file) nil file)))
          (if (zerop status)
              (revert-buffer t t t)
            (error "%s"
                   (with-temp-buffer
                     (insert-file-contents stderr-file)
                     (string-trim (buffer-string))))))
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

(defun chief/nim-debug-buffer ()
  "Compile the current Nim file with debug info and launch DAP."
  (interactive)
  (chief/nim--save-current-buffer)
  (let* ((file (chief/nim-current-file))
         (binary (chief/nim-artifact-path "-debug"))
         (directory (file-name-directory file)))
    (chief/nim--run-command-async
     (chief/nim-command "c" "--debugger:native" "--lineDir:on"
                        (format "--out:%s" binary)
                        file)
     directory
     "*nim debug build*"
     (lambda (_output)
       (chief/nim--lldb-dap-debug binary
                                      (format "Nim file :: %s" (file-name-nondirectory file))
                                      directory
                                      "nim")))))

(defun chief/nim-debug-test-at-point ()
  "Compile the current Nim test with debug info and launch DAP."
  (interactive)
  (chief/nim--save-current-buffer)
  (let* ((item (chief/nim-read-test-item))
         (filter (plist-get item :filter))
         (file (chief/nim-current-file))
         (binary (chief/nim-artifact-path "-tests-debug"))
         (directory (file-name-directory file)))
    (chief/nim--run-command-async
     (chief/nim-command "c" "--debugger:native" "--lineDir:on"
                        (format "--out:%s" binary)
                        file)
     directory
     "*nim debug test build*"
     (lambda (_output)
       (chief/nim--lldb-dap-debug binary
                                      (format "Nim test :: %s" filter)
                                      directory
                                      "nim"
                                      (list filter))))))

(defun chief/nim-mode-setup ()
  "Configure Nim buffers."
  (setq-local compile-command
              (if buffer-file-name
                  (chief/nim-shell-command
                   (chief/nim-command "check" (expand-file-name buffer-file-name)))
                "nimble --noColor test"))
  (setq-local chief/lsp-root-function #'chief/nim-project-root)
  (chief/repl-configure
   :start #'chief/nim-secret-start-repl
   :restart #'chief/nim-secret-restart-repl
   :send-line #'chief/nim-secret-send-line
   :send-region #'chief/nim-secret-send-region
   :send-buffer #'chief/nim-secret-send-buffer
   :send-defun #'chief/nim-secret-send-defun
   :load-file #'chief/nim-secret-load-file)
  (chief/nim--completion-setup t)
  (chief/nim-configure-lsp)
  (when (require 'flycheck nil t)
    (chief/nim-select-flycheck-checker)
    (flycheck-mode 1))
  (when (fboundp 'chief/lsp-managed-mode-setup)
    (chief/lsp-managed-mode-setup)))

(chief/safe-use-package nim-mode
  :mode ("\\.nim\\'" . nim-mode)
  :mode ("\\.nims\\'" . nim-mode)
  :mode ("\\.nimble\\'" . nim-mode)
  :hook (nim-mode . chief/nim-mode-setup))

(with-eval-after-load 'nim-mode
  (chief/local-leader-def
    :keymaps 'nim-mode-map
    "c" '(:ignore t :which-key "nim")
    "cb" #'chief/nim-build-current-target
    "cB" #'chief/nim-build-release-buffer
    "cx" #'chief/nim-build-current-target-with-backend
    "cc" #'chief/nim-check-buffer
    "ck" #'chief/nim-check-buffer-with-backend
    "cC" #'chief/nimble-check-project
    "cr" #'chief/nim-run-current-target
    "cR" #'chief/nim-run-current-target-with-backend
    "ct" #'chief/nim-test-current-target
    "cT" #'chief/nimble-test-project
    "cX" #'chief/nim-test-current-target-with-backend
    "cl" #'chief/nim-test-library-module
    "cd" #'chief/nim-doc-buffer
    "cD" #'chief/nim-jsondoc-buffer
    "cH" #'chief/nim-doc-buffer-with-backend
    "cj" #'chief/nim-js-build-buffer
    "cJ" #'chief/nimble-js-current-file
    "cp" #'chief/nim-cpp-build-buffer
    "co" #'chief/nim-objc-build-buffer
    "cf" #'chief/nim-format-buffer
    "r" '(:ignore t :which-key "run")
    "rr" #'chief/nim-run-current-target
    "rR" #'chief/nim-run-current-target-with-backend
    "rm" #'chief/nim-run-buffer
    "rM" #'chief/nim-run-buffer-with-backend
    "rf" #'chief/nim-run-buffer
    "rp" #'chief/nimble-run-project
    "rb" #'chief/nimble-run-binary
    "rB" #'chief/nim-run-nimble-binary-with-backend
    "rt" #'chief/nimble-run-task
    "re" #'chief/nim-eval
    "t" '(:ignore t :which-key "test")
    "tt" #'chief/nim-test-current-target
    "tT" #'chief/nim-test-current-target-with-backend
    "tm" #'chief/nim-test-main-module
    "ta" #'chief/nim-test-at-point
    "tA" #'chief/nim-test-at-point-with-backend
    "tf" #'chief/nim-test-buffer
    "tF" #'chief/nim-test-buffer-with-backend
    "tl" #'chief/nim-test-library-module
    "tL" #'chief/nim-test-library-module-with-backend
    "tp" #'chief/nimble-test-project
    "d" '(:ignore t :which-key "debug")
    "dd" #'chief/nim-debug-buffer
    "dt" #'chief/nim-debug-test-at-point
    "p" '(:ignore t :which-key "project")
    "pb" #'chief/nimble-build-project
    "pB" #'chief/nimble-build-binary
    "pc" #'chief/nimble-check-project
    "pk" #'chief/nimble-clean-project
    "pr" #'chief/nimble-run-project
    "pR" #'chief/nimble-run-binary
    "px" #'chief/nim-run-nimble-binary-with-backend
    "pt" #'chief/nimble-test-project
    "ps" #'chief/nimble-run-task
    "pS" #'chief/nimble-show-tasks
    "pf" #'chief/nimble-compile-current-file
    "pF" #'chief/nimble-compile-current-file-with-backend
    "pJ" #'chief/nimble-js-current-file
    "pd" #'chief/nimble-doc-current-file
    "pD" #'chief/nimble-dump-project
    "pe" #'chief/nimble-deps-project
    "pl" #'chief/nimble-lock-project
    "pu" #'chief/nimble-sync-project
    "pi" #'chief/nimble-install-deps
    "pP" #'chief/nimble-setup-project
    "'" #'chief/nim-secret-start-repl
    "x" '(:ignore t :which-key "raw")
    "xn" #'chief/nim-run-arbitrary-command
    "xb" #'chief/nimble-run-arbitrary-command)
  (chief/repl-setup-standard-local-leader 'nim-mode-map)
  (chief/local-leader-def
    :keymaps 'nim-mode-map
    "s" '(:ignore t :which-key "send/repl")
    "s'" #'chief/nim-secret-start-repl
    "si" #'chief/nim-secret-import-current-module
    "sI" #'chief/nim-secret-include-current-file
    "sL" #'chief/nim-secret-send-line-safely
    "sE" #'chief/nim-secret-send-region-safely
    "sq" #'chief/nim-secret-quit-repl
    "sC" #'chief/nim-secret-clear-repl
    "sK" #'chief/nim-secret-interrupt-repl))

(defun chief/nim-secret-reconfigure-live-buffers ()
  "Reapply Nim REPL mode/highlighting to existing `nim secret' buffers."
  (dolist (buffer (buffer-list))
    (when (string-match-p "\\`\\*nim-secret\\[" (buffer-name buffer))
      (with-current-buffer buffer
        (let ((root (file-name-as-directory (expand-file-name default-directory))))
          (chief/nim-secret--configure-buffer buffer root))))))

(chief/nim-secret-reconfigure-live-buffers)

(provide 'lang-nim)
;;; lang-nim.el ends here
