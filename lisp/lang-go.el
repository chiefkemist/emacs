;;; lang-go.el --- Go, gopls, and templ support -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'compile)
(require 'core-projects)
(require 'json)
(require 'project)
(require 'seq)
(require 'subr-x)

(declare-function dap-debug "dap-mode" (debug-args))
(declare-function dap-dlv-go-debug-in-vterm "dap-dlv-go" ())
(declare-function lsp-execute-code-action "lsp-mode" ())
(declare-function lsp-organize-imports "lsp-mode" ())

(defcustom chief/go-gopls-directory-filters
  ["-**/node_modules"]
  "Directory filters passed to gopls for mixed-language monorepos.

These filters avoid indexing large non-Go trees that commonly live beside Go
modules in polyglot repos."
  :type '(vector string)
  :group 'chief)

(defcustom chief/go-gopls-expand-workspace-to-module t
  "Whether gopls should expand a workspace to the entire enclosing module.

This matches gopls' default and preserves full workspace behavior."
  :type 'boolean
  :group 'chief)

(defcustom chief/go-gopls-analyses nil
  "Extra gopls analyzers enabled for Go buffers.
Nil means use gopls/lsp-go defaults, matching the NeoVim config's `gopls = {}`."
  :type '(alist :key-type string :value-type boolean)
  :group 'chief)

(defcustom chief/go-gopls-codelenses
  '((gc_details . :json-false)
    (generate . t)
    (regenerate_cgo . t)
    (run_govulncheck . t)
    (test . t)
    (tidy . t)
    (upgrade_dependency . t)
    (vendor . t))
  "Code lenses enabled for gopls.
The server keeps these capabilities available, but `lsp-lens' overlays are
turned off for Go to match NeoVim's non-overlay LSP behavior."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'chief)

(defcustom chief/go-gopls-staticcheck nil
  "Whether gopls should run Staticcheck analyzers.
This is disabled by default because Staticcheck can make large workspaces feel
stuck at `Loading packages...' and can block editor responsiveness."
  :type 'boolean
  :group 'chief)

(defcustom chief/go-prefer-module-root nil
  "When non-nil, prefer the nearest go.mod over an enclosing go.work for Go files.
Leaving this nil preserves cross-module gopls behavior in go.work monorepos.
Set it to t only when a very large go.work workspace is making gopls unusably
slow and you accept narrower cross-module references/rename/symbols."
  :type 'boolean
  :group 'chief)

(defcustom chief/go-disable-lsp-file-watchers t
  "When non-nil, disable lsp-mode file watchers for Go buffers.
gopls already tracks the Go workspace; lsp-mode's generic recursive watchers
can make large monorepos extremely sluggish."
  :type 'boolean
  :group 'chief)

(defcustom chief/go-gopls-diagnostics-trigger "Edit"
  "When gopls should recompute diagnostics.
The default \"Edit\" matches gopls/NeoVim behavior.  Set this to \"Save\" only
as an emergency throttle for pathological workspaces."
  :type '(choice (const "Save") (const "Edit"))
  :group 'chief)

(defcustom chief/go-gopls-diagnostics-delay "3s"
  "Delay before gopls recomputes diagnostics after edits when trigger is Edit."
  :type 'string
  :group 'chief)

(defcustom chief/go-format-command-preference '("goimports" "gofumpt" "gofmt")
  "Formatter executables tried for Go format-on-save, in order.
This mirrors vim-go's fast local format/import-on-save path and avoids blocking
saves on a busy gopls process."
  :type '(repeat string)
  :group 'chief)

(defcustom chief/go-test-args '("-v" "-count=1")
  "Default arguments added to focused `go test' commands."
  :type '(repeat string)
  :group 'chief)

(defcustom chief/go-benchmark-args '("-benchmem")
  "Default arguments added to focused Go benchmark commands."
  :type '(repeat string)
  :group 'chief)

(defcustom chief/go-fuzz-time "10s"
  "Default fuzz duration used by focused Go fuzz commands."
  :type 'string
  :group 'chief)

(defun chief/go-work-file-p (&optional file)
  "Return non-nil when FILE is a go.work buffer/file."
  (string= (file-name-nondirectory (or file buffer-file-name "")) "go.work"))

(defun chief/go-module-root (&optional start)
  "Return the nearest Go module root, if any."
  (chief/project-nearest-marker-root '("go.mod") start))

(defun chief/go-workspace-root (&optional start)
  "Return the nearest Go workspace root containing go.work, if any."
  (chief/project-nearest-marker-root '("go.work") start))

(defun chief/go-project-root (&optional prefer-workspace)
  "Return the current Go project root, if any.
By default this prefers the nearest go.mod over an enclosing go.work for Go
source files.  That mirrors gopls' large-workspace guidance: open a narrower
nested workspace to avoid diagnosing/indexing every module in a monorepo after
edits.  When PREFER-WORKSPACE is non-nil, or in a go.work buffer, prefer the
nearest go.work root."
  (let ((start (chief/project-default-start-directory)))
    (if (or prefer-workspace
            (chief/go-work-file-p)
            (not chief/go-prefer-module-root))
        (or (chief/go-workspace-root start)
            (chief/go-module-root start)
            (chief/project-current-root start)
            default-directory)
      (or (chief/go-module-root start)
          (chief/go-workspace-root start)
          (chief/project-current-root start)
          default-directory))))

(defun chief/go-default-directory ()
  "Return the preferred working directory for Go commands."
  (or (chief/go-project-root) default-directory))

(defun chief/go-buffer-directory ()
  "Return the current buffer's package directory."
  (file-name-as-directory
   (expand-file-name
    (or (and buffer-file-name (file-name-directory buffer-file-name))
        (chief/go-project-root)
        default-directory))))

(defun chief/go-compile (command name &optional directory)
  "Run Go COMMAND in DIRECTORY using compilation buffer NAME."
  (let ((default-directory (or directory (chief/go-default-directory)))
        (compilation-read-command nil))
    (compilation-start
     (mapconcat #'shell-quote-argument command " ")
     'compilation-mode
     (lambda (_) name))))

(defun chief/go-command (&rest args)
  "Return a Go command list built from ARGS."
  (unless (executable-find "go")
    (user-error "go is not available on PATH"))
  (cons "go" args))

(defun chief/go-current-file ()
  "Return the current buffer file as an absolute Go source path."
  (unless buffer-file-name
    (user-error "This buffer is not visiting a Go file"))
  (expand-file-name buffer-file-name))

(defun chief/go-save-current-buffer ()
  "Save the current Go buffer when it is modified."
  (when (and buffer-file-name (buffer-modified-p))
    (save-buffer)))

(defun chief/go-shell-command (command)
  "Return shell-safe string for COMMAND list."
  (mapconcat #'shell-quote-argument command " "))

(defun chief/go-list-json (&optional pattern directory)
  "Return parsed `go list -json' for PATTERN in DIRECTORY."
  (when (executable-find "go")
    (let ((default-directory (or directory (chief/go-buffer-directory))))
      (with-temp-buffer
        (when (zerop (call-process "go" nil t nil "list" "-json" (or pattern ".")))
          (condition-case nil
              (json-parse-string (buffer-string)
                                 :object-type 'hash-table
                                 :array-type 'list
                                 :null-object nil
                                 :false-object :json-false)
            (error nil)))))))

(defun chief/go-package-name (&optional directory)
  "Return the current Go package name for DIRECTORY."
  (when-let* ((metadata (chief/go-list-json "." directory)))
    (gethash "Name" metadata)))

(defun chief/go-current-file-has-main-p ()
  "Return non-nil when the current file appears to define `func main'."
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward "^[ \t]*package[ \t]+main\\_>" nil t)
         (progn
           (goto-char (point-min))
           (re-search-forward "^[ \t]*func[ \t]+main[ \t]*()" nil t)))))

(defun chief/go-main-package-p (&optional directory)
  "Return non-nil when DIRECTORY is a Go main package."
  (or (string= (chief/go-package-name directory) "main")
      (and (or (null directory)
               (and buffer-file-name
                    (file-equal-p (file-name-as-directory (expand-file-name directory))
                                  (chief/go-buffer-directory))))
           (chief/go-current-file-has-main-p))))

(defun chief/go-build-package ()
  "Build the current Go package."
  (interactive)
  (chief/go-compile (chief/go-command "build" ".") "*go build*" (chief/go-buffer-directory)))

(defun chief/go-build-project ()
  "Build every package in the current Go project or workspace."
  (interactive)
  (chief/go-compile (chief/go-command "build" "./...") "*go build project*" (chief/go-default-directory)))

(defun chief/go-vet-package ()
  "Run `go vet' for the current Go package."
  (interactive)
  (chief/go-compile (chief/go-command "vet" ".") "*go vet*" (chief/go-buffer-directory)))

(defun chief/go-vet-project ()
  "Run `go vet ./...' for the current Go project or workspace."
  (interactive)
  (chief/go-compile (chief/go-command "vet" "./...") "*go vet project*" (chief/go-default-directory)))

(defun chief/go-run-package ()
  "Run the current Go package with `go run .'."
  (interactive)
  (chief/go-save-current-buffer)
  (unless (chief/go-main-package-p (chief/go-buffer-directory))
    (user-error "Current package is not package main"))
  (chief/go-compile (chief/go-command "run" ".") "*go run package*" (chief/go-buffer-directory)))

(defun chief/go-run-current-file ()
  "Run the current Go file directly with `go run file.go'."
  (interactive)
  (chief/go-save-current-buffer)
  (chief/go-compile
   (chief/go-command "run" (chief/go-current-file))
   "*go run file*"
   (chief/go-buffer-directory)))

(defun chief/go-run-current-target ()
  "Run the current Go target, preferring `go run .' for main packages."
  (interactive)
  (chief/go-save-current-buffer)
  (if (chief/go-main-package-p (chief/go-buffer-directory))
      (chief/go-run-package)
    (chief/go-run-current-file)))

(defconst chief/go-test-function-regexp
  "^[ \t]*func[ \t]+\\(Test\\|Benchmark\\|Example\\|Fuzz\\)\\([[:alnum:]_]*\\)[ \t]*("
  "Regexp matching top-level Go test, benchmark, example, and fuzz functions.")

(defun chief/go-test-kind (name)
  "Return the test kind for Go test function NAME."
  (cond
   ((string-prefix-p "Benchmark" name) 'benchmark)
   ((string-prefix-p "Example" name) 'example)
   ((string-prefix-p "Fuzz" name) 'fuzz)
   (t 'test)))

(defun chief/go-buffer-test-items ()
  "Return plist entries for Go test functions in the current buffer."
  (let (items)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward chief/go-test-function-regexp nil t)
        (unless (nth 8 (save-excursion (syntax-ppss (match-beginning 0))))
          (let ((name (concat (match-string 1) (match-string 2))))
            (push (list :name name
                        :kind (chief/go-test-kind name)
                        :position (match-beginning 0))
                  items)))))
    (nreverse items)))

(defun chief/go-test-item-at-point ()
  "Return the Go test item at point, or nil."
  (let ((position (point))
        (items (chief/go-buffer-test-items)))
    (cl-loop for item in items
             for next in (append (cdr items) (list nil))
             for start = (plist-get item :position)
             for end = (or (plist-get next :position) (point-max))
             when (and (<= start position) (< position end))
             return item)))

(defun chief/go-read-test-item ()
  "Return the Go test item at point, or prompt for one in the current file."
  (let* ((items (chief/go-buffer-test-items))
         (at-point (chief/go-test-item-at-point)))
    (unless items
      (user-error "No Go tests, benchmarks, examples, or fuzz targets found in this file"))
    (or at-point
        (cdr
         (assoc (completing-read
                 "Go test: "
                 (mapcar (lambda (item)
                           (cons (format "%s <%s>"
                                         (plist-get item :name)
                                         (plist-get item :kind))
                                 item))
                         items)
                 nil t)
                (mapcar (lambda (item)
                          (cons (format "%s <%s>"
                                        (plist-get item :name)
                                        (plist-get item :kind))
                                item))
                        items))))))

(defun chief/go-test-name-regexp (names)
  "Return an anchored Go regexp matching test NAMES."
  (if (= (length names) 1)
      (format "^%s$" (regexp-quote (car names)))
    (format "^(%s)$" (mapconcat #'regexp-quote names "|"))))

(defun chief/go-test-command-for-item (item &optional fuzz)
  "Return a focused `go test' command for ITEM.
When FUZZ is non-nil and ITEM is a fuzz target, run fuzzing instead of only the
seed corpus."
  (let* ((name (plist-get item :name))
         (regexp (format "^%s$" (regexp-quote name))))
    (pcase (plist-get item :kind)
      ('benchmark
       (append (chief/go-command "test")
               chief/go-test-args
               (list "-run" "^$" "-bench" regexp)
               chief/go-benchmark-args))
      ('fuzz
       (append (chief/go-command "test")
               (if fuzz
                   (seq-remove (lambda (arg) (string-prefix-p "-count" arg))
                               chief/go-test-args)
                 chief/go-test-args)
               (if fuzz
                   (list "-run" "^$" "-fuzz" regexp "-fuzztime" chief/go-fuzz-time)
                 (list "-run" regexp))))
      (_
       (append (chief/go-command "test")
               chief/go-test-args
               (list "-run" regexp))))))

(defun chief/go-test-current-file-command ()
  "Return a `go test' command focused on test-like functions in this file."
  (let* ((items (chief/go-buffer-test-items))
         (run-names (mapcar (lambda (item) (plist-get item :name))
                            (seq-filter (lambda (item)
                                          (memq (plist-get item :kind) '(test example fuzz)))
                                        items)))
         (bench-names (mapcar (lambda (item) (plist-get item :name))
                              (seq-filter (lambda (item)
                                            (eq (plist-get item :kind) 'benchmark))
                                          items))))
    (append (chief/go-command "test")
            chief/go-test-args
            (cond
             (run-names (list "-run" (chief/go-test-name-regexp run-names)))
             (bench-names (list "-run" "^$"))
             (t nil))
            (when bench-names
              (append (list "-bench" (chief/go-test-name-regexp bench-names))
                      chief/go-benchmark-args)))))

(defun chief/go-test-package ()
  "Run tests for the current Go package."
  (interactive)
  (chief/go-compile (chief/go-command "test") "*go test package*" (chief/go-buffer-directory)))

(defun chief/go-test-current-file ()
  "Run tests, examples, fuzz seed corpus, and benchmarks declared in this file."
  (interactive)
  (chief/go-save-current-buffer)
  (chief/go-compile (chief/go-test-current-file-command)
                    "*go test file*"
                    (chief/go-buffer-directory)))

(defun chief/go-test-at-point ()
  "Run the Go test, example, benchmark, or fuzz seed corpus at point."
  (interactive)
  (chief/go-save-current-buffer)
  (let ((item (chief/go-read-test-item)))
    (chief/go-compile (chief/go-test-command-for-item item)
                      (format "*go test %s*" (plist-get item :name))
                      (chief/go-buffer-directory))))

(defun chief/go-benchmark-at-point ()
  "Run the Go benchmark at point, prompting when point is not in one."
  (interactive)
  (chief/go-save-current-buffer)
  (let ((item (chief/go-read-test-item)))
    (unless (eq (plist-get item :kind) 'benchmark)
      (user-error "%s is not a benchmark" (plist-get item :name)))
    (chief/go-compile (chief/go-test-command-for-item item)
                      (format "*go bench %s*" (plist-get item :name))
                      (chief/go-buffer-directory))))

(defun chief/go-fuzz-at-point ()
  "Run the Go fuzz target at point with `chief/go-fuzz-time'."
  (interactive)
  (chief/go-save-current-buffer)
  (let ((item (chief/go-read-test-item)))
    (unless (eq (plist-get item :kind) 'fuzz)
      (user-error "%s is not a fuzz target" (plist-get item :name)))
    (chief/go-compile (chief/go-test-command-for-item item t)
                      (format "*go fuzz %s*" (plist-get item :name))
                      (chief/go-buffer-directory))))

(defun chief/go-test-project ()
  "Run tests for the current Go project or workspace."
  (interactive)
  (chief/go-compile (chief/go-command "test" "./...") "*go test project*" (chief/go-default-directory)))

(defun chief/go-dap-debug (name mode program &optional args)
  "Start a Go DAP session named NAME with Delve MODE for PROGRAM."
  (require 'dap-mode)
  (require 'dap-dlv-go)
  (dap-debug
   (list :type "go"
         :request "launch"
         :name name
         :mode mode
         :program program
         :cwd (if (file-directory-p program) program (file-name-directory program))
         :args (or args nil)
         :env nil)))

(defun chief/go-dap-test-args-for-item (item)
  "Return Delve test-binary args for Go test ITEM."
  (let* ((name (plist-get item :name))
         (regexp (format "^%s$" (regexp-quote name))))
    (pcase (plist-get item :kind)
      ('benchmark
       (append (list "-test.v" "-test.run=^$" (format "-test.bench=%s" regexp))
               (mapcar (lambda (arg) (concat "-test." (string-remove-prefix "-" arg)))
                       chief/go-benchmark-args)))
      (_
       (list "-test.v" "-test.count=1" (format "-test.run=%s" regexp))))))

(defun chief/go-debug-package ()
  "Debug the current Go main package with Delve."
  (interactive)
  (chief/go-save-current-buffer)
  (unless (chief/go-main-package-p (chief/go-buffer-directory))
    (user-error "Current package is not package main"))
  (chief/go-dap-debug "Go package" "debug" (chief/go-buffer-directory)))

(defun chief/go-debug-current-file ()
  "Debug the current Go file or main package with Delve."
  (interactive)
  (chief/go-save-current-buffer)
  (if (chief/go-main-package-p (chief/go-buffer-directory))
      (chief/go-debug-package)
    (chief/go-dap-debug "Go file" "auto" (chief/go-current-file))))

(defun chief/go-debug-test-at-point ()
  "Debug the Go test, example, or benchmark at point with Delve."
  (interactive)
  (chief/go-save-current-buffer)
  (let ((item (chief/go-read-test-item)))
    (when (eq (plist-get item :kind) 'fuzz)
      (user-error "Delve cannot debug active fuzzing; run the fuzz seed corpus with test-at-point"))
    (chief/go-dap-debug (format "Go test :: %s" (plist-get item :name))
                        "test"
                        (chief/go-buffer-directory)
                        (chief/go-dap-test-args-for-item item))))

(defun chief/go-debug-in-vterm ()
  "Debug an executable through Delve's vterm helper."
  (interactive)
  (require 'dap-dlv-go)
  (call-interactively #'dap-dlv-go-debug-in-vterm))

(defun chief/go-generate-package ()
  "Run `go generate' in the current Go package."
  (interactive)
  (chief/go-compile (chief/go-command "generate" ".") "*go generate package*" (chief/go-buffer-directory)))

(defun chief/go-generate-project ()
  "Run `go generate ./...' in the current Go project."
  (interactive)
  (chief/go-compile (chief/go-command "generate" "./...") "*go generate*" (chief/go-default-directory)))

(defun chief/go-mod-tidy ()
  "Run `go mod tidy' in the current Go module."
  (interactive)
  (chief/go-compile (chief/go-command "mod" "tidy") "*go mod tidy*" (or (chief/go-module-root) (chief/go-default-directory))))

(defun chief/go-mod-download ()
  "Run `go mod download' in the current Go module."
  (interactive)
  (chief/go-compile (chief/go-command "mod" "download") "*go mod download*" (or (chief/go-module-root) (chief/go-default-directory))))

(defun chief/go-mod-verify ()
  "Run `go mod verify' in the current Go module."
  (interactive)
  (chief/go-compile (chief/go-command "mod" "verify") "*go mod verify*" (or (chief/go-module-root) (chief/go-default-directory))))

(defun chief/go-work-sync ()
  "Run `go work sync' in the current Go workspace."
  (interactive)
  (unless (chief/go-workspace-root)
    (user-error "No go.work found for this buffer"))
  (chief/go-compile (chief/go-command "work" "sync") "*go work sync*" (chief/go-workspace-root)))

(defun chief/go-install-project ()
  "Install every package in the current Go project."
  (interactive)
  (chief/go-compile (chief/go-command "install" "./...") "*go install*" (chief/go-default-directory)))

(defun chief/go-clean-test-cache ()
  "Clear Go's test cache."
  (interactive)
  (chief/go-compile (chief/go-command "clean" "-testcache") "*go clean testcache*" (chief/go-default-directory)))

(defun chief/go-organize-imports ()
  "Run the LSP organize-imports code action for this Go buffer."
  (interactive)
  (if (fboundp 'lsp-organize-imports)
      (call-interactively #'lsp-organize-imports)
    (user-error "lsp-organize-imports is not available")))

(defun chief/go-format-executable ()
  "Return the first available Go formatter from `chief/go-format-command-preference'."
  (seq-some #'executable-find chief/go-format-command-preference))

(defun chief/go-format-buffer ()
  "Format the current Go buffer without involving gopls.
This keeps saving responsive even when gopls is still loading packages in a
large workspace."
  (interactive)
  (let ((formatter (or (chief/go-format-executable)
                       (user-error "No Go formatter found in %S"
                                   chief/go-format-command-preference)))
        (input (buffer-substring-no-properties (point-min) (point-max)))
        (stderr-file (make-temp-file "chief-go-format-"))
        formatted status)
    (unwind-protect
        (progn
          (setq formatted
                (with-temp-buffer
                  (insert input)
                  (setq status
                        (call-process-region (point-min) (point-max)
                                             formatter t (list t stderr-file) nil))
                  (buffer-string)))
          (if (zerop status)
              (unless (string= input formatted)
                (let ((point (point)))
                  (erase-buffer)
                  (insert formatted)
                  (goto-char (min point (point-max)))))
            (user-error "%s failed: %s"
                        (file-name-nondirectory formatter)
                        (with-temp-buffer
                          (insert-file-contents stderr-file)
                          (string-trim (buffer-string))))))
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

(defun chief/go-templ-command (&rest args)
  "Return a command list that runs templ with ARGS."
  (when-let* ((templ (executable-find "templ")))
    (cons templ args)))

(defun chief/go-templ-lsp-command ()
  "Return the command list used to start the templ language server."
  (chief/go-templ-command "lsp"))

(defun chief/go-templ-generate-file ()
  "Generate Go code for the current templ file."
  (interactive)
  (unless (and buffer-file-name
               (string-match-p "\\.templ\\'" buffer-file-name))
    (user-error "This buffer is not visiting a .templ file"))
  (save-buffer)
  (if-let* ((command (chief/go-templ-command
                      "generate"
                      "-f"
                      (expand-file-name buffer-file-name)
                      "-stdout")))
      (chief/go-compile command "*templ generate file*" (chief/go-default-directory))
    (user-error "templ is not available on PATH")))

(defun chief/go-templ-generate-project ()
  "Generate templ output for the current Go project."
  (interactive)
  (if-let* ((command (chief/go-templ-command "generate" "-path" ".")))
      (chief/go-compile command "*templ generate*" (chief/go-default-directory))
    (user-error "templ is not available on PATH")))

(defun chief/go-templ-format-buffer ()
  "Format the current templ buffer with `templ fmt'."
  (interactive)
  (unless (and buffer-file-name
               (string-match-p "\\.templ\\'" buffer-file-name))
    (user-error "This buffer is not visiting a .templ file"))
  (unless (chief/go-templ-command "fmt")
    (user-error "templ is not available on PATH"))
  (save-buffer)
  (let ((stderr-file (make-temp-file "chief-templ-fmt-"))
        (coding-system-for-read 'utf-8-unix)
        (coding-system-for-write 'utf-8-unix))
    (unwind-protect
        (let ((status (call-process (car (chief/go-templ-command "fmt"))
                                    nil
                                    (list nil stderr-file)
                                    nil
                                    "fmt"
                                    (expand-file-name buffer-file-name))))
          (if (zerop status)
              (revert-buffer t t t)
            (error "%s"
                   (with-temp-buffer
                     (insert-file-contents stderr-file)
                     (string-trim (buffer-string))))))
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

(defun chief/go-completion-setup ()
  "Enable Go popup completion, snippets, and auto-import completion edits."
  (setq-local lsp-completion-enable t)
  (setq-local lsp-completion-enable-additional-text-edit t)
  (when (boundp 'corfu-auto)
    (setq-local corfu-auto t))
  (when (boundp 'corfu-auto-delay)
    (setq-local corfu-auto-delay 0.05))
  (when (boundp 'corfu-auto-prefix)
    ;; Member and package completions often start immediately after `.'.
    (setq-local corfu-auto-prefix 0))
  (when (fboundp 'corfu-mode)
    (when (bound-and-true-p corfu-mode)
      (corfu-mode -1))
    (corfu-mode 1))
  (when (fboundp 'lsp-completion-mode)
    (lsp-completion-mode 1)))

(defun chief/go-apply-gopls-settings ()
  "Apply responsive gopls settings for the current buffer."
  (setq-local lsp-enabled-clients '(gopls))
  (setq-local lsp-go-directory-filters chief/go-gopls-directory-filters)
  (setq-local lsp-go-analysis-progress-reporting nil)
  (setq-local lsp-go-verbose-output nil)
  (setq-local lsp-go-analyses chief/go-gopls-analyses)
  (setq-local lsp-go-codelenses chief/go-gopls-codelenses)
  ;; NeoVim's config does not render automatic code-lens overlays.  In Emacs,
  ;; `lsp-lens-mode' can continuously request/resolve lenses while gopls is
  ;; still loading, causing overlay churn in large workspaces.  The gopls
  ;; code-lens capability remains available through explicit LSP commands.
  (setq-local lsp-lens-enable nil)
  (setq-local lsp-go-use-placeholders t)
  (setq-local lsp-go-complete-function-calls t)
  (setq-local lsp-go-completion-budget "75ms")
  (setq-local lsp-go-diagnostics-delay chief/go-gopls-diagnostics-delay)
  (setq-local lsp-go-use-gofumpt t)
  (setq-local lsp-go-symbol-matcher "FastFuzzy")
  (setq-local lsp-go-symbol-style "Dynamic")
  (setq-local lsp-go-symbol-scope "all")
  (setq-local lsp-go-matcher "Fuzzy")
  (setq-local lsp-go-import-shortcut "Both")
  (setq-local lsp-go-links-in-hover t)
  (setq-local lsp-go-template-extensions ["templ"])
  (setq-local lsp-idle-delay 0.8)
  (when chief/go-disable-lsp-file-watchers
    (setq-local lsp-enable-file-watchers nil))
  (when-let* ((command (and (fboundp 'chief/lsp-gopls-command)
                            (chief/lsp-gopls-command))))
    (setq-local lsp-go-gopls-server-path (car command))
    ;; Match the NeoVim config's plain `cmd = { "gopls" }` instead of
    ;; lsp-go's `-remote=auto` default.  A per-Emacs direct gopls process avoids
    ;; daemon state shared across unrelated workspaces.
    (setq-local lsp-go-gopls-server-args nil)))

(defun chief/go-mode-setup ()
  "Configure the current Go source buffer."
  (setq-local compile-command "go test ./...")
  (setq-local chief/lsp-root-function #'chief/go-project-root)
  (chief/go-completion-setup)
  (when (fboundp 'chief/format-enable-on-save)
    (chief/format-enable-on-save #'chief/go-format-buffer))
  (when (require 'lsp-go nil t)
    (chief/go-apply-gopls-settings))
  (when (fboundp 'chief/lsp-managed-mode-setup)
    (chief/lsp-managed-mode-setup)))

(defun chief/go-module-mode-setup ()
  "Configure the current Go module or workspace buffer."
  (setq-local compile-command "go mod tidy")
  (setq-local chief/lsp-root-function #'chief/go-project-root)
  (chief/go-completion-setup)
  (when (require 'lsp-go nil t)
    (chief/go-apply-gopls-settings))
  (when (fboundp 'chief/lsp-managed-mode-setup)
    (chief/lsp-managed-mode-setup)))

(defun chief/templ-mode-setup ()
  "Configure the current templ buffer."
  (setq-local compile-command "templ generate -path .")
  (setq-local chief/lsp-root-function #'chief/go-project-root)
  (chief/go-completion-setup)
  (when (require 'lsp-mode nil t)
    (setq-local lsp-enabled-clients '(templ-ls)))
  (when (fboundp 'chief/lsp-managed-mode-setup)
    (chief/lsp-managed-mode-setup)))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '("\\.templ\\'" . "templ"))
  (lsp-register-custom-settings
   '(("gopls.expandWorkspaceToModule" chief/go-gopls-expand-workspace-to-module t)
     ("gopls.staticcheck" chief/go-gopls-staticcheck t)
     ("gopls.diagnosticsTrigger" chief/go-gopls-diagnostics-trigger)
     ("gopls.diagnosticsDelay" chief/go-gopls-diagnostics-delay)))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection #'chief/go-templ-lsp-command)
    :activation-fn (lsp-activate-on "templ")
    :priority 2
    :server-id 'templ-ls)))

(chief/safe-use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :hook (go-mode . chief/go-mode-setup))

(with-eval-after-load 'go-ts-mode
  ;; `templ-ts-mode' currently depends on private `go-ts-mode' helpers that were
  ;; removed in newer Emacs releases. Provide tiny compatibility shims so the
  ;; mode can still initialize on Emacs 31.
  (unless (fboundp 'go-ts-mode--iota-query-supported-p)
    (defun go-ts-mode--iota-query-supported-p () t))
  (unless (fboundp 'go-ts-mode--method-elem-supported-p)
    (defun go-ts-mode--method-elem-supported-p () t)))

(with-eval-after-load 'js
  ;; Newer Emacs versions expose these as functions instead of vars.
  (unless (boundp 'js--treesit-indent-rules)
    (defvar js--treesit-indent-rules (js--treesit-indent-rules)))
  (unless (boundp 'js--treesit-font-lock-settings)
    (defvar js--treesit-font-lock-settings (js--treesit-font-lock-settings))))

(defun chief/templ-ts-mode--stub-eglot-require (orig feature &rest args)
  "Let `templ-ts-mode' load without pulling in Eglot.
The upstream package only uses Eglot to append to `eglot-server-programs', while
this config uses `lsp-mode' for templ buffers."
  (if (eq feature 'eglot)
      (progn
        (defvar eglot-server-programs nil)
        'eglot)
    (apply orig feature args)))

(defun chief/templ-ts--language-at-point (point)
  "Return the embedded language at POINT inside a templ buffer."
  (let ((templ-parser (treesit-parser-create 'templ))
        (js-parser (treesit-parser-create 'javascript))
        (css-parser (treesit-parser-create 'css)))
    (cond
     ((treesit-parser-range-on css-parser point) 'css)
     ((treesit-parser-range-on js-parser point) 'javascript)
     ((treesit-parser-range-on templ-parser point) 'templ)
     (t 'templ))))

(defun chief/templ-ts--setup ()
  "Compatibility wrapper for `templ-ts-mode' on newer Emacs releases."
  (unless (treesit-available-p)
    (error "Tree-sitter is not available"))
  (unless (and (treesit-language-available-p 'templ)
               (treesit-language-available-p 'javascript)
               (treesit-language-available-p 'jsdoc)
               (treesit-language-available-p 'css))
    (error "templ-ts-mode needs the templ, javascript, jsdoc, and css tree-sitter grammars"))
  (setq-local treesit-language-source-alist
              `((templ . (,templ-ts-mode-grammar))))
  (treesit-parser-create 'templ)
  (treesit-parser-create 'javascript)
  (treesit-parser-create 'css)
  (setq-local comment-start "// "
              comment-end ""
              comment-start-skip (rx "//" (* (syntax whitespace))))
  (setq-local treesit-language-at-point-function #'chief/templ-ts--language-at-point)
  (setq-local treesit-range-settings
              (treesit-range-rules
               :embed 'javascript
               :host 'templ
               '((script_block_text) @javascript)
               :embed 'css
               :host 'templ
               '((style_element_text) @css)))
  (setq-local indent-tabs-mode t
              treesit-simple-indent-rules templ-ts--indent-rules)
  (setq-local electric-indent-chars
              (append "{}()<>" electric-indent-chars))
  (setq-local treesit-font-lock-feature-list templ-ts--font-lock-feature-list)
  (setq-local treesit-font-lock-settings
              (append js--treesit-font-lock-settings
                      css--treesit-settings
                      (apply #'treesit-font-lock-rules
                             (append templ-ts--templ-font-lock-rules
                                     templ-ts--go-font-lock-rules))))
  (treesit-major-mode-setup))

(defun chief/load-templ-ts-mode ()
  "Load and patch `templ-ts-mode' from the Go/Templ module only."
  (condition-case err
      (progn
        (when (fboundp 'straight-use-package)
          (straight-use-package 'templ-ts-mode))
        (advice-add 'require :around #'chief/templ-ts-mode--stub-eglot-require)
        (unwind-protect
            (require 'templ-ts-mode)
          (advice-remove 'require #'chief/templ-ts-mode--stub-eglot-require))
        (advice-add 'templ-ts--setup :override #'chief/templ-ts--setup)
        (add-to-list 'auto-mode-alist '("\\.templ\\'" . templ-ts-mode))
        (add-hook 'templ-ts-mode-hook #'chief/templ-mode-setup))
    (error
     (display-warning 'chief
                      (format "Failed to initialize package templ-ts-mode: %s"
                              (error-message-string err))
                      :warning))))

(chief/load-templ-ts-mode)

(when (fboundp 'go-ts-mode)
  (add-hook 'go-ts-mode-hook #'chief/go-mode-setup))

(when (fboundp 'go-mod-ts-mode)
  (add-hook 'go-mod-ts-mode-hook #'chief/go-module-mode-setup))

(when (fboundp 'go-work-ts-mode)
  (add-hook 'go-work-ts-mode-hook #'chief/go-module-mode-setup))

(with-eval-after-load 'go-mode
  (chief/local-leader-def
    :keymaps 'go-mode-map
    "c" '(:ignore t :which-key "go")
    "cb" #'chief/go-build-package
    "cB" #'chief/go-build-project
    "cv" #'chief/go-vet-package
    "cV" #'chief/go-vet-project
    "ct" #'chief/go-test-package
    "cT" #'chief/go-test-project
    "cf" #'chief/go-test-current-file
    "cr" #'chief/go-run-current-target
    "cR" #'chief/go-run-current-file
    "cg" #'chief/go-generate-package
    "cG" #'chief/go-generate-project
    "cm" #'chief/go-mod-tidy
    "cM" #'chief/go-mod-download
    "cC" #'chief/go-clean-test-cache
    "ci" #'chief/go-install-project
    "r" '(:ignore t :which-key "run")
    "rr" #'chief/go-run-current-target
    "rp" #'chief/go-run-package
    "rf" #'chief/go-run-current-file
    "t" '(:ignore t :which-key "test")
    "tt" #'chief/go-test-at-point
    "tf" #'chief/go-test-current-file
    "tp" #'chief/go-test-package
    "ta" #'chief/go-test-project
    "tb" #'chief/go-benchmark-at-point
    "tz" #'chief/go-fuzz-at-point
    "tc" #'chief/go-clean-test-cache
    "d" '(:ignore t :which-key "debug")
    "dd" #'chief/go-debug-current-file
    "dp" #'chief/go-debug-package
    "dt" #'chief/go-debug-test-at-point
    "di" #'chief/go-debug-in-vterm
    "m" '(:ignore t :which-key "module")
    "mt" #'chief/go-mod-tidy
    "md" #'chief/go-mod-download
    "mv" #'chief/go-mod-verify
    "mw" #'chief/go-work-sync
    "l" '(:ignore t :which-key "lsp")
    "li" #'chief/go-organize-imports
    "la" #'lsp-execute-code-action))

(with-eval-after-load 'go-ts-mode
  (chief/local-leader-def
    :keymaps '(go-ts-mode-map go-mod-ts-mode-map go-work-ts-mode-map)
    "c" '(:ignore t :which-key "go")
    "cb" #'chief/go-build-package
    "cB" #'chief/go-build-project
    "cv" #'chief/go-vet-package
    "cV" #'chief/go-vet-project
    "ct" #'chief/go-test-package
    "cT" #'chief/go-test-project
    "cf" #'chief/go-test-current-file
    "cr" #'chief/go-run-current-target
    "cR" #'chief/go-run-current-file
    "cg" #'chief/go-generate-package
    "cG" #'chief/go-generate-project
    "cm" #'chief/go-mod-tidy
    "cM" #'chief/go-mod-download
    "cC" #'chief/go-clean-test-cache
    "ci" #'chief/go-install-project
    "r" '(:ignore t :which-key "run")
    "rr" #'chief/go-run-current-target
    "rp" #'chief/go-run-package
    "rf" #'chief/go-run-current-file
    "t" '(:ignore t :which-key "test")
    "tt" #'chief/go-test-at-point
    "tf" #'chief/go-test-current-file
    "tp" #'chief/go-test-package
    "ta" #'chief/go-test-project
    "tb" #'chief/go-benchmark-at-point
    "tz" #'chief/go-fuzz-at-point
    "tc" #'chief/go-clean-test-cache
    "d" '(:ignore t :which-key "debug")
    "dd" #'chief/go-debug-current-file
    "dp" #'chief/go-debug-package
    "dt" #'chief/go-debug-test-at-point
    "di" #'chief/go-debug-in-vterm
    "m" '(:ignore t :which-key "module")
    "mt" #'chief/go-mod-tidy
    "md" #'chief/go-mod-download
    "mv" #'chief/go-mod-verify
    "mw" #'chief/go-work-sync
    "l" '(:ignore t :which-key "lsp")
    "li" #'chief/go-organize-imports
    "la" #'lsp-execute-code-action))

(with-eval-after-load 'templ-ts-mode
  (chief/local-leader-def
    :keymaps 'templ-ts-mode-map
    "c" '(:ignore t :which-key "templ")
    "cf" #'chief/go-templ-format-buffer
    "cg" #'chief/go-templ-generate-file
    "cG" #'chief/go-templ-generate-project))

(provide 'lang-go)
;;; lang-go.el ends here
