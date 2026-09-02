;;; lang-lean.el --- Lean 4, elan, and Lake support -*- lexical-binding: t; -*-

;; This module intentionally owns a small Lean 4 integration instead of
;; depending on lean4-mode or Nael.  It talks to the maintained Lean tooling
;; directly: elan chooses toolchains, Lake manages projects and executable
;; workflows, `lean --run' runs programs, optional JSON REPLs can be launched
;; when supplied by the user, and `lake serve' or `lean --server' provides LSP
;; features.
;;
;; A few small ideas (syntax table comments, `lake serve' preference, and the
;; refresh-dependencies trick) are adapted from lean4-mode, licensed under
;; Apache-2.0: https://github.com/leanprover-community/lean4-mode

(require 'cl-lib)
(require 'comint)
(require 'compile)
(require 'core-projects)
(require 'core-repl nil t)
(require 'json)
(require 'lang-lean-input)
(require 'project)
(require 'rx)
(require 'seq)
(require 'subr-x)

(declare-function chief/local-leader-def "core-evil" (&rest args))
(declare-function chief/repl-configure "core-repl" (&rest plist))
(declare-function chief/lsp-managed-mode-setup "core-lsp" ())
(declare-function chief/lsp-ensure-active-for-navigation "core-lsp" ())
(declare-function lsp "lsp-mode" (&optional arg))
(declare-function lsp-deferred "lsp-mode")
(declare-function lsp-get "lsp-mode" (hash key))
(declare-function lsp-notify "lsp-mode" (method params))
(declare-function lsp-request "lsp-mode" (method params))
(declare-function lsp--buffer-content "lsp-mode" ())
(declare-function lsp--buffer-uri "lsp-mode" ())
(declare-function lsp--text-document-identifier "lsp-mode" ())
(declare-function lsp--text-document-position-params "lsp-mode" ())
(declare-function lsp--get-buffer-diagnostics "lsp-mode" ())
(declare-function lsp-buffer-language "lsp-mode" ())
(declare-function lsp-register-client "lsp-mode" (client))
(declare-function lsp-stdio-connection "lsp-mode" (command))
(declare-function make-lsp-client "lsp-mode" (&rest args))
(declare-function evil-insert-state "evil-states" ())
(defvar lsp--cur-version)
(defvar chief/lsp-managed-major-modes)
(defvar chief/lsp-manual-install-specs)
(defvar chief/treesit-fallback-language-alist)
(defvar lsp-diagnostics-updated-hook)
(defvar lsp-file-watch-ignored-directories)
(defvar lsp-language-id-configuration)
(defvar lsp-semantic-tokens-enable)
(defvar treesit-language-source-alist)

(defgroup chief/lean nil
  "Lean 4 support for Chief's Emacs configuration."
  :group 'chief)

(defcustom chief/lean-extra-args nil
  "Extra arguments appended to direct `lean' invocations from this module."
  :type '(repeat string)
  :group 'chief/lean)

(defcustom chief/lean-lake-build-target nil
  "Optional target appended to `lake build' by `chief/lean-lake-build'."
  :type '(choice (const :tag "Default package target" nil)
                 string)
  :group 'chief/lean)

(defcustom chief/lean-use-lake-serve t
  "When non-nil, start the Lean LSP server through `lake serve' in Lake projects."
  :type 'boolean
  :group 'chief/lean)

(defcustom chief/lean-confirm-mutating-commands t
  "When non-nil, confirm Lean/Lake commands that can delete, download, or rewrite project state."
  :type 'boolean
  :group 'chief/lean)

(defcustom chief/lean-lake-exe-target nil
  "Default executable target for `chief/lean-lake-exe'."
  :type '(choice (const :tag "Prompt" nil)
                 string)
  :group 'chief/lean)

(defcustom chief/lean-lake-script nil
  "Default Lake script for `chief/lean-lake-script-run'."
  :type '(choice (const :tag "Prompt" nil)
                 string)
  :group 'chief/lean)

(defcustom chief/lean-repl-command nil
  "Command used by `chief/lean-repl-start' for leanprover-community/repl.
When nil, prefer the current Lake project's `REPL' dependency, then look for an
executable named `repl', and otherwise prompt for the binary.  In Lake projects,
standalone REPL binaries are run through `lake env' so imports and native
libraries are found in the current workspace."
  :type '(choice (const :tag "Prompt or find repl on PATH" nil)
                 string
                 (repeat string))
  :group 'chief/lean)

(defcustom chief/lean-show-info-inline t
  "When non-nil, show Lean informational command output inline.
This renders info diagnostics from commands such as `#eval' and `#check' as
small overlays at the end of the source line."
  :type 'boolean
  :group 'chief/lean)

(defcustom chief/lean-repl-auto-build t
  "When non-nil, build the project-local REPL executable before starting it.
This applies when a Lake project has fetched the `REPL' dependency but the
`.lake/packages/REPL/.lake/build/bin/repl' executable is not built yet."
  :type 'boolean
  :group 'chief/lean)

(defface chief/lean-inline-info-face
  '((t :inherit font-lock-doc-face))
  "Face used for LSP inline Lean info messages such as `#eval' results."
  :group 'chief/lean)

(defface chief/lean-repl-result-face
  '((t :inherit font-lock-doc-face))
  "Face used for explicit Lean REPL evaluation result overlays.
These overlays are separate from `chief/lean-inline-info-face', which is only
for Lean LSP informational diagnostics such as automatic `#eval' output."
  :group 'chief/lean)

(defconst chief/lean-keywords
  '("abbrev" "axiom" "builtin_initialize" "by" "calc" "class" "coinductive"
    "def" "deriving" "do" "else" "end" "example" "export" "extends" "for"
    "forall" "fun" "have" "if" "import" "in" "include" "inductive"
    "infix" "infixl" "infixr" "initialize" "instance" "let" "macro"
    "macro_rules" "match" "mutual" "namespace" "notation" "opaque" "open"
    "partial" "postfix" "prefix" "private" "protected" "public" "rec"
    "return" "scoped" "section" "show" "structure" "syntax" "then"
    "theorem" "unsafe" "variable" "variables" "where" "with")
  "Lean keywords used for lightweight pre-LSP highlighting.")

(defconst chief/lean-tactics
  '("admit" "apply" "assumption" "by_cases" "cases" "constructor" "contradiction"
    "exact" "exists" "ext" "first" "funext" "generalize" "have" "intro"
    "intros" "left" "omega" "rfl" "right" "ring" "rw" "simp" "simpa"
    "skip" "solve_by_elim" "sorry" "specialize" "split" "subst" "suffices"
    "symm" "trivial" "unfold")
  "Common Lean tactics for lightweight pre-LSP highlighting.")

(defconst chief/lean-constants
  '("Bool" "False" "Fin" "Float" "IO" "Int" "List" "Nat" "Option" "Prop"
    "Sort" "String" "Subtype" "True" "Type" "UInt8" "UInt16" "UInt32"
    "UInt64" "Unit")
  "Common Lean constants for lightweight pre-LSP highlighting.")

(defconst chief/lean-font-lock-keywords
  `((,(regexp-opt chief/lean-keywords 'symbols) . font-lock-keyword-face)
    (,(regexp-opt chief/lean-tactics 'symbols) . font-lock-builtin-face)
    (,(regexp-opt chief/lean-constants 'symbols) . font-lock-type-face)
    ("^\\s-*\\(#\\(?:check\\|eval!?\\|print\\|reduce\\|synth\\|guard_msgs\\|check_failure\\|where\\|version\\|help\\)\\)\\_>"
     1 font-lock-preprocessor-face)
    (,(rx "@[" (* (not (any "]" "\n"))) "]") . font-lock-preprocessor-face)
    ("^\\s-*\\(?:private\\|protected\\|noncomputable\\|unsafe\\|partial\\|instance\\s-+\\)*\\(?:def\\|theorem\\|lemma\\|example\\|instance\\|class\\|structure\\|inductive\\|abbrev\\)\\s-+\\([[:word:]_'.]+\\)"
     1 font-lock-function-name-face))
  "Font-lock rules for `chief/lean-mode'.")

(defconst chief/lean-imenu-generic-expression
  '( ("Definitions" "^\\s-*\\(?:private\\|protected\\|noncomputable\\|unsafe\\|partial\\|instance\\s-+\\)*\\(?:def\\|abbrev\\|opaque\\)\\s-+\\([[:word:]_'.]+\\)" 1)
     ("Theorems" "^\\s-*\\(?:private\\|protected\\|noncomputable\\|unsafe\\|partial\\)*\\(?:theorem\\|lemma\\|example\\)\\s-+\\([[:word:]_'.]+\\)" 1)
     ("Types" "^\\s-*\\(?:private\\|protected\\)*\\(?:class\\|structure\\|inductive\\|coinductive\\)\\s-+\\([[:word:]_'.]+\\)" 1)
     ("Namespaces" "^\\s-*namespace\\s-+\\([[:word:]_'.]+\\)" 1))
  "Imenu expressions for `chief/lean-mode'.")

(defvar chief/lean-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Matching delimiters.
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    ;; Lean comments: `-- line', `/- block -/', and doc comments `/-!' or `/--'.
    ;; The descriptor is borrowed from lean4-mode; Emacs will not fully model
    ;; Lean's nested block comments, but this is good enough for editing.
    (modify-syntax-entry ?/ ". 14nb" st)
    (modify-syntax-entry ?- ". 123" st)
    (modify-syntax-entry ?\n ">" st)
    ;; Lean identifiers commonly contain apostrophes and underscores.
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?' "w" st)
    st)
  "Syntax table for `chief/lean-mode'.")

(defvar chief/lean-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-b") #'chief/lean-lake-build)
    (define-key map (kbd "C-c C-c") #'chief/lean-lake-build)
    (define-key map (kbd "C-c C-d") #'chief/lean-refresh-file-dependencies)
    (define-key map (kbd "C-c C-e") #'chief/lean-lake-env)
    (define-key map (kbd "C-c C-g") #'chief/lean-show-goal)
    (define-key map (kbd "C-c C-i") #'chief/lean-show-info)
    (define-key map (kbd "C-c C-k") #'chief/lean-insert-unicode-symbol)
    (define-key map (kbd "C-c C-l") #'chief/lean-check-current-file)
    (define-key map (kbd "C-c C-p") #'chief/lean-lake-exe)
    (define-key map (kbd "C-c C-s") #'chief/lean-lake-script-run)
    (define-key map (kbd "C-c C-x") #'chief/lean-run-current-file)
    (define-key map (kbd "C-c C-\\") #'chief/lean-toggle-input-method)
    (define-key map (kbd "C-c C-t") #'chief/lean-lake-test)
    (define-key map (kbd "C-c C-u") #'chief/lean-lake-update)
    (define-key map (kbd "C-c C-v") #'chief/lean-show-versions)
    map)
  "Keymap for `chief/lean-mode'.")

(defvar chief/lean-goal-buffer-name "*Lean Goals*"
  "Buffer name used by `chief/lean-show-goal'.")

(defvar chief/lean-info-buffer-name "*Lean Info*"
  "Buffer name used by `chief/lean-show-info'.")

(defvar chief/lean-repl-buffer-name "*Lean REPL*"
  "Buffer name used by `chief/lean-repl-start'.")

(defvar-local chief/lean--inline-info-overlays nil
  "Overlays displaying Lean LSP informational command output inline.
This is the automatic `#eval'/`#check' diagnostic workflow, not the explicit
REPL-send workflow.")

(defvar chief/lean--inline-info-global-last-update 0.0
  "Last time Lean inline info overlays were globally refreshed.")

(defvar-local chief/lean--inline-info-last-update 0.0
  "Last time this buffer's inline Lean info overlays were refreshed.")

(defvar-local chief/lean--eval-result-overlays nil
  "Overlays displaying results produced by explicit REPL evaluations.
This is the Common Lisp/Clojure-style send-line/send-region/send-buffer
workflow, not the automatic Lean LSP `#eval' diagnostic workflow.")

(defvar-local chief/lean-repl-env nil
  "Latest Lean JSON REPL environment id associated with this source buffer.")

(defvar-local chief/lean-repl--output-accumulator ""
  "Partial JSON output accumulated from the Lean JSON REPL process.")

(defvar-local chief/lean-repl--pending-requests nil
  "Pending source-buffer requests awaiting Lean JSON REPL responses.")

(defconst chief/lean-repl--prompt "lean> "
  "Pseudo-prompt inserted into Lean JSON REPL buffers.")

(defconst chief/lean-repl--usage-banner
  "\nType Lean commands directly, e.g. #eval 1 + 1, then press RET.\nRaw JSON objects are also accepted.\n\n"
  "Usage text inserted into Lean JSON REPL comint buffers.")

(defun chief/lean--face-property-contains-p (face property)
  "Return non-nil when FACE appears in text face PROPERTY."
  (cond
   ((eq face property) t)
   ((and (listp property) (memq face property)) t)
   (t nil)))

(defun chief/lean--overlay-after-string-face-p (overlay face)
  "Return non-nil when OVERLAY's after-string starts with FACE."
  (let ((after-string (overlay-get overlay 'after-string)))
    (and (stringp after-string)
         (> (length after-string) 0)
         (chief/lean--face-property-contains-p
          face
          (get-text-property 0 'face after-string)))))

(defun chief/lean--delete-overlays-matching (predicate)
  "Delete all current-buffer overlays satisfying PREDICATE."
  (dolist (overlay (delete-dups
                    (append (overlays-in (point-min) (point-max))
                            (overlays-at (point-min))
                            (overlays-at (point-max)))))
    (when (and (overlayp overlay)
               (eq (overlay-buffer overlay) (current-buffer))
               (funcall predicate overlay))
      (delete-overlay overlay))))

(defun chief/lean-clear-inline-info-overlays ()
  "Remove Lean LSP directive info overlays from the current buffer.
This also removes legacy stale overlays from earlier formatter versions that did
not tag overlays with `chief/lean-overlay'.  That prevents duplicate displays
such as one flattened single-line result plus one multiline result after a
reload."
  (mapc #'delete-overlay chief/lean--inline-info-overlays)
  (setq chief/lean--inline-info-overlays nil)
  (chief/lean--delete-overlays-matching
   (lambda (overlay)
     (or (eq (overlay-get overlay 'chief/lean-overlay) 'lsp-inline-info)
         ;; Legacy cleanup for overlays created before they were tagged.
         (chief/lean--overlay-after-string-face-p
          overlay 'chief/lean-inline-info-face)))))

(defun chief/lean-clear-eval-result-overlays ()
  "Remove explicit Lean REPL evaluation result overlays from current buffer."
  (mapc #'delete-overlay chief/lean--eval-result-overlays)
  (setq chief/lean--eval-result-overlays nil)
  (chief/lean--delete-overlays-matching
   (lambda (overlay)
     (eq (overlay-get overlay 'chief/lean-overlay) 'repl-result))))

(defun chief/lean-clear-all-inline-overlays ()
  "Remove all inline Lean overlays managed by this module."
  (chief/lean-clear-inline-info-overlays)
  (chief/lean-clear-eval-result-overlays))

(defun chief/lean--object-get (object key)
  "Return OBJECT's KEY from either a plist or hash table."
  (cond
   ((hash-table-p object)
    (or (gethash key object)
        (and (keywordp key)
             (gethash (substring (symbol-name key) 1) object))))
   ((listp object)
    (plist-get object key))))

(defun chief/lean--diagnostic-line (diagnostic)
  "Return DIAGNOSTIC's zero-based start line, or nil."
  (when-let* ((range (chief/lean--object-get diagnostic :range))
              (start (chief/lean--object-get range :start)))
    (chief/lean--object-get start :line)))

(defun chief/lean--diagnostic-message (diagnostic)
  "Return DIAGNOSTIC's message string, or nil."
  (chief/lean--object-get diagnostic :message))

(defun chief/lean--diagnostic-severity (diagnostic)
  "Return DIAGNOSTIC's LSP severity number, or nil."
  (chief/lean--object-get diagnostic :severity))

(defun chief/lean--info-command-line-p ()
  "Return non-nil when point is on a Lean command whose info should be inline."
  (save-excursion
    (back-to-indentation)
    (looking-at-p "#\\(?:eval\\|check\\|reduce\\|print\\|synth\\|version\\)\\_>")))

(defun chief/lean--normalize-overlay-message (message)
  "Return MESSAGE normalized for an overlay without destroying indentation."
  (let ((text (replace-regexp-in-string "\r\n?" "\n" (format "%s" message))))
    ;; Drop surrounding blank lines produced by tools, but preserve indentation
    ;; inside the actual result.  `string-trim' would destroy meaningful leading
    ;; spaces in pretty-printed Lean output.
    (while (string-match "\\`[ \t]*\n" text)
      (setq text (substring text (match-end 0))))
    (while (string-match "\n[ \t]*\\'" text)
      (setq text (replace-match "" nil nil text)))
    text))

(defun chief/lean--format-multiline-overlay (message &optional column)
  "Return MESSAGE formatted as an inline overlay at source COLUMN.
The first result line starts after the arrow on the source line.  If MESSAGE has
embedded newlines, they are kept as real newlines and continuation lines are
aligned under the first result column while preserving Lean's own indentation."
  (let* ((prefix "  ⟹ ")
         (text (chief/lean--normalize-overlay-message message))
         (lines (split-string text "\n"))
         (continuation-prefix
          (make-string (+ (or column 0) (string-width prefix)) ?\s)))
    (concat prefix
            (mapconcat #'identity lines
                       (concat "\n" continuation-prefix)))))

(defun chief/lean--format-directive-inline-info (message &optional column)
  "Format LSP informational directive MESSAGE for inline display.
This is for the automatic Lean/LSP `#eval', `#check', `#print', etc. workflow,
not for the explicit REPL send workflow."
  (chief/lean--format-multiline-overlay message column))

(defun chief/lean-repl--format-inline-result (message &optional column)
  "Format explicit REPL evaluation MESSAGE for inline display.
This is separate from the Lean/LSP directive formatter."
  (chief/lean--format-multiline-overlay message column))

(defun chief/lean-update-inline-info-overlays ()
  "Display Lean info diagnostics from command lines as inline overlays.
Lean reports `#eval' output as an informational diagnostic.  lsp-mode normally
shows that in the echo area, where it can be immediately replaced by signature
help or eldoc text.  These overlays keep the command result visible in the
source buffer."
  (chief/lean-clear-inline-info-overlays)
  (when (and chief/lean-show-info-inline
             (fboundp 'lsp--get-buffer-diagnostics)
             buffer-file-name)
    (dolist (diagnostic (or (lsp--get-buffer-diagnostics) '()))
      (let ((severity (chief/lean--diagnostic-severity diagnostic))
            (message (chief/lean--diagnostic-message diagnostic))
            (line (chief/lean--diagnostic-line diagnostic)))
        (when (and message line
                   ;; LSP DiagnosticSeverity.Information is 3.  Accept nil as
                   ;; a fallback for older/odd diagnostic conversions, but never
                   ;; render errors or warnings as inline info.
                   (not (member severity '(1 2))))
          (save-excursion
            (goto-char (point-min))
            (forward-line line)
            (when (chief/lean--info-command-line-p)
              (let* ((point (line-end-position))
                     (column (save-excursion
                               (goto-char point)
                               (current-column)))
                     (overlay (make-overlay point point (current-buffer) nil t)))
                (overlay-put overlay 'after-string
                             (propertize (chief/lean--format-directive-inline-info
                                          message column)
                                         'face 'chief/lean-inline-info-face))
                ;; This is an intentionally zero-width end-of-line overlay.
                ;; Do NOT set `evaporate': an empty evaporating overlay can be
                ;; deleted immediately, which makes the inline result disappear.
                (overlay-put overlay 'chief/lean-overlay 'lsp-inline-info)
                (overlay-put overlay 'priority 100)
                (push overlay chief/lean--inline-info-overlays)))))))))

(defun chief/lean--maybe-update-inline-info-in-buffer (buffer now)
  "Update inline Lean info overlays in BUFFER using timestamp NOW."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (derived-mode-p 'chief/lean-mode)
                 (> (- now chief/lean--inline-info-last-update) 0.25))
        (setq chief/lean--inline-info-last-update now)
        (chief/lean-update-inline-info-overlays)))))

(defun chief/lean-maybe-update-inline-info-overlays ()
  "Update inline Lean info overlays in all live Lean buffers.
This is intentionally throttled so it can run from global LSP and post-command
hooks.  Updating all Lean buffers is necessary because lsp-mode's diagnostics
hook is not guaranteed to run with the receiving source buffer current."
  (let ((now (float-time)))
    (when (> (- now chief/lean--inline-info-global-last-update) 0.25)
      (setq chief/lean--inline-info-global-last-update now)
      (dolist (buffer (buffer-list))
        (chief/lean--maybe-update-inline-info-in-buffer buffer now)))))

(defun chief/lean--root-dir-p (dir)
  "Return non-nil when DIR contains a Lake package file."
  (or (file-exists-p (expand-file-name "lakefile.lean" dir))
      (file-exists-p (expand-file-name "lakefile.toml" dir))))

(defun chief/lean-nearest-lake-root (&optional start)
  "Return the nearest ancestor Lake root from START."
  (let ((start (or (chief/project-normalize-root start)
                   (chief/project-default-start-directory))))
    (when start
      (chief/project-normalize-root
       (locate-dominating-file start #'chief/lean--root-dir-p)))))

(defun chief/lean-nearest-toolchain-root (&optional start)
  "Return the nearest ancestor containing `lean-toolchain' from START."
  (let ((start (or (chief/project-normalize-root start)
                   (chief/project-default-start-directory))))
    (when start
      (chief/project-normalize-root
       (locate-dominating-file start "lean-toolchain")))))

(defun chief/lean-project-root ()
  "Return the preferred Lean workspace root for the current buffer.
Prefer the nearest Lake package so `lake serve' runs in the package whose
imports and build configuration apply to the file.  A parent `lean-toolchain'
is only a fallback; it must not pull a nested Lake package up to a polyglot
workspace root."
  (chief/project-preferred-root
   (chief/lean-nearest-lake-root)
   (chief/lean-nearest-toolchain-root)
   (chief/project-current-root)
   default-directory))

(defun chief/lean-command-directory ()
  "Return the directory in which user-facing Lean/Lake commands should run."
  (or (chief/lean-nearest-lake-root)
      (chief/lean-project-root)
      default-directory))

(defun chief/lean-current-file ()
  "Return the current buffer file as an absolute path."
  (unless buffer-file-name
    (user-error "This buffer is not visiting a Lean file"))
  (expand-file-name buffer-file-name))

(defun chief/lean-save-current-buffer ()
  "Save the current Lean buffer when it is modified."
  (when (and buffer-file-name (buffer-modified-p))
    (save-buffer)))

(defun chief/lean-toggle-input-method ()
  "Toggle automatic Lean Unicode abbreviation input in the current buffer.
This is not Emacs' generic TeX input method.  It uses the official Lean 4
abbreviation table vendored in `lang-lean-input', so examples such as `\\forall',
`\\and', `\\R', `\\to', and hundreds of others work directly in Lean buffers."
  (interactive)
  (chief/lean-unicode-input-mode 'toggle))

(defalias 'chief/lean-toggle-unicode-input #'chief/lean-toggle-input-method)

(defun chief/lean-executable (name)
  "Return executable NAME, or signal a user-facing error."
  (or (executable-find name)
      (user-error "%s is not available on PATH. Install Lean with elan: https://lean-lang.org/install/manual" name)))

(defun chief/lean-lake-command (&rest args)
  "Return a Lake command list built from ARGS."
  (cons (chief/lean-executable "lake") args))

(defun chief/lean-lean-command (&rest args)
  "Return a Lean command list built from ARGS."
  (append (list (chief/lean-executable "lean")) chief/lean-extra-args args))

(defun chief/lean-shell-command (command)
  "Return shell-safe string for COMMAND list."
  (mapconcat #'shell-quote-argument command " "))

(defun chief/lean-compile (command buffer-name &optional directory)
  "Run COMMAND in DIRECTORY using compilation buffer BUFFER-NAME."
  (let ((default-directory (or directory (chief/lean-command-directory)))
        (compilation-read-command nil))
    (compilation-start
     (chief/lean-shell-command command)
     'compilation-mode
     (lambda (_) buffer-name))))

(defun chief/lean--process-name-from-buffer (buffer-name)
  "Return a readable process name derived from BUFFER-NAME."
  (string-trim buffer-name "\\`\\*+" "\\*+\\'"))

(defun chief/lean-run-interactive (command buffer-name &optional directory)
  "Run COMMAND in DIRECTORY using an interactive comint BUFFER-NAME.
Use this for Lean programs and Lake executable/script targets that may read from
stdin.  Unlike `chief/lean-compile', this preserves a live stdin/stdout process
so user input is sent to the Lean program instead of being interpreted as editor
commands."
  (unless (consp command)
    (user-error "No command to run"))
  (let* ((directory (file-name-as-directory
                     (expand-file-name
                      (or directory (chief/lean-command-directory)))))
         (buffer (get-buffer-create buffer-name))
         (process-name (chief/lean--process-name-from-buffer buffer-name))
         (program (car command))
         (args (cdr command)))
    (when (comint-check-proc buffer)
      (unless (yes-or-no-p (format "Restart running process in %s? " buffer-name))
        (user-error "Process still running in %s" buffer-name))
      (delete-process (get-buffer-process buffer)))
    (with-current-buffer buffer
      (setq default-directory directory)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Working directory: %s\nCommand: %s\n\n"
                        directory
                        (chief/lean-shell-command command)))))
    (let ((default-directory directory)
          ;; Prefer a pty so interactive programs see stdin/stdout like a
          ;; terminal when the platform supports it.
          (process-connection-type t))
      (apply #'make-comint-in-buffer process-name buffer program nil args))
    (with-current-buffer buffer
      (setq-local comint-process-echoes nil)
      (setq-local comint-scroll-to-bottom-on-input t)
      (setq-local comint-scroll-show-maximum-output t)
      (goto-char (point-max)))
    (pop-to-buffer buffer)
    (when (fboundp 'evil-insert-state)
      (evil-insert-state))
    buffer))

(defun chief/lean--string-present-p (value)
  "Return non-nil when VALUE is a non-empty string after trimming."
  (and (stringp value)
       (not (string-empty-p (string-trim value)))))

(defun chief/lean-read-shell-args (prompt &optional initial-input)
  "Read shell-like arguments with PROMPT and return a list of arguments."
  (let ((input (read-string prompt initial-input)))
    (when (chief/lean--string-present-p input)
      (split-string-shell-command input))))

(defun chief/lean-read-nonempty-string (prompt &optional initial-input)
  "Read a non-empty string with PROMPT, defaulting to INITIAL-INPUT."
  (let ((input (read-string prompt initial-input)))
    (unless (chief/lean--string-present-p input)
      (user-error "No value entered"))
    (string-trim input)))

(defun chief/lean-lake-root-required ()
  "Return the current Lake root or signal a user-facing error."
  (or (chief/lean-nearest-lake-root)
      (user-error "No lakefile.lean or lakefile.toml found above this buffer")))

(defun chief/lean-confirm-command (description command)
  "Ask before running COMMAND when `chief/lean-confirm-mutating-commands' is non-nil.
DESCRIPTION should explain the project state that may change."
  (when (and chief/lean-confirm-mutating-commands
             (not (yes-or-no-p
                   (format "%s\n\nCommand:\n  %s\n\nProceed? "
                           description
                           (chief/lean-shell-command command)))))
    (user-error "Aborted")))

(defun chief/lean--lake-build-target-args (&optional targets)
  "Return Lake build TARGETS as an argument list."
  (cond
   ((listp targets) targets)
   ((chief/lean--string-present-p targets)
    (split-string-shell-command targets))
   ((chief/lean--string-present-p chief/lean-lake-build-target)
    (split-string-shell-command chief/lean-lake-build-target))
   (t nil)))

(defun chief/lean--lake-lean-command (file &optional lean-args)
  "Return a `lake lean' command for FILE with optional LEAN-ARGS."
  (append (chief/lean-lake-command "lean" file)
          (when (or chief/lean-extra-args lean-args)
            (append (list "--") chief/lean-extra-args lean-args))))

(defun chief/lean-lake (&optional args)
  "Run an arbitrary `lake' command with shell-like ARGS.
This is an escape hatch for Lake subcommands not wrapped explicitly.  Potentially
state-changing subcommands ask for confirmation."
  (interactive
   (list (chief/lean-read-shell-args "lake args: ")))
  (unless args
    (user-error "No Lake arguments entered"))
  (let ((command (append (chief/lean-lake-command) args)))
    (when (member (car args) '("update" "clean" "cache" "new" "init" "pack" "unpack" "upload" "translate-config"))
      (chief/lean-confirm-command
       "This Lake command may create, delete, download, upload, or rewrite project state."
       command))
    (chief/lean-compile command "*lake*")))

(defun chief/lean-lean (&optional args)
  "Run an arbitrary `lean' command with shell-like ARGS."
  (interactive
   (list (chief/lean-read-shell-args "lean args: ")))
  (unless args
    (user-error "No Lean arguments entered"))
  (chief/lean-compile
   (apply #'chief/lean-lean-command args)
   "*lean*"))

(defun chief/lean-leanc (&optional args)
  "Run Lean's bundled C compiler `leanc' with shell-like ARGS."
  (interactive
   (list (chief/lean-read-shell-args "leanc args: ")))
  (unless args
    (user-error "No leanc arguments entered"))
  (chief/lean-compile
   (cons (chief/lean-executable "leanc") args)
   "*leanc*"))

(defun chief/lean-leanmake (&optional args)
  "Run Lean's bundled `leanmake' with shell-like ARGS."
  (interactive
   (list (chief/lean-read-shell-args "leanmake args: ")))
  (unless args
    (user-error "No leanmake arguments entered"))
  (chief/lean-compile
   (cons (chief/lean-executable "leanmake") args)
   "*leanmake*"))

(defun chief/lean-elan (&optional args)
  "Run an arbitrary `elan' command with shell-like ARGS.
Because elan can install, remove, link, or override toolchains, this command
always asks for confirmation before starting."
  (interactive
   (list (chief/lean-read-shell-args "elan args: ")))
  (unless args
    (user-error "No Elan arguments entered"))
  (let ((command (cons (chief/lean-executable "elan") args)))
    (chief/lean-confirm-command
     "This elan command may inspect or change Lean toolchain state."
     command)
    (chief/lean-compile command "*elan*")))

(defun chief/lean-lake-build (&optional targets)
  "Run `lake build' for the current Lean project.
With prefix argument, prompt for one or more TARGETS."
  (interactive
   (list (when current-prefix-arg
           (chief/lean-read-shell-args "Lake build targets: " chief/lean-lake-build-target))))
  (chief/lean-lake-root-required)
  (chief/lean-compile
   (append (chief/lean-lake-command "build")
           (chief/lean--lake-build-target-args targets))
   "*lake build*"))

(defun chief/lean-lake-query (&optional targets)
  "Run `lake query' for TARGETS and print their build results."
  (interactive
   (list (chief/lean-read-shell-args "Lake query targets: ")))
  (chief/lean-lake-root-required)
  (chief/lean-compile
   (append (chief/lean-lake-command "query") targets)
   "*lake query*"))

(defun chief/lean-lake-update (&optional packages)
  "Run `lake update' for the current Lean project.
With prefix argument, prompt for specific PACKAGES to update."
  (interactive
   (list (when current-prefix-arg
           (chief/lean-read-shell-args "Lake update packages: "))))
  (chief/lean-lake-root-required)
  (let ((command (append (chief/lean-lake-command "update") packages)))
    (chief/lean-confirm-command
     "`lake update' may fetch dependencies and rewrite lake-manifest.json or lean-toolchain."
     command)
    (chief/lean-compile command "*lake update*")))

(defun chief/lean-lake-test (&optional args)
  "Run `lake test' for the current Lean project.
With prefix argument, prompt for ARGS passed to the configured test driver."
  (interactive
   (list (when current-prefix-arg
           (chief/lean-read-shell-args "Lake test driver args: "))))
  (chief/lean-lake-root-required)
  (chief/lean-compile
   (append (chief/lean-lake-command "test")
           (when args (cons "--" args)))
   "*lake test*"))

(defun chief/lean-lake-lint (&optional args)
  "Run `lake lint' for the current Lean project.
ARGS are passed directly after `lake lint', so they may include builtin-lint
options, module names, or `--' followed by lint-driver arguments."
  (interactive
   (list (chief/lean-read-shell-args "Lake lint args: ")))
  (chief/lean-lake-root-required)
  (chief/lean-compile
   (append (chief/lean-lake-command "lint") args)
   "*lake lint*"))

(defun chief/lean-lake-clean ()
  "Run `lake clean' for the current Lean project."
  (interactive)
  (chief/lean-lake-root-required)
  (let ((command (chief/lean-lake-command "clean")))
    (chief/lean-confirm-command
     "`lake clean' removes Lake build outputs for the workspace."
     command)
    (chief/lean-compile command "*lake clean*")))

(defun chief/lean-lake-check-build ()
  "Run `lake check-build' for the current Lean project."
  (interactive)
  (chief/lean-lake-root-required)
  (chief/lean-compile (chief/lean-lake-command "check-build") "*lake check-build*"))

(defun chief/lean-lake-check-test ()
  "Run `lake check-test' for the current Lean project."
  (interactive)
  (chief/lean-lake-root-required)
  (chief/lean-compile (chief/lean-lake-command "check-test") "*lake check-test*"))

(defun chief/lean-lake-check-lint ()
  "Run `lake check-lint' for the current Lean project."
  (interactive)
  (chief/lean-lake-root-required)
  (chief/lean-compile (chief/lean-lake-command "check-lint") "*lake check-lint*"))

(defun chief/lean-lake-env (&optional command-args)
  "Print Lake's environment, or run COMMAND-ARGS in it with prefix argument."
  (interactive
   (list (when current-prefix-arg
           (chief/lean-read-shell-args "Command to run in Lake env: "))))
  (chief/lean-lake-root-required)
  (chief/lean-compile
   (append (chief/lean-lake-command "env") command-args)
   "*lake env*"))

(defun chief/lean-lake-lean-current-file (&optional lean-args)
  "Run `lake lean' on the current file.
With prefix argument, prompt for extra LEAN-ARGS passed after `--'."
  (interactive
   (list (when current-prefix-arg
           (chief/lean-read-shell-args "Extra lean args: "))))
  (chief/lean-save-current-buffer)
  (let* ((file (chief/lean-current-file))
         (lake-root (chief/lean-lake-root-required)))
    (chief/lean-compile
     (chief/lean--lake-lean-command file lean-args)
     "*lake lean*"
     lake-root)))

(defun chief/lean-lake-exe (&optional exe-target args)
  "Build and run Lake executable EXE-TARGET with ARGS interactively.
The process runs in a comint buffer so stdin/stdout programs work correctly."
  (interactive
   (let ((target (chief/lean-read-nonempty-string
                  "Lake executable target: " chief/lean-lake-exe-target)))
     (list target
           (chief/lean-read-shell-args "Program args: "))))
  (let ((lake-root (chief/lean-lake-root-required)))
    (chief/lean-run-interactive
     (append (chief/lean-lake-command "exe" exe-target) args)
     (format "*lake exe %s*" exe-target)
     lake-root)))

(defun chief/lean-lake-script-list ()
  "Run `lake script list' for the current Lean project."
  (interactive)
  (chief/lean-lake-root-required)
  (chief/lean-compile (chief/lean-lake-command "script" "list") "*lake scripts*"))

(defun chief/lean-lake-script-run (&optional script args)
  "Run Lake SCRIPT with ARGS using `lake script run' interactively.
The process runs in a comint buffer so scripts that read stdin work correctly."
  (interactive
   (let ((script (chief/lean-read-nonempty-string
                  "Lake script: " chief/lean-lake-script)))
     (list script
           (chief/lean-read-shell-args "Script args: "))))
  (let ((lake-root (chief/lean-lake-root-required)))
    (chief/lean-run-interactive
     (append (chief/lean-lake-command "script" "run" script) args)
     (format "*lake script %s*" script)
     lake-root)))

(defun chief/lean-lake-script-doc (&optional script)
  "Show Lake documentation for SCRIPT using `lake script doc'."
  (interactive
   (list (chief/lean-read-nonempty-string
          "Lake script: " chief/lean-lake-script)))
  (chief/lean-lake-root-required)
  (chief/lean-compile
   (chief/lean-lake-command "script" "doc" script)
   (format "*lake script doc %s*" script)))

(defun chief/lean-lake-shake (&optional args)
  "Run `lake shake' to inspect or minimize imports.
ARGS are passed directly after `lake shake'.  Commands containing `--fix' are
confirmed because they rewrite source files."
  (interactive
   (list (chief/lean-read-shell-args "Lake shake args: ")))
  (chief/lean-lake-root-required)
  (let ((command (append (chief/lean-lake-command "shake") args)))
    (when (member "--fix" args)
      (chief/lean-confirm-command
       "`lake shake --fix' rewrites import lists in Lean source files."
       command))
    (chief/lean-compile command "*lake shake*")))

(defun chief/lean-lake-cache-get (&optional mathlib-style)
  "Run Lake cache retrieval for the current project.
By default, run the official `lake cache get'.  With prefix argument
MATHLIB-STYLE, run the older Mathlib-style `lake exe cache get'."
  (interactive "P")
  (chief/lean-lake-root-required)
  (let ((command (if mathlib-style
                     (chief/lean-lake-command "exe" "cache" "get")
                   (chief/lean-lake-command "cache" "get"))))
    (chief/lean-confirm-command
     "Lake cache retrieval may download build artifacts into Lake-managed storage."
     command)
    (chief/lean-compile command "*lake cache get*")))

(defun chief/lean-lake-mathlib-cache-get ()
  "Run `lake exe cache get' for Mathlib-style projects."
  (interactive)
  (chief/lean-lake-cache-get t))

(defun chief/lean-check-current-file ()
  "Run Lean on the current file.
In Lake projects, use `lake lean FILE' so imports are built and Lake's Lean
arguments/environment are respected.  Outside Lake projects, run `lean FILE'."
  (interactive)
  (chief/lean-save-current-buffer)
  (let* ((file (chief/lean-current-file))
         (lake-root (chief/lean-nearest-lake-root))
         (command (if lake-root
                      (chief/lean--lake-lean-command file)
                    (chief/lean-lean-command file))))
    (chief/lean-compile command "*lean check*" (or lake-root default-directory))))

(defun chief/lean-run-current-file (&optional args)
  "Run the current Lean file as an interactive program with `lean --run'.
In Lake projects, run through `lake env' so dependencies and native libraries are
available.  With prefix argument, prompt for program ARGS.  The process runs in a
comint buffer so stdin/stdout programs work correctly."
  (interactive
   (list (when current-prefix-arg
           (chief/lean-read-shell-args "Program args: "))))
  (chief/lean-save-current-buffer)
  (let* ((file (chief/lean-current-file))
         (lake-root (chief/lean-nearest-lake-root))
         (command (if lake-root
                      (append (chief/lean-lake-command "env" "lean")
                              chief/lean-extra-args
                              (list "--run" file)
                              args)
                    (append (chief/lean-lean-command "--run" file)
                            args))))
    (chief/lean-run-interactive command "*lean run*" (or lake-root default-directory))))

(defun chief/lean-compile-current-file-to-c (&optional output-file)
  "Compile the current Lean file to C source OUTPUT-FILE with `lean -c'."
  (interactive
   (let* ((file (chief/lean-current-file))
          (default (concat (file-name-sans-extension file) ".c")))
     (list (read-file-name "Output C file: " nil default nil
                           (file-name-nondirectory default)))))
  (chief/lean-save-current-buffer)
  (let* ((file (chief/lean-current-file))
         (output-file (expand-file-name output-file))
         (lake-root (chief/lean-nearest-lake-root))
         (command (if lake-root
                      (append (chief/lean-lake-command "env" "lean")
                              chief/lean-extra-args
                              (list "-c" output-file file))
                    (chief/lean-lean-command "-c" output-file file))))
    (chief/lean-confirm-command
     (format "This writes generated C output to %s." output-file)
     command)
    (chief/lean-compile command "*lean -c*" (or lake-root default-directory))))

(defun chief/lean-leanchecker (&optional olean-file)
  "Run `leanchecker' on OLEAN-FILE."
  (interactive
   (list (read-file-name "OLean file: " nil nil t nil
                         (lambda (name)
                           (or (file-directory-p name)
                               (string-suffix-p ".olean" name))))))
  (chief/lean-compile
   (list (chief/lean-executable "leanchecker") (expand-file-name olean-file))
   "*leanchecker*"))

(defun chief/lean-read-toolchain (&optional root)
  "Return the Lean toolchain string from ROOT or an ancestor, if any."
  (when-let* ((root (or root (chief/lean-nearest-toolchain-root)))
              (file (expand-file-name "lean-toolchain" root))
              ((file-readable-p file)))
    (string-trim (with-temp-buffer
                   (insert-file-contents file)
                   (buffer-string)))))

(defun chief/lean-show-toolchain ()
  "Show the active Lean toolchain according to elan, when available."
  (interactive)
  (let ((toolchain (chief/lean-read-toolchain))
        (elan (executable-find "elan")))
    (cond
     (elan
      (let ((default-directory (or (chief/lean-project-root) default-directory)))
        (with-current-buffer (get-buffer-create "*elan show*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Project root: %s\n" default-directory))
            (when toolchain
              (insert (format "lean-toolchain: %s\n\n" toolchain)))
            (call-process elan nil t nil "show")
            (goto-char (point-min))
            (special-mode))
          (display-buffer (current-buffer)))))
     (toolchain
      (message "lean-toolchain: %s (elan is not on PATH)" toolchain))
     (t
      (message "No lean-toolchain found and elan is not on PATH")))))

(defalias 'chief/lean-elan-show #'chief/lean-show-toolchain)

(defun chief/lean--insert-command-output (command)
  "Insert COMMAND and its output into the current buffer."
  (insert (format "$ %s\n" (chief/lean-shell-command command)))
  (condition-case err
      (let ((exit (apply #'process-file (car command) nil t nil (cdr command))))
        (unless (eq exit 0)
          (insert (format "\n[exit %s]\n" exit))))
    (error
     (insert (format "[error] %s\n" (error-message-string err)))))
  (insert "\n"))

(defun chief/lean-display-command-output (commands buffer-name &optional directory)
  "Run COMMANDS synchronously and show their output in BUFFER-NAME."
  (let ((default-directory (or directory (chief/lean-command-directory))))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Directory: %s\n\n" default-directory))
        (dolist (command commands)
          (chief/lean--insert-command-output command))
        (goto-char (point-min))
        (special-mode))
      (display-buffer (current-buffer)))))

(defun chief/lean-show-versions ()
  "Show Lean, Lake, Elan, and toolchain information in one buffer."
  (interactive)
  (let* ((toolchain (chief/lean-read-toolchain))
         (commands (delq nil
                         (list
                          (when (executable-find "lean")
                            (list (chief/lean-executable "lean") "--version"))
                          (when (executable-find "lean")
                            (list (chief/lean-executable "lean") "--githash"))
                          (when (executable-find "lean")
                            (list (chief/lean-executable "lean") "--print-prefix"))
                          (when (executable-find "lake")
                            (list (chief/lean-executable "lake") "--version"))
                          (when (executable-find "elan")
                            (list (chief/lean-executable "elan") "show"))
                          (when (executable-find "elan")
                            (list (chief/lean-executable "elan") "which" "lean"))
                          (when (executable-find "elan")
                            (list (chief/lean-executable "elan") "which" "lake"))))))
    (with-current-buffer (get-buffer-create "*Lean Tooling*")
      (let ((inhibit-read-only t)
            (default-directory (or (chief/lean-project-root) default-directory)))
        (erase-buffer)
        (insert (format "Project root: %s\n" default-directory))
        (insert (format "lean-toolchain: %s\n\n" (or toolchain "<none found>")))
        (if commands
            (dolist (command commands)
              (chief/lean--insert-command-output command))
          (insert "No Lean tooling found on PATH.\n"))
        (goto-char (point-min))
        (special-mode))
      (display-buffer (current-buffer)))))

(defun chief/lean-elan-which (&optional tool)
  "Show the toolchain-specific path for TOOL using `elan which'."
  (interactive
   (list (chief/lean-read-nonempty-string "elan which tool: " "lean")))
  (chief/lean-display-command-output
   (list (list (chief/lean-executable "elan") "which" tool))
   "*elan which*"))

(defun chief/lean-lsp-server-command ()
  "Return the Lean LSP server command for the current buffer."
  (let ((lake (executable-find "lake"))
        (lean (executable-find "lean")))
    (or (when (and chief/lean-use-lake-serve
                   (chief/lean-nearest-lake-root)
                   lake)
          (list lake "serve"))
        (when lean
          (list lean "--server"))
        ;; Let lsp-mode report a missing executable and trigger our install
        ;; guidance when Lean is not installed yet.
        (if (chief/lean-nearest-lake-root)
            '("lake" "serve")
          '("lean" "--server")))))

(defun chief/lean-refresh-file-dependencies ()
  "Ask the Lean LSP server to reload the current file and its dependencies."
  (interactive)
  (unless (bound-and-true-p lsp-managed-mode)
    (unless (and (fboundp 'chief/lsp-ensure-active-for-navigation)
                 (chief/lsp-ensure-active-for-navigation))
      (user-error "No active Lean LSP workspace")))
  (lsp-notify
   "textDocument/didClose"
   `(:textDocument ,(lsp--text-document-identifier)))
  (lsp-notify
   "textDocument/didOpen"
   (list :textDocument
         (list :uri (lsp--buffer-uri)
               :languageId (lsp-buffer-language)
               :version lsp--cur-version
               :text (lsp--buffer-content)))))

(define-derived-mode chief/lean-goal-buffer-mode special-mode "Lean-Goals"
  "Mode for buffers showing Lean goal state.")

(define-derived-mode chief/lean-info-buffer-mode special-mode "Lean-Info"
  "Mode for buffers showing Lean goals, term goals, and diagnostics.")

(defun chief/lean--response-field (response field)
  "Return FIELD from Lean LSP RESPONSE."
  (or
   (when (hash-table-p response)
     (or (gethash field response)
         (gethash (intern field) response)
         (gethash (intern (concat ":" field)) response)))
   (when (listp response)
     (or (plist-get response (intern (concat ":" field)))
         (plist-get response (intern field))))
   (when (fboundp 'lsp-get)
     (or (ignore-errors (lsp-get response field))
         (ignore-errors (lsp-get response (intern field)))
         (ignore-errors (lsp-get response (intern (concat ":" field))))))))

(defun chief/lean--goal-value-string (value)
  "Return Lean goal VALUE as a display string."
  (cond
   ((null value) nil)
   ((stringp value)
    (unless (string-empty-p value) value))
   ((vectorp value)
    (chief/lean--goal-value-string (append value nil)))
   ((listp value)
    (let ((text (string-join (mapcar (lambda (item) (format "%s" item)) value) "\n\n")))
      (unless (string-empty-p text) text)))
   (t
    (format "%s" value))))

(defun chief/lean-goal-text ()
  "Return Lean goal text at point using Lean-specific LSP extensions."
  (unless (bound-and-true-p lsp-managed-mode)
    (unless (and (fboundp 'chief/lsp-ensure-active-for-navigation)
                 (chief/lsp-ensure-active-for-navigation))
      (user-error "No active Lean LSP workspace")))
  (let* ((params (lsp--text-document-position-params))
         (plain-goal (ignore-errors (lsp-request "$/lean/plainGoal" params)))
         (term-goal (ignore-errors (lsp-request "$/lean/plainTermGoal" params)))
         (goals (chief/lean--goal-value-string
                 (chief/lean--response-field plain-goal "goals")))
         (term (chief/lean--goal-value-string
                (chief/lean--response-field term-goal "goal"))))
    (string-join
     (delq nil
           (list goals
                 (and term (concat "\nTerm goal:\n" term))))
     "\n")))

(defun chief/lean-show-goal ()
  "Display the Lean proof/term goal at point."
  (interactive)
  (let ((text (chief/lean-goal-text)))
    (when (or (null text) (string-empty-p text))
      (setq text "No goal at point."))
    (with-current-buffer (get-buffer-create chief/lean-goal-buffer-name)
      (let ((inhibit-read-only t))
        (chief/lean-goal-buffer-mode)
        (erase-buffer)
        (insert text)
        (goto-char (point-min)))
      (display-buffer (current-buffer)))))

(defun chief/lean--diagnostic-severity-name (severity)
  "Return a human-readable name for LSP diagnostic SEVERITY."
  (pcase severity
    (1 "error")
    (2 "warning")
    (3 "information")
    (4 "hint")
    (_ "diagnostic")))

(defun chief/lean--diagnostic-position (diagnostic)
  "Return DIAGNOSTIC position as a cons cell (LINE . CHARACTER), both 1-based."
  (let* ((range (chief/lean--response-field diagnostic "range"))
         (start (chief/lean--response-field range "start"))
         (line (chief/lean--response-field start "line"))
         (character (chief/lean--response-field start "character")))
    (cons (if (numberp line) (1+ line) 1)
          (if (numberp character) (1+ character) 1))))

(defun chief/lean--diagnostic< (a b)
  "Return non-nil when diagnostic A should sort before diagnostic B."
  (let ((pos-a (chief/lean--diagnostic-position a))
        (pos-b (chief/lean--diagnostic-position b)))
    (or (< (car pos-a) (car pos-b))
        (and (= (car pos-a) (car pos-b))
             (< (cdr pos-a) (cdr pos-b))))))

(defun chief/lean--buffer-diagnostics ()
  "Return current buffer diagnostics from lsp-mode, if available."
  (when (and (bound-and-true-p lsp-managed-mode)
             (fboundp 'lsp--get-buffer-diagnostics))
    (let ((diagnostics (lsp--get-buffer-diagnostics)))
      (cond
       ((vectorp diagnostics) (append diagnostics nil))
       ((listp diagnostics) diagnostics)
       (t nil)))))

(defun chief/lean--diagnostics-text ()
  "Return a display string for current Lean diagnostics."
  (let ((diagnostics (sort (copy-sequence (or (chief/lean--buffer-diagnostics) nil))
                           #'chief/lean--diagnostic<)))
    (if diagnostics
        (string-join
         (mapcar
          (lambda (diagnostic)
            (let* ((pos (chief/lean--diagnostic-position diagnostic))
                   (severity (chief/lean--diagnostic-severity-name
                              (chief/lean--response-field diagnostic "severity")))
                   (source (chief/lean--response-field diagnostic "source"))
                   (message (or (chief/lean--response-field diagnostic "message") "")))
              (format "%s:%s: %s%s\n%s"
                      (car pos)
                      (cdr pos)
                      severity
                      (if source (format " [%s]" source) "")
                      message)))
          diagnostics)
         "\n\n")
      "No diagnostics reported for this buffer.")))

(defun chief/lean-info-text ()
  "Return goals, term goal, and diagnostics for the current Lean buffer."
  (let ((goal-text (condition-case err
                       (chief/lean-goal-text)
                     (error (format "Goals unavailable: %s"
                                    (error-message-string err)))))
        (diagnostics-text (chief/lean--diagnostics-text)))
    (string-join
     (list "Goals"
           (if (chief/lean--string-present-p goal-text)
               goal-text
             "No goal at point.")
           "Diagnostics"
           diagnostics-text)
     "\n\n")))

(defun chief/lean-show-info ()
  "Display Lean goals, expected type, and diagnostics for the current buffer."
  (interactive)
  (let ((source-buffer (current-buffer))
        (text (chief/lean-info-text)))
    (with-current-buffer (get-buffer-create chief/lean-info-buffer-name)
      (let ((inhibit-read-only t))
        (chief/lean-info-buffer-mode)
        (erase-buffer)
        (insert (format "Buffer: %s\n\n" (buffer-name source-buffer)))
        (insert text)
        (goto-char (point-min)))
      (display-buffer (current-buffer)))))

(defun chief/lean-repl--json-read (text)
  "Read Lean REPL JSON object from TEXT as an alist, or nil."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol))
    (ignore-errors (json-read-from-string text))))

(defun chief/lean-repl--alist-get (key alist)
  "Return KEY from ALIST, accepting symbol or string keys."
  (or (alist-get key alist)
      (alist-get (intern (format "%s" key)) alist)
      (alist-get (format "%s" key) alist nil nil #'equal)))

(defun chief/lean-repl--format-message (message)
  "Format one Lean REPL MESSAGE alist for display."
  (let ((severity (chief/lean-repl--alist-get 'severity message))
        (data (chief/lean-repl--alist-get 'data message)))
    (if data
        (if (member severity '("info" info))
            (format "%s" data)
          (format "%s: %s" severity data))
      (format "%S" message))))

(defun chief/lean-repl--format-response (response)
  "Format Lean JSON REPL RESPONSE for human-readable comint display."
  (let* ((messages (or (chief/lean-repl--alist-get 'messages response) '()))
         (sorries (or (chief/lean-repl--alist-get 'sorries response) '()))
         (tactics (or (chief/lean-repl--alist-get 'tactics response) '()))
         (goals (chief/lean-repl--alist-get 'goals response))
         (env (chief/lean-repl--alist-get 'env response))
         (proof-state (chief/lean-repl--alist-get 'proofState response))
         (proof-status (chief/lean-repl--alist-get 'proofStatus response))
         (error-message (chief/lean-repl--alist-get 'message response))
         (parts nil))
    (when messages
      (push (mapconcat #'chief/lean-repl--format-message messages "\n") parts))
    (when goals
      (push (format "goals:\n%s" (if (listp goals)
                                      (string-join
                                       (mapcar (lambda (goal) (format "%s" goal)) goals)
                                       "\n\n")
                                    goals))
            parts))
    (when sorries
      (push (format "sorries:\n%s"
                    (mapconcat #'chief/lean-repl--sorry-text sorries "\n\n"))
            parts))
    (when tactics
      (push (format "tactics:\n%s"
                    (mapconcat #'chief/lean-repl--tactic-text tactics "\n\n"))
            parts))
    (when proof-state
      (push (format "proofState: %s" proof-state) parts))
    (when proof-status
      (push (format "proofStatus: %s" proof-status) parts))
    (when error-message
      (push (format "error: %s" error-message) parts))
    (when env
      (push (format "env: %s" env) parts))
    (concat (if parts
                (string-join (nreverse parts) "\n")
              (format "%S" response))
            "\n")))

(defun chief/lean-repl--message-line (message)
  "Return one-based line for REPL MESSAGE, or nil."
  (when-let* ((pos (chief/lean-repl--alist-get 'pos message)))
    (chief/lean-repl--alist-get 'line pos)))

(defun chief/lean-repl--message-data (message)
  "Return display data from REPL MESSAGE."
  (chief/lean-repl--alist-get 'data message))

(defun chief/lean-repl--message-severity (message)
  "Return severity from REPL MESSAGE."
  (chief/lean-repl--alist-get 'severity message))

(defun chief/lean-repl--item-line (item)
  "Return one-based source line for a REPL response ITEM, or nil."
  (when-let* ((pos (chief/lean-repl--alist-get 'pos item)))
    (chief/lean-repl--alist-get 'line pos)))

(defun chief/lean-repl--sorry-text (sorry)
  "Return display text for REPL SORRY information."
  (let ((goal (chief/lean-repl--alist-get 'goal sorry))
        (proof-state (chief/lean-repl--alist-get 'proofState sorry)))
    (string-join
     (delq nil
           (list (and goal (format "goal:%s" goal))
                 (and proof-state (format "proofState: %s" proof-state))))
     "\n")))

(defun chief/lean-repl--tactic-text (tactic)
  "Return display text for REPL TACTIC information."
  (let ((goals (chief/lean-repl--alist-get 'goals tactic))
        (proof-state (chief/lean-repl--alist-get 'proofState tactic)))
    (string-join
     (delq nil
           (list (and goals (format "goals:\n%s" goals))
                 (and proof-state (format "proofState: %s" proof-state))))
     "\n")))

(defun chief/lean-repl--command-has-import-p (command)
  "Return non-nil when COMMAND contains a top-level import."
  (string-match-p "^\\s-*import\\_>" command))

(defun chief/lean-repl--next-env-for-command (command)
  "Return the current source buffer REPL env suitable for COMMAND."
  (unless (chief/lean-repl--command-has-import-p command)
    chief/lean-repl-env))

(defun chief/lean-repl--line-end-point (line-one-based)
  "Return end position of one-based LINE-ONE-BASED in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line (max 0 (1- line-one-based)))
    (line-end-position)))

(defun chief/lean-repl--put-inline-result (line text &optional errorp)
  "Put inline evaluation result TEXT on one-based LINE.
When ERRORP is non-nil, use a warning face."
  (let* ((point (chief/lean-repl--line-end-point line))
         (column (save-excursion
                   (goto-char point)
                   (current-column)))
         (overlay (make-overlay point point (current-buffer) nil t)))
    (overlay-put overlay 'after-string
                 (propertize (chief/lean-repl--format-inline-result text column)
                             'face (if errorp
                                       'font-lock-warning-face
                                     'chief/lean-repl-result-face)))
    (overlay-put overlay 'chief/lean-overlay 'repl-result)
    (overlay-put overlay 'priority 200)
    (push overlay chief/lean--eval-result-overlays)))

(defun chief/lean-repl--render-inline-response (request response)
  "Render explicit REPL RESPONSE inline in the source buffer described by REQUEST.
This function belongs to the source-to-REPL workflow.  It does not read or
modify the separate LSP diagnostic overlays used for automatic `#eval' output."
  (when-let* ((source (plist-get request :source-buffer))
              ((buffer-live-p source)))
    (with-current-buffer source
      (when-let* ((env (chief/lean-repl--alist-get 'env response)))
        (setq chief/lean-repl-env env))
      ;; A new explicit REPL evaluation supersedes previous explicit REPL result
      ;; overlays, like CIDER/sly-style eval result hints.  This deliberately
      ;; leaves LSP `#eval' diagnostic overlays alone.
      (chief/lean-clear-eval-result-overlays)
      (let* ((start-line (or (plist-get request :start-line) 1))
             (end-line (or (plist-get request :end-line) start-line))
             (messages (or (chief/lean-repl--alist-get 'messages response) '()))
             (sorries (or (chief/lean-repl--alist-get 'sorries response) '()))
             (tactics (or (chief/lean-repl--alist-get 'tactics response) '()))
             (goals (chief/lean-repl--alist-get 'goals response))
             (proof-status (chief/lean-repl--alist-get 'proofStatus response))
             (error-message (chief/lean-repl--alist-get 'message response))
             (env (chief/lean-repl--alist-get 'env response))
             (rendered nil))
        (cl-labels
            ((line-for (item)
               (+ start-line (or (chief/lean-repl--item-line item) 1) -1))
             (put (line text &optional errorp)
               (when (chief/lean--string-present-p text)
                 (chief/lean-repl--put-inline-result line text errorp)
                 (setq rendered t))))
          ;; Messages cover `#eval', `#check', errors, warnings, and any other
          ;; Lean command diagnostics produced by the explicit REPL request.
          (dolist (message messages)
            (when-let* ((data (chief/lean-repl--message-data message)))
              (put (line-for message)
                   data
                   (member (chief/lean-repl--message-severity message)
                           '("error" error)))))
          ;; These are not `#eval' results; they are proof/tactic state returned
          ;; by the explicit REPL for arbitrary Lean commands with holes/sorries.
          (dolist (sorry sorries)
            (put (line-for sorry) (chief/lean-repl--sorry-text sorry) t))
          (dolist (tactic tactics)
            (put (line-for tactic) (chief/lean-repl--tactic-text tactic)))
          (when goals
            (put end-line
                 (format "goals:\n%s"
                         (if (listp goals)
                             (string-join
                              (mapcar (lambda (goal) (format "%s" goal)) goals)
                              "\n\n")
                           goals))))
          (when proof-status
            (put end-line (format "proofStatus: %s" proof-status)))
          (when error-message
            (put end-line error-message t))
          ;; A successful definition/theorem/open/import/etc. may produce no
          ;; printable message.  Still show that the arbitrary Lean code was
          ;; accepted into the REPL environment.
          (unless rendered
            (put end-line
                 (if env
                     (format "✓ env: %s" env)
                   "✓"))))))))

(defun chief/lean-repl--handle-response (response)
  "Handle parsed Lean JSON REPL RESPONSE in the current REPL buffer."
  (when-let* ((env (chief/lean-repl--alist-get 'env response)))
    ;; Keep direct typing in *Lean REPL* stateful too: `def f := 1' followed by
    ;; `#check f' should behave like a normal Lisp/Clojure REPL workflow.
    (setq chief/lean-repl-env env))
  (when chief/lean-repl--pending-requests
    (let ((request (pop chief/lean-repl--pending-requests)))
      (chief/lean-repl--render-inline-response request response))))

(defun chief/lean-repl--queue-request (request)
  "Queue source REQUEST in the current REPL buffer."
  (when request
    (setq chief/lean-repl--pending-requests
          (append chief/lean-repl--pending-requests (list request)))))

(defun chief/lean-repl--echo-request (request)
  "Echo source REQUEST in the current REPL buffer without sending it as input."
  (when-let* ((command (plist-get request :command)))
    (let ((process (get-buffer-process (current-buffer))))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (save-excursion
                  (beginning-of-line)
                  (looking-at-p (regexp-quote chief/lean-repl--prompt)))
          (insert chief/lean-repl--prompt))
        (insert command)
        (unless (string-suffix-p "\n" command)
          (insert "\n"))
        (when process
          (set-marker (process-mark process) (point-max)))))))

(defun chief/lean-repl-preoutput-filter (string)
  "Format blank-line separated JSON REPL output STRING for display."
  (setq chief/lean-repl--output-accumulator
        (concat chief/lean-repl--output-accumulator string))
  (let ((output "")
        (sep-regexp "\n[[:space:]\r]*\n"))
    (while (string-match sep-regexp chief/lean-repl--output-accumulator)
      (let* ((chunk (substring chief/lean-repl--output-accumulator 0 (match-beginning 0)))
             (rest (substring chief/lean-repl--output-accumulator (match-end 0)))
             (trimmed (string-trim chunk))
             (response (and (string-prefix-p "{" trimmed)
                            (chief/lean-repl--json-read trimmed))))
        (when response
          (chief/lean-repl--handle-response response))
        (setq output
              (concat output
                      (if response
                          (chief/lean-repl--format-response response)
                        (concat chunk "\n\n"))
                      chief/lean-repl--prompt))
        (setq chief/lean-repl--output-accumulator rest)))
    output))

(defun chief/lean-repl-input-sender (process input)
  "Send user INPUT to Lean JSON REPL PROCESS.
Plain Lean input is wrapped as `{\"cmd\": ...}` and separated by a blank line.
Raw JSON objects are sent unchanged for advanced REPL operations.  Plain input
uses the latest REPL environment when possible, so manual REPL typing is
stateful: a `def' entered at the prompt is visible to later prompt input."
  (let ((trimmed (string-trim input)))
    (unless (string-empty-p trimmed)
      (let ((payload
             (if (string-prefix-p "{" trimmed)
                 trimmed
               (let ((object `((cmd . ,input))))
                 (when-let* ((env (chief/lean-repl--next-env-for-command input)))
                   (setq object (append object `((env . ,env)))))
                 (json-encode object)))))
        (comint-send-string process (concat payload "\n\n"))))))

(define-derived-mode chief/lean-repl-mode comint-mode "Lean-JSON-REPL"
  "Comint mode for leanprover-community/repl.
Type ordinary Lean commands such as `#eval 1 + 1' at the prompt; this mode wraps
them in the JSON protocol expected by `leanprover-community/repl'.  To send raw
JSON, type a JSON object starting with `{'."
  (setq-local comint-input-sender #'chief/lean-repl-input-sender)
  (setq-local comint-process-echoes nil)
  (setq-local comint-scroll-to-bottom-on-input t)
  (setq-local comint-scroll-to-bottom-on-output t)
  (setq-local comint-prompt-read-only nil)
  (add-hook 'comint-preoutput-filter-functions
            #'chief/lean-repl-preoutput-filter nil t))

(defun chief/lean-repl-configure-buffer (buffer)
  "Configure BUFFER as a usable Lean JSON REPL comint buffer.
This is run even for already-live REPL buffers, so reloading this module repairs
old buffers that were started with the raw `comint-simple-send' sender."
  (with-current-buffer buffer
    (unless (derived-mode-p 'chief/lean-repl-mode)
      (chief/lean-repl-mode))
    (setq-local comint-input-sender #'chief/lean-repl-input-sender)
    (unless (member #'chief/lean-repl-preoutput-filter
                    comint-preoutput-filter-functions)
      (add-hook 'comint-preoutput-filter-functions
                #'chief/lean-repl-preoutput-filter nil t))
    (save-excursion
      (goto-char (point-min))
      (unless (search-forward "Type Lean commands directly" nil t)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert chief/lean-repl--usage-banner)))
      (goto-char (point-max))
      (unless (save-excursion
                (beginning-of-line)
                (looking-at-p (regexp-quote chief/lean-repl--prompt)))
        (let ((inhibit-read-only t))
          (insert chief/lean-repl--prompt))))
    ;; The banner/prompt is for humans, not process input.  Move comint's process mark
    ;; after it so RET sends only newly typed text.
    (when-let* ((process (get-buffer-process buffer)))
      (set-marker (process-mark process) (point-max)))))

(defun chief/lean--configured-repl-command-list ()
  "Return explicitly configured `chief/lean-repl-command', if any."
  (cond
   ((and (listp chief/lean-repl-command) chief/lean-repl-command)
    chief/lean-repl-command)
   ((chief/lean--string-present-p chief/lean-repl-command)
    (split-string-shell-command chief/lean-repl-command))))

(defun chief/lean--lake-repl-executable (lake-root)
  "Return the built project-local REPL executable under LAKE-ROOT, if present."
  (when lake-root
    (let ((candidate (expand-file-name
                      ".lake/packages/REPL/.lake/build/bin/repl"
                      lake-root)))
      (when (file-executable-p candidate)
        candidate))))

(defun chief/lean--lake-repl-dependency-p (lake-root)
  "Return non-nil when LAKE-ROOT has the `REPL' Lake dependency fetched."
  (and lake-root
       (file-directory-p (expand-file-name ".lake/packages/REPL" lake-root))))

(defun chief/lean--ensure-lake-repl-executable (lake-root)
  "Return LAKE-ROOT's project-local REPL executable, building it if needed."
  (or (chief/lean--lake-repl-executable lake-root)
      (when (and lake-root
                 chief/lean-repl-auto-build
                 (chief/lean--lake-repl-dependency-p lake-root))
        (let* ((buffer (get-buffer-create "*lake build @REPL/repl*"))
               (default-directory lake-root)
               (exit-code nil))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (format "Working directory: %s\nCommand: %s\n\n"
                              default-directory
                              (chief/lean-shell-command
                               (chief/lean-lake-command "build" "@REPL/repl"))))))
          (message "Building project-local Lean REPL executable...")
          (setq exit-code
                (apply #'process-file
                       (car (chief/lean-lake-command))
                       nil buffer t
                       (cdr (chief/lean-lake-command "build" "@REPL/repl"))))
          (if (zerop exit-code)
              (or (chief/lean--lake-repl-executable lake-root)
                  (progn
                    (display-buffer buffer)
                    (user-error "Lake build finished but REPL executable was not found")))
            (display-buffer buffer)
            (user-error "Failed to build project-local Lean REPL; see %s"
                        (buffer-name buffer)))))))

(defun chief/lean--repl-start-command (&optional lake-root)
  "Return the final command list used to start the optional Lean JSON REPL.
When possible, prefer the current Lake project's `REPL' dependency.  Project
REPL dependencies are started via their executable under `lake env'; `lake exe'
is deliberately not used as the persistent REPL process because Lake's own
stdout/build output is not JSON REPL traffic."
  (let ((configured (chief/lean--configured-repl-command-list))
        (project-repl (chief/lean--ensure-lake-repl-executable lake-root)))
    (cond
     (configured
      (if lake-root
          (append (chief/lean-lake-command "env") configured)
        configured))
     (project-repl
      (append (chief/lean-lake-command "env") (list project-repl)))
     ((executable-find "repl")
      (let ((repl (list (executable-find "repl"))))
        (if lake-root
            (append (chief/lean-lake-command "env") repl)
          repl)))
     (t
      (let ((repl (list (read-file-name "Lean repl executable: " nil nil t))))
        (if lake-root
            (append (chief/lean-lake-command "env") repl)
          repl))))))

(defun chief/lean-repl-start ()
  "Start an optional leanprover-community/repl JSON REPL.
This does not install the REPL.  If the current Lake project has a fetched `REPL'
dependency, use that project dependency automatically.  Otherwise configure
`chief/lean-repl-command' or provide an already-built `repl' executable when
prompted."
  (interactive)
  (let* ((lake-root (chief/lean-nearest-lake-root))
         (command (chief/lean--repl-start-command lake-root))
         (default-directory (or lake-root (chief/lean-project-root) default-directory))
         (buffer (get-buffer-create chief/lean-repl-buffer-name)))
    ;; Buffers created by the old implementation used raw `comint-simple-send',
    ;; which could leave the JSON REPL waiting on an unterminated/non-JSON input
    ;; block.  Restart those stale processes instead of trying to salvage them.
    (when (and (comint-check-proc buffer)
               (with-current-buffer buffer
                 (not (eq comint-input-sender #'chief/lean-repl-input-sender))))
      (delete-process (get-buffer-process buffer)))
    (unless (comint-check-proc buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Working directory: %s\nCommand: %s\n\n"
                          default-directory
                          (chief/lean-shell-command command)))))
      (apply #'make-comint-in-buffer "Lean REPL" buffer (car command) nil (cdr command)))
    (chief/lean-repl-configure-buffer buffer)
    (pop-to-buffer buffer)))

(defun chief/lean-repl-restart ()
  "Restart the optional Lean JSON REPL for the current project."
  (interactive)
  (when-let* ((buffer (get-buffer chief/lean-repl-buffer-name)))
    (when-let* ((process (get-buffer-process buffer)))
      (delete-process process))
    (kill-buffer buffer))
  (setq chief/lean-repl-env nil)
  (chief/lean-repl-start))

(defun chief/lean--repl-process ()
  "Return a live Lean REPL process, starting one if needed."
  (unless (comint-check-proc chief/lean-repl-buffer-name)
    (save-window-excursion
      (chief/lean-repl-start)))
  (or (get-buffer-process chief/lean-repl-buffer-name)
      (user-error "Lean REPL process is not running")))

(defun chief/lean-repl-send-json (object &optional request)
  "Send JSON OBJECT to the Lean REPL, followed by a blank line.
REQUEST, when non-nil, describes a source-buffer evaluation whose result should
be rendered inline when the REPL responds."
  (let ((process (chief/lean--repl-process)))
    (with-current-buffer (process-buffer process)
      (chief/lean-repl--queue-request request)
      (chief/lean-repl--echo-request request))
    (comint-send-string process (concat (json-encode object) "\n\n"))
    (display-buffer chief/lean-repl-buffer-name)))

(defun chief/lean-repl--source-request (beg end &optional command)
  "Return a pending REPL request for source range BEG..END."
  (list :source-buffer (current-buffer)
        :start beg
        :end end
        :start-line (line-number-at-pos beg)
        :end-line (line-number-at-pos end)
        :command command))

(defun chief/lean-repl-send-command (&optional command env request)
  "Send a Lean COMMAND-mode request to the optional JSON REPL.
With prefix argument, prompt for ENV, the environment id returned by a previous
REPL response.  REQUEST describes an optional source-buffer inline result target."
  (interactive
   (let ((cmd (read-string "Lean REPL command: ")))
     (list cmd
           (when current-prefix-arg
             (read-number "Environment id: "))
           (chief/lean-repl--source-request (point) (point) cmd))))
  (unless (chief/lean--string-present-p command)
    (user-error "No Lean command entered"))
  (let ((payload `((cmd . ,command)))
        (env (or env (chief/lean-repl--next-env-for-command command))))
    (when env
      (setq payload (append payload `((env . ,env)))))
    (chief/lean-repl-send-json payload request)))

(defun chief/lean-repl-send-region (beg end &optional env)
  "Send the active region as a Lean command to the optional JSON REPL.
With prefix argument, prompt for ENV, the environment id returned by a previous
REPL response.  Results are shown both in the REPL and inline at the evaluated
source lines."
  (interactive
   (progn
     (unless (use-region-p)
       (user-error "No active region"))
     (list (region-beginning)
           (region-end)
           (when current-prefix-arg
             (read-number "Environment id: ")))))
  (let* ((command (buffer-substring-no-properties beg end))
         (env (or env (chief/lean-repl--next-env-for-command command)))
         (payload `((cmd . ,command)))
         (request (chief/lean-repl--source-request beg end command)))
    (when env
      (setq payload (append payload `((env . ,env)))))
    (chief/lean-repl-send-json payload request)))

(defconst chief/lean-repl--command-start-regexp
  (concat "^\\s-*"
          "\\(?:#\\|"
          (regexp-opt
           '("abbrev" "attribute" "axiom" "builtin_initialize" "class"
             "coinductive" "def" "deriving" "elab" "end" "example"
             "export" "import" "inductive" "infix" "infixl" "infixr"
             "initialize" "instance" "lemma" "macro" "macro_rules"
             "mutual" "namespace" "notation" "opaque" "open" "open scoped"
             "postfix" "prefix" "section" "set_option" "structure"
             "syntax" "theorem" "universe" "universes" "variable"
             "variables")
           'symbols)
          "\\)")
  "Regexp matching the start of a likely top-level Lean command.
It is intentionally broad so `send defun' works for Lean commands in general,
not only for `#eval'.")

(defun chief/lean-repl-command-bounds ()
  "Return bounds of the likely top-level Lean command around point.
This is the Lean equivalent of a Lisp/Clojure defun for REPL sending."
  (save-excursion
    (let ((start nil)
          (end nil))
      (beginning-of-line)
      (setq start
            (if (looking-at-p chief/lean-repl--command-start-regexp)
                (point)
              (or (when (re-search-backward chief/lean-repl--command-start-regexp nil t)
                    (line-beginning-position))
                  (point-min))))
      (goto-char start)
      (forward-line 1)
      (setq end
            (or (when (re-search-forward chief/lean-repl--command-start-regexp nil t)
                  (match-beginning 0))
                (point-max)))
      (cons start end))))

(defun chief/lean-repl-send-line ()
  "Send the current line to the optional JSON REPL.
This sends whatever Lean code is on the line; it is not limited to `#eval'."
  (interactive)
  (chief/lean-repl-send-region (line-beginning-position) (line-end-position)))

(defun chief/lean-repl-send-defun ()
  "Send the top-level Lean command around point to the optional JSON REPL.
This is the Lean analogue of evaluating the current defun/form in a Lisp or
Clojure REPL workflow and is not limited to `#eval'."
  (interactive)
  (pcase-let ((`(,start . ,end) (chief/lean-repl-command-bounds)))
    (chief/lean-repl-send-region start end)))

(defun chief/lean-repl-send-buffer ()
  "Send the current buffer contents to the optional JSON REPL.
This is the Lean analogue of loading/evaluating a buffer in a Lisp/Clojure REPL:
it sends all Lean commands in the buffer, not just `#eval' lines, and updates
`chief/lean-repl-env' so later region/line evaluations can use definitions
introduced by the buffer.  It uses the live buffer text; it does not save first."
  (interactive)
  (chief/lean-repl-send-region (point-min) (point-max)
                               (unless (save-excursion
                                         (goto-char (point-min))
                                         (re-search-forward "^\\s-*import\\_>" nil t))
                                 chief/lean-repl-env)))

(defun chief/lean-repl-send-file (&optional all-tactics)
  "Send the current file to the optional JSON REPL in file mode.
With prefix argument ALL-TACTICS, ask the REPL to report all tactics."
  (interactive "P")
  (chief/lean-save-current-buffer)
  (let ((payload `((path . ,(chief/lean-current-file))))
        (request (chief/lean-repl--source-request (point-min) (point-max))))
    (when all-tactics
      (setq payload (append payload '((allTactics . t)))))
    (chief/lean-repl-send-json payload request)))

(defun chief/lean-repl-send-tactic (&optional proof-state tactic)
  "Send TACTIC to PROOF-STATE in the optional JSON REPL's tactic mode."
  (interactive
   (list (read-number "Proof state id: ")
         (read-string "Tactic: ")))
  (unless (chief/lean--string-present-p tactic)
    (user-error "No tactic entered"))
  (chief/lean-repl-send-json
   `((tactic . ,tactic)
     (proofState . ,proof-state))))

(defun chief/lean-mode-setup ()
  "Configure the current Lean buffer."
  (setq-local comment-start "--")
  (setq-local comment-start-skip "[-/]-[ \t]*")
  (setq-local comment-end "")
  (setq-local comment-end-skip "[ \t]*\\(-/\\|\\s>\\)")
  (setq-local comment-padding 1)
  (setq-local comment-use-syntax t)
  (setq-local font-lock-defaults '(chief/lean-font-lock-keywords))
  (setq-local imenu-generic-expression chief/lean-imenu-generic-expression)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (when chief/lean-unicode-input-auto-enable
    (chief/lean-unicode-input-mode 1))
  (setq-local compile-command
              (if (chief/lean-nearest-lake-root)
                  "lake build"
                (if buffer-file-name
                    (format "lean %s" (shell-quote-argument buffer-file-name))
                  "lean")))
  (setq-local chief/lsp-root-function #'chief/lean-project-root)
  ;; Do not force semantic tokens for Lean.  Some Lean/Lake server versions do
  ;; not advertise `textDocument/semanticTokens/full', and forcing it makes
  ;; lsp-mode poll an unsupported method and spam timer errors.
  (setq-local lsp-semantic-tokens-enable nil)
  (add-hook 'lsp-diagnostics-updated-hook
            #'chief/lean-maybe-update-inline-info-overlays nil t)
  (add-hook 'kill-buffer-hook #'chief/lean-clear-all-inline-overlays nil t)
  (when (fboundp 'chief/repl-configure)
    (chief/repl-configure
     :start #'chief/lean-repl-start
     :restart #'chief/lean-repl-restart
     :send-line #'chief/lean-repl-send-line
     :send-region #'chief/lean-repl-send-region
     :send-buffer #'chief/lean-repl-send-buffer
     :send-defun #'chief/lean-repl-send-defun
     :load-file #'chief/lean-repl-send-file))
  (when (fboundp 'electric-indent-local-mode)
    (electric-indent-local-mode -1))
  (when (fboundp 'chief/lsp-managed-mode-setup)
    (chief/lsp-managed-mode-setup)))

;;;###autoload
(define-derived-mode chief/lean-mode prog-mode "Lean"
  "Major mode for Lean 4 source files."
  :syntax-table chief/lean-mode-syntax-table
  :group 'chief/lean
  (chief/lean-mode-setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lean\\'" . chief/lean-mode))

(defun chief/lean-register-lsp-client ()
  "Register the Lean LSP client with lsp-mode."
  (add-to-list 'lsp-language-id-configuration '(chief/lean-mode . "lean"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection #'chief/lean-lsp-server-command)
    :major-modes '(chief/lean-mode)
    :priority 1
    :server-id 'chief-lean-lsp
    :request-handlers (let ((handlers (make-hash-table :test #'equal)))
                        (puthash "workspace/inlayHint/refresh" #'ignore handlers)
                        (puthash "workspace/semanticTokens/refresh" #'ignore handlers)
                        handlers)
    :notification-handlers (let ((handlers (make-hash-table :test #'equal)))
                             ;; Lean-specific progress notifications are useful
                             ;; for richer UIs, but ignoring them is preferable to
                             ;; noisy "unknown notification" warnings.
                             (puthash "$/lean/fileProgress" #'ignore handlers)
                             handlers)
    :semantic-tokens-faces-overrides
    '(:types (("leanSorryLike" . font-lock-warning-face))))))

(with-eval-after-load 'lsp-mode
  (chief/lean-register-lsp-client)
  ;; Global hook makes inline `#eval' output work even for Lean buffers that
  ;; already existed when this file was reloaded; the function itself checks the
  ;; major mode before doing anything.
  (add-hook 'lsp-diagnostics-updated-hook
            #'chief/lean-maybe-update-inline-info-overlays)
  ;; Refresh after commands too, so reloading this module fixes already-open Lean
  ;; buffers whose diagnostics were received before the LSP hook was installed.
  (add-hook 'post-command-hook #'chief/lean-maybe-update-inline-info-overlays)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.lake\\'")
  (when (boundp 'chief/lsp-managed-major-modes)
    (add-to-list 'chief/lsp-managed-major-modes 'chief/lean-mode))
  (when (boundp 'chief/lsp-manual-install-specs)
    (add-to-list 'chief/lsp-manual-install-specs
                 '(:modes (chief/lean-mode)
                   :label "Lean"
                   :doc-url "https://lean-lang.org/install/manual"
                   :help "Install elan so `lean' and `lake' are available on PATH; projects should normally contain `lean-toolchain' and `lakefile.lean' or `lakefile.toml'."))))

(when (boundp 'chief/project-root-markers)
  (dolist (marker '("lean-toolchain" "lakefile.lean" "lakefile.toml"))
    (add-to-list 'chief/project-root-markers marker t)))

(when (boundp 'project-vc-extra-root-markers)
  (dolist (marker '("lean-toolchain" "lakefile.lean" "lakefile.toml"))
    (add-to-list 'project-vc-extra-root-markers marker t)))

(when (boundp 'treesit-language-source-alist)
  ;; Optional only: Lean's syntax is extensible, so tree-sitter cannot replace
  ;; the Lean parser/LSP.  This grammar is useful for experiments and tooling.
  (add-to-list 'treesit-language-source-alist
               '(lean "https://github.com/Julian/tree-sitter-lean")))

(when (boundp 'chief/treesit-fallback-language-alist)
  (add-to-list 'chief/treesit-fallback-language-alist '(chief/lean-mode . lean)))

(when (fboundp 'chief/local-leader-def)
  (chief/local-leader-def
    :keymaps 'chief/lean-mode-map
    "b" #'chief/lean-lake-build
    "c" '(:ignore t :which-key "compile/run")
    "cb" #'chief/lean-lake-build
    "cc" #'chief/lean-lake-clean
    "cC" #'chief/lean-compile-current-file-to-c
    "ce" #'chief/lean-lake-exe
    "cf" #'chief/lean-check-current-file
    "cg" #'chief/lean-lake-cache-get
    "cG" #'chief/lean-lake-mathlib-cache-get
    "ck" #'chief/lean-lake-check-build
    "cl" #'chief/lean-lake-lint
    "cL" #'chief/lean-lake-check-lint
    "cq" #'chief/lean-lake-query
    "cr" #'chief/lean-run-current-file
    "cs" #'chief/lean-lake-script-run
    "cS" #'chief/lean-lake-script-list
    "cd" #'chief/lean-lake-script-doc
    "ct" #'chief/lean-lake-test
    "cT" #'chief/lean-lake-check-test
    "cu" #'chief/lean-lake-update
    "cx" #'chief/lean-lake-shake
    "e" #'chief/lean-lake-env
    "E" #'chief/lean-elan
    "g" #'chief/lean-show-goal
    "i" #'chief/lean-show-info
    "k" #'chief/lean-insert-unicode-symbol
    "K" #'chief/lean-show-unicode-abbreviations-for-symbol
    "l" #'chief/lean-lake
    "n" #'chief/lean-lean
    "N" #'chief/lean-leanc
    "m" #'chief/lean-leanmake
    "r" #'chief/lean-refresh-file-dependencies
    "R" #'chief/lean-repl-start
    "s" '(:ignore t :which-key "send/repl")
    "ss" #'chief/lean-repl-start
    "sR" #'chief/lean-repl-restart
    "sl" #'chief/lean-repl-send-line
    "se" #'chief/lean-repl-send-region
    "sr" #'chief/lean-repl-send-region
    "sb" #'chief/lean-repl-send-buffer
    "sd" #'chief/lean-repl-send-defun
    "sf" #'chief/lean-repl-send-file
    "sc" #'chief/lean-repl-send-command
    "st" #'chief/lean-repl-send-tactic
    "v" #'chief/lean-show-versions
    "V" #'chief/lean-show-toolchain))

(provide 'lang-lean)
;;; lang-lean.el ends here
