;;; core-projects.el --- Project root tuning -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'project)
(require 'seq)
(require 'subr-x)

(defcustom chief/project-root-markers
  '("pyproject.toml"
    "uv.lock"
    "setup.py"
    "setup.cfg"
    "requirements.txt"
    "go.mod"
    "go.work"
    "templ.toml"
    "mix.exs"
    "mix.lock"
    "rebar.config"
    "rebar.config.script"
    "rebar.lock"
    "build.zig"
    "build.zig.zon"
    "zls.json"
    "Cargo.toml"
    "Cargo.lock"
    "Package.swift"
    "pom.xml"
    "mvnw"
    "gradlew"
    "build.gradle"
    "build.gradle.kts"
    "settings.gradle"
    "settings.gradle.kts"
    "melos.yaml"
    "pubspec.yaml"
    "deno.json"
    "deno.jsonc"
    "bunfig.toml"
    "bun.lock"
    "bun.lockb"
    "package.json"
    "pnpm-workspace.yaml"
    "pnpm-lock.yaml"
    "package-lock.json"
    "yarn.lock"
    "turbo.json"
    "nx.json"
    "lerna.json"
    "tsconfig.json"
    "jsconfig.json"
    "dune-project"
    "dune-workspace")
  "Extra project root markers recognized by `project.el'."
  :type '(repeat string)
  :group 'chief)

(defun chief/project-normalize-root (root)
  "Return ROOT expanded as a directory name."
  (when root
    (file-name-as-directory (expand-file-name root))))

(defun chief/project-default-start-directory ()
  "Return the best starting directory for project root detection."
  (chief/project-normalize-root
   (or (and buffer-file-name (file-name-directory buffer-file-name))
       default-directory)))

(defun chief/project-current-root (&optional start)
  "Return the `project.el' root for START, if any."
  (let ((default-directory
         (or (chief/project-normalize-root start)
             (chief/project-default-start-directory)
             default-directory)))
    (when-let* ((project (project-current nil)))
      (chief/project-normalize-root (project-root project)))))

(defun chief/project-nearest-marker-root (markers &optional start)
  "Return the first parent root containing one of MARKERS from START.
MARKERS are checked in order so callers can prefer workspace markers over
leaf module markers."
  (let ((start (or (chief/project-normalize-root start)
                   (chief/project-default-start-directory))))
    (cl-loop for marker in markers
             for root = (and start (locate-dominating-file start marker))
             when root
             return (chief/project-normalize-root root))))

(defun chief/project-preferred-root (&rest choices)
  "Return the first usable root from CHOICES.
Each choice may be a root directory string, a list of marker names, or nil."
  (cl-loop for choice in choices
           thereis
           (cond
            ((null choice) nil)
            ((and (listp choice) (stringp (car choice)))
             (chief/project-nearest-marker-root choice))
            ((stringp choice)
             (chief/project-normalize-root choice))
            (t nil))))

(defun chief/project-directory-files-matching (directory regexp)
  "Return files in DIRECTORY whose basenames match REGEXP."
  (when (file-directory-p directory)
    (seq-filter
     (lambda (file)
       (string-match-p regexp (file-name-nondirectory file)))
     (directory-files directory t directory-files-no-dot-files-regexp t))))

(defun chief/project-nearest-matching-file (regexp &optional start limit)
  "Return the nearest file matching REGEXP between START and LIMIT."
  (let ((dir (or (chief/project-normalize-root start)
                 (chief/project-default-start-directory)))
        (limit (and limit (chief/project-normalize-root limit))))
    (catch 'found
      (while dir
        (when-let* ((files (chief/project-directory-files-matching dir regexp)))
          (throw 'found (car files)))
        (if (or (equal dir limit)
                (equal dir "/"))
            (setq dir nil)
          (setq dir
                (file-name-directory
                 (directory-file-name dir))))))))

(defun chief/project-nearest-regexp-root (regexp &optional start limit)
  "Return the directory containing the nearest file matching REGEXP."
  (when-let* ((file (chief/project-nearest-matching-file regexp start limit)))
    (chief/project-normalize-root (file-name-directory file))))

(when (boundp 'project-vc-extra-root-markers)
  (dolist (marker chief/project-root-markers)
    (add-to-list 'project-vc-extra-root-markers marker t)))

(provide 'core-projects)
;;; core-projects.el ends here
