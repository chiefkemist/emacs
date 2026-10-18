;;; lang-scala-test.el --- Tests for Scala project discovery -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused tests for nested build boundaries and mixed-tool monorepos.

;;; Code:

(require 'ert)
(require 'lang-scala)

(defun chief/scala-test--touch (root relative)
  "Create an empty file at RELATIVE under ROOT and return its path."
  (let ((file (expand-file-name relative root)))
    (make-directory (file-name-directory file) t)
    (write-region "" nil file nil 'silent)
    file))

(defun chief/scala-test--root= (left right)
  "Return non-nil when directory paths LEFT and RIGHT are equal."
  (equal (file-name-as-directory (expand-file-name left))
         (file-name-as-directory (expand-file-name right))))

(ert-deftest chief/scala-nearest-boundary-wins-in-mixed-layout ()
  "A nearer Maven module must beat farther Mill and sbt markers."
  (let* ((workspace (make-temp-file "chief-scala-mixed-" t))
         (mill-root (expand-file-name "platform" workspace))
         (maven-root (expand-file-name "platform/service" workspace))
         (source-dir (expand-file-name "src/main/scala" maven-root))
         (default-directory (file-name-as-directory source-dir))
         (chief/scala-build-tool-override nil)
         (chief/scala-metals-root-scope 'module))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" workspace) t)
          (chief/scala-test--touch workspace "build.sbt")
          (chief/scala-test--touch workspace "platform/build.mill")
          (chief/scala-test--touch workspace "platform/service/pom.xml")
          (make-directory source-dir t)
          (let ((boundaries (chief/scala-ancestor-build-boundaries)))
            (should (= (length boundaries) 3))
            (should (chief/scala-test--root=
                     (plist-get (nth 0 boundaries) :root) maven-root))
            (should (equal (plist-get (nth 0 boundaries) :tools) '(maven)))
            (should (chief/scala-test--root=
                     (plist-get (nth 1 boundaries) :root) mill-root))
            (should (equal (plist-get (nth 1 boundaries) :tools) '(mill)))
            (should (chief/scala-test--root=
                     (plist-get (nth 2 boundaries) :root) workspace))
            (should (equal (plist-get (nth 2 boundaries) :tools) '(sbt))))
          (should (chief/scala-test--root= (chief/scala-project-root) maven-root))
          (should (eq (chief/scala-project-build-tool) 'maven))
          (should (chief/scala-test--root= (chief/scala-workspace-root) workspace))
          (should (chief/scala-test--root= (chief/scala-metals-root) maven-root))
          (let ((chief/scala-metals-root-scope 'workspace))
            (should (chief/scala-test--root=
                     (chief/scala-metals-root) workspace))))
      (delete-directory workspace t))))

(ert-deftest chief/scala-nearest-boundary-wins-for-each-build-tool ()
  "Nested same-tool builds select their nearest module for all build tools."
  (dolist (case '((sbt "build.sbt" "module/build.sbt")
                  (mill "build.mill" "module/build.mill")
                  (gradle "settings.gradle" "module/build.gradle.kts")
                  (maven "pom.xml" "module/pom.xml")))
    (let* ((tool (nth 0 case))
           (outer-marker (nth 1 case))
           (inner-marker (nth 2 case))
           (workspace (make-temp-file (format "chief-scala-%s-" tool) t))
           (module (expand-file-name "module" workspace))
           (source-dir (expand-file-name "src/main/scala" module))
           (default-directory (file-name-as-directory source-dir))
           (chief/scala-build-tool-override nil))
      (unwind-protect
          (progn
            (make-directory (expand-file-name ".git" workspace) t)
            (chief/scala-test--touch workspace outer-marker)
            (chief/scala-test--touch workspace inner-marker)
            (make-directory source-dir t)
            (should (chief/scala-test--root= (chief/scala-project-root) module))
            (should (eq (chief/scala-project-build-tool) tool))
            (should (chief/scala-test--root= (chief/scala-workspace-root) workspace)))
        (delete-directory workspace t)))))

(ert-deftest chief/scala-sbt-commands-bind-nearest-project-root ()
  "Interactive sbt operations must not fall back to an outer nested build."
  (require 'sbt-mode)
  (let* ((workspace (make-temp-file "chief-scala-sbt-call-" t))
         (module (expand-file-name "module" workspace))
         (source-dir (expand-file-name "src/main/scala" module))
         (default-directory (file-name-as-directory source-dir))
         (chief/scala-build-tool-override nil)
         captured)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" workspace) t)
          (chief/scala-test--touch workspace "build.sbt")
          (chief/scala-test--touch workspace "module/build.sbt")
          (make-directory source-dir t)
          (cl-letf (((symbol-function 'chief/scala-tool-executable)
                     (lambda (&rest _) "/tmp/sbt")))
            (chief/scala--sbt-call
             (lambda ()
               (setq captured
                     (list default-directory
                           sbt:buffer-project-root
                           sbt:prefer-nested-projects)))))
          (should (chief/scala-test--root= (nth 0 captured) module))
          (should (chief/scala-test--root= (nth 1 captured) module))
          (should (eq (nth 2 captured) t)))
      (delete-directory workspace t))))

(ert-deftest chief/scala-discovers-all-monorepo-build-tools ()
  "Workspace target discovery finds sibling sbt, Mill, Gradle, and Maven roots."
  (let* ((workspace (make-temp-file "chief-scala-monorepo-" t))
         (default-directory (file-name-as-directory workspace))
         (chief/scala-build-tool-override nil)
         (chief/scala-workspace-root-override workspace))
    (unwind-protect
        (progn
          (chief/scala-test--touch workspace "apps/sbt-app/build.sbt")
          (chief/scala-test--touch workspace "apps/mill-app/build.mill")
          (chief/scala-test--touch workspace "services/gradle-app/settings.gradle.kts")
          (chief/scala-test--touch workspace "services/maven-app/pom.xml")
          (should (chief/scala-test--root= (chief/scala-workspace-root) workspace))
          (let ((targets (chief/scala-workspace-build-targets workspace)))
            (dolist (expected '(("apps/sbt-app" . sbt)
                                ("apps/mill-app" . mill)
                                ("services/gradle-app" . gradle)
                                ("services/maven-app" . maven)))
              (should
               (seq-some
                (lambda (target)
                  (and (eq (cdr target) (cdr expected))
                       (chief/scala-test--root=
                        (car target) (expand-file-name (car expected) workspace))))
                targets)))))
      (delete-directory workspace t))))

(ert-deftest chief/scala-default-operation-runs-at-nearest-module ()
  "Default builds use the nearest module cwd and an available outer wrapper."
  (let* ((workspace (make-temp-file "chief-scala-cwd-" t))
         (module (expand-file-name "module" workspace))
         (source-dir (expand-file-name "src/main/scala" module))
         (default-directory (file-name-as-directory source-dir))
         (chief/scala-build-tool-override nil)
         captured-directory
         captured-command)
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" workspace) t)
          (chief/scala-test--touch workspace "module/build.gradle.kts")
          (let ((wrapper (chief/scala-test--touch workspace "gradlew")))
            (set-file-modes wrapper #o755)
            (make-directory source-dir t)
            (cl-letf (((symbol-function 'compilation-start)
                       (lambda (command &rest _)
                         (setq captured-command command
                               captured-directory default-directory))))
              (chief/scala-run-operation 'build))
            (should (chief/scala-test--root= captured-directory module))
            (should (string-match-p (regexp-quote (shell-quote-argument wrapper))
                                    captured-command))))
      (delete-directory workspace t))))

(ert-deftest chief/scala-operation-command-uses-selected-module-wrapper ()
  "Selected monorepo targets use that module's wrapper and preserve spaces."
  (let* ((workspace (make-temp-file "chief-scala-wrapper-" t))
         (module (expand-file-name "module with spaces" workspace))
         (source-dir (expand-file-name "src/main/scala" module))
         (default-directory (file-name-as-directory source-dir))
         (chief/scala-build-tool-override nil))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" workspace) t)
          (chief/scala-test--touch workspace "module with spaces/build.gradle.kts")
          (let ((wrapper (chief/scala-test--touch
                          workspace "module with spaces/gradlew")))
            (set-file-modes wrapper #o755)
            (make-directory source-dir t)
            (let ((command (chief/scala-operation-command 'build module 'gradle)))
              (should (equal (car command) wrapper))
              (should (equal (cdr command) '("--console=plain" "build")))
              (should (string-match-p
                       (regexp-quote (shell-quote-argument wrapper))
                       (chief/scala-command-string command))))))
      (delete-directory workspace t))))

(provide 'lang-scala-test)
;;; lang-scala-test.el ends here
