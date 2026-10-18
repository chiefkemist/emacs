;;; lang-dotnet-test.el --- Tests for .NET project isolation -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests for projects below unrelated solution files.

;;; Code:

(require 'ert)
(require 'lang-dotnet)

(defun chief/dotnet-test--write (root relative content)
  "Write CONTENT to RELATIVE under ROOT and return the resulting path."
  (let ((file (expand-file-name relative root)))
    (make-directory (file-name-directory file) t)
    (write-region content nil file nil 'silent)
    file))

(defun chief/dotnet-test--same-directory-p (left right)
  "Return non-nil when directory paths LEFT and RIGHT are equal."
  (equal (file-name-as-directory (expand-file-name left))
         (file-name-as-directory (expand-file-name right))))

(ert-deftest chief/dotnet-detached-project-ignores-ancestor-slnx ()
  "An ancestor .slnx that omits a project must not become its tool target."
  (let* ((workspace (make-temp-file "chief-dotnet-detached-" t))
         (project
          (chief/dotnet-test--write
           workspace "detached/detached.fsproj"
           "<Project Sdk=\"Microsoft.NET.Sdk\" />\n"))
         (source
          (chief/dotnet-test--write
           workspace "detached/src/Types.fs" "module Types\n"))
         (solution
          (chief/dotnet-test--write
           workspace "workspace.slnx"
           (concat "<Solution>\n"
                   "  <Project Path=\"attached/attached.fsproj\" />\n"
                   "</Solution>\n")))
         (buffer-file-name source)
         (default-directory (file-name-directory source)))
    (unwind-protect
        (progn
          (chief/dotnet-test--write
           workspace "attached/attached.fsproj"
           "<Project Sdk=\"Microsoft.NET.Sdk\" />\n")
          (should-not (chief/dotnet-solution-contains-project-p solution project))
          (should-not (chief/dotnet-solution-file))
          (should (chief/dotnet-test--same-directory-p
                   (chief/dotnet-project-root)
                   (file-name-directory project)))
          (should (chief/dotnet-same-file-p
                   (chief/polyglot-dotnet-target) project))
          (should (chief/dotnet-same-file-p
                   (chief/dotnet-build-target "fsproj") project)))
      (delete-directory workspace t))))

(ert-deftest chief/dotnet-attached-project-uses-ancestor-slnx ()
  "An ancestor .slnx remains the target when it contains the project."
  (let* ((workspace (make-temp-file "chief-dotnet-attached-" t))
         (project
          (chief/dotnet-test--write
           workspace "app/app.fsproj"
           "<Project Sdk=\"Microsoft.NET.Sdk\" />\n"))
         (source
          (chief/dotnet-test--write
           workspace "app/src/App.fs" "module App\n"))
         (solution
          (chief/dotnet-test--write
           workspace "workspace.slnx"
           (concat "<Solution>\n"
                   "  <Folder Name=\"/src/\">\n"
                   "    <Project Path=\"app/app.fsproj\" />\n"
                   "  </Folder>\n"
                   "</Solution>\n")))
         (buffer-file-name source)
         (default-directory (file-name-directory source)))
    (unwind-protect
        (progn
          (should (chief/dotnet-solution-contains-project-p solution project))
          (should (chief/dotnet-same-file-p
                   (chief/dotnet-solution-file) solution))
          (should (chief/dotnet-test--same-directory-p
                   (chief/dotnet-project-root) workspace))
          (should (chief/dotnet-same-file-p
                   (chief/polyglot-dotnet-target) solution)))
      (delete-directory workspace t))))

(ert-deftest chief/dotnet-language-servers-target-only-detached-projects ()
  "Roslyn and FSAC must not discover sibling projects for detached projects."
  (let* ((workspace (make-temp-file "chief-dotnet-lsp-targets-" t))
         (root (expand-file-name "detached" workspace))
         (csharp-project
          (chief/dotnet-test--write
           workspace "detached/App.csproj"
           "<Project Sdk=\"Microsoft.NET.Sdk\" />\n"))
         (fsharp-project
          (chief/dotnet-test--write
           workspace "detached/App.fsproj"
           "<Project Sdk=\"Microsoft.NET.Sdk\" />\n"))
         (csharp-source
          (chief/dotnet-test--write
           workspace "detached/src/App.cs" "class App {}\n"))
         (fsharp-source
          (chief/dotnet-test--write
           workspace "detached/src/App.fs" "module App\n"))
         (csharp-buffer (generate-new-buffer " *dotnet-csharp-test*"))
         (fsharp-buffer (generate-new-buffer " *dotnet-fsharp-test*")))
    (unwind-protect
        (progn
          (chief/dotnet-test--write
           workspace "detached/tests/App.Tests.csproj"
           "<Project Sdk=\"Microsoft.NET.Sdk\" />\n")
          (chief/dotnet-test--write
           workspace "detached/tests/App.Tests.fsproj"
           "<Project Sdk=\"Microsoft.NET.Sdk\" />\n")
          (chief/dotnet-test--write
           workspace "workspace.slnx"
           "<Solution><Project Path=\"attached/Other.csproj\" /></Solution>\n")
          (with-current-buffer csharp-buffer
            (setq buffer-file-name csharp-source
                  default-directory (file-name-directory csharp-source)))
          (with-current-buffer fsharp-buffer
            (setq buffer-file-name fsharp-source
                  default-directory (file-name-directory fsharp-source)))
          (let ((csharp-workspace
                 (make-lsp--workspace
                  :root root :buffers (list csharp-buffer)))
                (fsharp-workspace
                 (make-lsp--workspace
                  :root root :buffers (list fsharp-buffer))))
            (should
             (equal (plist-get
                     (chief/dotnet-roslyn-workspace-targets csharp-workspace)
                     :projects)
                    (list (file-truename csharp-project))))
            (should
             (equal (chief/dotnet-fsharp-workspace-targets fsharp-workspace)
                    (list (file-truename fsharp-project))))))
      (when (buffer-live-p csharp-buffer) (kill-buffer csharp-buffer))
      (when (buffer-live-p fsharp-buffer) (kill-buffer fsharp-buffer))
      (delete-directory workspace t))))

(ert-deftest chief/dotnet-roslyn-retries-after-first-buffer-attaches ()
  "Roslyn must wait when empty, then open only the represented nested target."
  (let* ((root (make-temp-file "chief-dotnet-empty-workspace-" t))
         (workspace (make-lsp--workspace :root root :buffers nil))
         (buffer (generate-new-buffer " *dotnet-late-attach-test*"))
         notifications)
    (unwind-protect
        (let* ((member
                (chief/dotnet-test--write
                 root "member/Member.csproj"
                 "<Project Sdk=\"Microsoft.NET.Sdk\" />\n"))
               (source
                (chief/dotnet-test--write
                 root "member/Program.cs" "class Program {}\n"))
               (solution
                (chief/dotnet-test--write
                 root "workspace.slnx"
                 "<Solution><Project Path=\"member/Member.csproj\" /></Solution>\n")))
          (chief/dotnet-test--write
           root "omitted/Omitted.csproj"
           "<Project Sdk=\"Microsoft.NET.Sdk\" />\n")
          (should-not (chief/dotnet-roslyn-workspace-targets workspace))
          (cl-letf (((symbol-function 'lsp-notify)
                     (lambda (method params)
                       (push (list method params) notifications)))
                    ((symbol-function 'lsp--path-to-uri)
                     (lambda (path) (concat "file://" path))))
            (should-not (chief/dotnet-roslyn-initialized workspace))
            (should-not notifications)
            (with-current-buffer buffer
              (setq buffer-file-name source
                    default-directory (file-name-directory source)))
            (setf (lsp--workspace-buffers workspace) (list buffer))
            (should (chief/dotnet-roslyn-open-workspace-targets workspace))
            (should
             (equal notifications
                    (list
                     (list "solution/open"
                           `(:solution
                             ,(lsp--path-to-uri (file-truename solution)))))))
            (should (chief/dotnet-solution-contains-project-p solution member))))
      (remhash workspace chief/dotnet-roslyn-opened-targets)
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest chief/dotnet-mixed-membership-keeps-explicit-workspace-projects ()
  "A common solution is invalid when any represented project is detached."
  (let* ((root (make-temp-file "chief-dotnet-mixed-membership-" t))
         (attached
          (chief/dotnet-test--write
           root "attached/Attached.fsproj"
           "<Project Sdk=\"Microsoft.NET.Sdk\" />\n"))
         (detached
          (chief/dotnet-test--write
           root "detached/Detached.fsproj"
           "<Project Sdk=\"Microsoft.NET.Sdk\" />\n"))
         (attached-source
          (chief/dotnet-test--write
           root "attached/Attached.fs" "module Attached\n"))
         (detached-source
          (chief/dotnet-test--write
           root "detached/Detached.fs" "module Detached\n"))
         (attached-buffer (generate-new-buffer " *dotnet-attached-test*"))
         (detached-buffer (generate-new-buffer " *dotnet-detached-test*")))
    (unwind-protect
        (progn
          (chief/dotnet-test--write
           root "workspace.slnx"
           "<Solution><Project Path=\"attached/Attached.fsproj\" /></Solution>\n")
          (with-current-buffer attached-buffer
            (setq buffer-file-name attached-source
                  default-directory (file-name-directory attached-source)))
          (with-current-buffer detached-buffer
            (setq buffer-file-name detached-source
                  default-directory (file-name-directory detached-source)))
          (let* ((workspace
                  (make-lsp--workspace
                   :root root :buffers (list attached-buffer detached-buffer)))
                 (targets (chief/dotnet-fsharp-workspace-targets workspace)))
            (should-not
             (chief/dotnet-common-project-solution
              (list (file-truename attached) (file-truename detached))))
            (should
             (equal targets
                    (sort
                     (list (file-truename attached) (file-truename detached))
                     #'string<)))))
      (when (buffer-live-p attached-buffer) (kill-buffer attached-buffer))
      (when (buffer-live-p detached-buffer) (kill-buffer detached-buffer))
      (delete-directory root t))))

(ert-deftest chief/dotnet-classic-solution-membership-normalizes-separators ()
  "Classic .sln membership recognizes Windows separators on every host."
  (let* ((workspace (make-temp-file "chief-dotnet-sln-" t))
         (project
          (chief/dotnet-test--write
           workspace "src/App/App.csproj"
           "<Project Sdk=\"Microsoft.NET.Sdk\" />\n"))
         (solution
          (chief/dotnet-test--write
           workspace "workspace.sln"
           (concat
            "Microsoft Visual Studio Solution File, Format Version 12.00\n"
            "Project(\"{TYPE}\") = \"App\", \"src\\\\App\\\\App.csproj\", \"{APP}\"\n"
            "EndProject\n"))))
    (unwind-protect
        (should (chief/dotnet-solution-contains-project-p solution project))
      (delete-directory workspace t))))

(ert-deftest chief/dotnet-solution-filter-membership-is-explicit ()
  "A .slnf applies only to the projects selected by its projects array."
  (let* ((workspace (make-temp-file "chief-dotnet-slnf-" t))
         (included
          (chief/dotnet-test--write
           workspace "src/Included/Included.csproj"
           "<Project Sdk=\"Microsoft.NET.Sdk\" />\n"))
         (excluded
          (chief/dotnet-test--write
           workspace "src/Excluded/Excluded.csproj"
           "<Project Sdk=\"Microsoft.NET.Sdk\" />\n"))
         (filter
          (chief/dotnet-test--write
           workspace "workspace.slnf"
           (concat
            "{\"solution\":{\"path\":\"workspace.sln\","
            "\"projects\":[\"src/Included/Included.csproj\"]}}\n"))))
    (unwind-protect
        (progn
          (should (chief/dotnet-solution-contains-project-p filter included))
          (should-not (chief/dotnet-solution-contains-project-p filter excluded)))
      (delete-directory workspace t))))

(ert-deftest chief/dotnet-solution-filter-projects-use-solution-directory ()
  "Nested .slnf paths resolve against the referenced solution directory."
  (let* ((workspace (make-temp-file "chief-dotnet-nested-slnf-" t))
         (included
          (chief/dotnet-test--write
           workspace "solution/src/App/App.csproj"
           "<Project Sdk=\"Microsoft.NET.Sdk\" />\n"))
         (decoy
          (chief/dotnet-test--write
           workspace "filters/src/App/App.csproj"
           "<Project Sdk=\"Microsoft.NET.Sdk\" />\n"))
         (filter
          (chief/dotnet-test--write
           workspace "filters/workspace.slnf"
           (concat
            "{\"solution\":{\"path\":\"../solution/workspace.sln\","
            "\"projects\":[\"src/App/App.csproj\"]}}\n"))))
    (unwind-protect
        (progn
          (should (chief/dotnet-solution-contains-project-p filter included))
          (should-not (chief/dotnet-solution-contains-project-p filter decoy)))
      (delete-directory workspace t))))

(provide 'lang-dotnet-test)
;;; lang-dotnet-test.el ends here
