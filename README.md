# Emacs configuration

Modular Emacs configuration in `lisp/`, using straight.el for packages.
Runtime data, installed packages, compiled files, and local Custom settings are
ignored by Git. Tests and the package lockfile are tracked.

## Pi assistant

[Pilish](https://github.com/dnouri/pilish) is installed directly from GitHub.
It requires Emacs 29.1+ with tree-sitter, Node.js 22.19+, and Pi CLI 1.0.0+
(`pi` on Emacs's PATH). Authenticate using `pi` in a terminal first if needed.

- `M-x pilish` or `M-x pi`: start/focus the current project's session.
- `SPC a a` / `SPC a p`: start/focus Pilish.
- `SPC a t`: toggle its windows.
- `SPC a s`: browse sessions.
- In the prompt buffer, `C-c C-c` sends and `C-c C-p` opens the menu.

Pilish respects Pi's saved project trust policy; it does not automatically
approve every project's local extensions or MCP servers. Approve trusted
projects in the Pi CLI when necessary.

Pilish requires Markdown and Markdown-inline tree-sitter grammars. For a new
installation, use `M-x pilish-install-grammars`. If older grammars were already
loaded, restart Emacs after upgrading them.

## Package maintenance

The active package revisions are recorded in
`straight/versions/default.el`. All of `var/` remains ignored; installed
packages and runtime data never belong in source control.
Straight uses this lockfile for fresh package clones. To restore existing
checkouts to the recorded versions, run `M-x straight-thaw-versions` and restart
Emacs so packages rebuild before they are loaded.

To update packages deliberately:

1. Save work and run `M-x straight-pull-all`. Resolve any upstream branch changes
   or local checkout modifications without discarding them.
2. Restart Emacs to build and load the updated packages in a fresh process.
3. Run tests, then `C-u M-x straight-freeze-versions` and review the lockfile diff.

Built-in Emacs libraries follow the installed Emacs version. Language servers,
SDKs, compilers, and the Pi CLI are external tools, not straight.el packages.

Ghostel's native module is stored in `var/ghostel/`, outside its rebuildable
package directory. After a native API upgrade, use `M-x ghostel-download-module`
and restart Emacs. No native binaries belong in Git.

## Verification

Run configuration tests with the installed package environment:

```sh
emacs --batch --init-directory "$PWD" -l "$PWD/init.el" \
  -l "$PWD/test/core-tools-test.el" \
  -l "$PWD/test/lang-dotnet-test.el" \
  -l "$PWD/test/lang-scala-test.el" \
  -f ert-run-tests-batch-and-exit
```

The tooling tests require the installed Pilish Markdown grammars. They do not
send model prompts or consume API credits.
