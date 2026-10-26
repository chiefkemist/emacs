# Emacs configuration

Modular Emacs configuration in `lisp/`, using straight.el for packages.
Runtime data, installed packages, compiled files, and local Custom settings are
ignored by Git. Tests and the package lockfile are tracked.

## Terminal keyboard input

`emacs -nw` configures its own input handling; it does not modify terminal-app
preferences or installations.

- On macOS, `modules/macos-option-keys.c` exposes a read-only keyboard-layout
  query directly to Elisp through Emacs's native module API. It identifies
  printable Option characters and translates them into the corresponding Meta
  keys, including Canadian layouts where Option+x sends `≈`. GUI input, quoted
  insertion, bracketed paste, and input methods are left alone. Existing custom
  translations are preserved. No Python interpreter or helper process is used.
- `kkp` negotiates extended keys in compatible terminals, including `C-.`,
  `C-;`, and `C-S-SPC`. It restores ordinary `C-g` during blocking subprocesses.
- Legacy terminal NUL input reaches the command-state `C-SPC` leader while
  retaining insert-state completion. In minibuffers, `C-c .` and `C-c ;` provide
  Embark alternatives when a terminal cannot transmit `C-.` and `C-;`.

The macOS module builds automatically on first use using already-installed
Apple developer tools and the Emacs installation's `emacs-module.h`. It is
cached under `var/native/` by architecture, Emacs major version, and source hash;
subsequent layout reads call native functions directly. No tools are installed
automatically, and compiled binaries are never tracked in Git.

Setup is automatic for terminal frames. After changing the macOS keyboard
layout, use `M-x chief/tty-refresh-option-translations`. Dead keys that emit no
input and shortcuts consumed by the terminal/OS cannot be reconstructed;
unsupported protocols retain legacy input rather than guessing key events.

## Project integration

Treemacs loads its Evil integration so both terminal `RET` and GUI Return open
nodes. Failed tag lookups cannot leave hookless file buffers behind: actual
file visits still initialize language tools, LSP, line numbers, and snippets.
Editor-launched .NET tools use the newest installed runtime (including previews)
so servers can load assemblies from the selected SDK; OS settings are unchanged.
Tree-sitter remapping probes only the selected language and retains classic PHP
editing when its embedded parsers are unavailable. Geiser does not override
Racket's configured file association. Theme compatibility overlays retain the
underlying palette and avoid cyclic Org/Gnus face inheritance during switches
and terminal child-frame creation.

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
emacs --module-assertions --batch --init-directory "$PWD" -l "$PWD/init.el" \
  -l "$PWD/test/core-tools-test.el" \
  -l "$PWD/test/core-terminal-keyboard-test.el" \
  -l "$PWD/test/lang-dotnet-test.el" \
  -l "$PWD/test/lang-scala-test.el" \
  -f ert-run-tests-batch-and-exit
```

The tooling tests require the installed Pilish Markdown grammars. They do not
send model prompts or consume API credits.
