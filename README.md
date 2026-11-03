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

Backups in `var/backups/` are copied, not moved. This prevents LSP from treating
saves as file renames and rewriting JS/TS imports to backup paths; genuine file
renames retain LSP import updates.

## Project-scoped SharpLsp (.NET)

`lisp/lang-sharplsp.el` prefers SharpLsp for C#, F#, and Razor **only** beneath an
explicit `sharplsp.toml` (including `~/Documents/OGTECH_Workspace/concept_coach`).
An ordinary solution, project file, `.sharplsp` cache, or working directory does
not opt in. Unrelated projects keep the existing Roslyn/FsAutoComplete defaults.
Git/worktree boundaries stop configuration discovery; nearer configurations win.

For example:

```toml
[csharp]
solution_path = "concept_coach.slnx"
[fsharp]
enabled = true
```

The server is discovered through `chief/sharplsp-command`, the Emacs-owned
`var/sharplsp/` complete distribution, PATH, or an existing standalone Neovim
cache. Neovim itself is not required or launched. Keep the distribution's
C#/F# sidecars beside the executable. No project-local executable is trusted
implicitly. An absent server warns and retains the existing default backend.
The configured/sole solution is explicitly loaded into both sidecars.

`M-x chief/sharplsp-install-server` runs `scripts/install-sharplsp.sh`: it builds
upstream commit `e26a92f9a4c9396ccbdd6ab979dc4785c3ae686d` and its pinned
submodule, packages **both sidecars and the profiler engine**, and builds
SharpLsp's debugger fork at `2de84af51779449d34fb5597a438660ea77756bb` with
its managed startup hook enabled. The release adapter omits that hook and can
fail updates when a paused process is inside native code such as `Thread.Sleep`.
`scripts/sharplsp-baseline.patch` freezes compiler source snapshots before saves;
otherwise lazy file loading can make a real first edit appear stale. Provenance
records the patch hash, debugger source and CoreCLR source revisions. Arbitrary
modified build sources are refused, not overwritten or accepted under the pin.
This includes the upstream identity-based C#→F# source-navigation fix missing
from the 0.30.0 standalone release. The SDK and code generator live under
`var/sharplsp-build/`, not in
global installations. The installer preserves an existing distribution until
all components have built successfully and records provenance in
`var/sharplsp/emacs-build.json`. It requires existing Git, Rust, Node/npm, bash,
curl, Clang/C++, CMake and Ninja; it does not install OS packages or trust
certificates. Reinstall after moving the distribution: the private debugger's
startup-hook path is compiled for its selected installation directory.

### Navigation and editing

- Ordinary returned source files retain their originating SharpLsp context,
  even outside the config directory. Unrelated sibling files do not inherit it;
  a target's own explicit configuration takes precedence.
- Generated C# library views under the system temporary `sharplsp-decompiled/`
  and `MetadataAsSource/` directories, plus owned views in Emacs's existing
  ILSpy metadata cache, explicitly use **Roslyn**, read-only,
  with source-project context and suppressed diagnostics. Their first definition
  request waits for loose-document semantics; further library hops retain the
  origin. Returning to project source does not change its SharpLsp backend.
- Existing completion, hover, signatures, definitions, references, rename,
  code actions, document/workspace symbols and call-hierarchy commands remain
  available. Reference lenses use native Xref instead of VS Code commands.
  Imports such as `using System` retain the shared namespace/source/assembly
  resolver instead of stopping at a nil server definition. C# semantic
  highlighting is enabled locally unless explicitly overridden in the buffer.
- `lisp/lang-sharplsp-razor.el` routes opted-in `.razor` and `.cshtml` files to
  SharpLsp for embedded C# navigation and semantic highlighting. HTML
  tag/attribute completion uses its HTML projection without rewriting the
  source. Formatting uses atomic web-mode indentation rather than feeding
  markup to CSharpier. Other Razor/web projects retain their existing setup.
- `lisp/lang-sharplsp-fsharp.el` validates F# hints against syntax: function types
  appear above declarations, unsafe bare-parameter/receiver/operator hints are
  omitted, and proven argument hints move to their actual arguments. Source text
  and source highlighting are not rewritten. Curried call help falls back to
  compiler hover; it does **not** invent active-parameter/overload information.
  Declaration signatures toggle separately from inline hints.
- F# syntax presentation uses an installed compatible Emacs grammar, optionally
  reusing an existing compatible Neovim grammar read-only. On another machine,
  `M-x chief/sharplsp-install-fsharp-grammar` builds the official grammar into
  Emacs's ignored grammar directory. A missing grammar warns rather than
  displaying guessed hints; other LSP features remain available.

The installed server does **not** provide LSP formatting. Buffer/save formatting
uses CSharpier or Fantomas instead, preserving project settings and unsaved text;
formatter failure never substitutes an empty buffer or modifies the source file
behind Emacs. Existing build/run/watch/test-at-point, FSI/C# REPL and netcoredbg
DAP commands are retained, not replaced by unsupported SharpLsp operations.
`M-x chief/sharplsp-install-repl` installs the official C# interactive host into
`var/sharplsp-tools/`, visible only to opted-in buffers. The SharpLsp debugger
lives separately under `var/sharplsp/netcoredbg/`; the default debugger cache is
unchanged. Neither tool requires a global install.

### Terminal-compatible commands

Use the local leader (`,` in command state), then:

| Keys | Action |
| --- | --- |
| `L l` | Development workflow menu |
| `L I` | Actual backend, provenance, solution and capabilities |
| `L p` | Load a solution into both sidecars |
| `L H` / `L S` / `L C` | Toggle inline hints / F# signatures / lenses |
| `L s` | Signature help |
| `l f` | Format buffer using the selected dedicated formatter |
| `L x` | Custom request JSON workbench |
| `L X` | Running server's advertised commands, including profiler commands |
| `L n` / `L N` | Browse installed NuGet packages / search and install packages |
| `L P` | Managed-process browser, CPU captures and heap inspection |
| `L t` / `L m` | Choose target framework / sort the enclosing C# type |

The workflow menu includes native framework selection, member sorting, NuGet
search/install/update/remove, unused-package detection, consolidation preview,
CPU profiling and heap inspection. Tables have working `RET`, `g`, `j` and `k`
under Evil; control panels support `TAB`/`RET`. Project-file mutations require
confirmation and refuse to overwrite unsaved project changes.

The profiler browser operates on actual managed PIDs. CPU captures expose live
snapshots, sampling progress, sortable hotspots, source navigation when supplied,
and Speedscope/flamegraph/CPU-profile exports. Heap controls expose snapshots,
diffs, objects by type, retention references and explicit GC. These are native
Emacs controls, not a dependency on VS Code or a graphical webview. The JSON
workbenches remain available for inspecting the underlying protocols.

C# Edit-and-Continue initializes a compiler baseline at an entry stop and then
resumes automatically unless that stop was requested. Saves are debounced and
batched for the debug project's reference graph; running processes are privately
paused, updated, and resumed. Existing breakpoint stops remain paused. Set
`chief/sharplsp-hot-reload-on-save` to nil for manual operation; the menu's
baseline/apply/end commands remain available. Explicit adapter, environment and
hot-reload overrides are preserved. Runtime deltas are committed only after the
adapter accepts them; empty updates are not spuriously committed, and uncertain
runtime application latches a restart requirement instead of advancing state.

Opted-in launches **and attachments**, including selected VSTest debugging from
its process buffer, use the private fork. Unrelated projects retain their
original adapter and environment. Source-aware stepping calls
`sharplsp/statementStop`, skips proven brace-only stops, coalesces same-statement
sequence points, and preserves the first real statement on a brace's line.
Escapes from structural-origin frames, errors, unknown results, exhausted
budgets, exceptions and real breakpoints remain visible; stale replies cannot
issue another step.

### Verification

All **91 regression tests passed in actual `emacs -nw`**, with native-module
assertions enabled. Isolated terminal runs checked regression tests,
real-project/library navigation, development workflows, profiling, NuGet and
refactoring tools, target-framework switching, manual and save-triggered DAP hot
reload, Razor, C# navigation/stepping/highlighting, and selected-test debugging.
Read-only verification on `concept_coach` followed
`Identity.fs → System.Guid → ISpanFormattable → IFormattable`, including the
SharpLsp-to-Roslyn handoff and preserved origin. Private mixed-language fixtures
verified C# hover, completion, references, rename edits, code actions, symbols,
call signatures, F# curried signature fallback, both formatters, both REPLs
(evaluating `42`), the configured runner, one selected xUnit test and netcoredbg
stopping at entry and continuing to termination.

The strict cross-language test now requires the actual F# source file and
SharpLsp ownership; a metadata answer fails the test. CPU verification captured
real samples and validated all three export files. Heap verification captured
live objects, computed a diff, walked references and performed GC. Native NuGet
verification searched, installed, listed and removed a package in a private
project; sorting and rename edits were actually applied. The framework chooser
switched a multi-target project from net9.0 to net10.0. DAP hot reload changed a
running program's result from `42` to `43` without rebuilding or restarting it.
The save-triggered check also started/resumed its baseline automatically and
changed a continuously running, sleeping process from `42` to `43`. Selected
xUnit debugging attached the private fork, hit the actual test-source breakpoint,
and passed after continuing. C# stepping elided real brace stops; `using System`
opened owned Roslyn metadata. Both `.razor` and `.cshtml` passed embedded C#
hover/definition, active semantic-highlighting mode, projected HTML completion
and formatting checks. These are demonstrated workflows, not an assertion that
every possible C# feature or service has been exhaustively certified.

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
  -l "$PWD/test/core-backup-test.el" \
  -l "$PWD/test/core-tools-test.el" \
  -l "$PWD/test/core-terminal-keyboard-test.el" \
  -l "$PWD/test/lang-dotnet-test.el" \
  -l "$PWD/test/lang-scala-test.el" \
  -l "$PWD/test/lang-sharplsp-test.el" \
  -f ert-run-tests-batch-and-exit
```

The tooling tests require the installed Pilish Markdown grammars. They do not
send model prompts or consume API credits.
