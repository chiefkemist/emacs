#!/usr/bin/env bash
# Build the complete, pinned SharpLsp distribution in Emacs-owned directories.
# No global tool installation, terminal preferences, or OS settings are changed.
set -euo pipefail

EMACS_ROOT=$(cd "$(dirname "$0")/.." && pwd -P)
DEST=${1:-"$EMACS_ROOT/var/sharplsp"}
BUILD="$EMACS_ROOT/var/sharplsp-build"
SOURCE="$BUILD/source"
PATCH="$EMACS_ROOT/scripts/sharplsp-baseline.patch"
REV=e26a92f9a4c9396ccbdd6ab979dc4785c3ae686d
SDK=10.0.303
DBG_REV=2de84af51779449d34fb5597a438660ea77756bb
CLR_REV=ea346eaeda73d7ef1cc3b148939ac83729cc38dc

for tool in git cargo npm node curl tar cmake ninja clang clang++; do
    command -v "$tool" >/dev/null || { printf 'Required build tool missing: %s\n' "$tool" >&2; exit 1; }
done
export DOTNET_CLI_HOME="$BUILD/cli-home"
export DOTNET_CLI_TELEMETRY_OPTOUT=1
export DOTNET_GENERATE_ASPNET_CERTIFICATE=false
export DOTNET_ADD_GLOBAL_TOOLS_TO_PATH=false
mkdir -p "$BUILD" "$DOTNET_CLI_HOME" "$(dirname "$DEST")"
DEST="$(cd "$(dirname "$DEST")" && pwd -P)/$(basename "$DEST")"
if [[ ! -d "$SOURCE/.git" ]]; then
    git init "$SOURCE"
    git -C "$SOURCE" remote add origin https://github.com/Nimblesite/SharpLsp.git
fi
# A prior build may contain exactly our recorded patch, never arbitrary edits.
if git -C "$SOURCE" rev-parse --verify HEAD >/dev/null 2>&1 &&
   ! git -C "$SOURCE" diff --quiet HEAD; then
    if git -C "$SOURCE" diff --cached --quiet &&
       git -C "$SOURCE" -c core.abbrev=7 diff --no-color --no-ext-diff --no-renames \
           --src-prefix=a/ --dst-prefix=b/ --unified=3 --diff-algorithm=myers --binary HEAD | cmp -s - "$PATCH"; then
        git -C "$SOURCE" apply --reverse "$PATCH"
    else
        printf 'Refusing to overwrite modified build sources: %s\n' "$SOURCE" >&2
        exit 1
    fi
fi
if [[ "$(git -C "$SOURCE" rev-parse HEAD 2>/dev/null || true)" != "$REV" ]]; then
    if [[ -n "$(git -C "$SOURCE" status --porcelain --untracked-files=no)" ]]; then
        printf 'Refusing to overwrite modified build sources: %s\n' "$SOURCE" >&2
        exit 1
    fi
    git -C "$SOURCE" fetch --depth 1 origin "$REV"
    git -C "$SOURCE" checkout --detach "$REV"
fi
git -C "$SOURCE" submodule update --init --depth 1 lspkit
if [[ -n "$(git -C "$SOURCE" status --porcelain --untracked-files=no)" ]] ||
   [[ -n "$(git -C "$SOURCE/lspkit" status --porcelain --untracked-files=no)" ]]; then
    printf 'Refusing to build modified sources under a pinned manifest.\n' >&2
    exit 1
fi
git -C "$SOURCE" apply --check "$PATCH"
git -C "$SOURCE" apply "$PATCH"
if [[ ! -x "$BUILD/dotnet/dotnet" ]] || ! "$BUILD/dotnet/dotnet" --list-sdks | grep -q "^$SDK "; then
    curl --fail --location --silent --show-error https://dot.net/v1/dotnet-install.sh -o "$BUILD/dotnet-install.sh"
    bash "$BUILD/dotnet-install.sh" --version "$SDK" --install-dir "$BUILD/dotnet" --no-path
fi
npm install --prefix "$BUILD/generator" --no-audit --no-fund typediagram@0.11.0
export PATH="$BUILD/generator/node_modules/.bin:$PATH"
export CARGO_TARGET_DIR="$BUILD/target"
export DOTNET_ROOT="$BUILD/dotnet"
export DOTNET_ROLL_FORWARD=LatestMajor
export DOTNET_CLI_TELEMETRY_OPTOUT=1
cd "$SOURCE"
cargo build --release --locked -p sharplsp
# Sequential publishes: both sidecars share the Common project output.
"$DOTNET_ROOT/dotnet" publish src/sidecars/SharpLsp.Sidecar.CSharp/SharpLsp.Sidecar.CSharp.csproj \
    -c Release --no-self-contained -p:DebugType=none -p:DebugSymbols=false -o "$BUILD/sidecar-csharp"
"$DOTNET_ROOT/dotnet" publish src/sidecars/SharpLsp.Sidecar.FSharp/SharpLsp.Sidecar.FSharp.fsproj \
    -c Release --no-self-contained -p:DebugType=none -p:DebugSymbols=false -o "$BUILD/sidecar-fsharp"
"$DOTNET_ROOT/dotnet" publish lspkit/dotnet/LspProfiler.DotNet.Engine/LspProfiler.DotNet.Engine.csproj \
    -c Release --no-self-contained -p:DebugType=none -p:DebugSymbols=false -o "$BUILD/engine"

# Release adapters omit NCDB_DOTNET_STARTUP_HOOK: an asynchronous pause in
# Thread.Sleep then applies IL but fails its metadata-handler update. Build
# the same pinned fork WITH its managed safe-point hook, privately.
for component in netcoredbg runtime; do
    CHECKOUT="$BUILD/$component-source"
    if [[ "$component" == netcoredbg ]]; then
        ORIGIN=https://github.com/Nimblesite/netcoredbg.git; PIN=$DBG_REV
    else
        ORIGIN=https://github.com/dotnet/runtime.git; PIN=$CLR_REV
    fi
    if [[ ! -d "$CHECKOUT/.git" ]]; then
        git init --quiet "$CHECKOUT"
        git -C "$CHECKOUT" remote add origin "$ORIGIN"
    fi
    if [[ -n "$(git -C "$CHECKOUT" status --porcelain --untracked-files=no)" ]]; then
        printf 'Refusing to overwrite modified dependency: %s\n' "$CHECKOUT" >&2; exit 1
    fi
    if [[ "$(git -C "$CHECKOUT" rev-parse HEAD 2>/dev/null || true)" != "$PIN" ]]; then
        git -C "$CHECKOUT" fetch --depth 1 origin "$PIN"
        git -C "$CHECKOUT" checkout --detach "$PIN"
    fi
done
CC=clang CXX=clang++ cmake -S "$BUILD/netcoredbg-source" -B "$BUILD/netcoredbg-build" -G Ninja \
    -DCORECLR_DIR="$BUILD/runtime-source/src/coreclr" -DDOTNET_DIR="$DOTNET_ROOT" \
    -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX="$BUILD/netcoredbg-output" \
    -DNCDB_DOTNET_STARTUP_HOOK="$DEST/netcoredbg/ncdbhook.dll"
cmake --build "$BUILD/netcoredbg-build" --target netcoredbg --parallel 4
# Upstream managed targets have literal *.cs dependencies, invalid in Ninja;
# publish directly with separate obj/ trees instead of their shared one.
RID=$(node -p '`${({darwin:"osx",win32:"win"})[process.platform] || process.platform}-${process.arch}`')
"$DOTNET_ROOT/dotnet" publish "$BUILD/netcoredbg-source/src/managed/ManagedPart.csproj" \
    -r "$RID" --self-contained -c Release -o "$BUILD/netcoredbg-build/src" \
    -p:UseDbgShimDependency=true \
    -p:BaseIntermediateOutputPath="$BUILD/netcoredbg-build/managed-obj/" \
    -p:BaseOutputPath="$BUILD/netcoredbg-build/managed-bin/"
NCDB_HOOK_TargetFramework=net6.0 "$DOTNET_ROOT/dotnet" publish \
    "$BUILD/netcoredbg-source/src/ncdbhook/ncdbhook.csproj" -c Debug \
    -o "$BUILD/netcoredbg-build/src" \
    -p:BaseIntermediateOutputPath="$BUILD/netcoredbg-build/hook-obj/" \
    -p:BaseOutputPath="$BUILD/netcoredbg-build/hook-bin/"
cmake --install "$BUILD/netcoredbg-build"
EXE=
[[ "$OSTYPE" != msys* && "$OSTYPE" != cygwin* ]] || EXE=.exe
STAGE=$(mktemp -d "$(dirname "$DEST")/.sharplsp-stage.XXXXXX")
trap 'rm -rf "$STAGE"' EXIT
cp "$BUILD/target/release/sharplsp$EXE" "$STAGE/"
cp -R "$BUILD/sidecar-csharp" "$BUILD/sidecar-fsharp" "$STAGE/"
cp -R "$BUILD/engine/." "$STAGE/"
cp -R "$BUILD/netcoredbg-output" "$STAGE/netcoredbg"
[[ -x "$STAGE/sharplsp$EXE" && -x "$STAGE/lsp-profiler-dotnet-engine$EXE" &&
   -x "$STAGE/netcoredbg/netcoredbg$EXE" && -f "$STAGE/netcoredbg/ncdbhook.dll" ]] || {
    printf 'Incomplete build; existing installation was not changed.\n' >&2; exit 1;
}
"$STAGE/sharplsp$EXE" --version
"$STAGE/sidecar-csharp/SharpLsp.Sidecar.CSharp$EXE" --version
"$STAGE/sidecar-fsharp/SharpLsp.Sidecar.FSharp$EXE" --version
"$STAGE/netcoredbg/netcoredbg$EXE" --version
printf '{"source":"https://github.com/Nimblesite/SharpLsp","commit":"%s","lspkit":"%s","sdk":"%s","debuggerCommit":"%s","coreclrCommit":"%s","debuggerStartupHook":true,"baselinePatchSha256":"%s"}\n' \
    "$REV" "$(git -C lspkit rev-parse HEAD)" "$SDK" "$DBG_REV" "$CLR_REV" \
    "$(node -e 'const fs=require("fs"),c=require("crypto");process.stdout.write(c.createHash("sha256").update(fs.readFileSync(process.argv[1])).digest("hex"))' "$PATCH")" \
    > "$STAGE/emacs-build.json"
BACKUP=
if [[ -e "$DEST" ]]; then
    BACKUP="$DEST.previous.$(date +%s)"
    mv "$DEST" "$BACKUP"
fi
if ! mv "$STAGE" "$DEST"; then
    [[ -z "$BACKUP" ]] || mv "$BACKUP" "$DEST"
    exit 1
fi
trap - EXIT
printf 'Complete SharpLsp installed at %s\n' "$DEST"
[[ -z "$BACKUP" ]] || printf 'Previous distribution retained at %s\n' "$BACKUP"
