# REPL Implementation

The REPL implementation lives in `compiler/acton/Repl.hs`. The command-line
parser maps `acton repl` to `C.Repl`, and `Main.main` dispatches that command
to `Repl.runRepl` with a small hook record for compiler-driver operations.

The design is deliberately compiler-backed. The REPL does not maintain a live
Acton VM and does not evaluate AST nodes directly. Instead it owns a small
scratch Acton project, rewrites generated source files for each accepted input,
compiles the changed module, and runs a stable executable runner. This keeps
the REPL behavior close to ordinary compilation at the cost of compile latency.

## Generated project

`runRepl` creates a `ReplContext` with paths for a generated project:

- `src/repl_session.act`
- `src/repl_eval.act`
- `src/repl_main.act`
- `out/bin/repl_main`

`renderReplBuildAct` writes a `Build.act` that declares `repl_session` and
`repl_eval` as dynamic libraries. `repl_main` links against those libraries and
is the executable run after each successful evaluation.

The generated modules have fixed roles:

- `repl_session` contains retained imports and declarations.
- `repl_eval` renders retained imports, imports `repl_session`, and exposes one
  stable entry point, `proc def eval()`.
- `repl_main` renders retained imports, imports `repl_eval`, calls
  `repl_eval.eval()`, reports failure by exiting non-zero, and otherwise exits
  successfully.

The fixed `eval` symbol is the ABI between the runner executable and the
current evaluation module. User input can change the body of `eval`, but not the
entry point that `repl_main` calls.

## State model

`ReplState` is the in-memory source model for the current interactive session:

- `replImports` stores retained import source blocks.
- `replDecls` stores retained declaration source blocks plus their top-level
  names.
- `replReplay` stores accepted top-level assignment blocks.
- `replDeps` stores online dependencies added with `:dep`.
- `replUseProject` records whether the user's containing project should be
  added as a path dependency to the scratch project.

Retained declarations are replaced by top-level name. If a user enters a new
definition for `foo`, `replaceReplDecl` removes older retained declaration
blocks that provided `foo` before appending the new block. This avoids duplicate
C symbols after redefinition.

Assignments are not retained as module-level source. They are replayed inside
`repl_eval.eval()` before the current input. This is why setup such as
`f = Foo()` can run actor construction or other effectful code without placing
effectful statements at Acton module top level.

When an accepted assignment fails at runtime, `acceptReplTop` does not move to
the new `ReplState`. The source line is therefore not replayed by later inputs.

## Input classification

The REPL classifies input with the normal Acton parser, not by string matching.
`classifyReplInput` first tries to parse the input as a module-shaped top-level
chunk:

- imports become `ReplImport`
- signatures and declarations become `ReplDeclTop`
- assignment-only chunks become `ReplReplay`

If the module parse does not produce one of those accepted top-level shapes, the
REPL parses the input as an assignment-right-hand-side expression. A successful
expression parse becomes `ReplExpr`; otherwise the input becomes `ReplStmt` and
is inserted directly into `eval`.

`ReplExpr` does not try to classify effectful calls. It renders the expression
through a local result binding:

```python
__repl_value = EXPR
if __repl_value is not None:
    print(__repl_value)
```

This keeps display behavior in ordinary Acton code instead of maintaining a
parallel type or object model in the REPL. Expressions that produce a value are
shown. Calls that return `None`, including `print(...)` and `proc def` calls,
run without printing an extra `None`.

## Compile flow

Runner creation and evaluation use three paths:

- `compileReplRunnerNow` writes all generated sources and builds
  `repl_main`.
- `compileReplSession` rewrites `repl_session` and, after the startup runner
  build has succeeded, rebuilds only that source.
- `compileReplEval` rewrites `repl_eval`, rebuilds `repl_session` first if the
  rendered session source changed, and then rebuilds only `repl_eval`.

`compileReplFiles` calls the compiler-driver hook supplied by `Main`. That hook
wraps `compileFilesChanged` with the scratch project root.

By default, the REPL uses the normal compiler/Zig path for each changed module.
This keeps behavior simple and avoids managing a long-lived Zig process.

If `ACTON_REPL_ZIG_WATCH` is set to `1`, `true`, or `yes` in an interactive
terminal, `compileReplRunnerNow` starts a long-lived `zig build --watch`
process after the runner build succeeds. Later session and evaluation compiles
run the Acton compiler with `--skip-build`: they update the generated `.c` and
`.h` files, then wait briefly for the Zig watcher to rebuild the affected
dynamic library. The watcher is started in its own process group so shutdown
can terminate Zig's build runner and any child compiler processes together.

For non-interactive REPL runs, including tests and piped input, the watcher is
not started even if the environment variable is set.

Accepted imports rebuild the runner. The runner needs the retained imports so
symbols referenced from the dynamic evaluation library are resolvable by the
process that loads it.

`startReplWarmup` compiles a placeholder runner in the background as soon as the
REPL starts. The first user input waits for that warmup through
`waitReplWarmup` before it edits or compiles the generated sources. If the
warmup succeeds, later expression evaluations can use the single-module fast
path. If the watcher is enabled and does not rebuild the expected dynamic
library in time, the REPL stops it, falls back to the normal targeted module
build, and restarts the watcher.

The REPL does not inspect platform-specific dynamic-library artifacts to decide
whether a cached runner is valid. It relies on the normal compiler and Zig build
paths to decide which generated outputs are stale.

## Scratch directories

Without `--tempdir`, the REPL uses a persistent cache directory under
`actonCacheDir/repl`. The key is either `global` for standalone REPLs or a
fingerprint-derived key for a containing project. That lets a later `acton repl`
start by running the normal placeholder runner build against existing scratch
outputs while still beginning with an empty in-memory session.

With `--tempdir DIR`, `DIR` is treated as a parent directory. The generated
project is written under `DIR/.acton-repl` so a command such as
`acton repl --tempdir .` does not overwrite a real project's `Build.act` or
`src` tree.

## Dependencies

`:dep add NAME` uses `PkgCommands.resolveLibraryDependency`, renders a scratch
`Build.act` with the updated dependency set, and immediately rebuilds the
runner. The dependency is retained in `replDeps` only if that compile succeeds.

Imports also matter for local project access. `runRepl` detects whether the
current directory is inside an Acton project, but the scratch project only adds
that project dependency once the session has accepted an import. This is a
coarse activation point; the import may be from the project or from another
dependency.

## Compile options

`normalizeReplCompileOptions` clears compiler inspection and build-shape flags
that do not make sense for the generated runner, such as parse dumps, signature
dumps, `--skip-build`, `--only-build`, watch mode, and test mode. It also fixes
the root actor to `repl_main.main`.

This normalization is part of the REPL runtime contract: user-supplied compile
options may still affect target and optimization choices, but they must not
turn an evaluation into a parser dump or suppress the runner build.

## Shell behavior

The interactive shell is Haskeline-based. It keeps command history in the Acton
cache directory, enables interrupt handling, and exposes completion for REPL
commands and `:dep` subcommands. File-path completion is intentionally not
enabled.

Continuation reading is simple: a line ending in `:` enters continuation mode,
and a blank continuation line ends the block. This is easy to reason about but
does not try to be a full paste parser for every multiline expression shape.

## Tests

The focused tests are in `compiler/acton/test.hs` under the compiler test group.
They cover:

- retained functions, classes, proc functions, actors, and proc methods
- top-level assignment replay inside `eval`
- declaration redefinition removing old source
- `:reset` clearing generated session source
- persistent scratch reuse starting from an empty in-memory session
- explicit `--tempdir` writing under `.acton-repl`
- rejection of failing setup assignments

Use this focused command while working on REPL behavior:

```console
stack test acton --ta '-p "repl"'
```
