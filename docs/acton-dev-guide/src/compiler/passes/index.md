# Passes

The compiler runs a fixed sequence of AST-to-AST transforms before finally
generating C. The main entry point is `compiler/acton/Main.hs`, which wires
the stages together via the `compiler/lib` modules listed below. All the real
logic is contained in these modules and is thus accessible and usable as a
library.

The same AST data type is used throughout all compiler passes, making it easy to
read and understand what each transformation does.

## Stages (in order)

| Stage | Module | Description |
| --- | --- | --- |
| [Parse](parse.md) | `compiler/lib/src/Acton/Parser.hs` | |
| [Kinds check](kinds.md) | `compiler/lib/src/Acton/Kinds.hs` | |
| [Type check](type_check.md) | `compiler/lib/src/Acton/Types.hs`, `compiler/lib/src/Acton/TypeEnv.hs` | |
| [Normalize](normalize.md) | `compiler/lib/src/Acton/Normalizer.hs` | |
| [Deactorize](deactorize.md) | `compiler/lib/src/Acton/Deactorizer.hs` | |
| [CPS](cps.md) | `compiler/lib/src/Acton/CPS.hs` | |
| [Lambda lift](lambda_lift.md) | `compiler/lib/src/Acton/LambdaLifter.hs` | |
| [Boxing](boxing.md) | `compiler/lib/src/Acton/Boxing.hs` | |
| [Codegen](codegen.md) | `compiler/lib/src/Acton/CodeGen.hs` | |

The final C source is compiled and linked by the Zig build system.

For how imports are loaded before these passes run, see
[Imports and environments](../imports_and_envs.md).

## Front vs back passes

The pipeline is split into a front end and a back end. The scheduler runs front
passes across the dependency graph, then queues back-pass work once type
checking succeeds for a module. Whole-surface modules can queue their back job
immediately. Eligible selective modules retain only a reloadable deferred job
until all front passes needed by the global reachability closure have
completed.

Front passes (1–3) are:
- Parse
- Kinds check
- Type check

These passes produce all user-facing diagnostics, compute public hashes used
for incremental rebuilds, and commit the module interface cache (`.tydb`)
before reporting front completion. They also define the cross-module dependency
edges: a module can only start once its imports have completed the front passes.

Back passes (4–9) are:
- Normalize
- Deactorize
- CPS
- Lambda lift
- Boxing
- Codegen

The back end consumes the typed module from the front end and emits C and header
files (`.c`, `.h`). It does not introduce new user errors, so it can run after
front completion and even in the background. The CLI waits for all back jobs to
finish before invoking Zig; the LSP enqueues back jobs in the background and
does not wait for them or run Zig.

DBP still runs the same back-pass sequence once it is ready. After all required
front passes finish, one exact whole-program reachability closure starts at the
executable roots and the persisted whole summaries of ordinary whole surfaces,
then crosses module boundaries. Each selectively compiled module is materialized
from exact `.tydb` rows as a typed projection containing only the selected
top-level declarations, container methods and attributes, and their activated
initialization fragments. That projection enters normalization; no full typed
module is loaded and pruned afterward. Internal providers of a declared library
remain selective. Native `NotImplemented` modules and their provider closure
run whole back passes because hand-written C can contain hidden references.
Persistent `--db` builds also run whole because restored class ids are dynamic
roots. Dynamic `serialize` or `deserialize` interest makes the deferred batch
whole because runtime type names can escape the static reachability graph. A
matching projection-aware codegen hash lets the compiler skip the back job
entirely.

The shared orchestration lives in `compiler/lib/src/Acton/Compile.hs` and is
used by both `acton` and the LSP server.
