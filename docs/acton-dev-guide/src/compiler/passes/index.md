# Passes

The compiler runs a fixed sequence of AST-to-AST transforms before finally
generating C. Each stage is implemented in the `compiler/lib` modules listed
below, so the stages are usable as a library.

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
passes across the dependency graph, and a module whose front passes succeed gets
a back job. A module's back job is deferred until every module being compiled
that depends on it, directly or not, has completed its front passes, except for
the modules listed under [Deferred Back
Passes](../incremental_compilation.md#deferred-back-passes), which queue it as
soon as their own front passes finish.

Front passes (1–3) are:
- Parse
- Kinds check
- Type check

After type checking, the front stage also runs escape analysis
(`compiler/lib/src/Acton/EscapeAnalysis.hs`), which can fail the module, and
hashing (`compiler/lib/src/Acton/Hashing.hs`), which computes the hashes written
to the `.tydb`.

The front stage reports the diagnostics of a module's source, writes the module
interface cache (`.tydb`), and computes the hashes used for incremental
rebuilds. The front passes also define the cross-module dependency edges: a
module's front passes after parsing start only once its imports have completed
theirs, while its parse can run alongside them.

Back passes (4–9) are:
- Normalize
- Deactorize
- CPS
- Lambda lift
- Boxing
- Codegen

The back end consumes the typed module from the front end and emits C, header
and closure support files (`.c`, `.h`, `.closures.inc`). It does not introduce
new user errors, so it can run after front completion and even in the
background. The CLI waits for all back jobs to finish before invoking Zig. The
LSP server reports its checked modules first, then waits for its back jobs so
that it can report a back-pass failure, and does not run Zig.

DBP still runs the same back-pass sequence once it is ready. The difference is
that it may prune the typed module to the selected top-level declarations before
normalization, and it can skip the back job entirely when the selected DBP
codegen hash already matches the generated `.c`, `.h` and `.closures.inc` files.

The shared orchestration lives in `compiler/lib/src/Acton/Compile.hs` and is
used by both `acton` and the LSP server.
