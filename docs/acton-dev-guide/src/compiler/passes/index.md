# Passes

The compiler runs a fixed sequence of AST-to-AST transforms before finally
generating C. The main entry point is `compiler/actonc/Main.hs`, which wires
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

## Front vs back passes

The pipeline is split into a front end and a back end. The scheduler runs all
front passes across the dependency graph first, then queues back-pass work once
type checking succeeds for a module.

Front passes (1–3) are:
- Parse
- Kinds check
- Type check

These passes produce all user-facing diagnostics, write the module interface
header (`.ty`), and compute public hashes used for incremental rebuilds. They
also define the cross-module dependency edges: a module can only start once its
imports have completed the front passes.

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

The shared orchestration lives in `compiler/lib/src/Acton/Compile.hs` and is
used by both `actonc` and the LSP server.
