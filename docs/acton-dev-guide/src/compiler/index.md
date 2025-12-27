# Compiler

The compiler runs a fixed sequence of AST-to-AST transforms before finally
generating C.  The main entry point is `compiler/actonc/Main.hs`, which wires
the stages together via the `compiler/lib` modules listed below. All the real
logic are contained in these modules and are thus accessible and usable as a
library.

The same AST data type is used throughout all compiler passes, making it easy to
read and understand what each transformation does.

## Stages (in order)

| Stage | Module | Description |
| --- | --- | --- |
| [Parse](passes/parse.md) | `compiler/lib/src/Acton/Parser.hs` | |
| [Kinds check](passes/kinds.md) | `compiler/lib/src/Acton/Kinds.hs` | |
| [Type check](passes/type_check.md) | `compiler/lib/src/Acton/Types.hs`, `compiler/lib/src/Acton/TypeEnv.hs` | |
| [Normalize](passes/normalize.md) | `compiler/lib/src/Acton/Normalizer.hs` | |
| [Deactorize](passes/deactorize.md) | `compiler/lib/src/Acton/Deactorizer.hs` | |
| [CPS](passes/cps.md) | `compiler/lib/src/Acton/CPS.hs` | |
| [Lambda lift](passes/lambda_lift.md) | `compiler/lib/src/Acton/LambdaLifter.hs` | |
| [Boxing](passes/boxing.md) | `compiler/lib/src/Acton/Boxing.hs` | |
| [Codegen](passes/codegen.md) | `compiler/lib/src/Acton/CodeGen.hs` | |

The final C source is compiled and linked by the Zig build system.
