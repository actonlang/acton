# Compiler

The compiler runs a fixed sequence of AST-to-AST transforms before finally
generating C.  The main entry point is `compiler/acton/Main.hs`, which wires
the stages together via the `compiler/lib` modules listed below. All the real
logic are contained in these modules and are thus accessible and usable as a
library.

The same AST data type is used throughout all compiler passes, making it easy to
read and understand what each transformation does.

See [Passes](passes/index.md) for the stage-by-stage pipeline and how front and
back passes split the work.
