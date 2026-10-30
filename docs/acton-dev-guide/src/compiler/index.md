# Compiler

The compiler runs a fixed sequence of AST-to-AST transforms before finally
generating C. `Acton.Compile` (`compiler/lib/src/Acton/Compile.hs`) wires the
stages together, and the `acton` CLI (`compiler/acton/Main.hs`) and the LSP
server (`compiler/lsp-server/Main.hs`) drive it. [Passes](passes/index.md)
lists the stages and their modules. The stages and `Acton.Compile` are
`compiler/lib` modules, so they are usable as a library.

The same AST data type is used throughout all compiler passes, making it easy to
read and understand what each transformation does.

See [Imports and environments](imports_and_envs.md) for how the scheduler,
`.tydb` headers, and the active type-checker environment fit together.

See [Interface caches](interface_caches.md) for the LMDB-backed on-disk cache
format used to store typed module interfaces.

See [Incremental compilation](incremental_compilation.md) for the hash model,
back-pass freshness checks, and deferred back pass scheduling.

See [Project dependencies](project_dependencies.md) for how Acton package
dependencies and zig dependencies are discovered, fetched, and emitted into the
generated Zig build files.

See [REPL implementation](repl.md) for how `acton repl` maps interactive input
onto generated Acton modules and dynamic-library builds.

See [Witness sharing](witnesses.md) for how the compiler shares protocol
witness constructions within a call and across a module.

See [Passes](passes/index.md) for the stage-by-stage pipeline and how front and
back passes split the work.
