# Acton REPL

`acton repl` starts an interactive shell for trying Acton code without creating
a normal project by hand.

```console
$ acton repl
Welcome to InterActon, the interactive Acton REPL shell. Type :help for commands, :quit to exit.
acton> 1 + 2
3
acton> :quit
```

The REPL is still compiler-backed. It writes a small scratch project, compiles
the code you enter, and runs a small generated executable. This makes it slower
than a bytecode interpreter, but it uses the same parser, type checker, code
generator, and runtime as normal Acton programs.

## Session source

The REPL keeps a source model of the current interactive session.

Imports, signatures, functions, classes, actors, and other declarations are
retained as session source:

```python
acton> def greet(name: str) -> str:
...     return "hello " + name
...
acton> greet("Ada")
hello Ada
```

If you redefine a retained top-level name, the previous definition for that
name is removed from the session before the next compile:

```python
acton> def greet(name: str) -> str:
...     return "hi " + name
...
acton> greet("Ada")
hi Ada
```

Top-level assignments are handled differently. They are replayed as setup code
inside the implicit evaluation procedure before each evaluation, instead of
being emitted at module top level. This allows effectful setup, including actor
creation:

```python
acton> actor Foo():
...     print("hello from Foo")
...
acton> f = Foo()
hello from Foo
```

Use `:show` to inspect the retained session source and `:reset` to clear it:

```console
acton> :show
def greet(name: str) -> str:
    return "hi " + name

acton> :reset
Session reset.
acton> :show
<empty>
```

## Expressions and statements

Plain expressions are evaluated and non-`None` results are printed
automatically:

```python
acton> "ab" + "cd"
abcd
```

The generated `eval` procedure stores the expression result in a local variable
and prints it only when it is not `None`. Calls that already print or otherwise
return `None` therefore do not produce an extra `None` line:

```python
acton> proc def log():
...     print("logged")
...
acton> log()
logged
```

Inputs that are not a single expression compile as statement blocks and report
compiler errors in the usual way.

Actor construction returns an actor reference, so a direct constructor
expression displays that reference after starting the actor. Use a setup
assignment if you want to start the actor without displaying the reference:

```python
acton> f = Foo()
```

## Scratch project

By default, the REPL uses a cache-backed scratch directory so the generated
project outputs can be reused by the normal warmup build across REPL starts.
The scratch project contains three generated Acton modules:

- `repl_session` contains retained imports and declarations.
- `repl_eval` renders retained imports, imports `repl_session`, and defines a
  fixed `proc def eval()` entry point with setup replay and the current input.
- `repl_main` renders retained imports and is the executable runner that calls
  `repl_eval.eval()` and exits.

`repl_session` and `repl_eval` are built as dynamic libraries. Most evaluations
therefore rebuild only the changed evaluation module instead of relinking the
runner executable.

If you pass `--tempdir DIR`, Acton treats `DIR` as a parent directory and writes
the scratch project under `DIR/.acton-repl`. This avoids overwriting a real
project's `Build.act` or `src` directory.

## Dependencies and projects

In a standalone REPL, use `:dep add NAME` to add a library package from the
online package index:

```console
acton> :dep add textfsm
Added dependency textfsm
```

Use `:dep list` to show REPL dependencies and `:dep rm NAME` to remove one.
The REPL intentionally does not expose local path dependency commands.

When started inside an Acton project, the REPL adds that project as a local
scratch dependency after the first accepted import. The generated scratch
project stays outside the project source tree.

## Commands and editing

The interactive shell supports readline-style editing, including common cursor
movement keys such as `Ctrl-a` and `Ctrl-e`. Tab completion covers REPL commands
and `:dep` subcommands, not filesystem paths.

Available commands:

```console
:help          Show REPL help
:quit, :q      Exit
:reset         Clear retained imports, definitions, setup, and dependencies
:show          Show retained session source
:dep list      Show online package dependencies
:dep add NAME  Add an online package dependency
:dep rm NAME   Remove an online package dependency
```

For multi-line definitions, enter the header line ending in `:` and then
continue at the `...` prompt. A blank line ends the block.
