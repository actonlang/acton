# Derived modules

A derived module is a module of a project that has no file under `src/`. An
Acton function returns its source, and `acton build` calls the function
when the module is out of date. Other modules import a derived module like
any other module.

Use a derived module for source that follows from other files: classes that
are generated from a schema, tables that are computed from data files, or
files that are embedded in the program.

## The table in Build.act

The table `derived_modules` in `Build.act` lists the derived modules of a
project. The table below has one entry, for the module `schema`:

```python
derived_modules = {
    schema: (
        function=gen.schema,
        inputs=["data/schema.txt"],
        args=(prefix="S")
    )
}
```

- The name to the left of the colon is the name of the module, written as
  an `import` statement writes it. `schema` is the module that
  `src/schema.act` would be, and `model.schema` the module that
  `src/model/schema.act` would be.
- `function` is a module name followed by the name of a function in that
  module. The module name is written as an `import` statement in the
  project writes it. `gen.schema` is the function `schema` in the module
  `gen` of the project. `tools.gen.schema` is the function `schema` in the
  module `gen` of the dependency `tools`. The function of an entry cannot
  be in a derived module of the same project.
- `inputs` lists the input files of the function, as paths relative to
  the project root. A path names a file or a directory inside the project,
  and a directory stands for the files below it:
  `inputs=["data/schema.txt", "data/types/"]` names the file
  `data/schema.txt` and the files below `data/types/`. The section
  [Input paths](#input-paths) has the rules. `inputs` can be left out.
- `args` holds arguments for the function, written as the keyword
  arguments of a call, with commas between them. A name is the name of
  a parameter of the function. A value is a literal: a string, a number,
  `True`, `False`, `None`, a list, or a dictionary with strings as keys.
  The function gets the keys of a dictionary in sorted order, not in the
  order in which they are written. A number whose fraction is zero is
  passed as an integer: `2.0` is passed as `2`. A parameter of type
  `float` takes an integer too. `args` can be left out.

The module name and the value of `function` are written as names, without
quotes. Each path in `inputs` is a string.

A module that the table lists cannot have a file under `src/`, and the
table cannot list a module twice. With a dependency named `tools`, the
table cannot list the module `tools` or a module such as `tools.schema`:
an `import` statement in the project takes these names for modules of the
dependency. A build stops in each of these cases.

## Input paths

A path in `inputs` is written with `/` between its components. A build
stops with a message for a path that breaks one of these rules:

- The path is relative and has no `.` or `..` component, so it stays
  inside the project.
- Its first component is not `out`. A build writes the directory `out/`,
  so a file there is no input of a build.
- It names a file or a directory.
- Each component is spelled as its directory lists it. With a directory
  `data` in the project, `inputs=["Data/"]` is an error, also on a file
  system that finds `data` under the name `Data`. The same `Build.act`
  then builds on every file system.

A path that names a directory stands for every file below it, in the
directories below it too, with three exceptions:

- a file whose name starts with a dot, such as `.DS_Store`,
- a file below a directory whose name starts with a dot, such as `.git/`,
- a `--tydb` copy. A build with `--tydb` writes a copy of the interface
  of a module beside its source file, as a directory with the extension
  `.tydb`. A file in a directory with that extension, and a file with
  that extension, is no input file.

A file of these three kinds is no input file: the function does not get
it, and a change to it runs no function. Every other file is an input
file, also a backup file such as `app.js~`. A path that is written in
`inputs` is taken as it is written: `inputs=["web/.well-known/"]` names
the files below `web/.well-known/`.

A build stops with a message when an input file cannot be read, when it
is not a regular file, or when its name has bytes that are not text in
the encoding of the locale. Below a directory, a symbolic link that names
no file is an input file that cannot be read.

## The function

The function of the entry above is an ordinary function in `src/gen.act`:

```python
import std.build.derive as derive

def schema(inputs: dict[str, derive.InputFile], prefix: str) -> str:
    lines = []
    for name in inputs["data/schema.txt"].read().decode().splitlines():
        lines.append("class " + prefix + name + "(object):")
        lines.append("    def __init__(self):")
        lines.append('        self.kind = "' + name + '"')
    return "\n".join(lines) + "\n"
```

With the lines `Device` and `Link` in `data/schema.txt`, the module `schema`
has the classes `SDevice` and `SLink`. A module of the project imports
`schema` and uses the class `SDevice`:

```python
import schema

actor main(env):
    proc def __startup__():
        print(schema.SDevice().kind)
        env.exit(0)
```

The first parameter is a dictionary from the path of each input file to a
description of the file, an `InputFile` of the module `std.build.derive`.
A path is relative to the project root, with `/` between the components,
and the dictionary holds the paths in sorted order. The result is the
source of the module.

Write the type of the first parameter, `dict[str, derive.InputFile]`.
Without it the compiler infers a type from the body of the function. For
a function that calls `read()` the inferred type does not fit, and the
build stops at the call.

A description has three attributes and one method:

- `path` is the path of the file, as in the dictionary.
- `size` is the size of the content in bytes.
- `sha256` is the SHA-256 of the content, as 64 lowercase hexadecimal
  digits.
- `read()` returns the content as `bytes`. The text of a file is
  `read().decode()`.

A description holds no content. The function reads a file when it calls
`read()`, and a function that works from the paths, the sizes and the
SHA-256 values alone reads no file. `read()` raises `ValueError` when the
content of the file is not the content that the description is of, which
is so when the file changed while the function ran.

The other parameters take the arguments of the entry, by name: `prefix`
gets `"S"`. A parameter with a default value needs no argument. The build
calls the function as

```python
gen.schema(inputs, prefix="S")
```

and the compiler checks that call. A build stops when an argument has
another type than its parameter, when an argument has no parameter, or
when a parameter without a default value has no argument.

The function gets no `Env`, so it cannot connect to a host or start a
process, and the input files are the only files that it can read. It can
create actors and call them, and they get no `Env` either. Everything the
function reads comes from its arguments and from the modules it imports.
That is how the build knows when the function has to run again. The
section [In a build](#in-a-build) lists the cases.

The module of the function can import modules of the standard library,
modules of a dependency, and modules of the project that have a file under
`src/`. A module of a dependency can be a derived module of that
dependency. A build stops when the module of the function imports a
derived module of the project, directly or through other modules.

A library that `libraries` in `Build.act` declares is compiled as a whole:
when it holds the module of the function, or a module that this module
imports, a build compiles every module of the library before the function
runs. Keep the derived modules of the project, and the modules that import
them, out of such a library. Otherwise a derived module is compiled as the
last build left it, before the function runs, and the build stops when its
source is missing or has an error.

A function reports an error by raising an exception. The build then stops
and shows the exception. With `raise ValueError("data/schema.txt is empty")`
in the function `gen.schema`, the message is:

```console
The function gen.schema of the derived module schema in /home/user/app/Build.act failed:
ValueError: data/schema.txt is empty
```

What the function printed before the exception is shown after the first
line of this message. When the function returns without an exception, what
it printed is not shown.

## A test of the function

A test calls the function with descriptions of content that the test makes
up. `derive.input_of` takes a path and the content, and returns the
description of a file with that path and that content. No file is read.
The test module `src/test_gen.act` tests the function `gen.schema`:

```python
import std.build.derive as derive
import testing

import gen

def _test_schema():
    inputs = {"data/schema.txt": derive.input_of("data/schema.txt", b"Device\nLink\n")}
    source = gen.schema(inputs, prefix="S")
    testing.assertIn("class SLink(object):", source)
```

## Several modules from one function

A function can return several modules. It then returns a dictionary from
module name to source. Its entry in `Build.act` names every one of these
modules: to the left of the colon it has their names in braces, with
commas between them. The names can be written on one line or on several.
A command that writes `Build.act`, such as `acton pkg add`, writes them
as below: one name per line, in sorted order.

In `Build.act`:

```python
derived_modules = {
    {
        layers.base,
        layers.top,
        tables
    }: (
        function=gen.modules,
        inputs=["data/layers/"]
    )
}
```

The function, in `src/gen.act`:

```python
import std.build.derive as derive

def modules(inputs: dict[str, derive.InputFile]) -> dict[str, str]:
    res = {"tables": "names = []\n"}
    for path, f in inputs.items():
        name = path.split("/")[-1].split(".")[0]
        res["layers." + name] = "size = " + str(f.size) + "\n"
    return res
```

With `base.txt` and `top.txt` as the only files below `data/layers/`, the
function returns the modules `tables`, `layers.base` and `layers.top`. It
takes the size of each file from the description and reads no file.

The names of an entry are exactly the modules that its function returns. A
name stands for one module: `layers` does not stand for `layers.base` and
`layers.top`. A build stops when the function returns a module that its
entry does not name, or does not return a module that its entry names.
With a file `data/layers/extra.txt` in place of `data/layers/top.txt`, the
build stops with this message:

```console
The function gen.modules of the derived module layers.base in /home/user/app/Build.act returned other modules than its entry names.
Returned and not named: layers.extra
Named and not returned: layers.top
An entry names exactly the modules that its function returns. Change the names of the entry or the function.
```

The message calls the entry `the derived module layers.base`. Where a
message of the build says `the derived module` and a name, the name is the
first of the names of an entry in sorted order.

The order of the names has no meaning. A name that is written twice is an
error.

The result type of a function is `str` for one module or `dict[str, str]`
for several.

## In a build

`acton build`, `acton test` and a build of a single file bring the derived
modules up to date before they compile the project. The derived modules of
a dependency are brought up to date in the same build, before those of the
project that depends on it. While another build holds the compiler lock of
the dependency, the build prints `Waiting for compiler lock in` and the
directory of the dependency, and waits.

The source of a derived module is written to `out/derived/src/`, at the
path that the module would have under `src/`: `out/derived/src/schema.act`
for the module `schema`. Do not commit this file, and do not edit it: a
build that finds the file edited runs the function and writes the file
again. An error in a derived module points to this file, and a note after
the error names the function that returned the source. A build removes
every file under `out/derived/src/` that no entry names, except a `--tydb`
copy, and what was compiled from a module that no entry names any more.

The derived modules of an entry are out of date when the function has not
run for the entry yet, or when one of these changed since the function last
ran:

- an input file: its content changed, or a file was added or removed below
  an input path that names a directory. This holds for every input file,
  also for one that the function does not read: its description holds the
  SHA-256 of its content,
- the entry in `Build.act`: its module names, its function, its input
  paths or its arguments,
- the function, or anything that the function uses, in any module,
- the compiler version,
- the file of one of these modules under `out/derived/src/`: it was edited
  or removed.

A change to other code leaves the modules up to date, even code in the
function's own module: another function, a constant that the function does
not read, or a comment. With `--verbose`, a build says for each entry
whether it is up to date, and the reason when it is not.

A build keeps a record of each entry, with the SHA-256 of the content of
each input file, and metadata of the file such as its size and its
modification time. To tell whether an input file changed, a later build
first compares the metadata. When the metadata is the same and the file
was last modified before the record was written, the build takes the file
as unchanged and does not read it, whatever its size. Otherwise the build
reads the file and compares the SHA-256, so a file that was written again
with the same content does not run the function. The build then records
the new metadata, so that a later build does not read the file again. The
description of an input file holds this size and this SHA-256, and the
function reads the file only when it calls `read()`.

A build stops when an input file changes while the build runs:

```console
data/schema.txt changed while the build ran. Run the build again
```

The build looks at each input file before it runs the function and again
when the function has returned. A file that is not the same file both
times stops the build before a module of the entry is written. So a
module is never made from one version of a file and recorded with
another. A function that reads the changed file stops earlier, with the
`ValueError` of `read()`.

A line in the build log shows each function that ran:

```console
   schema  Derived by gen.schema                                                   0.412 s
```

The line of an entry with several modules starts with the first of its
names in sorted order, and says how many modules the function returned:

```console
   layers.base  Derived 3 modules by gen.modules                                        0.538 s
```

To call the function of an entry, a build writes a program under
`out/derived/runner/` and builds it. A later build runs the program
without building it again when only an input file changed. It builds the
program again when the function, anything that the function uses, an
argument of the entry or the compiler version changed. The time on the
line of an entry is the time to run the program. In a build that built
the program, it is the time to build the program and to run it.

`acton build --watch` and `acton test --watch` also build when an input
file is saved, added or removed: a file that `inputs` names, or a file
below a directory in `inputs` that the directory stands for. They watch
the input paths of the project and of its local dependencies, and read
them from the `Build.act` files again when a `Build.act` changes. A file
that no input path stands for, such as `web/.DS_Store`, starts no build.
A change of an input file starts a build of the whole project, and
`acton test --watch` then runs every selected test whose cached result no
longer applies. After a build that a saved source file started,
`acton test --watch` runs the tests of the modules that changed and of the
modules that import them, directly or through other modules. A derived
module is among the modules that changed when its source is not the
source that the tests last ran with.

`acton build --skip-build` and `acton test list` run no function: they use
the derived modules when they are up to date and fail otherwise.

`acton sig` runs no function and does not look at the input files. It
shows a derived module as the last build wrote it, and it does not know a
derived module that no build has written.

The language server runs no function either. It reads a derived module as
the last build wrote it.

`acton test` looks for tests in the modules that have a file under `src/`.
It does not list or run a test that is in a derived module.
