# Acton Projects

Besides compiling individual `.act` files, it is possible to organize Acton code into an **Acton Project**, which is suitable once you have more than one `.act` source code file.

Use `acton` to create a new project called `foo`:
```console
acton new foo
```

Output:
```console
Created project foo
Enter your new project directory with:
  cd foo
Compile:
  acton build
Run:
  ./out/bin/foo
```

## Description

Use `acton build` to build a project. The current working directory
must be the project directory or a sub-directory to the project
directory. `acton` will discover all source files and compile them
according to dependency order.

Add a `main` actor to any source file directly under `src/` to produce an executable binary. For example, if `src/hello.act` contains a `main` actor, it will produce `out/bin/hello` using `main` as the root actor.

Projects and modules work together: `src/` defines the local module
tree, while `Build.act` defines the project identity and its external
dependencies. See [Modules](modules.md) for local source layout and
[Package Management](package_management.md) for remote dependencies and
override behavior.

When another project depends on this project, the dependency name in the
consuming project's `Build.act` becomes the import prefix. A dependency
named `foo` exposes this project's `src/lib.act` root module as
`import foo`, and exposes `src/parser.act` as `import foo.parser`.
Currently, the dependency name must match this project's `name`.

## Build configuration and lineage

Projects must include a `Build.act` file. Two common fields are `name` and `fingerprint`, where the fingerprint captures the project’s **lineage**:

```python
name = "hello"
fingerprint = 0x1234abcd5678ef00
```

`name` and `fingerprint` are required for Acton projects. Acton validates that the fingerprint matches the name’s lineage prefix. A mismatch indicates a rename or a fork, so the build fails and tells you to generate a new fingerprint for the new name. If either field is missing, the build fails with guidance to add it.

## Application build options

`build_options` is an optional dictionary of string names and string values in
`Build.act`. It supplies options to the application's Zig build, with each entry
passed as `-Dname=value`. Values are literal arguments, not Zig expressions.
An unknown option or a value of the wrong type fails the build.

The root application's options govern its build, including test executables.
Options in a dependency's `Build.act` apply when building that dependency as a
project itself; they do not override the consuming application's choices.
Changing options rebuilds the affected artifacts without requiring a clean and
invalidates cached test results. Performance recordings retain the selected
options so comparisons can measure configuration changes.

Compiler options such as target, CPU, optimization, database support and
threading remain controlled by their existing command-line flags. Their Zig
option names (`target`, `cpu`, `ofmt`, `dynamic-linker`, `optimize`, `db`, `no_threads`,
`cpedantic` and names beginning with `acton_`) are reserved and cannot appear in `build_options`.
