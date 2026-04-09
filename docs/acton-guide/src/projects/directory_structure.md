# Project directory structure

The directory layout of an Acton project follows a certain structure.

```
.
├── Build.act
├── out
│   ├── bin
│   │   └── foo
│   └── types
│       ├── foo.c
│       ├── foo.h
│       ├── foo.root.c
│       └── foo.ty
├── README.md
└── src
    └── foo.act
```

A `Build.act` file and the `src/` directory must be present in the project
root, otherwise it is not considered a project.

`src/` is used for all source files of the project. Use subdirectories to create
hierarchies of modules, for example `src/a/b.act` is imported in an Acton
program as `import a.b`.

Output goes into `out/`, most importantly, executable binaries are placed in
`out/bin`. The name of the binary is the name of the module which contains the
specified root actor.  In the example above, the root actor is `foo.main`, i.e.
actor `main` in the module `foo` and consequently, the executable name is `foo`.

`out/types` contains internal compiler output. In particular, `.ty` files are
cached typed-module interfaces and `.c` / `.h` files are generated code derived
from them. Acton validates and regenerates these files as needed, so they
should be treated as internal build artifacts. Don't touch them. All other
output files, like object files and archives, are placed out-of-tree in the
cache directory (`~/.cache/acton/`).
