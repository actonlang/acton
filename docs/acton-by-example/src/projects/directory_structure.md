# Project directory structure

The directory structure of an Acton project follows a certain convention.

```
.
├── Acton.toml
├── build.sh
├── out
│   ├── dev
│   │   ├── bin
│   │   │   └── foo
│   │   └── lib
│   │       ├── foo.o
│   │       └── libActonProject.a
│   ├── rel
│   │   ├── bin
│   │   │   └── foo
│   │   └── lib
│   │       ├── foo.o
│   │       └── libActonProject.a
│   └── types
│       ├── foo.c
│       ├── foo.h
│       ├── foo.root.c
│       └── foo.ty
├── README.org
└── src
    └── foo.act
```

An `Acton.toml` file must be present in the project root, otherwise it is not
considered a project.

`src/` is used for all source files of the project. Use subdirectories to create
hierarchies of modules, for example `src/a/b.act` is imported in an Acton
program as `import a.b`.

Output goes into `out/`. A project archive file (`libActonProject.a`) contains
the compiled output of all modules, but is considered an internal implementation
detail. Don't touch it.

Executable binaries go in `out/bin`. The name of the binary is the name of the
module which contains the specified root actor.  In the example above, the root
actor is `foo.main`, i.e. actor `main` in the module `foo` and consequently, the
executable name is `foo`.
