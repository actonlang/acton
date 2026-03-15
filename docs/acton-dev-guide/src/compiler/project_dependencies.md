# Project Dependencies

Acton has two different dependency layers during a build:

- Acton package dependencies from `Build.act` `dependencies`
- Zig package dependencies from `Build.act` `zig_dependencies`

They are related, but they do not participate in the build in the same way.

## Discovery and fetch

Project discovery in `Acton.Compile` only follows Acton package dependencies.
Those edges determine the project graph used for module ordering, cache reuse,
import visibility, and type-check planning.

Zig dependencies are not Acton project edges. They do not contribute modules to
the import graph and they are not used for project discovery.

Fetch still needs them, though. For each reachable Acton project, the fetch
phase downloads or copies both that project's package dependencies and its zig
dependencies before later compile planning uses them.

## Generated `build.zig` inputs

`acton build` generates a `build.zig` and `build.zig.zon` for the current
project in `compiler/acton/Main.hs`.

At that point Acton combines:

- direct package dependencies of the current project
- transitive package dependencies reachable through package dependencies
- direct zig dependencies of the current project
- transitive zig dependencies from all reachable Acton projects

Transitive zig dependencies are flattened into the consuming project's generated
Zig manifest because the generated build may need to link their declared
artifacts directly.

### Example: one wrapper package

Conceptually, a root project might look like this:

```text
root
└── package dep "lmdb" -> acton-lmdb
    └── zig dep "lmdb" -> raw LMDB zig package
```

That means the root project imports an Acton package named `lmdb`, while that
package in turn links against a zig package also declared as `lmdb`.

The generated root `build.zig.zon` needs both of them as sibling entries, so
Acton emits separate local names:

```zig
.dependencies = .{
    .lmdb = .{ ... },             // Acton package dep
    .acton_zig_lmdb = .{ ... },   // underlying zig dep
},
```

The Acton package keeps the user-facing name `lmdb`. The zig package gets a
generated local alias because both entries live in the same Zig dependency
table.

## Local Zig names

Zig dependency names are only local aliases inside one generated
`build.zig.zon`. They are not global identities across the whole dependency
tree.

Acton therefore keeps package dependency names unchanged and assigns internal
names to zig dependencies when rendering build metadata:

- package dependencies keep their declared names
- zig dependencies get generated names with an `acton_zig_` prefix
- if multiple distinct zig dependencies would reuse the same local name, Acton
  appends a numeric suffix

This means an Acton package dependency named `lmdb` can coexist with a zig
dependency also declared as `lmdb`, and two different transitive zig packages
that both use the local name `shared` can still appear together in one
generated manifest.

### Example: colliding transitive zig names

Consider this project graph:

```text
root
├── package dep "dep_a"
│   └── zig dep "shared" -> zig_common
├── package dep "dep_b"
│   └── zig dep "shared" -> zig_common
└── package dep "dep_c"
    └── zig dep "shared" -> zig_other
```

All three zig dependencies are named `shared` in their own local `Build.act`,
but once they are flattened into the root manifest they become siblings.

The generated root `build.zig.zon` therefore looks conceptually like:

```zig
.dependencies = .{
    .dep_a = .{ ... },
    .dep_b = .{ ... },
    .dep_c = .{ ... },
    .acton_zig_shared = .{ ... },     // zig_common
    .acton_zig_shared_2 = .{ ... },   // zig_other
},
```

There is no `.acton_zig_shared_3`, because `dep_a` and `dep_b` refer to the
same zig package identity and are deduplicated before local names are assigned.

## Deduplication

Before assigning local zig names, Acton deduplicates zig dependency references
by resolved identity plus Zig build options.

Today that identity is derived from whichever source locator is present:

- rebased path plus options
- content hash plus options
- URL plus options

When two references resolve to the same identity, Acton emits one local zig
dependency entry and merges the requested artifact names. When the identities
differ, both dependencies stay present and receive separate local aliases.

So collisions only matter among sibling entries in one generated
`build.zig.zon`, and only after deduplication has decided whether two
references are really the same package instance.

### Example: same package, different options

Options are part of the zig dependency identity. So this conceptual tree:

```text
root
├── package dep "tls_a"
│   └── zig dep "mbedtls" -> same package, options { .pic = true }
└── package dep "tls_b"
    └── zig dep "mbedtls" -> same package, options { .pic = false }
```

still produces two sibling zig entries in the root `build.zig.zon`, because
the Zig build instances are not interchangeable:

```zig
.dependencies = .{
    .tls_a = .{ ... },
    .tls_b = .{ ... },
    .acton_zig_mbedtls = .{ ... },     // options { .pic = true }
    .acton_zig_mbedtls_2 = .{ ... },   // options { .pic = false }
},
```

## Practical debugging rules

- Import or type-check failures are usually about Acton package discovery, not
  zig dependencies.
- Linker or `b.dependency(...)` lookup failures are usually about generated
  `build.zig` and `build.zig.zon` contents.
- Root dependency pinning and `--dep` overrides apply to Acton package
  dependencies, not zig dependencies.
