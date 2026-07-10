# Interface Caches

Acton writes one compiled interface cache per module under `out/types`. The
cache path follows the module path and ends in `.tydb`, for example
`out/types/pkg/mod.tydb/`. A `.tydb` artifact is an LMDB environment directory
in normal LMDB directory mode, with `data.mdb` as the durable payload and
`lock.mdb` as LMDB runtime lock state.

The compiler code should not construct these paths directly. Use the
`InterfaceFiles` helpers, especially `interfacePath`, for cache paths.

## Interface API

`InterfaceFiles` exposes full reconstruction, narrow imported-name lookups, and
the exact row reads used by selective back passes:

- `writeFile` writes the cache for one module.
- `readHeaderSummary` reads metadata, imports, the stored name count, roots,
  tests, and docstring; `readHeader` additionally decodes every per-name hash
  record.
- `readExtensionsByClass` and `readExtensionsByProtocol` read exact extension
  index entries without decoding `NameInfo` entries or typed
  statements.
- `readFile` reconstructs the full cached payload: imports, `NameInfo`, typed
  module, metadata, module hashes, per-name hashes, roots, tests, and docstring.
- `readModuleHashInfo` reads the statement-owner schedule, module-owned
  import/initialization hash, and mandatory initialization dependencies without
  loading statements or per-name hashes.
- `readModuleSnapshotMaybe` atomically reads the generation, module hashes,
  identity, ordered source imports, closure imports, roots, native flag, and
  documentation.
- `readReachModule` and `readReachWholeModule` read the mandatory and aggregate
  summaries stored in the module reachability row.
- `readReachTop`, `readReachMember`, `readReachShape`, `readReachSlot`, and
  `readReachReflection` read one exact reachability row.
- `readModuleSelection` reconstructs one exact top/member/initializer projection
  from the selected syntax rows.
- `readHeaderMaybe`, `readHeaderSummaryMaybe`, and `readFileMaybe` turn missing,
  corrupt, or version-mismatched caches into cache misses. Other I/O and LMDB
  failures propagate.
- `openInterfaceDB` validates a `.tydb` for selective reads on the shared
  per-path environment; `openInterfaceDBMaybe` is its cache-miss form.
- `openInterfaceDBAtGeneration` binds every later lookup to one captured
  generation.
- `readInterfaceDBModuleInfo` reads only import/doc metadata.
- `readInterfaceDBNameInfoMaybe` reads exactly one `name-info` entry by `Name`.
- `readInterfaceDBPublicNames`, `readInterfaceDBConstructors`,
  `readInterfaceDBActors`, `readInterfaceDBConAttr`,
  `readInterfaceDBProtoAttr`, `readInterfaceDBDescendants`,
  `readInterfaceDBExtByProto`, and `readInterfaceDBExtByType` read narrow
  query indexes.

Full reads rebuild the complete typed payload, while imported-name lookup and
selective back passes decode only the rows they request.

## LMDB Key Layout

All values are persist-encoded Haskell values. The values are trusted local cache
data, not a stable cross-machine interchange format. The values are not
compressed; the layout splits the payload so readers can avoid decoding
unrelated sections.

Metadata and module-level keys:

| Key | Value type |
| --- | --- |
| `version` | `[Int]` (currently `[0,38]`) |
| `generation` | `ByteString` |
| `meta` | `(Maybe SourceFileMeta, ByteString, ByteString, ByteString)` |
| `imports` | `[(ModName, ByteString)]` |
| `deps` | `[DepModuleInfo]` |
| `roots` | `[Name]` |
| `tests` | `[String]` |
| `doc` | `Maybe String` |
| `module-header` | `(ModName, [Import], Maybe String)` |
| `name-count` | `Int` |
| `public-names` | `[Name]` |
| `constructors` | `[Name]` |
| `actors` | `[Name]` |
| `stmt-count` | `Int` |
| `stmt-mandatory` | `[Int]` |
| `stmt-has-not-impl` | `Bool` |
| `module-hash` | `ModuleHashInfo` |

`generation` is a fresh 32-byte nonce for each semantic interface commit. It
changes even across an A -> B -> A rewrite, unlike a content hash. A module
snapshot captures it in the same transaction as the three hashes in `meta`,
the module header, ordered source imports, closure imports, roots, native flag,
and documentation. The `ByteString` stored with each closure import is that
imported module's public hash.

`module-hash` is required for every current-version read. It stores one owner
list for each top-level statement in order, preserving initialization order
across independently hashed names. Its structural implementation hash covers
the ordered source imports and mandatory ownerless statements. It also stores
the ownerless statements' local implementation dependencies and their external
public and implementation dependency snapshots. These statements have no
top-level name, so this part is their module-owned counterpart to
`NameHashInfo`.

The `deps` row stores every module whose names this module depends on, together
with the recorded public and implementation hash for that dependency module.
This row is the cheap stale-check gate: if both recorded module hashes still
match the current dependency module hashes, the compiler does not need to read
any per-name dependency rows for that module.

Extension indexes use the same structural name digest as per-name entries, so
readers can look up one class or protocol without decoding the full index:

| Key | Value type |
| --- | --- |
| `ext-by-class/h/<name-hash>` | `(Name, [Name])` |
| `ext-by-protocol/h/<name-hash>` | `(Name, [Name])` |

The value repeats the keyed class or protocol name and stores the synthetic
top-level extension names produced by `extensionName`. These indexes support
exact imported-environment queries without decoding the complete `TEnv`.

Per-name keys:

| Key | Value type |
| --- | --- |
| `name-order/<index>` | `ByteString` key of the corresponding ordered row |
| `name-info/order/<index>` | `(Name, NameInfo)` |
| `name-info/h/<name-hash>` | `(Name, NameInfo)` |
| `name-hash/h/<name-hash>` | `NameHashInfo` |

Every `TEnv` occurrence has its own `name-info/order/<index>` row, and
`name-order/<index>` points to that exact row. This preserves signatures and
definitions with the same `Name` in their original order. The direct
`name-info/h/<name-hash>` index stores the last occurrence, matching keyed
environment lookup semantics.

Each `NameHashInfo` value contains the local name, `srcHash`, `pubHash`,
`implHash`, local public/implementation dependency names
(`pubLocalDeps` / `implLocalDeps`), and the indexes of its top-level statement
rows. External dependency snapshots live in the dependency rows instead of
being repeated inside every `NameHashInfo`.

Dependency rows:

| Key | Value type |
| --- | --- |
| `deps/<module-digest>` | `[DepNameInfo]` |
| `deps/name/<hash>` | `DepUsers` |

`deps/<module-digest>` uses the structural digest of the complete location-free
`ModName`; textual module renderings never form storage keys. It stores the
dependency names used from that module and the recorded public and
implementation hash for each name. A `deps/name` key similarly hashes the
complete module/name pair. Its `DepUsers` value stores top-level names using
that dependency; module-owned dependencies from `module-hash` deliberately
have no per-name user. A stale check reads the module row only after the
module-level hash gate changes, then reads exact dependency-name rows for names
that changed or disappeared.

Name indexes use `h/` plus the SHA-256 digest of the complete location-free
`Name` value. Hashing the structure, rather than `rawstr`, keeps a source name
such as `ownerD_part` distinct from `Derived owner part`. Ordered rows
reconstruct the exact `TEnv` without depending on LMDB cursor order.

Typed statements are stored by module order:

| Key | Value type |
| --- | --- |
| `stmt/<index>` | `StoredStmt` |
| `shape/h/<name-hash>` | `ContainerShape` |
| `body/member/<owner-hash>/<member-hash>` | `MemberContentRow` |
| `reach/module/<module-digest>` | `(ModName, ReachSummary, ReachSummary)` |
| `reach/top/<top-hash>` | `ReachTopRow` |
| `reach/member/<top-hash>/<member-hash>` | `ReachMemberRow` |
| `reach/shape/<top-hash>` | `ReachShapeRow` |
| `reach/slot/<top-hash>/<member-ref-hash>` | `ReachSlotRow` |
| `reach/reflection/<top-hash>` | `ReachReflectionRow` |

`StoredStmt` preserves module order while separating container declarations
from their bodies. `stmt-mandatory` lists ownerless statements such as
top-level expressions and control flow; every projection includes those rows.
A shape stores the container header, structural suite, and method ABI slots.
Member rows independently store methods, attributes, per-attribute static and
instance initialization fragments, and the remaining constructor body
(`InitRest`).

Reachability rows form the exact semantic index consumed by the global
worklist. The module row contains two summaries: dependencies of mandatory
ownerless statements, and a whole-module aggregate of every top, shape, member,
initializer, and generated-slot summary. Ordinary whole surfaces use the
aggregate to seed exact provider interest while those providers remain
selective. The other rows record top-level dependencies, member dependencies
and initializers, class/actor shape and lineage, effective dispatch slots, and
reflectable attributes. Top and member identities are source-location-free
semantic digests, and key/value identity is validated on read.

Narrow query indexes are stored as separate keys, so solver and environment
queries do not need to decode one combined module metadata value:

| Key | Value type |
| --- | --- |
| `con-attr/h/<name-hash>` | `(Name, [Name])` for public classes/actors declaring that attribute |
| `proto-attr/h/<name-hash>` | `(Name, [Name])` for public protocols declaring that attribute |
| `descendants/<hash>` | `(QName, [Name])` of public classes/protocols below that constructor |
| `ext-proto/<hash>` | `(QName, [Name])` of public extensions implementing that protocol |
| `ext-type/<hash>` | `(QName, [Name])` of public extensions for that type/class |

The `<hash>` suffix is a SHA-256 key for a source-location-free `QName`.
Attribute keys reuse the normal structural name digest. Readers resolve the
stored names through the matching `name-info/h/<name-hash>` entries, so the
query index itself stays small. The attribute indexes record attributes where
they are declared; readers complete inherited owners through the descendants
index.

For example, `base/src/base64.act` contains top-level `encode` and `decode`
definitions. Its `.tydb` uses these keys:

```text
name-count                        -> 2
stmt-count                        -> 2
stmt-mandatory                    -> []
stmt-has-not-impl                 -> False
module-hash                       -> import hash + owner schedule [[encode], [decode]]
generation                        -> fresh 32-byte nonce

name-order/000000000000           -> name-info/order/000000000000
name-info/order/000000000000      -> (Name "encode", NameInfo for encode)
name-info/h/<encode-name-digest>  -> (Name "encode", NameInfo for encode)
name-order/000000000001           -> name-info/order/000000000001
name-info/order/000000000001      -> (Name "decode", NameInfo for decode)
name-info/h/<decode-name-digest>  -> (Name "decode", NameInfo for decode)

name-hash/h/<decode-name-digest>  -> NameHashInfo for decode
name-hash/h/<encode-name-digest>  -> NameHashInfo for encode

deps                              -> dependency modules and module hashes
deps/<builtin-module-digest>      -> dependency names and name hashes
deps/name/<digest>                -> local names using __builtin__.bytes

stmt/000000000000                 -> StoredStmt for encode
stmt/000000000001                 -> StoredStmt for decode
reach/module/<module-digest>      -> empty mandatory + whole summaries
```

## Read And Write Behavior

`readHeaderSummary` opens a read-only LMDB transaction and reads the header
keys plus the `deps` row and the stored name count. It does not decode
`NameInfo` entries, per-name hash rows, or typed statements. Stale checks
first compare the dependency module hashes in `deps`; if that gate changes,
they use per-name dependency rows to decide which local names are affected.

Selective back passes first capture the resolved path and atomic module
snapshot for every interface in the closure. The global worklist reads the
mandatory summary for each selective module, whole summaries for ordinary
whole surfaces, and exact `reach/*` rows for demanded tops, shapes, members,
dispatch slots, and reflection requests. Once the closure converges,
`readModuleSelection` reads the mandatory statement rows plus only the selected
statement, shape, member, and initializer rows and reconstructs the projected
typed module. `SelectiveBack.materializeProjection` separately reads the
selected inferred headers and merges the projected type environment. Every
generation-bound lazy read checks its expected generation in the same
transaction as its row. The scheduler validates the complete batch snapshot
closure once preparation has finished, before enqueueing any back job, and
again when the last job completes its output.

A missing or inconsistent reachable row, a changed snapshot, or an unusable
selected content row is a compiler error. An exact selection run neither widens
the key nor falls back to full module or source loading. Rootless surfaces and
exposed `Build.act` library modules are chosen as ordinary whole surfaces and
seed selective internal providers through their whole summaries and exact
opaque shape/slot routing. The artifact is consumer-specific; a later consumer
recomputes the projection instead of broadening the present one to every public
surface. A native
`NotImplemented` module instead forces its transitive provider closure whole,
because hand-written `.ext.c` references are not represented in typed rows.
Persistent `--db` builds also stay whole because restored class ids are dynamic
roots. Dynamic `serialize` or `deserialize` interest likewise chooses a whole
deferred batch because runtime type names are not bounded by static edges. These
are chosen modes, not error recovery.

`readFile` opens a read-only transaction and reconstructs the full payload by
following the explicit order keys for ordered sections. It does not depend on
LMDB cursor order for `TEnv` or typed statement reconstruction. DBP calls this
only for compilation units already designated as whole surfaces.

All readers share one cached read-only LMDB environment per `.tydb` path,
opened with the normal reader lock table so concurrent writers in other
processes cannot recycle pages under an active read transaction. Writers keep
their exclusive per-path lock and retire the cached environment, waiting for
active readers to drain, before opening their own, so the process never holds
two environments for one path. `openInterfaceDB` validates the version once
and hands `Acton.Env` a handle it installs in a `ModuleInfo`; later exact-name
lookups call `readInterfaceDBNameInfoMaybe`, which runs a short read transaction
on the shared environment and decodes only the demanded
`name-info/h/<name-hash>` value. Selective back passes instead build a fresh
environment with `openInterfaceDBAtGeneration`; every lazy name or query-index
read validates the captured generation in the same transaction before it can
be memoized.

The same handle exposes the narrow query readers for non-name solver
questions: actor lookup reads `actors`; attribute lookup reads
`con-attr/h/<name-hash>` or `proto-attr/h/<name-hash>`; descendant lookup reads one
`descendants/<hash>` record; and witness lookup reads `ext-proto/<hash>` or
`ext-type/<hash>`. Plain imports do not read these records.

`writeFile` writes the full environment in one LMDB write transaction. The
compiler has one writer for a module cache, while multiple readers may run in
parallel.
The raw Haskell LMDB binding can race when several threads open the same
environment at once, so `InterfaceFiles` serializes only environment opening.
The read transactions themselves are still independent.

The `.tydb` commit is synchronous with front completion. A module cannot report
its front passes complete until all interface rows are committed on disk, so
dependent readers never wait for or race an in-flight interface write. A write
failure fails that module's front pass. Documentation output and an explicitly
requested interface copy may still run in the background because this build
does not read them back.

`copyInterface` uses LMDB's environment copy API. It copies durable data without
copying a stale `lock.mdb`, so copied `--tydb` artifacts reopen with fresh LMDB
runtime lock state.

## Performance Snapshot

The initial comparison used the generated `concurrent_typecheck_class_heavy`
fixture with a 5.0 MiB source file. On that local run:

```text
artifact size:     .ty 72 MiB, .tydb 101 MiB (+40%)
header read:       .ty 56.9 ms/read, .tydb 73.0 ms/read (+28%)
full read:         .ty 2.77 s/read, .tydb 2.91 s/read (+5%)
write:             .ty 1.68 s/write, .tydb 1.12 s/write (-33%)
```

The current `.tydb` format is larger because LMDB stores page-structured data
and we keep values uncompressed. The useful property is the storage boundary:
normal imported-module lookup loads only the names and index rows a dependent
module actually uses, while full reconstruction remains available for explicit
broad paths.
