# Interface Caches

Acton writes one compiled interface cache per module under `out/types`. The
cache path follows the module path and ends in `.tydb`, for example
`out/types/pkg/mod.tydb/`. A `.tydb` artifact is an LMDB environment directory
in normal LMDB directory mode, with `data.mdb` as the durable payload and
`lock.mdb` as LMDB runtime lock state.

The compiler code should not construct these paths directly. Use the
`InterfaceFiles` helpers, especially `interfacePath`, for cache paths.

## Compatibility Boundary

`InterfaceFiles` preserves the full-cache API and also exposes a selective
read API for imported-module lookup:

- `writeFile` writes the cache for one module.
- `readHeaderSummary` reads metadata, imports, the stored name count, roots,
  tests, and docstring; `readHeader` additionally decodes every per-name hash
  record.
- `readExtensionsByClass` and `readExtensionsByProtocol` read exact extension
  index entries without decoding `NameInfo` entries or typed
  statements.
- `readFile` reconstructs the full cached payload: imports, `NameInfo`, typed
  module, metadata, module hashes, per-name hashes, roots, tests, and docstring.
- `readHeaderMaybe`, `readHeaderSummaryMaybe`, and `readFileMaybe` turn
  missing, corrupt, unreadable, or version-mismatched caches into cache misses.
- `openInterfaceDB` validates a `.tydb` for selective reads on the shared
  per-path environment; `openInterfaceDBMaybe` is its cache-miss form.
- `readInterfaceDBModuleInfo` reads only import/doc metadata.
- `readInterfaceDBNameInfoMaybe` reads exactly one `name-info` entry by `Name`.
- `readInterfaceDBPublicNames`, `readInterfaceDBConstructors`,
  `readInterfaceDBActors`, `readInterfaceDBConAttr`,
  `readInterfaceDBProtoAttr`, `readInterfaceDBDescendants`,
  `readInterfaceDBExtByProto`, and `readInterfaceDBExtByType` read narrow
  query indexes.

That boundary lets the rest of the compiler keep treating cache access as a
plain lookup while the on-disk representation moves from one binary blob to a
keyed store. Full reads still rebuild the same payload as before, but normal
imported-module lookup no longer decodes every `NameInfo` entry.

## LMDB Key Layout

All values are persist-encoded Haskell values. The values are trusted local cache
data, not a stable cross-machine interchange format. The values are not
compressed; the layout splits the payload so readers can avoid decoding
unrelated sections.

Metadata and module-level keys:

| Key | Value type |
| --- | --- |
| `version` | `[Int]` |
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
| `stmt-has-not-impl` | `Bool` |

The three `ByteString` hashes in `meta` are the module source-bytes hash, module
public hash, and module implementation hash. The `ByteString` stored with each
import is that imported module's public hash.

The `deps` row stores every module whose names this module depends on, together
with the recorded public and implementation hash for that dependency module.
This row is the cheap stale-check gate: if both recorded module hashes still
match the current dependency module hashes, the compiler does not need to read
any per-name dependency rows for that module.

Extension indexes use the same suffix scheme as per-name entries, so readers
can look up one class or protocol without decoding the full index:

| Key | Value type |
| --- | --- |
| `ext-by-class/p/<name>` | `(Name, [Name])` |
| `ext-by-class/h/<hash>` | `(Name, [Name])` |
| `ext-by-protocol/p/<name>` | `(Name, [Name])` |
| `ext-by-protocol/h/<hash>` | `(Name, [Name])` |

The value repeats the keyed class or protocol name and stores the synthetic
top-level extension names produced by `extensionName`, which are also the names
used by per-name hashes and DBP pruning. This lets selective readers keep
extension declarations when a selected class, protocol, or local dependency
requires extension support. A returned extension must also be present in that
module's per-name hash records; otherwise the index is inconsistent with the
typed module.

Per-name keys:

| Key | Value type |
| --- | --- |
| `name-order/<index>` | `ByteString` suffix for a `name-info` key |
| `name-info/p/<name>` | `(Name, NameInfo)` |
| `name-info/h/<hash>` | `(Name, NameInfo)` |
| `name-hash/p/<name>` | `NameHashInfo` |
| `name-hash/h/<hash>` | `NameHashInfo` |

Each `NameHashInfo` value contains the local name, `srcHash`, `pubHash`,
`implHash`, local public/implementation dependency names
(`pubLocalDeps` / `implLocalDeps`), and the indexes of the typed top-level
statements owned by that name. External dependency snapshots are stored in
dependency rows instead of being repeated inside every `NameHashInfo`.

Dependency rows:

| Key | Value type |
| --- | --- |
| `deps/<module>` | `[DepNameInfo]` |
| `deps/<module>/p/<name>` | `DepUsers` |
| `deps/<module>/h/<hash>` | `DepUsers` |

`deps/<module>` stores the dependency names used from that module and the
recorded public and implementation hash for each name. `DepUsers` stores the
local names that use one dependency name in their public and implementation
hashes. A stale check reads `deps/<module>` only when the module-level hash gate
changed, then reads `deps/<module>/<name>` only for names whose hashes actually
changed or went missing.

Short safe source names are stored directly in key suffixes. Names longer than
400 bytes and names containing unsafe path-like bytes use SHA-256 based key
suffixes. Source names use their source text; derived and internal names use
the compiler-generated text from `rawstr`. `TEnv` entries are unique by `Name`;
`name-order/<index>` stores the name suffix so full reads reconstruct the
original `TEnv` order without depending on LMDB cursor order.

Typed statements are stored by module order:

| Key | Value type |
| --- | --- |
| `stmt/<index>` | `Stmt` |

Statement order is preserved for full typed-module reconstruction.

Narrow query indexes are stored as separate keys, so solver and environment
queries do not need to decode one combined module metadata value:

| Key | Value type |
| --- | --- |
| `con-attr/<name>` | `[Name]` of public classes/actors declaring that attribute |
| `proto-attr/<name>` | `[Name]` of public protocols declaring that attribute |
| `descendants/<hash>` | `[Name]` of public classes/protocols below that constructor |
| `ext-proto/<hash>` | `[Name]` of public extensions implementing that protocol |
| `ext-type/<hash>` | `[Name]` of public extensions for that type/class |

The `<hash>` suffix is a SHA-256 key for a source-location-free `QName`.
Attribute keys reuse the normal name-key suffix scheme. Readers resolve the
stored names through the matching `name-info/<suffix>` entries, so the query
index itself stays small. The attribute indexes record attributes where they
are declared; readers complete inherited owners through the descendants index.

For example, `base/src/base64.act` contains top-level `encode` and `decode`
definitions. Its `.tydb` uses these keys:

```text
name-count                        -> 2
stmt-count                        -> 2

name-order/000000000000           -> p/encode
name-info/p/encode                -> (Name "encode", NameInfo for encode)
name-order/000000000001           -> p/decode
name-info/p/decode                -> (Name "decode", NameInfo for decode)

name-hash/p/decode                -> NameHashInfo for decode
name-hash/p/encode                -> NameHashInfo for encode

deps                              -> dependency modules and module hashes
deps/__builtin__                  -> dependency names and name hashes
deps/__builtin__/p/bytes          -> local names using __builtin__.bytes

stmt/000000000000                 -> typed Stmt for encode
stmt/000000000001                 -> typed Stmt for decode
```

## Read And Write Behavior

`readHeaderSummary` opens a read-only LMDB transaction and reads the header
keys plus the `deps` row and the stored name count. It does not decode
`NameInfo` entries, per-name hash rows, or typed statements. Stale checks
first compare the dependency module hashes in `deps`; if that gate changes,
they use per-name dependency rows to decide which local names are affected.
DBP reads `roots` and exact `name-hash/<suffix>` entries while expanding the
selected local dependency closure. When the selected codegen hash is stale,
`readSelectedModule` reconstructs a pruned typed module from only the selected
`stmt/<index>` records; it falls back to `readFile` when statement ownership
is missing or the module contains NotImplemented hooks, whose
native-extension pairing needs the whole module.

`readExtensionsByClass` and `readExtensionsByProtocol` read one class or
protocol key directly. DBP uses these exact lookups while expanding the selected
local dependency closure, mapping visited class/protocol names to the top-level
extension names that the typed module can actually retain.

`readFile` opens a read-only transaction and reconstructs the full payload by
following the explicit order keys for ordered sections. It does not depend on
LMDB cursor order for `TEnv` or typed statement reconstruction. DBP calls this
only when selected statement reconstruction cannot serve a stale
selection-sensitive codegen output.

All readers share one cached read-only LMDB environment per `.tydb` path,
opened with the normal reader lock table so concurrent writers in other
processes cannot recycle pages under an active read transaction. Writers keep
their exclusive per-path lock and retire the cached environment, waiting for
active readers to drain, before opening their own, so the process never holds
two environments for one path. `openInterfaceDB` validates the version once
and hands `Acton.Env` a handle it installs in a `ModuleInfo`; later
exact-name lookups call `readInterfaceDBNameInfoMaybe`, which runs a short
read transaction on the shared environment and decodes only the demanded
`name-info/<suffix>` value. Those reads sit behind Env's pure-looking lookup
functions and are memoized per module.

The same handle exposes the narrow query readers for non-name solver
questions: actor lookup reads `actors`; attribute lookup reads
`con-attr/<name>` or `proto-attr/<name>`; descendant lookup reads one
`descendants/<hash>` record; and witness lookup reads `ext-proto/<hash>` or
`ext-type/<hash>`. Plain imports do not read these records.

`writeFile` writes the full environment in one write transaction. The compiler
has one writer for a module cache, while multiple readers may run in parallel.
The raw Haskell LMDB binding can race when several threads open the same
environment at once, so `InterfaceFiles` serializes only environment opening.
The read transactions themselves are still independent.

Within a single compiler process, completed front results are the authoritative
interface source for downstream front passes. The import loader should prefer
the in-memory `Env.modules` entry when a module has already been loaded, and use
`.tydb` only for modules absent from that environment. This lets `.tydb` writes
run asynchronously without making dependent front stages wait for the LMDB
directory to become readable.

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
