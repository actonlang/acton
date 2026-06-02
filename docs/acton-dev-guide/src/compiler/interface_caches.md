# Interface Caches

Acton writes one compiled interface cache per module under `out/types`. The
cache path follows the module path and ends in `.tydb`, for example
`out/types/pkg/mod.tydb/`. A `.tydb` artifact is an LMDB environment directory
in normal LMDB directory mode, with `data.mdb` as the durable payload and
`lock.mdb` as LMDB runtime lock state.

The compiler code should not construct these paths directly. Use the
`InterfaceFiles` helpers, especially `interfacePath`, for cache paths.

## Compatibility Boundary

`InterfaceFiles` deliberately preserves the old interface-cache API:

- `writeFile` writes the cache for one module.
- `readHeader` reads only metadata, imports, roots, tests, docstring, and
  per-name hash records.
- `readExtensionsByClass` and `readExtensionsByProtocol` read exact extension
  index entries without decoding `NameInfo` entries or typed
  statements.
- `readFile` reconstructs the full cached payload: imports, `NameInfo`, typed
  module, metadata, module hashes, per-name hashes, roots, tests, and docstring.
- `readHeaderMaybe` and `readFileMaybe` turn missing, corrupt, unreadable, or
  version-mismatched caches into cache misses.

That boundary lets the rest of the compiler keep treating cache access as a
plain lookup while the on-disk representation moves from one binary blob to a
keyed store. Current full reads still rebuild the same payload as before.
More selective lookup by imported name is future work.

## LMDB Key Layout

All values are Binary-encoded Haskell values. The values are not compressed;
the layout splits the payload so readers can avoid decoding unrelated sections.

Metadata and module-level keys:

| Key | Value type |
| --- | --- |
| `version` | `[Int]` |
| `meta` | `(Maybe SourceFileMeta, ByteString, ByteString, ByteString)` |
| `imports` | `[(ModName, ByteString)]` |
| `roots` | `[Name]` |
| `tests` | `[String]` |
| `doc` | `Maybe String` |
| `module-header` | `(ModName, [Import], Maybe String)` |
| `name-count` | `Int` |
| `stmt-count` | `Int` |

The three `ByteString` hashes in `meta` are the module source-bytes hash, module
public hash, and module implementation hash. The `ByteString` stored with each
import is that imported module's public hash.

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
(`pubLocalDeps` / `implLocalDeps`), and external public/implementation
dependency snapshots (`pubDeps` / `implDeps`) as `(QName, hash)` pairs. Header
readers use these records for cache freshness, interest registration, and DBP
local dependency closure without decoding the `NameInfo` entries or typed
statements.

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

stmt/000000000000                 -> typed Stmt for encode
stmt/000000000001                 -> typed Stmt for decode
```

## Read And Write Behavior

`readHeader` opens a read-only LMDB transaction and reads the header keys plus
the `name-hash` entries. It does not decode `NameInfo` entries or typed
statements. DBP uses this path to get per-name hashes, local dependency edges,
and root actor names before deciding whether the full typed module must be
decoded.

`readExtensionsByClass` and `readExtensionsByProtocol` read one class or
protocol key directly. DBP uses these exact lookups while expanding the selected
local dependency closure, mapping visited class/protocol names to the top-level
extension names that the typed module can actually retain.

`readFile` opens a read-only transaction and reconstructs the full payload by
following the explicit order keys for ordered sections. It does not depend on
LMDB cursor order for `TEnv` or typed statement reconstruction. DBP calls this
only when its selection-sensitive codegen hash says the existing `.c`/`.h`
output is missing or stale.

`writeFile` writes the full environment in one write transaction. The compiler
has one writer for a module cache, while multiple readers may run in parallel.
The raw Haskell LMDB binding can race when several threads open the same
environment at once, so `InterfaceFiles` serializes only environment opening.
The read transactions themselves are still independent.

`copyInterface` uses LMDB's environment copy API. It copies durable data without
copying a stale `lock.mdb`, so copied `--ty` artifacts reopen with fresh LMDB
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
and we keep values uncompressed. Header and full reads are still close to the
old binary blob path because the compiler still reads all per-name hash records
for cache decisions. The useful change is the storage boundary: names and typed
statements now have explicit keys, so future work can load only the imported
names a dependent module actually needs.
