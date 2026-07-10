# Content hash driven incremental compilation

Acton tracks changes at a finer level than whole modules so builds stay fast as your project grows. The compiler keeps multiple different hashes and uses them to decide what and how to recompile.

## What gets hashed and tracked

- **moduleSrcBytesHash**: hash of the raw bytes for a whole `.act` file. It is very cheap to read and hash a `.act` file from disk (GB/s). This is stored in the `.tydb` cache and is the authority for deciding whether a cached typed module still matches the source. Parsing is not incremental, it only supports re-parsing a complete module, which is also why a single hash for the entire `.act` module file makes sense.
- **per-name srcHash**: hash of a name's source-level definition, assembled
  after the module has completed its front passes. It identifies which names
  changed for diagnostics and later hash assembly; front passes are not
  incremental within one changed module, so parsing and type checking still
  rerun for the complete module.
- **per-name pubHash**: hash of a name's public interface (its type signature). Downstream modules only need to re-typecheck if a pubHash they depend on changes. The pubHash also contains the hashes of dependencies, and if those change, our pubHash will change, thus causing re-typecheck.
- **per-name implHash**: hash of a name's implementation plus the impl hashes it depends on. If an implHash changes, we re-run back passes and tests.
- **module-owned implHash**: hash of the ordered source imports, mandatory top-level statements with no typed binding, and the ordered ownership schedule of all top-level statements, combined with the mandatory statements' local and external implementation dependencies. It participates in the whole-module implementation hash, so changing import or initialization order is observable even when the individual names are unchanged.
- **per-name pubDeps**: the public (type signature) hashes of other names that we depend on. If the hashes of any deps change, we must re-typecheck.
- **per-name implsDeps**: the hashes of the implementation of other names that we depend on. If the hashes of any deps change, we must rerun back passes.

Most names have both a pubHash and implHash. Some derived internal names only
have implHash. Mandatory ownerless statements are tracked by the module-owned
component instead of being assigned a synthetic name; its dependency snapshots
include both provider public and implementation hashes.

## How `.tydb` cache validity is decided

Each module gets a cached typed interface database in
`out/types/<module>.tydb/`.
That cache stores:

- the module source content hash (`moduleSrcBytesHash`)
- fast-path source metadata such as modification time, change time, and file size
- file identity metadata where available, such as inode/device on POSIX
- the compiler/cache schema version

The important rule is:

- **content hash is the correctness authority**
- **filesystem metadata is only a fast path**

In practice, Acton decides reuse like this:

1. If the `.tydb` cache is missing, unreadable, or from an incompatible cache
   schema version, Acton reparses the `.act` file and rebuilds the `.tydb`.
2. If the cached source metadata still matches the current source file and the
   source mtime is strictly older than the `.tydb` data-file mtime, Acton reuses
   the cache header immediately without reading and hashing the source.
3. If the metadata differs, or the source and `.tydb` data-file mtimes are
   equal, Acton reads the `.act` file and compares its content hash to the
   stored `moduleSrcBytesHash`.
4. If the hash matches, the source content is unchanged, so Acton reuses the
   cached `.tydb` and refreshes the stored source metadata.
5. If the hash differs, the source really changed, so Acton reparses and
   recompiles that module.

This means harmless metadata drift, like a `touch`, checkout, restore, copy,
or cross-machine sync, does not by itself force a front-end rebuild. It also
means misleading timestamps cannot cause stale typed-module reuse, because
Acton falls back to the source content hash before trusting the cache.

The strict `source mtime < .tydb data-file mtime` check matters on filesystems
with coarse mtime resolution. If a source edit and `.tydb` write land in the
same timestamp tick, equal mtimes are ambiguous: the source may already have
changed even though the cached source metadata still looks identical. Acton
therefore hashes the current source instead of taking the metadata-only fast
path.

## What changes cause what work

**Change function body, same signature**

```
# a.act
def apa() -> int:
    return 1

def a() -> int:
    return apa()
```

```
# c.act
import a
import testing

def _test_foo() -> None:
    testing.assertEqual(a.a(), 1)
```

If you change `apa()` to return `2`, only the impl hashes change since the return type and overall type signature remains the same. `c` does not re-typecheck, but back passes and tests are re-run.

Example `acton build --verbose` output (trimmed):

```
Stale a: source changed
Stale c: impl changes in a.a (used by _test_foo)
```

**Change a signature**

If `a.a()` changes its return type, its pubHash changes. Any module that uses `a.a()` will re-run front passes.

```
Stale c: pub changes in a.a (used by _test_foo)
```

**Add or remove an unused import**

The edited module reruns its front passes, and its ordered import list changes
the module-owned implementation hash. If no name actually uses the import,
per-name dependencies do not change, so the change does not propagate to
downstream modules. Changes are only propagated for names that are actually in
use, which also means that it is possible to create quite large and monolithic
modules without paying a higher cost for longer compilation times of downstream
dependents.

## Code generation staleness

Generated C/H files embed an Acton codegen hash. An ordinary whole-module output
combines the compiler identity and interface version with the module
implementation hash, raw source hash, and line-emission mode. Raw bytes and the
line mode matter because `#line` directives and other source mappings can
change without a semantic change.
Selective output fingerprints the exact whole-program projection, so changed
consumer interest also invalidates affected provider output. A module forced
whole as part of a native provider closure starts with the same whole-module
hash and additionally binds it to the public hashes in that batch's exact
interface snapshots. Those snapshots also retain the ordered source imports
used to rebuild module environments. If the embedded codegen hash differs from
the current one, the compiler regenerates the C/H files.

Selective back passes start at executable roots and materialize only the exact
top-level names, methods, attributes, and initialization fragments reached from
them. A directly requested library surface stays whole, but its internal
providers can still be selective: the whole surface records the exact provider
interest of everything it emits and routes inherited construction and calls
through exact provider slots. The projection is specific to the consumers in
the current build; compiling a different consumer recomputes it. Declared
library entry points work the same way. Modules implemented by native
`NotImplemented` bodies are the exception; their provider closure stays whole
because hand-written C can contain references that are not visible in Acton
source. Persistent `--db` builds also stay whole because stored class ids can
restore actors and messages that have no current static construction edge.
Reaching dynamic `serialize` or `deserialize` also makes the deferred batch
whole because runtime type names can reach classes absent from static edges.

The compiler also guards each selective run with generation-consistent
interface snapshots. Its codegen hash conservatively includes the public hashes
of the complete captured interface closure because witness and type-index
queries can remain lazy during codegen. Thus an unselected public-interface
change may regenerate selective output, although unrelated content rows are
still never loaded and implementation-only changes outside the projection do
not affect this guard.

## Tests and hashes

Test results are cached by the per-name implHash (plus impl deps). Cached failures are still shown by default. Use `--show-cached` to include cached successes, or `--no-cache` to force reruns.

For more on tests, see the [Testing](testing.md) section.
