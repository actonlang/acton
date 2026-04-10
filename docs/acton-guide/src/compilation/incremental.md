# Content hash driven incremental compilation

Acton tracks changes at a finer level than whole modules so builds stay fast as your project grows. The compiler keeps multiple different hashes and uses them to decide what and how to recompile.

## What gets hashed and tracked

- **moduleSrcBytesHash**: hash of the raw bytes for a whole `.act` file. It is very cheap to read and hash a `.act` file from disk (GB/s). This is stored in the `.ty` file and is the authority for deciding whether a cached typed module still matches the source. Parsing is not incremental, it only supports re-parsing a complete module, which is also why a single hash for the entire `.act` module file makes sense.
- **per-name srcHash**: hash of a name's source code. Since this is only the hash of the source code, it can be computed after the parser and before type checking in order to determine what functions we need to rerun through later passes, including type checking which is typically a relatively expensive pass. Note how the next pubHash and implHash are after type checking, so they cannot be used in order to determine if type-checking should be rerun for a function.
- **per-name pubHash**: hash of a name's public interface (its type signature). Downstream modules only need to re-typecheck if a pubHash they depend on changes. The pubHash also contains the hashes of dependencies, and if those change, our pubHash will change, thus causing re-typecheck.
- **per-name implHash**: hash of a name's implementation plus the impl hashes it depends on. If an implHash changes, we re-run back passes and tests.
- **per-name pubDeps**: the public (type signature) hashes of other names that we depend on. If the hashes of any deps change, we must re-typecheck.
- **per-name implsDeps**: the hashes of the implementation of other names that we depend on. If the hashes of any deps change, we must rerun back passes.

Most names have both a pubHash and implHash. Some derived internal names only have implHash.

## How `.ty` cache validity is decided

Each module gets a cached typed interface file in `out/types/<module>.ty`.
That cache stores:

- the module source content hash (`moduleSrcBytesHash`)
- fast-path source metadata such as modification time, change time, and file size
- file identity metadata where available, such as inode/device on POSIX
- the compiler/cache schema version

The important rule is:

- **content hash is the correctness authority**
- **filesystem metadata is only a fast path**

In practice, Acton decides reuse like this:

1. If the `.ty` file is missing, unreadable, or from an incompatible cache
   schema version, Acton reparses the `.act` file and rebuilds the `.ty`.
2. If the cached source metadata still matches the current source file and the
   source mtime is strictly older than the `.ty` mtime, Acton reuses the `.ty`
   header immediately without reading and hashing the source.
3. If the metadata differs, or the source and `.ty` mtimes are equal, Acton
   reads the `.act` file and compares its content hash to the stored
   `moduleSrcBytesHash`.
4. If the hash matches, the source content is unchanged, so Acton reuses the
   cached `.ty` and refreshes the stored source metadata.
5. If the hash differs, the source really changed, so Acton reparses and
   recompiles that module.

This means harmless metadata drift, like a `touch`, checkout, restore, copy,
or cross-machine sync, does not by itself force a front-end rebuild. It also
means misleading timestamps cannot cause stale typed-module reuse, because
Acton falls back to the source content hash before trusting the cache.

The strict `source mtime < .ty mtime` check matters on filesystems with coarse
mtime resolution. If a source edit and `.ty` write land in the same timestamp
tick, equal mtimes are ambiguous: the source may already have changed even
though the cached source metadata still looks identical. Acton therefore treats
equal source and `.ty` mtimes as a signal to hash the current source instead of
taking the metadata-only fast path.

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

If no name actually uses the import, per-name deps do not change, so nothing propagates. Changes are only computed and propagated for names that are actually in use, which also means that it is possible to create quite large and monolithic modules without paying a higher cost for longer compilation times of downstream dependents.

## Code generation staleness

Generated C/H files embed the module impl hash. If the embedded hash differs from the current module impl hash, the compiler treats the generated code as out of date and regenerates it.

## Tests and hashes

Test results are cached by the per-name implHash (plus impl deps). Cached failures are still shown by default. Use `--show-cached` to include cached successes, or `--no-cache` to force reruns.

For more on tests, see the [Testing](testing.md) section.
