# Incremental Compilation

Acton compiles a whole program by walking a dependency graph that spans the
root project and all of its dependencies. The scheduler in `Acton.Compile`
builds this total graph, runs the front passes (parse, kinds, types) in
topological order, and queues back passes as soon as a module's front passes
finish. Front work gates downstream modules, while back jobs overlap ongoing
front work. The scheduler runs tasks concurrently with async jobs across worker
threads. In watch/LSP mode, each change event bumps a generation id, cancels
in-flight tasks, and drops stale diagnostics or back jobs that still carry the
old generation, so we can react to new changes without waiting for obsolete
work to finish.

Interface files and content hashes tell the scheduler what work it can safely
reuse. `.ty` interface headers store hashes and snapshots of dependencies and
their hashes. It is cheap both to compute hashes and to read and compare them.
If the hashes show no relevant change, the compiler can skip parsing, type
checking, or back passes for large parts of the graph.

At the module level we store **moduleSrcBytesHash**, **modulePubHash**, and
**moduleImplHash**. The source-bytes hash (**moduleSrcBytesHash**) is the
SHA-256 of the raw source bytes and is used to decide whether a cached `.ty`
header is still valid; if it changes, we must re-parse the `.act` file and rerun
front passes for that module. Reading the file is done at GB/s on modern NVMe
hardware and hashing is typically running at roughly the same speed on modern
CPUs.

The public hash (**modulePubHash**) is derived from the module's public-name
`pubHash` values (which already include any external signature dependencies).
The implementation hash (**moduleImplHash**) is derived from per-name
`implHash` values; it is stored in the `.ty` header and written into generated
`.c`/`.h` files as an "Acton impl hash" tag. If the tag is missing or
mismatched, we rerun back passes even if no other hashes changed.

Hashing within a module is done per top-level name. A hashable unit is a `Def`,
`Actor`, `Class`, `Protocol`, `Extension`, or any top-level value introduced by
a binding statement such as `a = 123` or `b, c = f()`. Entries are keyed by the
local `Name`; external references are stored as `QName` (canonicalized to
`GName` with `unalias`), and extensions use the derived name from
`extensionName`. For now class/actor implementation hashes are coarse and cover
the entire body; per-method hashing can refine this later.

Each unit has three hashes. **srcHash** is the hash of the kinds-checked AST for
that name (pretty-printed), and is the earliest signal that a single declaration
changed. We use the kinds-checked form because it is the first pass that
normalizes type-level syntax (implicit type arguments, canonical kind structure)
without paying the cost of full type inference. Hashing the raw parsed AST would
make equivalent signatures hash differently and trigger needless rebuilds.
**pubHash** is the hash of the unit's public interface (doc-free `NameInfo`)
combined with the public hashes of external declarations referenced by its type
signature. **implHash** is the hash of the unit's implementation combined with
the implementation hashes of external declarations referenced from the body.
Because we hash pretty-printed source, docstrings currently influence
`implHash`; we can tighten the normalization later without changing the overall
model.

Name hashes depend on `NameInfo`. The type checker environment (`teAll`) is
authoritative for user-facing declarations, but it does not include compiler-
generated names (for example, protocol lowering emits classes such as
`MinusD_Number`). The typed AST does include those generated names, so we derive
their `NameInfo` with `QuickType.envOf`. Conversely, `envOf` omits `Protocol`
and `Extension` entries because type inference translates them away. To cover
both, hashing uses the union of `teAll` and `QuickType.envOf` as its name-info
map, ensuring generated names appear in `.ty` and still keeping protocol and
extension hashes intact.

To detect changes without recompiling, each per-name entry in `.ty` stores
dependency snapshots. **pubDeps** is a list of `(QName, pubHash)` for external
declarations referenced from the unit's type signature and from value-level
uses, which is what triggers re-typechecking when a provider's public interface
changes. **implDeps** is a list of `(QName, implHash)` referenced from the body
and is what triggers codegen and test invalidation. Only external dependencies
are stored; local dependencies are folded into the computed hash during
compilation.

Mutually recursive groups are hashed as a unit to avoid fixed-point iteration.
We compute `selfPubHash`/`selfImplHash` for each unit, compute a `groupHash`
from the sorted self-hashes plus all external dependency hashes of the group,
and then finalize each unit as `H(selfHash || groupHash || unitKey)`.

The `.ty` header stays ordered for fast reads:

```
version
moduleSrcBytesHash
modulePubHash
moduleImplHash
imports
nameHashInfo      -- per-name src/pub/impl hash + dep snapshots
roots
docstring
nameInfo
typedModule
```

A source change always re-runs front passes for that module. Downstream modules
compare recorded `pubDeps` against current provider hashes; any delta triggers
re-typechecking. Separately, `implDeps` changes cause back passes and tests to
rerun even when public hashes are stable. Missing or stale `.c`/`.h` outputs
also force back passes so codegen can recover. To see the decisions, run
`acton build --verbose`; it prints stale reasons and per-name hash deltas. The
snippets below abbreviate hashes to 8 hex digits for readability.

**Implementation-only change.** Before:

```acton
# a.act
def foo(x: int) -> int:
    return x + 1

# b.act
import a

def bar() -> int:
    return a.foo(1)
```

After changing only the body of `foo`:

```acton
# a.act
def foo(x: int) -> int:
    return x + 2
```

`srcHash(foo)` and `implHash(foo)` change while `pubHash(foo)` does not.
Downstream modules keep their public hashes stable, so they do not re-typecheck
but they do rerun back passes:

```text
$ acton build --verbose
Building project in /path/proj
Resolving dependencies (fetching if missing)...
  Stale a: source changed
  Hash deltas a: ~foo{src 12fa0b1c -> 3e3485be, impl 4bf97616 -> f736da16}
   Finished type check of     /path/proj/a      0.004 s
  Stale b: impl changes in a.foo 4bf97616 -> f736da16 (used by bar)
   Finished compilation of    /path/proj/a      0.002 s
   Finished compilation of    /path/proj/b      0.003 s
```

**Public signature change.** Before:

```acton
# a.act
def foo(x: int) -> int:
    return x + 1

# b.act
import a

def bar() -> int:
    return a.foo(1)
```

After changing the signature of `foo`:

```acton
# a.act
def foo(x: int) -> float:
    return float(x)

# b.act
import a

def bar() -> float:
    return a.foo(1)
```

Here `pubHash(foo)` changes, so `modulePubHash(a)` changes and any dependent
that recorded `a.foo` in `pubDeps` must re-typecheck:

```text
$ acton build --verbose
Building project in /path/proj
Resolving dependencies (fetching if missing)...
  Stale a: source changed
  Hash deltas a: ~foo{src 2f1a8c9b -> 3a7bb6e1, pub 91c2efb8 -> 5e7d9b02, impl 11c0aa34 -> 6b3e42f1}
   Finished type check of     /path/proj/a      0.004 s
  Stale b: pub changes in a.foo 91c2efb8 -> 5e7d9b02 (used by bar)
   Finished type check of     /path/proj/b      0.005 s
   Finished compilation of    /path/proj/a      0.002 s
   Finished compilation of    /path/proj/b      0.003 s
```

**Codegen-only staleness.** If generated output is missing or mismatched, we
rerun back passes even when hashes are unchanged. For example, deleting the
outputs forces codegen to refresh:

```text
$ rm out/types/b.c out/types/b.h
$ acton build --verbose
Building project in /path/proj
Resolving dependencies (fetching if missing)...
  Stale b: generated code out of date {impl c missing -> 7aa13f90, h missing -> 7aa13f90}
   Finished compilation of    /path/proj/b      0.003 s
```

**Non-propagating change.** Adding an unused import changes the module source
bytes, so the module itself re-runs front passes, but downstream modules do
not re-typecheck because no public hashes changed:

```acton
# before (b.act)
import a

def bar() -> int:
    return a.foo()

# after (b.act)
import a
import testing  # unused

def bar() -> int:
    return a.foo()
```

Known limitations include the lack of incremental typechecking within a
module, coarse class/actor impl hashes, and docstrings affecting `implHash`.
