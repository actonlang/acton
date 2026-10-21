# Incremental Compilation

See also [Imports and environments](imports_and_envs.md) for the distinction
between scheduler dependency discovery and the actual import closure loaded into
the type-checker environment.

Acton compiles a whole program by walking a dependency graph that spans the
root project and all of its dependencies. The scheduler in `Acton.Compile`
builds this total graph and runs the front passes (parse, kinds, types, escape
analysis, hashing). A module's parse runs at once. Its later front passes wait
for the front stages of its imports. Back passes are deferred (DBP, deferred
back passes): a module's back job waits until every module of the run that
depends on it, directly or not, has finished its front stage. The exceptions
are listed under [Deferred Back Passes](#deferred-back-passes) below. A module
among them queues its back job as soon as its own front stage finishes. Front
work gates downstream modules, while ready back jobs overlap ongoing front work.
The scheduler runs tasks concurrently with async jobs across worker threads. In
watch/LSP mode, each change event bumps a generation id, cancels in-flight
tasks, and drops stale diagnostics or back jobs that still carry the old
generation, so we can react to new changes without waiting for obsolete work to
finish.

Interface files and content hashes tell the scheduler what work it can safely
reuse. `.tydb` interface headers store hashes and snapshots of dependencies and
their hashes. It is cheap both to compute hashes and to read and compare them.
If the hashes show no relevant change, the compiler can skip parsing, type
checking, or back passes for large parts of the graph.

At the module level we store **moduleSrcBytesHash**, **modulePubHash**, and
**moduleImplHash**. The source-bytes hash (**moduleSrcBytesHash**) is the
SHA-256 of the raw source bytes and is used to decide whether a cached `.tydb`
header is still valid; if it changes, we must re-parse the `.act` file and rerun
front passes for that module. Reading the file is done at GB/s on modern NVMe
hardware and hashing is typically running at roughly the same speed on modern
CPUs.

The public hash (**modulePubHash**) is derived from the module's public-name
`pubHash` values (which already include any external signature dependencies).
The implementation hash (**moduleImplHash**) is derived from per-name
`implHash` values; it is stored in the `.tydb` header. Normal generated `.c`,
`.h` and `.closures.inc` files are tagged with this module implementation hash
as an "Acton impl hash". DBP generated files reuse the same tag but write a DBP
codegen hash that also includes the selected top-level names. If the tag of any
of the three files is missing or mismatched, we rerun back passes even if no
other hashes changed.

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
map, ensuring generated names appear in `.tydb` and still keeping protocol and
extension hashes intact.

To detect changes without recompiling, each per-name entry in `.tydb` stores
dependency snapshots. **pubDeps** is a list of `(QName, pubHash)` for external
declarations referenced from the unit's type signature and from value-level
uses, which is what triggers re-typechecking when a provider's public interface
changes. **implDeps** is a list of `(QName, implHash)` referenced from the body
and is what triggers codegen and test invalidation. Local dependencies are
stored separately as **pubLocalDeps** and **implLocalDeps** lists of `Name`s.
Those local edges are folded into the computed hash during compilation, and
they also give selective consumers such as DBP a cached local dependency graph.

Mutually recursive groups are hashed as a unit to avoid fixed-point iteration.
We compute `selfPubHash`/`selfImplHash` for each unit, compute a `groupHash`
from the sorted self-hashes plus all external dependency hashes of the group,
and then finalize each unit as `H(selfHash || groupHash || unitKey)`.

Header reads return the per-name hashes in deterministic name order. See
[Interface caches](interface_caches.md) for the exact LMDB key layout.

```
version
meta              -- source metadata + module src/pub/impl hashes
imports
name-hash/*       -- per-name src/pub/impl hash + dep snapshots
roots
tests
docstring
```

Fresh front passes also write HTML docs for normal project builds. To avoid
running `DocPrinter` on huge modules, build-time doc output is skipped when the
module records more than 10000 top-level names; `.tydb` and compilation still
proceed. The documentation index leaves those oversized modules visible without
linking to missing module pages. The explicit `acton doc FILE.act` command still
prints docs for the requested file.

## Deferred Back Passes

DBP is decided when a module's front stage finishes. Every module's back passes
are deferred, except those of:

- `__builtin__`
- a module with `NotImplemented` bodies, whose code a hand-written `.ext.c`
  provides
- a module at a library boundary (below)
- every module under `--only-build`, under alternate-output modes such as
  `--cgen`/`--hgen`, or under `--no-dbp`

A module whose back passes are not deferred is generated whole.

A module at a library boundary is a member of a library declared in `Build.act`
that is imported by any module outside that same library. The library boundary
needs the full generated C/H surface, so that module is compiled normally.
Internal library modules remain DBP-eligible.

When a fresh front pass produces a DBP candidate, the scheduler stores a
`DeferredBackJob` instead of queueing a normal `BackJob`. Cached `TyTask`
modules can also register deferred jobs on later builds. This matters because
the provider source and implementation hash can stay unchanged while a consumer
starts selecting a different provider name. The scheduler also records
interested names in an `InterestMap`. Fresh front results contribute the
external dependency facts they just computed; cached modules reconstruct the
same interest set from the `.tydb` dependency rows.

Deferred jobs do not wait for every front pass in the project. Their wait set is
the reverse dependency closure of the deferred module among the modules the run
compiles. After each front stage completes, the scheduler flushes any deferred
job whose wait set is now contained in the completed-front set. If a module in
that wait set fails front passes, the normal failed-build path prevents the
deferred back job from running.

When a deferred job is ready, DBP reads the module's root actor names from its
`.tydb`, then the `name-hash` rows of the names it selects, one name at a time,
without decoding the typed module. The selection seeds are:

- the collected `InterestMap` names for the module
- the module's root actor names

An empty interested-name set is valid; if the module has no root actors, DBP can
produce an effectively empty module body. DBP maps derived names back to their
owning top-level name, closes over `nhPubLocalDeps` and `nhImplLocalDeps`, and
uses exact `readExtensionsByClass` / `readExtensionsByProtocol`
lookups to keep top-level extension declarations required by selected classes
or protocols. Selection metadata that cannot map a seed, local dependency, or
extension back to a top-level name is a compiler error. DBP never falls back to
full-module compilation: a module that contains `NotImplemented`/native hooks
is not deferred in the first place.

Pruning happens on the typed module immediately before the normal back-pass
chain. Selected declarations, signatures, assignments, and compiler-introduced
`VarAssign` statements are retained by their bound top-level names; `Pass` is
dropped. Other source-level top-level statements are not a fallback case because
the parser rejects them before type checking.

Before reading any statement, DBP computes a codegen hash from the module
implementation hash and the sorted selected top-level names. If the existing
`.c`, `.h` and `.closures.inc` files already carry that hash in their generated
"Acton impl hash" tag, the deferred job is skipped. Otherwise DBP reads only the
statements of the selected names (`readSelectedModule`), prunes them to the
selected top-level declarations, runs the normal back-pass chain, and writes all
three files tagged with that DBP codegen hash. A `.tydb` written by the same
compiler version always has those statement rows, so when they cannot be read
the file is corrupt, and the build stops with an internal error instead of
reading the whole module. A later consumer change that selects a different
provider name makes the provider's DBP output stale even when the provider
source did not change.

A source change always re-runs front passes for that module. Downstream modules
compare recorded `pubDeps` against current provider hashes; any delta triggers
re-typechecking. Separately, `implDeps` changes cause back passes and tests to
rerun even when public hashes are stable. Missing or stale `.c`, `.h` or
`.closures.inc` outputs also force back passes so codegen can recover. To see
the decisions, run `acton build --verbose`; it prints stale reasons and
per-name hash deltas. The snippets below abbreviate hashes to 8 hex digits for
readability.

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
outputs forces codegen to refresh. This build uses `--no-dbp`. With DBP, the
default, the deferred back job reports `DBP b: ..., generated code out of date`
instead:

```text
$ rm out/types/proj/b.c out/types/proj/b.h out/types/proj/b.closures.inc
$ acton build --verbose --no-dbp
Building project in /path/proj
Resolving dependencies (fetching if missing)...
  Stale b: generated code out of date {impl c missing -> 7aa13f90, h missing -> 7aa13f90, support missing -> 7aa13f90}
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
