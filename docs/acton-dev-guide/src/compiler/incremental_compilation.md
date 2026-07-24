# Incremental Compilation

See also [Imports and environments](imports_and_envs.md) for the distinction
between scheduler dependency discovery and the actual import closure loaded into
the type-checker environment.

Acton compiles a whole program by walking a dependency graph that spans the
root project and all of its dependencies. The scheduler in `Acton.Compile`
builds this total graph and runs the front passes (parse, kinds, types) in
topological order. Whole-module back jobs can overlap ongoing front work.
Selective back jobs are held until all required front passes have succeeded,
because their reachability closure spans the complete program. The scheduler
runs tasks concurrently with async jobs across worker threads. In watch/LSP
mode, each change event bumps a generation id, cancels
in-flight tasks, and drops stale diagnostics or back jobs that still carry the
old generation, so we can react to new changes without waiting for obsolete
work to finish.

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
The implementation hash (**moduleImplHash**) combines per-name `implHash`
values with a module-owned component. That component hashes the ordered source
imports, mandatory ownerless statements, and the ordered owner list for every
top-level statement. Reordering imports or independently hashed definitions
therefore changes the module hash when it changes module initialization order.
It is stored in the `.tydb` header.

Generated `.c`/`.h` files carry an "Acton codegen hash". A whole-module back
pass combines the compiler identity and interface version with
`moduleImplHash`, `moduleSrcBytesHash`, and the line-emission mode, because
generated `#line` directives and source mappings depend on raw source and that
mode even when semantics do not change. Selective output instead fingerprints
the complete projection universe, including compiler identity and interface
version. A deferred module forced whole by a native provider
closure starts with the same implementation-plus-source hash and additionally
binds it to the public hashes in the exact interface snapshots captured for
that batch. Those snapshots also retain the ordered source imports used to
rebuild each module environment. If the tag is missing or mismatched, we rerun
back passes even if no other hashes changed.

Hashing within a module is done per top-level name. A hashable unit is a `Def`,
`Actor`, `Class`, `Protocol`, `Extension`, or any top-level value introduced by
a binding statement such as `a = 123` or `b, c = f()`. Entries are keyed by the
local `Name`; external references are stored as `QName` (canonicalized to
`GName` with `unalias`), and extensions use the derived name from
`extensionName`. Top-level expressions and control flow with no typed binding
belong to the module initialization component instead of an invented name. For
now class/actor implementation hashes are coarse and cover the entire body;
per-method hashing can refine this later.

Each unit has three hashes. **srcHash** structurally hashes that name's fragments
from the parsed AST and is the earliest signal that a single declaration
changed. The feed ignores source locations and does not pass through the pretty
printer, so formatting and position changes do not alter a per-name source
hash. **pubHash** structurally hashes the unit's public interface (doc-free
`NameInfo`) together with the public hashes of external declarations referenced
by its type signature. **implHash** structurally hashes the final typed fragments
and combines them with the implementation hashes of declarations referenced
from the body. Declaration docstrings are still part of the parsed and typed
fragments, so they currently influence `srcHash` and `implHash`; that
normalization can be tightened without changing the dependency model.

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
The required `module-hash` row stores the ordered statement-owner schedule. Its
module-owned structural hash covers the ordered source imports and mandatory
ownerless statements. The latter additionally contribute local implementation
dependencies and external public/implementation snapshots to that row. Their
external dependencies also enter the normal dependency-module rows, without a
per-name user entry.

Mutually recursive groups are hashed as a unit to avoid fixed-point iteration.
We compute `selfPubHash`/`selfImplHash` for each unit, compute a `groupHash`
from the sorted self-hashes plus all external dependency hashes of the group,
and then finalize each unit as `H(selfHash || groupHash || unitKey)`.

Header reads return the per-name hashes in deterministic name order. See
[Interface caches](interface_caches.md) for the exact LMDB key layout.

```
version
meta              -- source metadata + module src/pub/impl hashes
module-hash       -- source imports + owner schedule + ownerless init hash/deps
imports
deps/*            -- structurally keyed dependency-module rows
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

Every eligible source module defers its back passes after the front passes have
committed its interface. Eligibility does not depend on module size or number
of names. Fresh and cached front results both register a `DeferredBackJob`,
which retains only paths, options, and the canonical module name so the large
typed front result can be released. `--no-dbp` disables this path. `__builtin__`,
modules containing `NotImplemented` native hooks, persistent `--db` builds,
`--only-build`, and alternate-output modes such as `--cgen` and `--hgen` use the
normal whole-module path. Database restoration is a dynamic root surface: an
actor or message class can be reached by its stored class id even when no
current source expression constructs it, so it remains whole until persistence
has an explicit schema/root manifest. A selection that reaches dynamic
`serialize` or `deserialize` also chooses a whole deferred batch because runtime
type names can reach classes not represented by static construction edges.

After all required front passes succeed, the scheduler seeds one global
selection from the executable roots recorded in the root modules' interfaces:
`main`, `test_main`, or the explicitly named root. The worklist closes those
seeds across every selectively deferred module. Its persisted summaries carry
exact `Declare`, `Need`, `Construct`, direct member, dynamic dispatch, and
reflection edges using canonical module, top, and member names. Construction
also replays pending dispatch and reflection against newly reachable concrete
types. Every lookup is for one exact key; a missing or inconsistent reachable
row is a compiler error, and selection never substitutes a broader key.

The front pass stores both syntax fragments and the semantic index needed by
that worklist:

- top-level statement rows, compact container shapes, method ABI slots, and
  member bodies keyed as `Method`, `Attr`, or `InitRest`
- reachability rows keyed by top, member, shape, effective slot, and reflectable
  attribute, plus a mandatory module summary and a whole-module aggregate
- exact inferred headers and name hashes used to rebuild the selected type
  environment and fingerprint opaque declarations

The mandatory module summary is seeded for every selective module. The
whole-module summary aggregates all top, shape, member, initializer, generated
slot, and mandatory edges. A module whose own C/H surface must stay whole uses
that aggregate to contribute exact interest to selective providers without
loading all of those provider rows. It also records inherited class-table value
slots that whole CodeGen initializes unconditionally.

Class-suite initialization and the declarative prefix of `__init__` are split
into per-attribute fragments. Selecting an attribute activates its class-suite
static initializer; its constructor fragment activates only when a compatible
receiver is initialized. Conditional class/actor-suite alternatives that
initialize several attributes are stored as an explicit atomic group, so
selecting one member adds the other exact named fragments in that group without
wildcard interest. The remaining constructor statements live in `InitRest`;
this preserves the
operational constructor body without retaining initializers for unselected
attributes. Nested control flow in the declarative prefix is projected with
the same per-attribute rule.

Selection materializes a typed module projection directly from these `.tydb`
rows. It reads only the selected tops, container shapes, members, initializer
fragments, and compact inferred headers. It neither reads the source file nor
decodes a full typed module and then prunes it. The resulting projected module
and projected type environment enter the ordinary back-pass chain, with the
original witness indexes retained for forwarding resolution.

Some compilation units deliberately form opaque boundaries. A directly
requested rootless module is a whole library surface. Within a declared
`Build.act` library, externally exposed or terminal modules likewise keep their
complete generated C/H surface, while internal provider modules remain
selective. These outputs are consumer-specific: a later consumer reruns the
provider selection instead of requiring every possible future public method in
the current artifact. Each whole surface contributes the exact interest of all
bodies it emits. Construct, direct-call, dispatch, and reflection edges can
traverse its persisted shape and slot rows into a selective inherited provider
without materializing the whole module. Across such an opaque inheritance
barrier, all inherited attributes are retained, including private padding, so
the whole subclass and projected base agree on object layout.

`__builtin__` is opaque for code materialization, but source-backed builtin
functions retain their semantic reach summaries. Constructed witness
dictionaries retain every concrete slot. Runtime value conversion slots and
`__next__` are constructor obligations because native builtin code can invoke
them after the typed front tree has been summarized. These are bounded,
explicit runtime contracts rather than wildcard selection.

A module containing `NotImplemented` is different because its hand-written
`.ext.c` can contain references absent from the typed Acton rows. Such a native
module and its transitive provider closure therefore run whole back passes.
This is a chosen compilation mode, not recovery from a failed selective
lookup.

All project output roots participating in one selection share canonical,
sorted `.acton.output.lock` files. Dependency refresh and project discovery
happen before choosing that lock set; planning, interface reads, back passes,
and native consumption stay inside it. This prevents concurrent root projects
from replacing a shared provider projection underneath one another.

One projection-universe hash covers the canonical selection facts, the
structural hashes of every materialized module and projected type environment,
and the interface fingerprints of selected opaque names. It is also bound to the
public hash of every interface captured for the lazy back-pass environment.
This last guard is deliberately conservative: codegen can still ask lazy
witness, descendant, attribute, and extension indexes questions that are not
represented by an exact selected content row. Until those query buckets have
their own persisted fingerprints, a public-interface change anywhere in the
captured closure invalidates the selective outputs. It does not widen selection
or load unrelated content, and implementation-only changes outside the
projection do not enter this guard.

Each selected module's codegen hash combines that universe hash with its module
name. A changed consumer selection therefore invalidates its provider output.
Existing `.c` and `.h` files whose generated "Acton codegen hash" tags already
match are skipped.

Selective reads also form a consistent interface snapshot. Each semantic
`.tydb` write or hash refresh assigns a new interface generation. After
resolving a module path, one read transaction captures its generation, hashes,
identity, ordered source imports, closure imports, roots, native flag, and
documentation. The compiler follows only those captured closure imports. Exact
lazy reads validate their expected generation in the same transaction as the
requested row. Once the complete selective/native batch has been prepared, the
scheduler validates its whole captured closure before enqueueing any back job
and again after the last job has produced its output.

The back-pass environment is rebuilt from generation-bound interface handles.
Every lazy name or query-index read checks the expected generation in the same
transaction as the requested row. A changed path, generation, or snapshot fact
aborts selection instead of mixing interface generations. This is separate
from the scheduler generation that prevents an obsolete watch/LSP compile from
publishing back-pass output.

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
  Stale b: generated code out of date {codegen c missing -> 7aa13f90, h missing -> 7aa13f90}
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
