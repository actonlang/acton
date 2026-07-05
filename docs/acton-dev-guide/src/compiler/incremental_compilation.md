# Incremental Compilation

See also [Imports and environments](imports_and_envs.md) for the distinction
between scheduler dependency discovery and the actual import closure loaded into
the type-checker environment.

Acton compiles a whole program by walking a dependency graph that spans the
root project and all of its dependencies. The scheduler in `Acton.Compile`
builds this total graph, runs the front passes (parse, kinds, types) in
topological order, and normally queues back passes as soon as a module's front
passes finish. Deferred back pass (DBP) candidates are held until all modules
that can add interest for that candidate have completed their front passes.
Front work gates downstream modules, while ready back jobs overlap ongoing front
work. The scheduler runs tasks concurrently with async jobs across worker
threads. In watch/LSP mode, each change event bumps a generation id, cancels
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
The implementation hash (**moduleImplHash**) is derived from per-name
`implHash` values; it is stored in the `.tydb` header. Normal generated
`.c`/`.h` files are tagged with this module implementation hash as an "Acton
impl hash". DBP generated files reuse the same tag but write a DBP codegen hash
that also includes the selected top-level names **and the kept method subset**
(two builds can select the same classes yet keep a different set of their
methods, so the kept methods are part of the key). If the tag is missing or
mismatched, we rerun back passes even if no other hashes changed.

Hashing within a module is done per top-level name. A hashable unit is a `Def`,
`Actor`, `Class`, `Protocol`, `Extension`, or any top-level value introduced by
a binding statement such as `a = 123` or `b, c = f()`. Entries are keyed by the
local `Name`; external references are stored as `QName` (canonicalized to
`GName` with `unalias`), and extensions use the derived name from
`extensionName`. Class/actor implementation **hashes** are still coarse and
cover the entire body — a change anywhere in a class re-runs that module's front
pass. What *is* tracked per method is the **dependency** information that drives
deferred back passes: each name hash also records, per class method, the
top-level names that method's body depends on (`nhMethodCodeDeps`), and the set
of attribute names it accesses through a `Dot` — method calls and field reads
alike — (`nhCalledMethods`). See
[Deferred Back Passes](#deferred-back-passes) for how that per-method dependency
graph lets a back pass regenerate only the methods the program actually reaches.

All of a module's dependency information — the impl-hash deps, the per-method
deps, and the called-method set — is collected in a **single position-tagged
descent** over the typed AST. Each local reference is bucketed by position:
*code* references (calls, constructions) versus *type* references (annotations,
return and parameter types). The impl-hash view is the union of both buckets;
the deferred-back base view is the code bucket only (so an optional `?Child`
field type does not pull its class in unless the field is actually constructed).
Computing both views from one walk means each method body is visited once rather
than once for hashing and again for back-pass selection.

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

DBP is selected after type checking, when the full `NameHashInfo` list is
available. DBP is on by default: every module is a candidate except
`__builtin__`, `--only-build`, alternate-output modes such as
`--cgen`/`--hgen`, and modules at an explicit `Build.act` library boundary.
`--no-dbp` is a hard kill switch that disables DBP for the whole build.

Modules at an explicit `Build.act` library boundary are excluded from DBP.
A boundary module is a member of a declared library that is imported by any
module outside that same library. The library boundary needs the full generated
C/H surface, so a boundary module is compiled whole. Internal library modules
remain DBP-eligible.

When a fresh front pass produces a DBP candidate, the scheduler stores a
`DeferredBackJob` instead of queueing a normal `BackJob`. Cached `TyTask`
modules can also register deferred jobs on later builds. This matters because
the provider source and implementation hash can stay unchanged while a consumer
starts selecting a different provider name. The scheduler also records
interested names in an `InterestMap`. Fresh front results contribute the
external dependency facts they just computed; cached modules reconstruct the
same interest set from the `.tydb` dependency rows.

Deferred jobs do not wait for every front pass in the project. Their wait set is
the reverse dependency closure of the deferred module in the total build graph.
After each front stage completes, the scheduler flushes any deferred job whose
wait set is now contained in the completed-front set. If a module in that wait
set fails front passes, the normal failed-build path prevents the deferred back
job from running.

When a deferred job is ready, DBP first reads the candidate's `.tydb` header.
It uses the header's `NameHashInfo` list and cached root actor names without
decoding the typed module. The initial selection seeds are:

- the collected `InterestMap` names for the module
- cached root actor names from the `.tydb` header

An empty interested-name set is valid; if the module has no root actors, DBP can
produce an effectively empty module body. DBP maps derived names back to their
owning top-level name, closes over the code-position local deps
(`nhCodeLocalDeps`, falling back to `nhPubLocalDeps`+`nhImplLocalDeps` for caches
written before code deps existed), and
uses exact `readExtensionsByClass` / `readExtensionsByProtocol`
lookups to keep top-level extension declarations required by selected classes
or protocols. Selection metadata that cannot map a seed, local dependency, or
extension back to a top-level name is a compiler error. DBP should not silently
fall back to full-module compilation except for modules that contain
`NotImplemented`/native hooks.

### Per-method selection

Selecting a class does not select all of its methods. A method is *kept* only
when it is **reached**: either it is always-kept (a constructor `__init__`,
`append`, or a dunder dispatched through a witness rather than a source call), or
its name is in the program-wide **called-method set** (CN). CN is accumulated
across the build as the union of every module's `nhCalledMethods` — the
attribute names dot-called anywhere in the program. A kept method's per-method
dependencies (`nhMethodCodeDeps`, which include the *types* it returns or uses,
not just code-position references) are followed by the selection closure; an
*unkept* method's dependencies are not, so its subtree is never pulled in.

This is what makes auto-vivifying tree code (the yang device models) compile
cheaply. A generated node class has one lazy accessor method per child. Calling
`root.configuration()` puts `configuration` in CN, so the closure follows that
accessor's deps and pulls in the configuration subtree — but a sibling accessor
that nobody calls stays unkept and its subtree is never selected.

CN is closed **transitively**: after the name closure settles, every selected
class folds its own `nhCalledMethods` back into CN and the closure re-runs to a
fixpoint. This retains a method that is reached only from another kept method —
for example a list wrapper's `create()` calls its own `get()`; `get` is never
called from source, so it enters CN only because `create` (which a consumer did
call) names it. A consumer that is reused from cache still contributes its calls:
each module persists its called-method union under a `.tydb` `called-methods`
key, so a method called only from a cached module is not dropped from a selected
provider.

Extensions are kept verbatim by the pruner, so a selected class's extension
methods are emitted whole; the closure therefore follows their *type-position*
references too (e.g. an `Indexed` extension's `__getitem__` returning the list
entry class), or that class could be left unselected.

Pruning happens on the typed module immediately before the normal back-pass
chain. Selected declarations, signatures, assignments, and compiler-introduced
`VarAssign` statements are retained by their bound top-level names, with unkept
methods removed from kept classes; `Pass` is dropped. Other source-level
top-level statements are not a fallback case because the parser rejects them
before type checking.

Before decoding the full typed module, DBP computes a codegen hash from the
module implementation hash, the sorted selected top-level names, and the kept
method subset. If the existing `.c` and `.h` files already carry that hash in
their generated "Acton impl hash" tag, the deferred job is skipped. Otherwise DBP
reads the full typed module with `readFile`, prunes it, runs the normal back-pass
chain, and writes `.c`/`.h` tagged with that DBP codegen hash. A later consumer
change that selects a different provider name or method therefore makes the
provider's DBP output stale even when the provider source did not change.

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

## Per-method interest and incremental invalidation (design)

This section describes a planned refinement, not current behaviour. Today the
called-method set (CN) is a single program-wide set of **bare** method names, and
a provider's deferred back pass is **re-selected on every build** — the codegen
hash then decides whether the regenerated output is actually new. That is correct
but does more work than necessary: selection (and the CN recomputation that feeds
it) runs even when nothing relevant to the provider changed.

### Two levels of interest

Interest in a provider `foo` has two dimensions:

- **Name interest** — which top-level names of `foo` a consumer references
  (`foo.Thing`). Already tracked per provider: each consumer contributes
  `(provider, name)` pairs (`frInterestDeps`), merged into
  `InterestMap : Map ModName (Set Name)` by set union. Because it is a union, two
  consumers referencing the same name is already a no-op on the merge.
- **Method interest** — which methods of `foo`'s classes are called
  (`thing.bar()`). This is the CN dimension, and today it is the part that is
  both global and imprecise.

The proposal makes the method dimension a peer of the name dimension:
per-provider, merged by union, and **resolved**, so overlapping consumer requests
collapse exactly like names do.

### Resolved, not bare

The current CN records `bar` from `thing.bar()` by reading the `Dot` node
syntactically and discarding the receiver, so `foo.Thing.bar` and an unrelated
`other.Widget.bar` collapse to the same bare `bar`. To make per-provider interest
precise we record the **resolved** target instead:

```text
called methods : Map ModName (Set Name)        -- ideally (ModName, Class) -> Set Method
```

so `thing.bar()` contributes to `foo`'s set and `other.bar()` to *its* provider's
set, and the two never collide. Two realities make this more than relabeling what
the checker already knows:

- **The receiver type is not on the typed `Dot` node.** `Dot {eloc, exp1, attr}`
  carries no type, and the dep walk runs without a scoped environment, so it
  cannot recover the receiver's class after the fact. The resolution *is* known
  during type inference (where the receiver type and the `Sel` constraint are
  solved); capturing it means emitting it there — as an annotation or side table
  the hash walk consumes — not re-deriving it syntactically. This is real work,
  not a free read.
- **Dispatch is often through a witness.** Type checking lowers an instance call
  `x.bar()` to a witness application such as `W_Barrable.bar(x)`; the surviving
  receiver is the witness (a protocol type), not a concrete class, so for
  protocol/generic dispatch there is no single concrete target. The key must
  therefore be `(ModName, Protocol-or-Class)`: witness receivers key by the
  protocol (covering every conforming class), and a receiver that cannot be
  resolved to a concrete provider falls back to the bare name (correct but
  imprecise, as today). Crucially, that bare-name residue must be folded into
  **both** the signature *and* the selection's reachability check, identically —
  if it appeared in only one, an unresolvable call to a `foo` method name could
  change foo's selection without moving `sig(foo)` (or vice versa). With the
  residue in both, the fallback merely over-invalidates (the imprecision the
  resolved key removes elsewhere); it never skips a changed selection. So the
  shared domain is "resolved where possible, bare where not", consumed the same
  way by `sig` and by `dbpMethodReached`.

Note that `nhCalledMethods` records *every* `Dot` attribute, including field
reads, so the resolved key must classify non-method attributes too. And the
parity with name interest is only partial: name interest is already canonicalized
`QName`s, while method interest is bare `Dot` attributes today — resolving it is
new work, not a renaming.

### Provider interest signature

Each provider `foo` gets an **interest signature**:

```text
sig(foo) = H( implHash(foo),
              nameInterest(foo),      -- InterestMap[foo], sorted
              methodInterest(foo) )   -- resolved foo-methods called anywhere, sorted
```

All three inputs are known *before* selection. `implHash(foo)` covers foo's own
code — and therefore its root actors and its internal `create`->`get` style calls.
`nameInterest` and `methodInterest` are the merged consumer demand. `methodInterest`
must be an **unconditional, source-presence** union — every (resolved) foo-method
name that appears as a call anywhere, exactly mirroring how the current base CN is
built — *not* gated by which methods selection ends up keeping. A kept-gated
definition would be circular (the kept set is the selection output the signature
is meant to predict). The selection output (selected names + kept methods) is then
a deterministic function of exactly these inputs, so the signature is a sound cache
key.

The signature is sound **only if the selection it gates consumes the same resolved
domain.** Today `dbpMethodReached` tests membership in the bare, program-wide CN,
so a call `other.Other().helper()` puts bare `helper` into the set that decides
whether `foo.Thing.helper` is kept — even though `sig(foo)`, built from resolved
interest, would not move. Gating selection on an unchanged `sig(foo)` would then
skip a provider whose kept-method set actually changed: a stale-codegen
miscompile. So resolving the called-method set is not just a precision win for the
signature; the **reachability check inside selection (`dbpMethodReached` / CN)
must use the same provider-qualified set**, so that equal `sig(foo)` really does
imply an identical selection. Signature and selection must move together.

`sig(foo)` is persisted with foo's generated output (where the codegen tag lives).
When foo's deferred job becomes ready, the scheduler computes the new `sig(foo)`
and compares it to the stored one **before** running selection:

- **equal** → foo's selection and output are provably identical; skip selection
  and codegen entirely.
- **different** → select, regenerate, store the new signature.

This is the cheap "did this provider's overall interest change?" test the
two-level model is for: unchanged consumers contribute their persisted partials
(no recomputation), the per-provider merge is set union, and the comparison is a
single hash. Selection only runs for providers whose interest actually moved, so
incremental cost scales with the change rather than with project size.

### Examples

**Overlapping interest is a no-op.** `a` uses `foo.Thing().bar()`; later `b` is
added, also using `foo.Thing().bar()`.

```acton
# a.act
import foo
def use_a() -> int:
    return foo.Thing().bar()

# b.act  (newly added)
import foo
def use_b() -> int:
    return foo.Thing().bar()
```

Adding `b` re-fronts `b` and recomputes the merged interest for `foo`. `b`'s
contribution — `(foo, Thing)` for names, `foo.Thing.bar` for methods — is already
present from `a`. So `nameInterest(foo)` and `methodInterest(foo)` are unchanged,
`sig(foo)` is unchanged, and `foo` is **not** re-selected or regenerated.

**A distinct method on the same class re-DBPs.** If `b` instead calls a method `a`
never used:

```acton
# b.act
import foo
def use_b() -> int:
    return foo.Thing().baz()
```

`methodInterest(foo)` gains `foo.Thing.baz`, so `sig(foo)` changes; `foo` is
re-selected and the new selection keeps `baz` and follows its subtree.

**The same bare name on a different provider does nothing.** If `b` calls `bar`
on an unrelated provider that happens to share the method name:

```acton
# b.act
import other
def use_b() -> int:
    return other.Widget().bar()
```

Because method interest is resolved, this contributes `other.Widget.bar` to
`other`'s set, not `foo`'s. `sig(foo)` is unchanged and `foo` is untouched — the
collision a bare-name CN would cause does not happen.

### What this requires

1. **Resolve the call target at type-check time.** The receiver type is not on the
   typed `Dot` node and the dep walk has no scoped env, so the resolution must be
   captured during type inference — where the receiver type and `Sel` constraint
   are solved — and emitted as an annotation or side table the hash walk reads.
   Define the key as `(ModName, Protocol-or-Class)`, with witness/protocol
   receivers keyed by protocol and unresolvable receivers falling back to the bare
   name. Classify non-method `Dot` attributes (field reads) too.
2. **Resolve the selection's CN, not just the signature.** `dbpMethodReached` /
   the program-wide CN must be made provider-qualified in lockstep, so the
   reachability test selection performs matches the resolved domain `sig(foo)` is
   built from. If only the signature is resolved, an unchanged `sig(foo)` could
   skip a selection that the bare CN would have changed (see the soundness note
   above).
3. **Per-provider method interest, persisted.** Compute `methodInterest(foo)` per
   provider and persist it next to the existing `(provider, name)` interest
   material so cached consumers still contribute it.
4. **Signature + early check.** Store `sig(foo)` with the provider's output and
   move the up-to-date check ahead of selection, so an unchanged signature skips
   selection as well as codegen.

Shrinking interest (the last consumer of `foo.Thing.bar` is removed) is detected
because the merged interest is recomputed from the consumers that still exist;
per-consumer refcounting would only make that recomputation incremental rather
than from scratch, and is a later optimization.

## Known limitations

Known limitations include the lack of incremental typechecking within a module,
coarse class/actor impl hashes, docstrings affecting `implHash`, the bare
(unresolved, program-wide) called-method set, and the fact that a deferred
provider is re-selected on every build rather than gated by a persisted interest
signature.
