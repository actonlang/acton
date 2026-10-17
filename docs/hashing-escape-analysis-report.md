# Hashing checker and escape analysis status

16 October 2026 — `experimental-escape-analysis`

Normal compilation now checks the lifetime of the hasher argument specifically
in `Hashable.hash` implementations. This preserves automatic storage for the
hasher and its native state without requiring general parameter escape analysis
for every function in a module.

## Current implementation

The hashing checker examines hash implementations and supports forwarding to
other checked `Hashable` implementations, including user-defined classes,
generic fields and tuples. Local aliases, conditionals, loops and built-in
hasher operations are supported. Passing the supplied hasher to an arbitrary
helper or capturing it in a closure is conservatively rejected. Hashing must
also agree with equality; that separate obligation remains the programmer's.

The [Hashable guide](acton-guide/src/stdlib/builtin_protocols/hashable.md)
documents the rules and examples, including the explicit
`Hashable.hash(value, h)` call needed for concrete tuple fields. The checker
reuses the local value-flow machinery in
[EscapeAnalysis.hs](../compiler/lib/src/Acton/EscapeAnalysis.hs), while
[Compile.hs](../compiler/lib/src/Acton/Compile.hs) selects which analysis runs.

**Full escape analysis remains present and is invoked only when Acton is called
with `--escape-analysis`**, for example `acton build --escape-analysis`. It still
computes general parameter summaries, can consult summaries from imported
modules, and records its summaries in interfaces. Normal builds do not compute
or populate these general summaries. The hashing checker always applies;
enabling the full experiment does not relax its restrictions.

## Performance and validation

Two comparison runs supplied for the large `juniper_crpd` module showed about
47 seconds added to the reported type-checking time, roughly 48%, and about
59 seconds added to the complete build, roughly 5.3%. That timing includes
escape analysis rather than isolating type inference. The extra cost is
consistent with adding a serial pass after parallel type reconstruction,
although the measurements do not isolate all allocation and GC effects.

Small local measurements compared the two analyses on already forced, cached
typed modules:

| Module | General analysis | Hashing checker |
| --- | ---: | ---: |
| Pruned Cisco module | 23.9 ms | 0.15 ms |
| `testing` | 12.5 ms | 0.79 ms |
| `__builtin__` | 10.4 ms | 0.33 ms |

These are isolated pass timings with imported summaries unavailable. The Cisco
module has 604 functions and no hash implementations, so their bodies are
skipped. The large Juniper build still requires validation on a machine with
sufficient memory.

Validation includes `make`, the full library suite of 638 tests, eight focused
hashing tests, and 112 compiler/runtime cases across the broad run and focused
reruns. A compiled example verified nested user-class and tuple hashing,
control flow, and dictionary lookup. A bytes-field user class also retained
zero measured allocation over one million and two million hash calls; this
does not imply that scalar boxing or arbitrary hash bodies allocate nothing.

## Future parallel escape analysis

The current full analysis is sequential within each module. Its architecture
does not require all its work to remain sequential. A practical progression is:

1. Measure discovery, witness metadata collection, function scanning, graph
   solving and summary serialization separately, including CPU time and memory.
2. Build shared lookup indexes once, then scan functions concurrently into
   independent fact sets. Merge the results deterministically and initially
   retain the existing sequential graph solver. Use a bounded worker queue to
   control memory, as parsing and type reconstruction already do.
3. If graph solving proves significant, process independent dependency groups
   concurrently while solving mutually recursive groups together. Reuse
   unchanged function summaries where possible, with dependencies included in
   incremental invalidation.

An initial parallel version can run after type reconstruction. Integration
with type-checking workers could follow, but must account for shared witness
bindings and calls between declarations; source chunks cannot simply be
analyzed as independent programs.

General analysis could later support checked borrowing contracts outside
hashing and permit verified hasher helpers. It could also contribute to
allocation optimizations for temporary objects, iterators and closures. Those
optimizations need allocation-site and lifetime reasoning beyond the current
parameter summaries, particularly when continuations outlive C stack frames.
The immediate hashing improvement does not depend on that further work.
