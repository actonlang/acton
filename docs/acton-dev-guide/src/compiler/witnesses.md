# Witness construction and local sharing

Witness objects carry protocol implementations. Their identity is unobservable,
so equivalent compiler-generated constructions can share an existing binding.
`Acton.Transform.wtrans` performs this reuse within its existing lexical
regions. It expands witness aliases before comparing recipes, and indexes
recipes by a structural fingerprint of their result type and expression.

The fingerprint is an index, not an equivalence proof. Reuse also requires exact
`Type` and `Expr` equality, including the selected constructor, type arguments,
and dependency witnesses. Locations are ignored, following the AST's equality
rules. Optional defaults contribute their presence to a type's fingerprint, but
not their expression metadata. Unusual expression forms use a conservative
collision bucket and the existing exact comparison.

The index follows the persistent lexical environment. A binding is available
only after its definition; sibling branches do not acquire each other's
bindings. Shadowing a term or type dependency invalidates the index. Safe reuse
of an already available outer binding is retained. This step performs no code
motion and introduces no runtime cache or singleton initialization scheme.

## Collecting statistics

Run a complete compiler pass over the desired corpus:

```sh
acton build --always-build --skip-build --witness-stats --quiet --jobs 4 > witnesses.jsonl
python3 compiler/lib/bench/witness-stats.py witnesses.jsonl
```

The Python command is relative to the Acton repository. For a project with
locally overridden dependencies, use the same dependency arguments as its normal
build, for example `--dep yang=../acton-yang`. Add `--prefix yang.` or
`--prefix stratoweave.` to the summarizer to select the project's own modules.
Its `--json` option produces an aggregate JSON report.

`--witness-stats` accompanies normal compilation and code generation. It does
not force a cached frontend to run; `--always-build` is needed for a complete
survey. It disables deferred back passes for newly compiled modules so the
survey sees their full trees. The bootstrap `__builtin__` uses a separate cache
path; collect its statistics from a fresh copy of the base project. The extra traversal and original extension
environment are retained only when the diagnostic flag is enabled. Parallel
passes serialize complete UTF-8 JSON records to stdout.

Version 1 records are emitted after type reconstruction (`typed`), normalization
(`normalized`), and boxing (`boxed`). The typed tree has already undergone the
existing witness transformations; these are not raw solver equation counts.

| Field | Meaning |
| --- | --- |
| `bindings` | Assignments to generated witness names |
| `alias_bindings`, `lambda_bindings` | Reference/projection aliases and coercion lambdas still present at this stage |
| `static_bindings` | Witness RHSs recognized by code generation's builtin static-witness rules |
| `constructor_expressions` | Calls to identified extension, primitive, or generated witness constructors, including inline expressions |
| `nonstatic_constructor_expressions` | Constructor expressions outside the builtin static-witness rules |
| `nonstatic_constructor_expressions_in_loops` | Those expressions syntactically within loops or comprehensions in their execution context |
| `distinct_constructor_expressions` | Distinct expressions under the compiler's structural equality; summed module counts are not a global unique count |
| `constructors`, `nonstatic_constructors` | Per-constructor histograms |

Defaults stored in parameter/type metadata are excluded; their expanded uses at
call sites are visited. Constructor identities come from witness RHS recipes,
original extension interfaces, and the primitive tuple witness family. The
original environment is needed because later passes turn protocols and
extensions into ordinary classes.

These are syntax counts. A repeated expression in another function need not be
shareable, and one site may execute many times. Code generation can discard a
static root's dependency expressions or duplicate an expression while rendering
C. Constructors can allocate additional objects internally. Use generated C and
runtime counters to establish actual construction frequency and heap cost.

Boxing's unused-binding removal matters: an improvement in the typed count can
have no runtime effect if boxing already removes those constructions.

## Repeatable performance cases

`test/perf/src/witnesses.act` contains concrete and generic nested-list equality
helpers. Each makes three comparisons using the same two levels of witnesses.
Inputs are prepared outside the measured loop; each loop body calls one helper.
These are fixed workloads, independent of scale:

```sh
cd test/perf
acton test perf --module witnesses --scale 1
```

For an explicit allocation experiment, call `compare_concrete` one million times
in an actor, with both input lists prepared beforehand. Read
`acton.rts.get_gc_total_bytes(env.syscap)` immediately before and after the loop.
Use identical source and optimization settings for both compilers. The initial
2026-10-20 experiment used `--fast`; it measured about 192 MB before and 64 MB
after local sharing. A measurement-only C wrapper counted six million and two
million `B_EqD_listG_new` calls respectively. The counter is not part of normal
code generation.

See [the initial measurements](witness_measurements.md) for the corpus results
and limits of that comparison.
