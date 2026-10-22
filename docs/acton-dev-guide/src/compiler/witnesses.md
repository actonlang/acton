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

## Sharing inline constructions after boxing

Stage 4 adds `Acton.WitnessSharing`, called at the end of `doBoxing` in normal
builds as well as diagnostic builds. Earlier reuse works on witness equations;
this pass also catches equal constructors embedded in expressions, including
expanded default arguments. It runs after boxing removes unused equations, so
those equations cannot accidentally become eager allocations again.

For example, consider this schematic generated function:

```text
consume(make_list_eq(element_eq))
consume(make_list_eq(element_eq))
```

It becomes:

```text
shared = make_list_eq(element_eq)
consume(shared)
consume(shared)
```

`shared` is a typed local variable holding the ordinary witness pointer. The
witness constructor, object layout, method table and serialization methods are
unchanged. Generic `element_eq` remains a parameter of this invocation. Calling
the function again still constructs its own witness.

The pass builds a function-local registry with the same binding identities and
recipe rules as the module-wide diagnostic graph. It considers only known
compiler-generated witness constructors with pure initializers, and only closed
recipes or recipes depending on this function's type/witness parameters.
Optional witness parameters, local-value dependencies and unknown recipes are
excluded. Explicit type arguments and the selected implementation must match.
Initializers are skipped because their receiver fields may still change.

A new local is placed immediately before the first unconditionally evaluated
statement that needs it. Later uses can reuse it only where that definition
already dominates them. An outer definition can be used inside a branch or
loop; a definition made inside one branch or loop body does not become
available after that region. A constructor in a zero-trip loop body stays
inside the body. Conditional expression arms, short-circuit right operands and
later `elif` conditions can use an existing local but cannot create one outside
their condition. Nested functions, lambdas and `Let` expressions are barriers.
A proposed local used only once is removed again.

This policy does not pool witnesses across functions or invocations, create a
runtime cache, or introduce module-level singleton initialization. Source
procedures split into several generated continuation functions are subject to
the same function boundary. More ambitious placement needs further work on
initialization order, lifetime and serialization policy.

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

## Module-wide description graph

Stage 3 adds the module-wide diagnostic graph in `Acton.WitnessRegistry`.
The full module graph is built only by `--witness-stats`; the graph itself does
not rewrite allocations. Stage 4 uses its function-local builder during normal
compilation to support the placement pass described above.

Each recipe records its selected implementation, canonical dependency IDs,
captured bindings and dependency lifetime. Explicit constructor type arguments
are retained, with type variables identified by their binding rather than
spelling. Witness aliases and projections become graph references. Witness
assignment roots retain their original type, execution scope and source
location; constructor sites and witness-variable uses are recorded separately.
Recipe IDs are internal to one registry build, not persistent interface IDs.

| Lifetime | Meaning |
| --- | --- |
| `closed` | No runtime-value or type-parameter captures were found |
| `parameter_dependent` | Depends on generic witness/type parameters, possibly captured from an outer declaration |
| `local_dependent` | Depends on a local value or a binding conservatively invalidated at a control-flow join |
| `unknown` | Contains an unresolved dependency or an unsupported computation |

Closed and parameter-dependent descriptions are interned using structural
fingerprints followed by exact expression/implementation comparison. Two
parameters with the same name in different functions have different binding
identities. An inner function can retain the identity of a captured outer
parameter. Reassignment and type shadowing receive fresh identities. Local and
unknown recipes are not merged or inserted into the canonicalization index.

The graph follows source order. Forward witness dependencies and cycles remain
unknown;
branches and exception/loop joins conservatively invalidate affected aliases.
It does not perform dominance, interprocedural argument propagation or field
analysis. For example, a witness loaded from an instance field can remain
unknown even when a human can identify the dictionary stored there. A witness
constructor inside a lambda starts a new execution context for loop counts.

Version 1 statistics now optionally include a `registry` object:

| Field | Meaning |
| --- | --- |
| `recipe_nodes`, `dependency_edges` | All description nodes, including projections and parameter roots, and direct dependency edges |
| `binding_roots`, `uses`, `scopes` | Recorded witness assignments, witness-variable references and execution contexts |
| `constructor_recipes`, `constructor_sites` | Canonical constructor descriptions and original construction sites, respectively |
| `constructor_recipes_by_lifetime`, `constructor_sites_by_lifetime` | Those counts split by dependency lifetime |
| `nonstatic_sites_by_lifetime`, `nonstatic_recipes_by_lifetime` | Construction sites outside existing static-witness rules, and their distinct descriptions |
| `nonstatic_recipe_scope_pairs_by_lifetime` | Distinct (recipe, execution scope) pairs outside static-witness rules; helps separate repetition within functions from repetition across functions |
| `nonstatic_loop_sites_by_lifetime` | Nonstatic construction sites within loops in their execution context |
| `nonstatic_closed_constructors` | Closed, nonstatic sites grouped by selected implementation |

The summarizer accepts older records without `registry` and displays lifetime
counts when present. Counts of distinct descriptions are summed per module;
there is no cross-module canonicalization.

A shared description is not a shared allocation. In particular, `closed` does
not establish that eager module initialization is legal, that a binding
already dominates a use, or that its initialization/serialization dependencies
are ready. Even a parameter-dependent description captured by two functions
still needs a placement decision. Stage 4 makes a conservative decision within
each generated function. Singleton initialization, broader code motion and
integration with escape analysis remain subsequent work.

## Repeatable performance cases

`test/perf/src/witnesses.act` contains concrete and generic nested-list equality
helpers. Each makes three comparisons using the same two levels of witnesses.
Two additional concrete/generic helpers exercise equal inline witnesses in
expanded default arguments. Their user objects must remain distinct even when
the witnesses are shared. Inputs are prepared outside the measured loop; each
loop body calls one helper. These are fixed workloads, independent of scale:

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
