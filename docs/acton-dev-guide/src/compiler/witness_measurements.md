# Initial witness measurements

The `witness-local-dedup` work implements the first two stages of the witness
plan: opt-in statistics and stronger local sharing. The baseline is main at
`7153db457dd6d43d0beaf237ead16ff1c91f49e7`, with identical statistics instrumentation
added for comparison. The optimization is independent of the pending closure
and stack-iterator branches.

## Corpus and method

The 2026-10-20 survey compiled 175 Acton source files: 27 from `base/src`, 35 from
acton-yang, 95 from StratoWeave, and 18 small programs. Sources were copied before
measurement. Both application repositories were clean: acton-yang at
`3a544347b506997e49b45ffbb832300a3cfa5010`, StratoWeave at
`2059737715c737fa069ad0011f2796c48e7c70ed`. Source hashes and detailed counts are in
[the measurement data](witness_measurements.json).

The small corpus combined `examples/{eq_list_dict,inter,server,reprtest,sieve}.act`
with nbody, fannkuch-redux, helpers, both coroutine sieves, fasta, knucleotide,
lru, mandelbrot, merkletrees, nsieve and pidigits from `~/acton-local/bench`, plus
a repeated nested-list comparison workload. The new `test/perf/src/witnesses.act`
keeps both a concrete and a generic version of the useful comparison case.

Project commands used `build --always-build --skip-build --witness-stats --quiet
--jobs 4`, with `--dep yang=../acton-yang` for StratoWeave. The copied base project
retained its native dependency paths through links to the existing distribution.
Statistics for each application include only its own module prefix; dependency
modules are not added to its totals. These are full frontend/backend compiler
runs, excluding the final native C build. Runs were sequential, with no compiler
rebuild occurring alongside them.

## Surviving witnesses

Counts below are after boxing, including inline constructor expressions. The
reduction column applies to nonstatic recipes, not to all program allocation.

| Corpus | Modules | Witness bindings, before → after | Nonstatic recipes, before → after | Reduction |
| --- | ---: | ---: | ---: | ---: |
| base | 27 | 1,461 → 1,018 | 123 → 67 | 45.5% |
| acton-yang | 35 | 2,742 → 2,212 | 1,477 → 983 | 33.4% |
| stratoweave | 95 | 7,698 → 6,660 | 1,693 → 1,207 | 28.7% |
| eq_list_dict | 1 | 40 → 22 | 34 → 16 | 52.9% |
| witness_workload | 1 | 8 → 4 | 6 → 2 | 66.7% |

Most numeric and sieve controls have zero nonstatic witness recipes after
boxing in both compilers. This is useful evidence that their large allocation
costs come from other machinery. In fannkuch-redux, nonstatic recipes in the
typed tree fall from 27 to 2, but boxing removes all of them in both versions.
That typed-tree reduction gives no witness allocation saving at runtime.

Remaining concentrations include `yang.gdata` (449 recipes), `yang.schema`
(269), and `stratoweave.ttt` (193). These are useful candidates for the later
registry work. Syntactic repetition across functions does not by itself prove
that the witnesses are closed or that their construction can be shared.

## Generated C volume

Generated C bytes exclude implementation-hash comments and `#line` directives;
header bytes are complete files. Counts cover the same own-module sets.

| Corpus | C bytes, before → after | Header bytes, before → after |
| --- | ---: | ---: |
| base | 4,768,549 → 4,613,488 | 1,544,720 → 1,507,938 |
| acton-yang | 7,525,653 → 7,465,976 | 1,715,631 → 1,711,697 |
| stratoweave | 65,223,956 → 64,880,402 | 25,985,333 → 25,918,450 |

The reduction in whole-file volume is modest because method bodies, closures,
serialization and unrelated initialization still dominate these files.

## Runtime allocation experiment

The concrete helper compares two prebuilt `list[list[int]]` values three times.
An actor calls it one million times, reading the RTS allocation counter directly
before and after the loop. Both versions were compiled with `--fast` and returned
`True`. Three runs allocated approximately 192.10 MB before and 64.14–64.22 MB
after the change: about 128 MB, or two thirds of the measured allocation, saved.

A separate measurement-only C wrapper incremented a counter at each
`B_EqD_listG_new` call in the workload's translation unit. It measured exactly
6,000,000 calls before and 2,000,000 after. It left the constructors in place;
normal compiler/runtime sources contain no such counter. This confirms the
allocation mechanism, rather than inferring it solely from AST counts.

Uninstrumented warm runs took about 0.06 s before and 0.05 s after, which is too
short for a strong runtime-speed claim. `examples/eq_list_dict.act` also built and
ran with identical expected output in both versions.

## Compiler resources

These are medians of three runs per compiler, alternating compiler order. The
same four-job, stats-enabled, compile-only commands were timed with
`/usr/bin/time -l` on the local Mac. Every run produced the same own-module
statistics for the recompiled modules; all records parsed successfully under
parallel compilation. The base timing repeats recompiled 26 modules, with the
bootstrap `__builtin__` already cached in both versions. The initial fresh-copy
survey above includes all 27; the bootstrap uses a separate freshness path.

| Corpus | Wall seconds, before → after | User seconds, before → after | Peak RSS MiB, before → after |
| --- | ---: | ---: | ---: |
| base | 1.13 → 1.09 | 2.59 → 2.56 | 801 → 810 |
| acton-yang | 11.35 → 11.33 | 20.33 → 20.40 | 1089 → 1103 |
| stratoweave | 19.90 → 19.94 | 70.69 → 70.46 | 1660 → 1585 |

Total compilation time is essentially unchanged on this corpus. Peak memory is
broadly similar and varies between runs. The indexed lookup removes the usual
linear search over preceding witness bindings, but these measurements do not
establish an end-to-end compiler speedup or a general memory reduction. The
statistics traversal itself is opt-in and included in the timings above.

## Verification and next work

`make` succeeds. All 667 compiler library tests pass, including alias/projection
normalization, collisions, type/default equality, scope shadowing, safe outer
reuse, branch separation and a 4,000-recipe stress case. The three changed
`lines` snapshots remove two duplicate optional-int witnesses and redirect their
uses. Both new performance cases passed as native Acton tests. The broad native suite was not run at that stage. Broader native verification
was subsequently authorized and is recorded with stage 3 below.

The measurements justify continuing to a module-wide description graph, while
keeping runtime-dependent witness parameters explicit. First classify the
remaining recipes by dependency and lifetime, so closed witnesses can be
distinguished from generic activation-dependent ones. That graph could then
serve escape analysis as well as sharing. Dominance placement, loop-invariant
movement and closed singleton pooling remain later steps; the current change
preserves witness construction and serialization conventions.


## Stage 3: module-wide descriptions

The 2026-10-21 survey uses the same 175 source files, checked against the saved
source hashes. Its baseline includes stages 1–2 on main `e871f7b0`, and its
comparison adds the diagnostic registry. Thus these measurements isolate the
registry; they do not repeat the earlier local-sharing optimization comparison.
The work was subsequently rebased onto main `46e1ccaf`, including the merged
builtin iterator optimization. Final verification on that base is recorded
below. Detailed counts, source hashes, compiler binary hashes and resource
samples are in [the stage 3 data](witness_registry_measurements.json).

After boxing, the remaining nonstatic construction sites classify as follows:

| Corpus | Modules | Closed sites | Parameter-dependent sites | Local sites | Unknown sites | Distinct closed descriptions |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| base | 27 | 56 | 11 | 0 | 0 | 30 |
| acton-yang | 35 | 925 | 53 | 0 | 5 | 190 |
| StratoWeave | 95 | 1,194 | 13 | 0 | 0 | 381 |
| eq_list_dict | 1 | 16 | 0 | 0 | 0 | 14 |
| witness_workload | 1 | 2 | 0 | 0 | 0 | 2 |

The main opportunities are now concrete: `yang.gdata` has 441 closed sites with
42 descriptions, `yang.schema` 230 with 49, and `stratoweave.ttt` 185 with 35.
The five unknown sites are in `yang.adata`, which includes dictionaries obtained
from generic instance fields. These remain conservative rather than being
promoted through field analysis. After boxing, acton-yang has two nonstatic
sites syntactically inside loops
and StratoWeave has five. Loop counts are not execution frequencies: a function
containing a site can also be called repeatedly from a loop.

These numbers support investigating closed witness sharing across functions.
They do not predict allocation savings: description count is neither the number
of runtime constructor calls nor a proof of legal placement. Generic parameters
introduced by lifting are classified by their actual local bindings, without
propagating the arguments supplied at their call sites.

Every existing statistics field agrees between the two compilers, at all three
stages. All 350 generated C/header files agree after excluding implementation
hash comments and source-path `#line` directives. The native `eq_list_dict`
outputs agree. The million-call witness workload returns `True`, allocates
64.20–64.23 MB in both versions across three runs, and still makes exactly
2,000,000 instrumented `B_EqD_listG_new` calls. This is expected: stage 3 describes
constructions and changes no allocation policy.

Resource measurements again use three sequential, alternating, four-job,
compile-only runs with statistics enabled. The cached base repeats contain 26
modules; the initial fresh-copy count above contains all 27. Medians are:

| Corpus | Wall seconds, baseline → registry | User seconds, baseline → registry | Peak RSS MiB, baseline → registry |
| --- | ---: | ---: | ---: |
| base | 1.09 → 1.09 | 2.52 → 2.55 | 800 → 803 |
| acton-yang | 11.36 → 11.50 | 20.39 → 20.76 | 1,058 → 1,195 |
| StratoWeave | 19.68 → 19.50 | 68.59 → 68.34 | 1,470 → 1,426 |

Wall-time differences are small. Peak memory varies, including an increase for
acton-yang in these samples; there is no claim that diagnostics are free or
reduce memory. The graph is constructed only with `--witness-stats`, so normal
builds do not pay for this analysis.

On main `46e1ccaf`, `make` and all 729 library tests pass. All 525 own-module
statistics records (175 modules at three stages) agree with the pre-rebase
survey, including the registry summaries. The broader native selection passes
all 267 tests: Builtins auto, Core language, Regression auto, Regression
segfaults, Examples and compiler tests, using two test jobs. This includes
release CPS forwarding, release iterator lifetimes, witness imports and generic
regressions. It is not the entire native suite: cross-compilation, stdlib,
RTS/backend and benchmark groups were not included.

The native run initially hit a full disk. Removing the rebuildable Zig local
cache, including its stale manifests, freed about 48 GB; the complete clean-cache
retry passed. This cleanup happened after the paired measurements above.

The next optimization stage should consume these descriptions to choose legal
sharing locations. Before promoting closed recipes, it must establish the
initialization and serialization policy and account for dependency order.
Generic recipes need scope/dominance placement; equal spelling of witness
parameters is insufficient. The graph can later support escape analysis, but
this stage does not replace that pass's witness resolution.


## Stage 4: sharing inline constructions within a function

The 2026-10-22 comparison starts from stage 3 (`fad5a7c3`), already based on
main `46e1ccaf` with the iterator optimization. It adds the actual placement
pass, `Acton.WitnessSharing`, at the end of boxing. The same 175 modules and
saved source hashes are used. This is an additional saving beyond the original
local-equation optimization. Detailed counts, source/binary hashes and samples
are in [the stage 4 data](witness_sharing_measurements.json).

The pass introduces an ordinary typed local at an existing unconditional
construction point and reuses it at dominated later uses. It handles closed
recipes and safe generic-parameter recipes. Branch-local definitions stay in
their branch; loop-body definitions stay in the loop. Initializers, optional
witness parameters and unknown/local dependencies are excluded. The guide gives
[a worked example and the placement rules](witnesses.md#sharing-inline-constructions-after-boxing).
There are no new global witness objects, cache lookups or object layouts.

After boxing, nonstatic constructor expressions change as follows. These are
remaining sites in generated functions, not runtime call counts:

| Corpus | Modules | Sites, before → after | Removed |
| --- | ---: | ---: | ---: |
| base | 27 | 67 → 67 | 0 |
| acton-yang | 35 | 983 → 901 | 82 |
| StratoWeave | 95 | 1,207 → 1,159 | 48 |
| small programs | 18 | 23 → 23 | 0 |

In acton-yang, 81 removed sites are closed and one is parameter-dependent; in
StratoWeave, 44 are closed and four are parameter-dependent. `yang.gdata`
accounts for 58 of the removed sites (449 → 391); `stratoweave.ttt` falls from
193 to 182. The five unknown sites in `yang.adata` remain unknown. Typed and
normalized statistics agree at every module: this pass changes only the final
boxed tree. The new recipe/scope-pair counter helps distinguish repetition
within a function from repetition across functions, but a shared scope alone
does not prove that construction can be moved across branches or initialization.

The base and small-program C/header files are unchanged. All headers in both
larger projects are also unchanged. Nine yang and fifteen StratoWeave C files
change. C volume is essentially flat: yang decreases by 370 bytes and
StratoWeave increases by 221 bytes, after excluding implementation-hash comments
and `#line` directives. Local variables replace repeated expressions; this is
an allocation optimization rather than a substantial C-volume reduction.

### Runtime allocation and serialization

The new workload compares two prebuilt `list[int]` values through two expanded
object default arguments. Those arguments contain equal inline list-equality
witness constructions that the earlier witness-equation pass did not share.
Over one million helper calls, the baseline allocates 144.08–144.24 MB; the
new compiler allocates about 112.06 MB in three runs. The saving is about 32 MB,
or one 32-byte witness per invocation. User objects still allocate separately.

A private measurement-only C wrapper counts 2,000,010 list-witness constructor
calls before and 1,000,006 after. Of these, the measured loop contributes
exactly 2,000,000 → 1,000,000; the remainder comes from setup and correctness
checks. The wrapper leaves the constructors in place and is absent from compiler
and runtime sources. The native workload is too short for a reliable speed claim.

The regression also exercises generic witnesses with integers and strings,
short-circuit false results, both branches of object creation, and an object
graph whose fields carry shared witnesses. Serialization/deserialization and
subsequent witness method calls succeed. Mutating one restored user object
leaves the other distinct. No serialization implementation or registration
scheme was changed.

The existing nested-list workload and all eighteen small corpus programs have
unchanged generated C. In particular, an existing witness constructed once per
helper invocation still allocates on every call. Sharing across calls or across
separate generated continuation functions remains future work.

### Compiler resources and verification

Medians of three sequential paired runs, alternating compiler order, with four
compiler jobs and statistics enabled, are:

| Corpus | Wall seconds, before → after | User seconds, before → after | Peak RSS MiB, before → after |
| --- | ---: | ---: | ---: |
| base | 1.27 → 1.27 | 2.90 → 2.96 | 800 → 799 |
| acton-yang | 12.13 → 12.05 | 22.28 → 22.23 | 1,077 → 1,061 |
| StratoWeave | 21.80 → 21.95 | 76.61 → 77.56 | 1,408 → 1,462 |

The cached base repeats contain 26 modules; the fresh-copy survey contains 27.
The pass now does real analysis during normal builds. These measurements show
small timing differences and variable memory use, not a compiler speedup.

`make` succeeds, all 751 compiler library tests pass, all four witness
performance cases pass, and the broader native selection passes all 268 tests.
The added library cases cover control-flow placement, alias and binding
identity, generic/optional parameters, initializers, shadowing, exact comparisons,
type arguments, fresh names, singleton pruning, idempotence and 4,000 unsupported
constructor arguments. The native selection is the same six groups used for
stage 3, with the new serialization/default-argument regression included; it is
not the entire native suite.

The larger remaining opportunity is sharing closed witnesses across functions
and invocations. That should be a separate stage with an explicit initialization
and serialization policy. This stage establishes a working consumer of the
registry while preserving ordinary witness construction and persistence.
