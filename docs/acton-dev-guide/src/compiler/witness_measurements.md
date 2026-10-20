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
uses. Both new performance cases passed as native Acton tests. The broad native
suite was not run after its execution request was rejected by the app
(`rejected by user`).

The measurements justify continuing to a module-wide description graph, while
keeping runtime-dependent witness parameters explicit. First classify the
remaining recipes by dependency and lifetime, so closed witnesses can be
distinguished from generic activation-dependent ones. That graph could then
serve escape analysis as well as sharing. Dominance placement, loop-invariant
movement and closed singleton pooling remain later steps; the current change
preserves witness construction and serialization conventions.
