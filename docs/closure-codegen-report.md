# Acton generated closure code review

11 October 2026. Status: implemented and locally validated; ready for colleague review. The compiler changes are currently uncommitted on `main`, relative to revision `11280260799fde63102de8ca87db5dc31825f4bb`.

The changes make generated C easier to study and reduce the amount of repeated closure code, while retaining closure serialization and deserialization. Closures keep typed capture structs and the code needed for their distinct behavior. Common field traversal and calling adapters are shared, and mechanical closure support and initialization move into an included file.

The largest measured project, StratoWeave, produces 33.1% fewer total C lines and 39.9% less optimized project instruction code. Its main C files become 82.9% shorter. These are different benefits: moving code into an include improves readability, whereas sharing and simplifying implementations reduce total source and compiled code. The project tests pass with the modified compiler. Runtime throughput and serialization speed have not been benchmarked.

## Motivation and scope

Lambda lifting creates closure classes in five families: Cont, Proc, Action, Mut, and Pure. Much of their generated C implements recurring patterns: allocating captured state, walking fields for persistence, adapting a virtual evaluation method to a continuation, and filling method tables. This machinery can obscure the C corresponding to the programmer's Acton code.

The motivating `inter.act` example, located in `/Users/sydow/acton-local/newexamples`, has 23 lines of Acton but originally generated seven closure classes and 456 lines of C. `examples/server.act` and the larger projects show the same problem at greater scale.

A fixed catalogue of predefined closures would have to cover capture arity, reference types, scalar widths, and different bodies and effects. The implemented approach allows arbitrary capture configurations: it describes the fields as data and shares operations whose behavior is independent of those fields. Distinct bodies still have distinct classes unless the lambda lifter establishes structural equivalence.

This work does not revive the unfinished `codex/generic-closures` experiment. It also leaves the proposed Bool, Show, and Serialize protocols and a general deriving feature for future language work.

## Implemented changes

### Equivalent closure classes are shared within a module

`closureConvert` interns structurally equivalent generated classes. Equality ignores source locations but includes effect family, type quantification, capture names and types, and complete method bodies. This is deliberately conservative: it does not rename captures or equate different bodies merely because their state has the same layout.

Sharing applies to the class implementation. Each construction still allocates a fresh object with its own captured state. Different expressions with the same physical capture layout can share field metadata later without sharing their code identity.

Some CPS continuations that ignore a `None` result and return a captured value to a captured continuation now use the existing serializable `$ConstCont`. Recognition requires local variable captures, preserving the timing of property and global reads. The lifted helper remains available for direct calls.

### Mechanical support moves into an included file

File builds now emit a module's closure implementations, global initialization chunks, and method-table initialization into:

```text
out/types/<module-directory>/generated/<module>.inc
```

The main `.c` includes this fragment after its ordinary definitions. It remains one C translation unit, so this organization does not introduce another link unit or a new runtime boundary. Typed declarations remain in the header, and source line directives and external C extensions remain supported. The `--cgen` diagnostic output still presents a complete listing.

The `.c`, `.h`, and `.inc` carry the same generated-code hash. Freshness checks detect a missing or stale support fragment and regenerate it without unnecessarily repeating the front passes. Module deletion and orphan pruning remove the owned fragment as well. Tests cover regeneration with dependency-based selection and stale support hashes.

### Serialization uses typed layout descriptors and shared runtime walkers

Each generated closure retains a static `$ClosureLayout` containing its object size, method table, field count, and field array. Each `$ClosureField` records a C-computed offset, actual field width, and either a scalar serialization class ID or the `$CLOSURE_REF` marker. Offsets and widths use `offsetof` and `sizeof`; the compiler does not guess C padding.

Tiny typed methods call the shared `$closure_serialize` and `$closure_deserialize` runtime walkers. Reference fields use the existing object-graph traversal. Raw scalars use the existing scalar codecs, with `memcpy` transferring exactly the field's width. Padding is not serialized, and narrow captures do not cause a full-word read beyond their storage.

Deserialization preserves the existing recovery behavior:

- A newly allocated object receives its method table and is registered before its captures are restored, allowing cycles and back-references.
- An allocation-only call with no serialization state returns the allocated object.
- A later call can fill an already allocated object.
- Shared references and external actor references continue through the existing serialization state and actor reference map.

Within a module, closures with the same ordered capture representations share one field array. Capture names and reference types may differ. Scalar class IDs remain distinct even when their widths match, and field order remains significant. Generated C static assertions verify every reused field's actual offset and width. Each class still owns its layout, size, method table, and serialization identity. Empty closures need no field array, and no descriptor pointer is added to each closure object.

The walkers exchange repeated field-specific instructions for a loop and descriptor reads. The measured code savings therefore do not establish faster serialization. Also, class sharing and `$ConstCont` substitution can change generated names and registration order: compatibility with persistent snapshots from an older build is not provided by this work.

### Constructors and calling adapters are simplified and shared

Generated closure constructors allocate and assign captures directly after the backend checks that constructor parameters match the capture properties. Their unused `__init__` method-table slot is null. A Proc closure's `__exec__` reuses `__call__` when their actual emitted C slot signatures agree.

Pure and Mut wrappers forward an already boxed evaluation result to the continuation, avoiding a redundant unbox and allocation of another box. Action preserves its two behaviors: `__call__` waits for the action result, whereas `__exec__` hands its message to the continuation.

Capture-independent Pure, Mut, and Action adapters are pooled within each module. The recognizer accepts the generated forwarding pattern, including argument order, and rejects extra local dependencies or more complex bodies. Parameter names are canonicalized and boxed pointers use the existing RTS `$WORD` representation. Arity, effect family, exact raw scalar representations, remaining conversions, and Action's wait versus message distinction remain part of the sharing key. Only identical emitted signatures and bodies share a helper.

The helpers are private static functions. Typed method tables refer to them through the existing slot casts; generated static assertions check the target slot's offset and size against the primitive closure header. Per-class adapter definitions and header prototypes disappear where sharing applies. Closure allocation and typed captures remain intact.

## Validation

The default compiler build, `make`, passed. Full `make test-compiler` validation passed: 619 library tests, 29 incremental tests, and 96 compiler tests. Golden outputs were updated for the generated class and C changes.

The new `closure_serialization` regression invokes restored closures, rather than checking only their serialized bytes. It covers mixed integer widths, floats, bools, strings, generic and protocol references, changed capture order and padding, same-width scalars with different serialization IDs, empty closures, equivalent classes with separate objects, and same-layout classes with different bodies. It also checks shared captured references, cycles through a closure, allocation followed by filling, external actor references, and continuation snapshot and resume.

C extension probes exercise restored Pure, Mut, and Action adapters with multiple arguments. They check argument order, pointer identity, result forwarding, call counts, and the distinction between an Action result and its message. Proc normal and explicit execution paths are exercised too. The regression passed in normal and `--fast` builds; focused serialization and continuation regressions also passed.

Interactive smoke tests passed for `inter` and for three server PING/PONG exchanges. Acton YANG passed all 481 project tests in one uncached iteration. StratoWeave passed 575 tests in one uncached iteration; 14 tests requiring optional external systems retained their normal skips.

One compiler invocation during development suffered a pre-parse SIGSEGV. A retry and the subsequent validation passed. Its diagnostic resembles older GHC runtime crash reports, but the cause has not been established. No such crash occurred in the larger project comparisons. `git diff --check` passed.

## Measurements

The baseline compiler was built from revision `11280260799fde63102de8ca87db5dc31825f4bb`. The comparison uses identical application sources and matching dependency builds for each compiler. Both compilers use the same runtime and standard-library artifacts, isolating the effect of the project code generation changes.

C line counts exclude `#line` directives. Total C includes the new support fragments; the main-file reduction alone must not be interpreted as total code elimination. Object instruction sizes are the sum of project `__text` sections compiled with identical flags:

```text
-c -O2 -fwrapv -DACTON_THREADS -Wno-incompatible-pointer-types
```

Object totals include test modules and unchanged project C extensions, and exclude runtime and dependency instructions, debug data, and unwind information. Measurements used the installed Zig C compiler on macOS ARM64, with four parallel object compilation jobs. Debug executable sizes are reported separately.

### Acton YANG

Source revision: `e0506110e1f89ef8e612531f37d06ad54af0501b`; 35 Acton files and 33,272 lines. The modified project passed `make clean` followed by `make`.

| Measure | Before | After | Reduction |
| --- | ---: | ---: | ---: |
| Closure classes | 660 | 599 | 9.2% |
| Main C lines | 97,682 | 57,501 | 41.1% |
| Total C lines including support | 97,682 | 85,579 | 12.4% |
| Total C bytes including support | 7,877,640 | 7,425,050 | 5.7% |
| Header lines | 29,719 | 26,472 | 10.9% |
| Optimized project instruction bytes | 1,638,696 | 1,430,212 | 12.7% |
| Debug executable bytes | 28,310,008 | 27,494,072 | 2.9% |

The `ayangc` module illustrates the readability gain: its main C fell from 6,031 to 760 lines, while its total C fell to 3,095 lines and optimized instructions from 73,000 to 36,752 bytes. Benefits vary by module: `schema` total C was nearly unchanged, at 30,316 versus 30,311 lines.

Layout and field constants added approximately 26 KB to object constant sections. Single clean-build observations were 31.17 seconds before and 28.92 seconds after, with approximately 1.23 GB maximum resident memory in both cases. These timings used existing caches and do not establish a reliable speedup.

These YANG figures are from its standalone build. Compiling it later as a StratoWeave dependency selected a different code-generation scope and produced different closure and line counts; those outputs were matched between variants for the StratoWeave comparison.

### StratoWeave

Source revision: `d1504bd387cd106aa9146c1d496183ee5577b0de`; 93 Acton files and 62,533 lines. Both variants built with `--dep yang=../acton-yang`, pointing to the corresponding YANG build. Other pinned dependencies were copied into isolated locations to prevent reuse of outputs generated by the other compiler. Application and dependency Acton source fingerprints matched the originals.

| Measure | Before | After | Reduction |
| --- | ---: | ---: | ---: |
| Closure classes | 11,390 | 9,189 | 19.3% |
| Main C lines | 714,888 | 122,372 | 82.9% |
| Total C lines including support | 714,888 | 478,551 | 33.1% |
| Total C bytes including support | 60,561,523 | 48,034,743 | 20.7% |
| Header lines | 320,873 | 254,300 | 20.7% |
| Optimized project instruction bytes | 8,369,456 | 5,032,164 | 39.9% |
| Debug `ncurl` executable bytes | 77,265,960 | 65,497,528 | 15.2% |
| Debug `yp_mockserver` executable bytes | 60,627,360 | 54,690,016 | 9.8% |

The modified build has 356,179 support-file lines, already included in the total above. It emits 252 shared adapter helpers. The family counts show that the result is not confined to one kind of closure:

| Family | Before | After |
| --- | ---: | ---: |
| Cont | 7,067 | 5,193 |
| Proc | 2,957 | 2,785 |
| Action | 887 | 874 |
| Mut | 263 | 252 |
| Pure | 216 | 85 |

Instruction code falls by 3,337,292 bytes, while object constant sections grow by 373,062 bytes, principally for closure layouts and field arrays. In `ttt`, main C falls from 56,696 to 11,246 lines, total C to 34,619 lines, and optimized instructions from 634,208 to 343,080 bytes. The `subscriptions` module has unchanged instruction size, illustrating that improvements depend on the generated closure patterns.

Repeated builds removed project outputs but kept previously warmed dependency and native caches. Before and after observations were 31.23 and 24.34 seconds, with maximum resident memory of 2,212,839,424 and 1,733,836,800 bytes. These are provisional single-run reductions of 22.1% in time and 21.6% in reported maximum resident memory. Repeated module C, header, and support outputs matched the measured and tested outputs byte for byte.

Three compatibility adjustments were necessary for pinned native dependencies under the installed toolchain. They were applied identically in temporary copies for both variants: replace SSH's removed `LazyPath.getPath` uses with the same header paths; remove four unused zlib config-header values rejected by Zig; and compile libssh with `-Dsha1=acton_libssh_sha1` to avoid a bsdnt symbol collision. No application or dependency Acton sources were changed. Original project source and build files remain unchanged.

### Small server example

The server comparison used `--dbg-no-lines` and matched native `-O2` settings. Closure classes fell from 17 to 14. Generated C fell from 1,087 lines to 178 main-file lines plus 588 support lines, or 766 total, a reduction of 29.5%. Optimized project instructions fell from 11,516 to 6,696 bytes, a reduction of 41.9%. Final shared adapters saved a further 288 instruction bytes relative to the preceding implementation, with descriptor storage unchanged at 664 bytes.

## Integration with the await fix and CPS simplification, 20 October 2026

The branch is now rebased onto main at `7153db45`, which includes the
`await-action-proc-param` fix. It also includes the separate
`cps-continuation-simplification` change. The compiler and runtime source changes
merged without conflicts; conflicting generated snapshots were regenerated from
the combined compiler.

The await fix distinguishes calls through a proc parameter that consume an
action's result from statement calls that discard it. Result-using calls use
`__call__` and await the reply; statement calls use `__exec__` and retain the
`$SKIPRESc` marker. The closure adapter sharing preserves this distinction.
The [CPS simplification](acton-dev-guide/src/compiler/passes/cps.md) bypasses
immediate forwarding jumps while retaining continuations passed to suspending
calls. Its commit remains separate for review.

Ignored-result continuations now have a `None` parameter, so more of them can
use `$ConstCont`. Its runtime call method previously invoked the next
continuation directly, which let long chains consume the C stack. It now
returns an `R_CONT` record and leaves the invocation to the worker trampoline.
No closure layout or serialization format changes are needed for this repair.

The serialization regression checks that one ConstCont invocation returns one
step without calling the next continuation. An isolated probe returned false
with the old runtime and true after the fix. Another case snapshots and resumes
a ConstCont introduced after an ignored-result call, checking its captured
return value. The CPS regression additionally unwinds 100,000 await-separated
recursive calls. The expanded serialization regression passes in release mode.
The final `make` build passed, followed by 662 library tests, all 74 incremental
tests, and 176 native compiler, core-language, regression and sieve tests.
The native run includes the action/proc tests, serialization checks and the
100,000-call CPS regression in both debug and release modes.

The larger-project tables above predate this integration. As a smaller smoke
comparison, the exact saved sieve sources from 19 October produce identical
prime lists for arguments 100 and 4000. At 4000, the combined compiler generates
17 closure classes for each version, versus 26 and 25 in that saved baseline.
Cumulative allocation is approximately 1.568 GB and 62.2 MB, versus 1.827 GB and
70.8 MB. Main also changed between these runs; these observations do not isolate
all effects of the integration, and no new timing claim is made.

## Review focus and remaining opportunities

The implementation is ready for colleague review. The most consequential points to examine are conservative class equivalence and `$ConstCont` recognition; exact-width scalar persistence and early registration for cycles; shared field offsets and scalar IDs; adapter calling conventions, casts, and Action behavior; and support-file freshness and ownership during incremental builds. The new regression files should be reviewed together with the compiler, runtime, documentation, and golden-output changes.

Current tests and measurements establish correct behavior for the exercised cases and substantial code-size reductions on these projects. Cross-platform ABI validation and focused serialization timing would provide additional evidence before drawing broader portability or runtime-performance conclusions. Old-build snapshot compatibility would require separate design work.

A small follow-up on 19 October 2026 removes redundant scalar reboxing inside identity lambdas. When an expression already produces a box of exactly the required scalar type, CodeGen reuses that box. For example:

```c
// Before
return toB_int(((B_int)x)->val);
// After
return x;
```

The compiler regression checks that identity bodies lose their reboxing while computed and converted results retain the required boxing. Runtime coverage invokes restored identity closures for all eleven unboxable scalar types and checks that they return the original box; computed and int-to-float lambdas are also checked. The measurements above predate this follow-up.

This is separate from the redundant reboxing removed from adapters. A computed scalar still commonly travels through evaluation, boxing, a generic continuation, and unboxing; eliminating that boundary would require specialization or a broader interface change.

Bool, Show, Serialize, general deriving, more aggressive capture renaming, and sharing across modules remain future work. None is required to review the present changes.

## Implementation and measurement locations

The principal implementation files are `compiler/lib/src/Acton/LambdaLifter.hs`, `compiler/lib/src/Acton/CodeGen.hs`, `compiler/lib/src/Acton/Compile.hs`, `compiler/acton/Main.hs`, and `base/builtin/serialize.{h,c}`. Regression coverage is in `test/regression_auto/closure_serialization.{act,ext.c}`, the compiler and incremental test suites, and the pass goldens. The developer guide's lambda lift and codegen pages describe the new behavior.

Detailed measurements, source fingerprints, build and test logs, generated outputs, and measurement scripts remain locally under `/private/tmp/acton-closure-yang` and `/private/tmp/acton-closure-stratoweave`; the small-example results are in `/private/tmp/acton-closure-adapters/metrics.json`. These are temporary local artifacts. The tables and comparison conditions above preserve the essential results independently of those directories.
