# Builtin and collection benchmarks

Run these modules with a compiler that supports `t.loop()` and workload scale:

```sh
acton test list --module builtin_functions --module collection_iterators
acton test perf --module builtin_functions --module collection_iterators
```

Performance mode builds optimized code and runs one benchmark process at a time.
`t.loop()` handles calibration, warmup and measurement. Ordinary `acton test`
runs each loop body once at scale 1 and checks its result.

Scale means **repetitions of one operation**, each using the same 1,024-element
input. Empty-input cases repeat the corresponding operation on an empty input.
For example, `sum` at scale 8 calls `sum(xs)` eight times per measured body;
`filter_all` at scale 8 creates and fully consumes eight filter iterators.
The report gives time and allocation per body at that scale. Scale changes the
amount of repeated work, not the collection size. These tests measure operation
costs at a fixed size, rather than how an algorithm grows with input size.

Collection inputs and accumulators are prepared before `t.loop()`. Range inputs
are consuming iterators, so `sum_range` and `zip_mixed` create a fresh range for
each operation inside the body. Result checks run after the loop and tolerate
no bodies when setup exhausts the time budget.
Checksums and consumed counts cover all completed bodies. Collection tests check
the accumulated lengths and the final contents. Constructors include output
allocation; dict and set updates reuse a target after its first population, so
subsequent updates measure replacement or duplicate insertion without growth.
Each measured invocation starts with fresh setup.

The builtin tests cover numeric sum variants, extrema with and without defaults,
filter selectivity and mixed zip iterators. The input permutation avoids
monotonic extrema. Lazy iterators are consumed with ordinary loops so another
builtin under comparison cannot hide their cost. The `control` case traverses
the input directly to help assess measurement noise. Enumeration retains its C
implementation; its cases cover the declaration change and provide another
iterator control.

The collection tests cover list, dict and set construction, list and bytearray
slice assignment, dict and set updates, and set disjointness. The disjointness
cases cover a full scan, an immediate overlap, an empty set, and a one-element
set against 1,024 elements to exercise the smaller-set path. Bytearray slice
inputs include every byte value.

The disjoint and empty-set cases and both bytearray slice cases expose iterator
exhaustion bugs fixed in #3104. Before that fix, `StopIteration` escapes the
operation and the completion checks fail. Report a failed baseline rather than
treating the failure time as an operation latency or speedup. The header and
iterator-registration edits accompany the same builtin migration and do not add
separate collection operations.

## Comparing implementations

Record the first implementation, then run the second against the same reference:

```sh
acton test perf --module builtin_functions --module collection_iterators --record
# Build the other implementation, keeping these benchmark sources and perf_data.
acton test perf --module builtin_functions --module collection_iterators --json > comparison.json
```

A compatible baseline supplies its chosen scale. Keep that scale fixed between
implementations; independently calibrated scales measure different workloads.
Use `--scale N` to choose an exact scale and `--time 10s` to extend the budget:

```sh
acton test perf --module builtin_functions --name '(control|filter_all)' --scale 256 --time 10s
```

Keep the compiler, measurement code, machine, build options and worker count
the same when isolating a builtin change. Rebuild with `make` and regenerate
`test/perf/out` when switching implementations. Performance mode bypasses saved
test results, but that does not replace rebuilding generated code. On macOS,
both installations need the elapsed GC clock fix from #3107.

Use new recordings for these loop tests; the earlier fixed-repetition benchmarks
have different names, inputs and measurement boundaries. Repeat comparisons in
alternating process order, retain JSON reports and inspect controls, CPU work
and allocation alongside wall time. Confidence markers describe variation within
one process, and do not capture variation between processes. Peak RSS includes
setup, calibration and warmup and is not an allocation or leak verdict.

See the [performance guide](../../docs/acton-guide/src/testing/performance.md)
and [baseline workflow](../../docs/acton-guide/src/testing/perf_record.md) for
measurement definitions and compatibility rules.
