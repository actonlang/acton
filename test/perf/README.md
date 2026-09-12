# Builtin and collection benchmarks

Run these modules with a compiler that supports `t.scale()` and `t.loop()`:

```sh
acton test list --module builtin_functions --module collection_iterators
acton test perf --module builtin_functions --module collection_iterators
```

Scale means **input element count**. Each loop body performs one operation on
that input. For example, `sum` at scale 100000 sums a list of 100,000 elements;
`filter_all` creates and fully consumes one filter iterator over that list.
The report gives time and allocation per operation. Empty inputs stay empty at
every scale, and their controls measure one empty operation per body.

Read `t.scale()` to construct the input before `t.loop()`. Scale stays fixed
throughout an invocation. Calibration, warmup and measured invocations each
prepare their own inputs. Within an invocation, the runner repeats loop bodies
as needed for the time budget. Ordinary `acton test` runs a body once at scale 1
and checks its result.

Performance mode builds optimized code and runs one benchmark process at a time.
To use the representative input size from the earlier fixed-size benchmarks:

```sh
acton test perf --module builtin_functions --module collection_iterators --scale 1024
```

To explore the growth curve over a chosen range:

```sh
acton test scale --module builtin_functions --name filter_all --start-scale 1 --end-scale 100000
```

Empty-input controls and operations that can return immediately need not get
slower with scale. They are useful controls, but do not exercise a growing scan.
Use `acton test perf --scale 1` for fixed empty-operation measurements.

Collection inputs, mutable targets and accumulators are prepared before
`t.loop()`. Range inputs are consuming iterators, so `sum_range` and `zip_mixed`
create a fresh range inside each body. Result checks run after the loop and
tolerate no bodies when setup exhausts the time budget. Checksums and consumed
counts cover all completed bodies; collection tests check accumulated lengths
and final contents.

Constructors include output allocation. Dict updates start with all keys present
and measure value replacement. Set updates start with all elements present and
measure duplicate insertion. List and bytearray slice assignments start with a
target of the matching size. These cases measure the same kind of operation
from their first body, including when a scaling study measures just one body.

The builtin tests cover numeric sum variants, extrema with and without defaults,
filter selectivity and mixed zip iterators. The shared integer-list inputs contain
every integer from 0 to scale minus one, with odd values first, then positive
even values, and zero last. This permutation works at arbitrary positive scales
and keeps the minimum away from the first element when the input has more than
one element. Lazy iterators are consumed with ordinary loops so another builtin
under comparison cannot hide their cost. The `control` case traverses the input directly to help
assess measurement noise. Enumeration retains its C implementation and provides
another iterator control.

The collection tests cover list, dict and set construction, slice assignment,
dict and set updates, and set disjointness. Disjointness cases cover a full scan
of two size-N sets, immediate overlap, a size-N set against an empty set, and a
one-element set against N elements. The last case exercises the smaller-set path
when N is greater than one. Bytearray slice inputs cycle through the byte values
0 to 255, covering every byte value once scale reaches 256.

The disjoint and empty-set cases and both bytearray slice cases expose iterator
exhaustion bugs fixed in #3104. Before that fix, `StopIteration` escapes the
operation and the completion checks fail. Report a failed baseline rather than
treating the failure time as an operation latency or speedup.

## Comparing implementations

Record the first implementation, then run the second against the same reference:

```sh
acton test perf --module builtin_functions --module collection_iterators --scale 1024 --record
# Build the other implementation, keeping these benchmark sources and perf_data.
acton test perf --module builtin_functions --module collection_iterators --json > comparison.json
```

A compatible baseline supplies its chosen scale. Keep that scale fixed between
implementations; independently calibrated scales measure different input sizes.
Use `--scale N` to choose an exact input size and `--time 10s` to extend the budget:

```sh
acton test perf --module builtin_functions --name '(control|filter_all)' --scale 100000 --time 10s
```

Keep the compiler, measurement code, machine, build options and worker count
the same when isolating a builtin change. Rebuild with `make` and regenerate
`test/perf/out` when switching implementations. Performance mode bypasses saved
test results, but that does not replace rebuilding generated code. On macOS,
both installations need the elapsed GC clock fix from #3107.

Create new recordings after this migration. Scale previously meant repetitions
of an operation on a fixed 1,024-element input; it now means input element count,
and each body performs one operation. The old measurements describe different
workloads even when their numeric scales match. The permutation and initial
mutable-target states have also changed.

Repeat comparisons in alternating process order, retain JSON reports and inspect
controls, CPU work and allocation alongside wall time. Confidence markers describe
variation within one process, and do not capture variation between processes.
Peak RSS includes setup, calibration and warmup and is not an allocation or leak
verdict.

See the [performance guide](../../docs/acton-guide/src/testing/performance.md)
and [baseline workflow](../../docs/acton-guide/src/testing/perf_record.md) for
measurement definitions and compatibility rules.
