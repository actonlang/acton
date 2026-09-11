# Performance testing

Performance mode uses the same test definitions as ordinary testing, runs one
benchmark process at a time, and always takes fresh measurements. It builds
optimized code by default. Tests run serially even when `--jobs` allows parallel
compilation; each test retains the application's runtime worker threads, so actor
work can still run concurrently.

```python
import testing

def _test_sort(t: testing.SyncT):
    source = list(range(1000, 0, -1))
    result: ?list[int] = None
    for scale in t.loop():
        for i in range(scale):
            result = sorted(source)
    if result is not None:
        assert result == list(range(1, 1001))
```

```sh
acton test perf
```

The runner has a total budget of five seconds per benchmark, including
calibration, warmup, setup and teardown. Use `--time` to change it:

```sh
acton test perf --time 10s
```

Durations accept units such as `5s` and `250ms`. The budget is a target: the runner
finishes a whole invocation, so slow work can overrun it. A separate watchdog
allows at least five minutes for a slow test; longer budgets extend it.
Compilation is outside the benchmark budget.

## Studying growth with input size

Use `acton test scale` to explore how performance changes with input size:

```sh
acton test scale
```

No flags are required. The runner increases the workload and collects repeated
measurements until the observed growth settles over a broad range, or a resource
limit stops the study. There is no fixed number of points or target duration.
To select a starting scale or change the resource ceilings:

```sh
acton test scale --name my_test \
    --start-scale 1000 --max-memory 50% --max-time 2h
```

The study starts at scale 1 unless `--start-scale` is given. It normally doubles
the scale between points, taking smaller steps when previous measurements predict
that the next workload will approach the memory ceiling. Short and noisy points
remain in the results. The default ceiling is 50% of physical memory, reduced by
visible container limits and available memory headroom. `--max-memory` also
accepts byte quantities such as `512MiB` or `4GiB`. The runner prints the actual
ceiling before starting.

`--max-time` caps the entire study across all selected tests, including process
startup, warmup, setup and teardown. It defaults to one hour; durations support
`ms`, `s`, `m` and `h`. Compilation is outside this budget. The parent stops a
running sample when the deadline expires. This is a safety ceiling, not the usual
completion condition. `--time` and `--scale` belong to `acton test perf`; use
`--max-time` and `--start-scale` with `acton test scale`.

Scale must represent the workload dimension you want to investigate. For example,
sorting `scale` elements tests growth with input size. Sorting the same small
list `scale` times measures repetition. If scale represents the side length of
a matrix, quadratic growth in its element count is expected. The runner can
require `t.loop()`, but cannot verify what the test does with the yielded value.

Each point starts with three samples. If mean wall time is at least 0.1ms and
the estimated relative standard error exceeds 5%, the runner takes two more
samples, then another two if needed, for at most seven. Relative standard error
is the sample standard deviation divided by the square root of the sample count
and by the mean. It guides sampling; it is not a confidence guarantee. A point
below the timing floor advances to a larger scale instead of collecting more
samples at the same size.

Every sample uses a fresh process that runs one complete warmup invocation
followed by one complete measured invocation. In both invocations, `t.loop()`
yields the requested scale exactly once. The measured invocation has fresh setup
and teardown, while retaining the process's warmed runtime and GC state. Three
samples therefore cost six complete invocations. No extra loop repetitions fill
a time budget. Natural GC remains enabled. Peak RSS includes startup, setup,
warmup and teardown; it is process capacity, not a count of live application
data. Allocation volume is recorded separately.

The table and plots show mean wall time, sample ranges, time per unit of scale,
peak RSS and observed growth between successive sizes. An exponent near 1 means
time grew roughly linearly over those sizes; near 2 means roughly quadratically.
A growth estimate needs at least three samples at both sizes, mean times of at
least 0.1ms and relative standard errors no greater than 5%.

The runner reports stable growth after covering at least 1000 times the first
reliable scale and finding five consecutive reliable growth estimates whose
total spread is at most 0.25. Each step in that window must increase scale by at
least 1.5 times. A bend or unreliable point breaks the window. The first reliable
scale is rechecked every four sizes and before declaring stability. Each check
uses three to seven samples under the same precision rule. It must remain
reliable and its mean must stay within 20% of the original mean. Reference checks
are separate observations, not additional curve samples. The study ends as
inconclusive after three consecutive failed reference checks, or 20 consecutive
sizes that remain too short or too variable.

These are practical stopping heuristics. Stable growth describes the measured
range; it does not prove an algorithm's asymptotic complexity or rule out a bend
at a larger size. The result includes the covered scales, point and sample counts,
and the recent scale range that supports its growth summary. A resource limit
leaves a partial curve with the stopping reason.

Each benchmark ends with terminal charts for time, time divided by scale and
process peak memory. Both axes are logarithmic. Points show means, bars show
sample ranges, and hollow points mark incomplete sampling at a size. Flat
time/scale suggests roughly linear time over the measured range. The wall-time
chart includes a dashed guide for time proportional to scale, anchored at the
largest completed size. It is an illustration, not a fitted model or a complexity
claim. Each chart highlights its latest point and shows its latest mean.
Cyan, violet and teal distinguish time, time/scale and memory; `--color never`
and `NO_COLOR` select monochrome output. The summary counts curve samples
separately from reference checks and identifies partial sizes. Kitty and
Ghostty use inline graphics; other terminals, multiplexers and redirected output
use Unicode plots. Very small terminals keep the table without charts. No
external image viewer is needed, and drawing happens after measurement.

While sampling, one terminal line shows the current scale and sample number.
The target increases from three to five or seven when more measurements are
needed. Completed measurements remain in the table. Redirected output prints
one status line per size or reference check; `--no-progress` suppresses sampling
status.

Each benchmark writes its own JSON Lines journal under `out/perf_scaling/`.
Filenames include the module, test name, UTC timestamp and a short implementation
hash, for example
`perf.dct._test_dct__2026-09-11T19-05-20.643370Z__a1b2c3d4e5f6.jsonl`.
The header retains the full hash, timestamp, machine and build settings,
compiler version and available Git revision and dirty status. A common run ID
connects benchmarks selected by the same command. Renaming a file is harmless;
the journal contents identify its benchmark.

The implementation hash covers the test's typed Acton code and its tracked
implementation dependencies. Changing the sampling settings does not change
this hash. It does not cover external input files, handwritten C or the
compiler/runtime binaries. Those can affect performance even when the benchmark
hash stays the same. An unavailable hash is shown as `unknown` in the filename.

To display an earlier recording with the same terminal charts:

```sh
acton test scale --report after.jsonl
```

Use the path printed by that run. Rendering only reads the journal, so it works
outside the project without compiling or running tests. Completed samples from
interrupted studies remain visible, with unfinished sizes marked as partial.

Compare two recordings, or run the benchmark again against a recording:

```sh
acton test scale --report after.jsonl --compare before.jsonl
acton test scale --compare before.jsonl
```

Each option takes one filename. `--compare` always supplies the baseline.
The charts overlay at most two studies, with solid current curves and dashed
baseline curves. Both retain their full recorded ranges and distinguish partial
points. Comparison requires the same machine, build settings, input tags,
measurement version, loop usage, GC policy and runtime worker count. Different
implementation hashes are expected. Files without enough measurement identity
can still be displayed individually.

A live comparison selects the recorded benchmark before compilation, then
remeasures its completed sizes in ascending order using fresh processes and the
usual warmup and repeat policy. It finishes after those sizes rather than
stopping early when a growth trend appears stable. `--start-scale` restricts the
schedule to recorded sizes at or above that value. Current time and memory
ceilings still apply; an early stop retains the partial curve and reports which
baseline sizes were not reached. Every run writes a new recording and leaves
the baseline untouched.

Older journals containing multiple benchmarks require `--module` and/or `--name`
to select one for a live comparison. Two-file reports compare matching benchmark
identities. Old timestamp-only filenames and journal formats remain readable.

Every sample start and completed result is flushed immediately, so
interruption leaves the completed work available and identifies the unfinished
sample. `--json`
streams these same events to standard output. The final event explains whether
the study reached a time or memory limit, completed its selected tests, was
interrupted, or encountered an error. No interrupted measurement is fitted as a
completed point. The ordinary `perf_data` baseline is untouched; `--record` and
snapshot updates are not supported during a scaling study.
Each sample may emit at most 1MiB on each output stream. Exceeding this limit
stops the study with an error so diagnostic output cannot exhaust the runner's
memory during a long study. Raw diagnostics stay in the journal; terminal charts
retain only timing and peak memory samples for the current benchmark.

The memory guard monitors the benchmark process during execution and leaves a
reserve for other work. Linux observes RSS and visible cgroup memory limits;
macOS observes physical footprint and uses a conservative estimate of available
pages. This is a best-effort guard, not a memory reservation or an OS-enforced
allocation limit. A sudden allocation can exceed it before the next observation,
and macOS can stop earlier than its apparent reclaimable memory would suggest.
Memory in separate child processes is not included in the benchmark process's
ceiling, although the machine's available headroom is still checked. Unavailable
memory observations stop the study with an error. Supported platforms are Linux
and macOS.

Compare curves only on the same machine, at matching scales and with matching
build, input and runtime settings. Cache effects, GC, input distribution and
machine load all affect observed growth; the raw results retain timing and CPU
counters for investigating these effects.

## Choosing the measured work

Use `for scale in t.loop():` on a `testing.SyncT`, `testing.AsyncT` or
`testing.EnvT` context. The runner measures each loop body and excludes code before
and after the loop from its timing and counter totals. In the example, building
the input and checking the final result are excluded; sorting and allocating each
sorted copy are measured. Ordinary testing runs the body once with scale 1.
Tests without `t.loop()`, including tests without a context, measure their whole
invocation.

Scale is a positive integer whose meaning the author defines. The example sorts
the same input `scale` times per body. It could instead control input size or the
number of concurrent actors. Document that meaning. The runner tries increasing
scales 1, 2, 4 and so on to find enough work for useful measurement. At the end of
calibration, it chooses whichever of the last two scales ran closer to the target
duration, then freezes that scale. A compatible baseline supplies its recorded
scale instead.
The calibration points show observed growth; they do not establish an algorithm's
complexity. The terminal abbreviates long curves to the first and last two
points; JSON retains every point. Time divided by scale is not generally
comparable across scales.

Use `--scale` to choose an exact positive integer for tests that use `t.loop()`:

```sh
acton test perf --scale 8 --time 10s
```

An explicit scale overrides the baseline's scale and skips calibration. Warmup
still runs, and `--time` independently controls the total budget. Without
`--scale`, the runner uses a compatible baseline's scale or calibrates one.
Tests that do not call `t.loop()` reject an explicit `--scale`.

After preparation and warmup, a loop test aims for four complete measured
invocations within the remaining budget. Each starts the test from the beginning,
with fresh setup and teardown. Its loop repeats at the fixed scale while time
remains. Tests without a loop repeat whole invocations until the budget ends.
Expensive setup can leave fewer measured runs, or no measurement at all. In
performance mode the loop may be empty if setup has already consumed its budget;
teardown must handle that case. The report explains when there are no samples.
Increase `--time` when needed.

Use one `t.loop()` per invocation and allow it to finish normally in both ordinary
and performance testing. An unfinished loop, including an early `break` or
`return`, fails the test. For asynchronous work, wait for completion inside the
body before the loop advances.
Call `t.success()` after the loop and teardown have finished.

The author owns input and reset behavior. Each loop body must perform the intended
work again; repeatedly sorting an already sorted list measures a different
workload. Setup allocations can still affect later GC, even though setup itself
is excluded. Natural GC remains enabled throughout. Warmup does not guarantee
stable caches, representative GC activity or freedom from scheduling noise.

## Measurements

Each benchmark has a table with `mean ± σ`, `min … max`, outliers and an optional
baseline delta. Time and memory values use compact units such as µs, ms, KB and
MB; memory prefixes are decimal (1 KB = 1000 bytes). Numeric and unit fields have
fixed widths, so columns stay aligned across benchmarks. The default report is
116 columns wide. Smaller terminals use compact layouts, omitting outliers,
then the range, then the standard deviation to preserve the mean and delta.
A dash means that statistic was not collected.

Each complete measured invocation contributes one statistical sample. For a loop
test, that sample is the invocation's measured total divided by its completed
loop bodies. The table summarizes those run averages, at the selected scale.
For a test without a loop, each sample covers one whole invocation. Memory
averages and quartiles can contain fractions of a byte.

The result includes:

- **Wall time:** the primary measurement, including natural GC and waiting within
  the measured region.
- **σ:** sample standard deviation of the row's measurements. Requires at least
  two measured runs; a constant measurement has zero deviation.
- **GC time:** measured full GC time within the measured region. The GC counter has
  millisecond resolution. A zero can mean no collection occurred or that the
  counter was too coarse to record it. There are no forced collections between
  performance invocations.
- **CPU user, CPU system:** CPU time spent in user code and the kernel across
  all threads in the test process. Unlike wall time, this excludes time waiting
  to be scheduled and can exceed wall time when threads work in parallel.
- **Instructions, cycles:** hardware counts across all threads in the test
  process. Counts use decimal prefixes: K is one thousand and M is one million.
  These include GC and runtime work during the measurement, as do CPU times.
  They exclude child processes.
- **IPC:** total measured instructions divided by total measured cycles for each
  run. The table summarizes these ratios. A higher IPC does not necessarily mean
  a faster program, so its comparison has neutral coloring and no improvement
  or regression icon.
- **Allocated:** average bytes reported by the GC allocation counter
  in the measured region, averaged per loop body or whole invocation. This
  measures allocation volume, not peak memory usage.
- **Process peak RSS:** peak resident memory since the test executable started, below the table.
  This includes startup, the runtime, all threads, the test harness, calibration,
  warmup and measurement. It is recorded before computing the final statistics
  and is omitted on platforms where it is unavailable. RSS minus GC heap size
  is not used as a performance or leak verdict. No baseline delta is shown,
  because calibration and warmup can differ between runs.
  Linux reads the executable's `VmHWM` rather than an inherited launcher peak.
  Older Linux recordings may include the launcher's footprint; replay cannot
  correct those recorded values.
- **Outliers:** run averages outside the range from Q1 minus 1.5 times the
  interquartile range to Q3 plus 1.5 times that range. Quartiles use linear
  interpolation between sorted samples. Outliers remain in all statistics;
  they are counted separately for each row. When most GC samples are zero,
  nonzero collections can all count as outliers.
- **Scale, runs and loop iterations:** the fixed workload scale, complete measured
  invocations and total completed bodies within those invocations. Many bodies in
  one run still contribute only one statistical sample.
- **Measured and elapsed time:** accumulated wall time inside measured regions,
  and elapsed time across measured invocations including setup, teardown and
  harness work. Both exclude calibration and warmup. Whole-run elapsed time
  includes preparation.

Very short tests are sensitive to measurement overhead and scheduling noise.
Prefer workloads lasting at least a few milliseconds, using scale where useful,
and repeat measurements before drawing conclusions from small percentage changes.
Quartiles describe the run averages. They are not individual loop-body or request
latency percentiles.

CPU counters bracket each measured region, including a small amount of harness and
counter-reading work. They do not isolate instructions spent on GC from other
work. Wall time, CPU time and instruction counts provide different views of the
same run; counters help explain variation but do not remove it.

Linux uses `perf_event_open`, with thread inheritance requiring Linux 5.13 or
later and matching build headers. It first requests user and kernel events,
then tries user-only events if permissions prevent kernel counting. The report
states which scope was collected. macOS uses the kernel's process instruction
and cycle accounting through `proc_pid_rusage`, where the OS and hardware support
it. Hardware counters are optional: denied permissions, unsupported hardware,
failed reads or incomplete counter coverage omit those rows with an explanation.
CPU times remain available independently. Linux samples with multiplexed events
are omitted rather than scaled; a row is reported only when every measured run has
a valid sample.

`--json` includes a `performance` object for each successful test. Its
`measurements` contain the displayed quantities, iteration count, median and
quartiles for every row. For example, `stdev_wall_duration`,
`q1_mem_usage_delta` and `outlier_count_gc_duration` describe wall time spread,
the first allocation quartile and GC outliers. JSON also retains the earlier
diagnostic fields for time excluding GC, including its `outlier_count` key.
Durations are in milliseconds and memory quantities are in bytes. `baseline`
contains saved reference quantities when available. `mean_difference_ci95_ms`
contains the comparison interval's `lower` and `upper` bounds for wall time, or
`null` when the conditions differ or the
necessary statistics are unavailable. `comparison_unavailable_reason` explains
missing or incompatible baselines. Failed or skipped tests have `performance: null`.

CPU durations are also in milliseconds. Instruction and cycle values are counts
per loop body or whole invocation; IPC is a unitless ratio. For example,
`avg_instructions`, `stdev_cycles` and `median_ipc` follow the same statistics
naming scheme. `counter_info` records
the backend, accounting scope, availability, CPU model, architecture and OS
version. Missing counters are absent, while a measured zero remains zero.

The `perf_info` object records measurement version, machine and build identity,
scale, whether `loop` was used, worker count and `time_budget_ms`.
`measurement_ms` sums timed body wall time; `measurement_duration_ms` includes
setup and teardown across the measured invocations. Both are in milliseconds and
exclude warmup. At the top level of `measurements`, `loop_iterations` counts
completed measured bodies and `num_iterations` counts complete invocation samples.
`calibration` retains each observed `{scale, wall_ms}` point; these are sizing
observations, separate from the measured samples.

See [Performance comparisons](perf_record.md) to record a baseline and compare
later runs, or [Stress testing](stress.md) for concurrency-focused tests.
