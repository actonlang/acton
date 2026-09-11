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
- **Process peak RSS:** peak resident memory for the test process, below the table.
  This includes startup, the runtime, all threads, the test harness, calibration,
  warmup and measurement. It is recorded before computing the final statistics
  and is omitted on platforms where it is unavailable. RSS minus GC heap size
  is not used as a performance or leak verdict. No baseline delta is shown,
  because calibration and warmup can differ between runs.
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
