# Performance testing

Performance mode uses the same test definitions as ordinary testing, but runs one
test at a time and always takes fresh measurements. Tests run serially even when
`--jobs` allows parallel compilation.

```python
import testing

actor _test_simple(t: testing.AsyncT):
    a = 0
    for i in range(99999):
        a += i
    assert a == 4999850001
    t.success()
```

```sh
acton test perf --release
```

Use `--release` to measure optimized code. Without an explicit optimization
option, the normal Debug build default applies. For more samples, increase the
minimum duration, for example `--min-time 5000` for five seconds per test, or use
`--iter` to select a fixed number of iterations.

Sampling limits are checked between iterations. A separate watchdog allows at
least five minutes for a slow or hung performance test, so a long iteration can
finish even when the sampling target is shorter. Longer configured run limits
extend the watchdog; `--max-time 0` disables it.

Each benchmark has a table with `mean ± σ`, `min … max`, outliers and an optional
baseline delta. Time and memory values use compact units such as µs, ms, KB and
MB; memory prefixes are decimal (1 KB = 1000 bytes). Numeric and unit fields have
fixed widths, so columns stay aligned across benchmarks. The default report is
116 columns wide. Smaller terminals use compact layouts, omitting outliers,
then the range, then the standard deviation to preserve the mean and delta.
A dash means that statistic was not collected.

Each row summarizes its own per-iteration samples. Wall time combines the time
excluding GC and the GC time from the same iteration before calculating its
distribution. Memory averages and quartiles can contain fractions of a byte.

The result includes:

- **Time excl. GC:** wall time per test iteration with measured full
  garbage collection time subtracted. These include waiting within asynchronous
  tests. The row shows the mean and range; the median appears below the table.
- **σ:** sample standard deviation of the row's measurements. Requires at least
  two iterations; a constant measurement has zero deviation.
- **Wall time, GC time:** unadjusted wall time and measured full GC
  time within an iteration. The GC counter has millisecond resolution. These
  exclude the forced collections between iterations.
- **Allocated:** average bytes reported by the GC allocation counter
  during an iteration. This measures allocation volume, not peak memory usage.
- **Non-GC change:** estimated average change per iteration in resident memory outside
  the GC heap. This subtracts GC memory usage from RSS before and after the
  iteration, with collections around the test. It is noisy and can be negative.
- **Process peak RSS:** peak resident memory for the test process, below the table.
  This includes startup, the runtime, all threads, the test harness and all
  iterations. It is recorded before computing the final statistics and is
  omitted on platforms where it is unavailable.
- **Outliers:** iterations outside the range from Q1 minus 1.5 times the
  interquartile range to Q3 plus 1.5 times that range. Quartiles use linear
  interpolation between sorted samples. Outliers remain in all statistics;
  they are counted separately for each row. When most GC samples are zero,
  nonzero collections can all count as outliers.
- **Runs, total time, runs/second:** elapsed time and throughput for the whole
  test loop, including harness and collection overhead. This differs from the
  per-iteration times above.

Very short tests are sensitive to measurement overhead and scheduling noise.
Prefer workloads lasting at least a few milliseconds and repeat measurements
before drawing conclusions from small percentage changes.

`--json` includes a `performance` object for each successful test. Its
`measurements` contain the displayed quantities, iteration count, median and
quartiles for every row. For example, `stdev_wall_duration`,
`q1_mem_usage_delta` and `outlier_count_gc_duration` describe wall time spread,
the first allocation quartile and GC outliers. The time excluding GC retains
its original `outlier_count` key. Durations are in milliseconds and memory
quantities are in bytes. `baseline` contains the available reference quantities,
or `null` when no baseline matches. `mean_difference_ci95_ms` contains the
comparison interval's `lower` and `upper` bounds for time excluding GC, or `null`
when the necessary statistics are unavailable. Failed or skipped tests have
`performance: null`.

See [Performance comparisons](perf_record.md) to record a baseline and compare
later runs, or [Stress testing](stress.md) for concurrency-focused tests.
