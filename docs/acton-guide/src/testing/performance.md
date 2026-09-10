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
- **CPU user, CPU system:** CPU time spent in user code and the kernel across
  all threads in the test process. Unlike wall time, this excludes time waiting
  to be scheduled and can exceed wall time when threads work in parallel.
- **Instructions, cycles:** hardware counts across all threads in the test
  process. Counts use decimal prefixes: K is one thousand and M is one million.
  These include GC and runtime work during the measurement, as do CPU times.
  They exclude child processes and the forced collections between iterations.
- **IPC:** instructions divided by cycles for each iteration. The table
  summarizes these per-iteration ratios. A higher IPC does not necessarily mean
  a faster program, so its comparison has neutral coloring and no improvement
  or regression icon.
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

CPU counters bracket each iteration, including a small amount of harness and
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
are omitted rather than scaled; a row is reported only when every iteration has
a valid sample.

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

CPU durations are also in milliseconds. Instruction and cycle values are raw
counts; IPC is a unitless ratio. For example, `avg_instructions`, `stdev_cycles`
and `median_ipc` follow the same statistics naming scheme. `counter_info` records
the backend, accounting scope, availability, CPU model, architecture and OS
version. Missing counters are absent, while a measured zero remains zero.

See [Performance comparisons](perf_record.md) to record a baseline and compare
later runs, or [Stress testing](stress.md) for concurrency-focused tests.
