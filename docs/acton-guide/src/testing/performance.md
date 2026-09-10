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

The result includes:

- **Min, Mean, Max:** minimum, average and maximum wall time per test iteration,
  with measured garbage collection time subtracted. These include waiting within
  asynchronous tests.
- **Allocated / run:** average bytes reported by the GC allocation counter
  during an iteration. This measures allocation volume, not peak memory usage.
- **Non-GC change / run:** estimated average change in resident memory outside
  the GC heap. This subtracts GC memory usage from RSS before and after the
  iteration, with collections around the test. It is noisy and can be negative.
- **Runs, total time, runs/second:** elapsed time and throughput for the whole
  test loop, including harness and collection overhead. This differs from the
  per-iteration times above.

Very short tests are sensitive to measurement overhead and scheduling noise.
Prefer workloads lasting at least a few milliseconds and repeat measurements
before drawing conclusions from small percentage changes.

See [Performance comparisons](perf_record.md) to record a baseline and compare
later runs, or [Stress testing](stress.md) for concurrency-focused tests.
