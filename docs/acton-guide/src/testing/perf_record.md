# Performance comparisons

Record a baseline with `acton test perf --record`. This writes measurements to
`perf_data` in the project directory. Later performance runs compare with that
file without changing it:

```sh
acton test perf --release --record
# Make a change, then measure it against the baseline.
acton test perf --release
```

Each measured test shows timing statistics, CPU measurements, allocation volume, estimated non-GC
memory change and process peak RSS. If the baseline contains a successful
measurement of that test, the delta column shows each row's mean percentage
change. The median and peak RSS comparisons appear below the table.
Positive values mean a higher measured value; negative values mean
less. A change from zero to a nonzero value has no defined percentage and is
shown as `from 0`.

For example:

```text
Tests - module sample:
Benchmark (3 runs): sample
  measurement                 mean ±            σ             min …          max          outliers             delta
  time excl. GC              126µs ±       27.9µs           107µs …        158µs            0 (0%)            +40.0%
  wall time                  126µs ±       27.9µs           107µs …        158µs            0 (0%)                 —
  GC time                   0.00ms ±       0.00ms          0.00ms …       0.00ms            0 (0%)                 —
  allocated                 30.1KB ±       64.0B           30.0KB …       30.1KB            0 (0%)             +3.8%
  non-GC change             61.4KB ±       4.10KB          57.3KB …       65.5KB            0 (0%)            -11.8%
  median: 113µs
  process peak RSS: 12.4MB
  total: 9.54ms (314.4 runs/s)
```

Old recordings still provide comparisons for the quantities they contain, as
in this example. Record again to save the additional statistics.

When both runs have at least two samples and include standard deviations, the
report adds an approximate 95% Welch confidence interval for the difference in
mean iteration time, with units shown beside each bound. Positive bounds indicate
an increase; negative bounds indicate a decrease. Each table row's mean percentage
change is colored only when its own interval excludes zero. The same check adds
⚡ after the delta for an improvement or 💩 for a regression, including when
color is disabled.
IPC comparisons stay neutral: higher IPC alone does not establish an improvement.
Older recordings without a standard deviation still show a percentage, with
neutral coloring.
The median and process peak RSS colors show direction without an uncertainty
estimate.

The interval allows different sample counts and variances. It assumes independent
iterations and cannot account for correlations within a process, machine load or
changes between runs. Treat it as a guide to measured variability and repeat
benchmarks before attributing small changes to code.

Performance tests always run afresh, even when their source code is unchanged.
Compilation still reuses unchanged build artifacts.

Use `--record` again to update the baseline. The run compares against the previous
values before replacing them. Filters such as `--module` and `--name` update only
the selected successful tests; other measurements remain in the file. Failed,
skipped and incomplete tests do not replace saved measurements. If no test
produces a successful measurement, the file is left untouched. Invalid JSON is
reported as an error so that a damaged baseline is not silently overwritten.

Use the same machine and build options for comparable measurements. In
particular, keep `--release` consistent between runs. The baseline matches tests
by their stored module and test names; renamed tests or recordings using older
module names need a new recording. Remove `perf_data` to start a new baseline
without retaining old entries. `--record` is only supported in performance mode.

CPU measurements also check the recorded CPU model, architecture and OS version.
Hardware counts additionally check the backend and accounting scope. Deltas are
omitted when the relevant metadata differs or is missing. For example, user-only
Linux counts cannot be compared against user-plus-kernel counts, while CPU-time
comparisons remain available. Older recordings still provide their original
timing and memory comparisons; record again to establish a CPU baseline.

See [Performance testing](performance.md) for the meaning of each measurement.
