# Performance comparisons

Record a baseline with `acton test perf --record`. This writes measurements to
`perf_data` in the project directory. Later performance runs compare with that
file without changing it:

```sh
acton test perf --record
# Make a change, then measure it against the baseline.
acton test perf
```

Each measured test shows wall time, CPU measurements, allocation volume and
process peak RSS. If the baseline contains a successful, compatible measurement
of that test, the delta column shows each row's mean percentage change. The wall
median comparison appears below the table. Process peak RSS is shown without a
delta because it includes calibration and warmup, which can differ between runs.
Positive values mean a higher measured value; negative values mean
less. A change from zero to a nonzero value has no defined percentage and is
shown as `from 0`.

## Matching conditions

Every comparison requires the same actual machine identity. Matching CPU models,
architecture or OS version does not establish that two results came from the same
machine. When identity is unavailable or different, the report shows the new
measurement and explains why no delta is available. This applies to all metrics,
including wall time, CPU time, allocation, hardware counts and the wall median.

The test name, build settings, enabled capability tags, runtime worker count,
scale, use of `t.loop()`, measurement version and GC policy must also match.
Source code can change: that is what the comparison is intended to measure.
The `--time` budget and number of measured runs can differ without invalidating
an otherwise compatible comparison.
Hardware counts additionally require the same backend and accounting scope;
user-only counts cannot be compared with user-plus-kernel counts.

Performance mode uses optimized builds by default. Keep explicit build options
consistent between recording and comparison. Old recordings
without machine identity or the required measurement metadata need to be recorded
again before any deltas are shown.

Same-machine comparisons can still be affected by other processes, thermal state
and scheduling. Mandatory warmup does not remove those sources of variation.

## Reusing scale

For a test that uses `t.loop()`, a compatible baseline supplies the scale for
the new run. The iterator yields that scale during warmup and measurement instead of
calibrating a different workload. For example, after recording a list workload at
scale 8, a slower version still processes the scale-8 input. It may complete fewer
measured invocations or overrun the sampling target. It does not silently reduce
the input to fit the target.

Use `--scale N` to override the baseline with an exact positive integer. This
skips calibration while preserving warmup and the independent `--time` budget.
The same scale can still be compared with a compatible baseline; a different
scale prevents a delta against the old scale. Use `--scale N --record` to record
the chosen scale, or remove the recording to calibrate a fresh baseline.
Dividing time by scale does not make different input sizes comparable, because
workload costs can be nonlinear.

## Reading uncertainty

When both runs have at least two samples and include standard deviations, the
report adds an approximate 95% Welch confidence interval for the difference in
mean time per loop body, or per invocation for tests without a loop, with units
shown beside each bound. Positive bounds indicate an increase; negative bounds
indicate a decrease. Each table row's mean percentage
change is colored only when its own interval excludes zero. The same check adds
⚡ after the delta for an improvement or 💩 for a regression, including when
color is disabled.
IPC comparisons stay neutral: higher IPC alone does not establish an improvement.
Compatible recordings without a standard deviation show a percentage with neutral
coloring.
The median's color shows direction without an uncertainty estimate.

The interval allows different sample counts and variances. Each sample is one
complete invocation's average, even if that invocation ran many loop bodies.
It assumes independent samples and cannot account for correlations within a
process, machine load or changes between runs. Treat it as a guide to measured
variability and repeat benchmarks before attributing small changes to code.
These samples come from one
process per benchmark; they do not estimate variation between fresh processes.

## Updating recordings

Performance tests always run afresh, even when their source code is unchanged.
Compilation still reuses unchanged build artifacts.

Use `--record` again to update the baseline. The run compares against the previous
values before replacing them. Filters such as `--module` and `--name` update only
the selected successful tests; other measurements remain in the file. Failed,
skipped and incomplete tests do not replace saved measurements. If no test
produces a successful measurement, the file is left untouched. Invalid JSON is
reported as an error so that a damaged baseline is not silently overwritten.

The baseline matches tests by their stored module and test names; renamed tests
or recordings using older module names need a new recording. Remove `perf_data`
to start a new baseline without retaining old entries. `--record` is only
supported in performance mode.

See [Performance testing](performance.md) for the meaning of each measurement.
