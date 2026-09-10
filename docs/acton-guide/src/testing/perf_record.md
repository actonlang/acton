# Performance comparisons

Record a baseline with `acton test perf --record`. This writes measurements to
`perf_data` in the project directory. Later performance runs compare with that
file without changing it:

```sh
acton test perf --release --record
# Make a change, then measure it against the baseline.
acton test perf --release
```

Each measured test shows its minimum, mean and maximum iteration time, average
allocated bytes, and estimated non-GC memory change. If the baseline contains a
successful measurement of that test, each available metric also shows its
percentage change. Positive values mean more time or memory; negative values mean
less. A change from zero to a nonzero value has no defined percentage and is
shown as `from 0; % n/a`.

For example:

```text
Tests - module sample:
   sample:          OK             :    3 runs in 9.542ms @  314.4/s
      Min:                 0.107 ms (+94.55%)
      Mean:                0.126 ms (+40.00%)
      Max:                 0.158 ms (+6.76%)
      Allocated / run:     30101 B (+3.83%)
      Non-GC change / run: 61440 B (-11.76%)
```

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

See [Performance testing](performance.md) for the meaning of each measurement.
