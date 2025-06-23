# Performance comparisons

When running in `performance mode` you can record a snapshot of performance using `acton test perf --record`. A `perf_data` file is written to disk with the stored performance data. Subsequent test runs will read this file and show a comparison. The difference is displayed as a percentage increase or decrease in time.

Source:
```python
import testing

def _test_simple():
    a = 0
    for i in range(99999):
        a += i
```

Run:
```sh
acton test perf --record
acton test perf
```

Output:
```sh
Building project in /home/user/foo
  Compiling example.act for release
   Finished compilation in   0.017 s
  Final compilation step
   Finished final compilation step in   0.452 s

Tests - module example:
  simple:                OK:  3.25ms            Avg:  4.16ms             7.38ms             122 runs in 1006.002ms

All 1 tests passed (1.565s)

Building project in /home/user/foo
  Compiling example.act for release
   Already up to date, in    0.000 s
  Final compilation step
   Finished final compilation step in   0.116 s

Tests - module example:
  simple:                OK:  3.23ms -0.50%     Avg:  4.17ms +0.19%      6.35ms -13.91%     119 runs in 1001.375ms

All 1 tests passed (1.215s)

```
(note that the output is rather wide, scroll horizontally to see the full output)
