# Performance testing

It is also possible to run tests in a *performance mode*, which uses the same basic test definitions (so you can run your tests both as logic test and for performance purposes) but alters the way in which the tests are run. In performance mode, only a single test will be run at a time unlike the normal mode in which many tests are typically run concurrently.

To get good numbers in performance mode, it's good if test functions run for at least a couple of milliseconds. With very short tests, very small differences lead to very large percentage differences.

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
acton test perf
```

Output:
```sh
Building project in /home/user/foo
  Compiling example.act for release
   Finished compilation in   0.016 s
  Final compilation step
   Finished final compilation step in   0.451 s

Tests - module example:
  simple:                OK:  3.21ms            Avg:  4.20ms             5.11ms             106 runs in 1005.261ms

All 1 tests passed (1.571s)

```
(note that the output is rather wide, scroll horizontally to see the full output)
