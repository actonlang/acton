# Flaky tests

Flaky tests are those that have different outcomes during different runs, i.e. they are not deterministic. To combat these, `acton test` will per default attempt to run tests multiple times to ensure that the result is the same. It runs as many test iterations as possible for at least 50ms. If a test is flaky, this will be displayed in the test output.

Source:
```python
import random
import testing

def _test_flaky():
    i = random.randint(0, 2)
    if i == 0:
        return
    elif i == 1:
        testing.assertEqual(1, 2)
    else:
        raise ValueError("Random failure")
```

Run:
```sh
acton test
```

Output:
```sh
Building project in /home/user/foo
  Compiling example.act for release
   Finished compilation in   0.017 s
  Final compilation step
   Finished final compilation step in   0.453 s

Tests - module example:
  flaky:                 FLAKY FAIL: 565 failures out of 1140 runs in 50.043ms
    testing.NotEqualError: Expected equal values but they are non-equal. A: 1 B: 2

1 out of 1 tests failed (0.625s)

```
