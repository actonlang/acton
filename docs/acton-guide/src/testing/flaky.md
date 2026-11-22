# Flaky tests

Flaky tests are those that have different outcomes during different runs, i.e. they are not deterministic. To combat these, `acton test` will per default attempt to run tests multiple times to ensure that the result is the same. It runs as many test iterations as possible for at least 50ms. If a test is flaky, this will be displayed in the test output.

Source:
```python
import testing

def _test_flaky(t: testing.SyncT):
    i = t.random.below(3)
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
Tests - module example:
   flaky:           FLAKY ERR/FAIL : 1838 runs in 50.218ms @ 36600.4/s
    outcomes: 604 ok, 616 fail, 618 err
    testing.NotEqualError: Expected equal values but they are non-equal. A: 1 B: 2
    @@ -1,1 +1,1 @@
    -1
    +2

    seed: 5423337750926700240 (failed the same way in 3 of 3 replays)
    rerun: acton test --module example --name flaky --seed 5423337750926700240

1 out of 1 tests failed ( 0.331 s)
```

The test draws its random numbers from `t.random`, so `acton test` shows the seed of the first failed run and a command that runs the test with that seed again. See [Random numbers in tests](random.md).
