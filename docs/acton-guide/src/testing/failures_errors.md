# Failures vs errors

Tests can have tree different outcomes; success, failure and error.

**Success** and **failure** are the two common cases where success is when the test meets the expected assertions and a failure is when it fails to meet a test assertion like `testing.assertEqual(1, 2)`. We also distinguish a third case for test **errors** which is when a test does not run as expected, hitting an unexpected exception. This could indicate a design issue or that the test environment is not as expected.

All test assertions raise exceptions inheriting from `AssertionError` which are considered **test failures**. Any other exception will be considered a **test error**.

For example, if a test attempts to retrieve `https://dummyjson.com/products/1` and check that the returned JSON looks a certain way, it would be a test failure if the returned JSON does not match the expected value. If we try to connect with an invalid URL, like `htp://` we would get a different exception and that would be considered a **test error**. It's probably a bad idea to try to connect to something on the Internet in a test, so avoid that and other sources of non-determinism when possible.

# Unit tests

Source:
```python
import testing

def _test_failure():
    testing.assertEqual(1, 2)

def _test_flaky(t: testing.SyncT):
    i = t.random.below(3)
    if i == 0:
        return
    elif i == 1:
        testing.assertEqual(1, 2)
    else:
        raise ValueError("Random failure")

def _test_error() -> None:
    # Now we could never use a unit test to fetch things from the Internet
    # anyway, but it's just to show what the results look like
    raise ValueError()

```

Run:
```sh
acton test
```

Output:
```sh
Tests - module example:
   error:           ERR            : 2269 runs in 50.162ms @ 45233.4/s
    outcomes: 2269 err
    ValueError: 
   failure:         FAIL           : 1547 runs in 50.165ms @ 30838.2/s
    outcomes: 1547 fail
    testing.NotEqualError: Expected equal values but they are non-equal. A: 1 B: 2
    @@ -1,1 +1,1 @@
    -1
    +2

   flaky:           FLAKY ERR/FAIL : 1838 runs in 50.218ms @ 36600.4/s
    outcomes: 604 ok, 616 fail, 618 err
    testing.NotEqualError: Expected equal values but they are non-equal. A: 1 B: 2
    @@ -1,1 +1,1 @@
    -1
    +2

    seed: 5423337750926700240 (failed the same way in 3 of 3 replays)
    rerun: acton test --module example --name flaky --seed 5423337750926700240

1 error and 2 failure out of 3 tests ( 0.331 s)
```

Unit tests are a good starting point for testing small units of your program. Pure functions are deterministic and are thus preferable for tests over non-deterministic tests using actors. You are limited in what you can do though, since all called functions must be pure.
