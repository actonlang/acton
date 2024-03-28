# Failures vs errors

Tests can have tree different outcomes; success, failure and error.

**Success** and **failure** are the two common cases where success is when the test meets the expected assertions and a failure is when it fails to meet a test assertiong like `testing.assertEqual(1, 2)`. We also distinguish a third case for test **errors** which is when a test does not run as expected, hitting an unexpected exception. This could indicate a design issue or that the test environment is not as expected.

All test assertions raise exceptions inheriting from `AssertionError` which are considered test failures. Any other exception will be considered a test error.

For example, if a test attempts to retrieve `https://dummyjson.com/products/1` and check that the returned JSON looks a certain way, it would be a test failure if the returned JSON does not mach the expected value and it would be a test error if we don't have an Internet connection and are thus unable to retrieve the JSON. It's probably a bad idea to try to connect to something on the Internet in a test, so avoid that and other sources of non-determinism when possible.

# Unit tests

Source:
```python
import random
import testing

def _test_failure():
    testing.assertEqual(1, 2)

def _test_flaky():
    i = random.randint(0, 2)
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
Building project in /home/user/foo
  Compiling example.act for release
   Finished compilation in   0.020 s
  Final compilation step
   Finished final compilation step in   0.482 s

Tests - module example:
  error:                 ERR: 454 errors out of  454 runs in 52.733ms
    ValueError: 
  flaky:                 FLAKY FAIL: 231 failures out of  471 runs in 52.819ms
    testing.NotEqualError: Expected equal values but they are non-equal. A: 1 B: 2
  failure:               FAIL: 408 failures out of  408 runs in 52.837ms
    testing.NotEqualError: Expected equal values but they are non-equal. A: 1 B: 2

1 error and 2 failure out of 3 tests (0.691s)

```

Unit tests are a good starting point for testing small units of your program. Pure functions are deterministic and are thus preferable for tests over non-deterministic tests using actors. You are limited in what you can do though, since all called functions must be pure.
