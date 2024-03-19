# Unit tests

Source:
```python
import testing

def _test_simple():
    foo = 3+4
    testing.assertEqual(7, foo)
```

Run:
```sh
acton test
```

Output:
```sh
Building project in /home/user/foo
  Compiling example.act for release
   Finished compilation in   0.016 s
  Final compilation step
   Finished final compilation step in   0.442 s

Tests - module example:
  simple:                OK: 1565 runs in 50.079ms

All 1 tests passed (0.600s)

```

Unit tests are a good starting point for testing small units of your program. Pure functions are deterministic and are thus preferable for tests over non-deterministic tests using actors. You are limited in what you can do though, since all called functions must be pure.
