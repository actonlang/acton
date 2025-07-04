# Sync actor tests

Source:
```python
import logging
import testing

actor MathTester():
    def add(a, b):
        return a + b

# It is possible to call actors in tests too, in which case the test is an 
# "actor test" (the function gets a 'proc' effect inferred when calling actors).
def _test_syncact_simple():
    m = MathTester()
    testing.assertEqual(m.add(1, 2), 3)

# Actors named prefixed with _tests_ are also considered tests
actor _test_SyncTester():
    m = MathTester()
    testing.assertEqual(m.add(1, 2), 3)

# Use any test actor name by taking a testing.SyncT as only parameter
actor _SyncTester2(t: testing.SyncT):
    log = logging.Logger(t.log_handler)
    m = MathTester()
    log.info("Calculating numbers..")
    testing.assertEqual(m.add(1, 2), 3)

# The traditional function-based approach can also take a testing.SyncT arg
def _test_syncact(testing.SyncT):
    """A test using actors and synchronous control flow"""
    # We make use of an actor as the central point for running our test logic.
    s = _SyncTester(t)
```

Run:
```sh
acton test
```

Output:
```sh
Building project in /home/user/foo
  Compiling example.act for release
   Finished compilation in   0.027 s
  Final compilation step
   Finished final compilation step in   0.526 s

Tests - module example:
  SyncTester:            OK: 1029 runs in 50.015ms
  SyncTester2:           OK: 1103 runs in 50.002ms
  syncact:               OK: 1175 runs in 50.005ms
  syncact_simple:        OK: 1231 runs in 50.014ms

All 4 tests passed (0.655s)

```

Since the Acton RTS is multi-threaded and actors are scheduled concurrently on worker threads, using actors imply a degree of non-determinism and so unlike unit tests, which are completely deterministic, actors tests are fundamentally non-deterministic. You can still write deterministic tests as long as you pay attention to how you construct your test results.

For example, actor A might be scheduled before or after actor B so if the test relies on ordering of the output, it could fail or succeed intermittently. Interacting with the surrounding environment by reading files or communicating over the network introduces even more sources of non-determinism. Avoid it if you can. 

The test discovery system finds synchronous tests through:
- **Functions**: with names starting with `_test_` and signatures `proc() -> None` or `proc(testing.SyncT) -> None`
- **Actors**: that take a `testing.SyncT` parameter (the `_test_` prefix is optional) or actors with names starting with `_test_` and no parameters

*Golden testing* can be enabled by returning a *str*. This only works for test *functions*, not test *actors*. Use a wrapping function if you want golden testing. The Acton test framework will take care about recognizing the test as a golden test and comparing its output to the expected *golden value*.
