# Sync actor tests

Source:
```python
import logging
import testing

actor MathTester():
    def add(a, b):
        return a + b

actor SyncTester(t):
    log = logging.Logger(t.log_handler)
    def test():
        m = MathTester()
        log.info("Calculating numbers..")
        testing.assertEqual(m.add(1, 2), 3)
    after 0: test()

def _test_syncact(t: testing.SyncT) -> None:
    """A test using actors and synchronous control flow"""
    # We make use of an actor as the central point for running our test logic.
    # This _test_syncact function is just a wrapper picked up by the acton
    # test framework runner
    s = SyncTester(t)

def _test_syncact_simple() -> None:
    # The simplest sync test style, without the SyncT argument
    m = MathTester()
    testing.assertEqual(m.add(1, 2), 3)
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
  syncact:               OK: 1175 runs in 50.005ms
  syncact_simple:        OK: 1231 runs in 50.014ms

All 2 tests passed (0.655s)

```

Since the Acton RTS is multi-threaded and actors are scheduled concurrently on worker threads, using actors imply a degree of non-determinism and so unlike unit tests, which are completely deterministic, actors tests are fundamentally non-deterministic. You can still write deterministic tests as long as you pay attention to how you construct your test results.

For example, actor A might be scheduled before or after actor B so if the test relies on ordering of the output, it could fail or succeed intermittently. Interacting with the surrounding environment by reading files or communicating over the network introduces even more sources of non-determinism. Avoid it if you can. 

The test discovery finds synchronous actor tests based on the name starting with `_test_` and has a function signature of `proc() -> None` or `proc(testing.SyncT) -> None`.

*Golden testing* can be enabled by returning a *str*. The Acton test framework will take care about recognizing the test as a golden test and comparing its output to the expected *golden value*.
