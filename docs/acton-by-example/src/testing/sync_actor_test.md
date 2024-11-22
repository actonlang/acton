# Sync actor tests

Source:
```python
import logging
import testing

actor MathTester():
    def add(a, b):
        return a + b

actor SyncTester(log_handler):
    def test():
        m = MathTester()
        testing.assertEqual(m.add(1, 2), 3, "1 + 2 = 3")

def _test_syncact(log_handler: logging.Handler) -> None:
    """A test using actors and synchronous control flow"""
    # We make use of an actor as the central point for running our test logic.
    # This _test_syncact function is just a wrapper picked up by the acton
    # test framework runner
    s = SyncTester(log_handler)
    return s.test()
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

All 1 tests passed (0.655s)

```

Since the Acton RTS is multi-threaded and actors are scheduled concurrently on worker threads, using actors imply a degree of non-determinism and so unlike unit tests, which are completely deterministic, actors tests are fundamentally non-deterministic. You can still write deterministic tests as long as you pay attention to how you construct your test results.

For example, actor A might be scheduled before or after actor B so if the test relies on ordering of the output, it could fail or succeed intermittently. Interacting with the surrounding environment by reading files or communicating over the network introduces even more sources of non-determinism. Avoid it if you can. 

The test discovery finds synchronous actor tests based on the name starting with `_test_` and has a function signature of `mut(logging.Handler) -> None` or `pure(logging.Handler) -> None`.
