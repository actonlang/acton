# Async actor tests

Source:
```python
import logging
import testing

actor MathTester():
    def add(a, b):
        return a + b

actor AsyncTester(report_result, log_handler):
    log = logging.Logger(log_handler)
    def test():
        log.info("AsyncTester.test()", None)
        report_result(True, None)

def _test_asyncact1(report_result: action(?bool, ?Exception) -> None, log_handler: logging.Handler) -> None:
    """A test using actors and asynchronous control flow"""
    # We make use of an actor as the central point for running our test logic.
    # This _test_asyncact function is just a wrapper picked up by the acton
    # test framework runner
    s = AsyncTester(report_result, log_handler)
    s.test()
```

Run:
```sh
acton test
```

Output:
```sh
Building project in /home/user/foo
  Compiling example.act for release
   Finished compilation in   0.028 s
  Final compilation step
   Finished final compilation step in   0.516 s

Tests - module example:
  asyncact1:             OK: 1171 runs in 56.181ms

All 1 tests passed (0.695s)

```

If a particular module is written to be called asynchronously, you will need to use asynchronous tests to test it.

The test discovery finds asynchronous actor tests based on the name starting with `_test_` and has a function signature of `proc(action(?bool, ?Exception) -> None, logging.Handler) -> None`.
