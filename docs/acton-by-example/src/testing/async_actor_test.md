# Async actor tests

Source:
```python
import logging
import testing

actor MathTester():
    def add(a, b):
        return a + b

actor AsyncTester(t):
    log = logging.Logger(t.log_handler)
    def test():
        log.info("AsyncTester.test() doing its thing")
        t.success()
        # Provide output to .success to enable golden testing
        #   t.success("some_output")
        # Or if things aren't going well, use .failure or .error
        #   t.failure(ValueError("whopsy"))
        #   t.error(ValueError("whopsy"))
    after 0: test()

def _test_asyncact1(t: testing.AsyncT):
    """A test using actors and asynchronous control flow"""
    # We make use of an actor as the central point for running our test logic.
    # This _test_asyncact function is just a wrapper picked up by the acton
    # test framework runner
    s = AsyncTester(t)
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

The test discovery finds asynchronous actor tests based on the name starting with `_test_` and has a function signature of `proc(testing.AsyncT) -> None`.

*Golden testing* can be enabled by providing an output of type *str* to the `.success(output: ?str)` function. The Acton test framework will take care about recognizing the test as a golden test and comparing its output to the expected *golden value*.
