# Async actor tests

Source:
```python
import logging
import testing

actor MathTester():
    def add(a, b):
        return a + b

actor _AsyncTester(t: testing.AsyncT):
    log = logging.Logger(t.log_handler)
    def test():
        log.info("AsyncTester.test() doing its thing")
        t.success()
        # Provide output to .success to enable snapshot testing
        #   t.success("some_output")
        # Or if things aren't going well, use .failure or .error
        #   t.failure(ValueError("whopsy"))
        #   t.error(ValueError("whopsy"))
    after 0: test()
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

The test discovery system finds asynchronous tests by looking for *actors* that take a `testing.AsyncT` parameter.

*Snapshot testing* can be enabled by providing an output of type *str* to the `.success(output: ?str)` function. The Acton test framework will take care about recognizing the test as a snapshot test and comparing its output to the expected *snapshot value*.
