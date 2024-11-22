# Env tests

When you need to test functionality that accesses the environment, you need an env test. Do beware of errors related to test setup though, since you now depend on the external environment. TCP ports that you try to listen to might be already taken. Files that you assume exist might not be there.

Source:
```python
import logging
import testing

actor EnvTester(report_result, env, log_handler):
    log = logging.Logger(log_handler)
    def test():
        log.info("EnvTester.test()", None)
        report_result(True, None)

def _test_envtest1(report_result: action(?bool, ?Exception) -> None, env: Env, log_handler: logging.Handler) -> None:
    """A test interacting with the environment"""
    # We make use of an actor as the central point for running our test logic.
    # This _test_envtest1 function is just a wrapper picked up by the acton
    # test framework runner
    s = EnvTester(report_result, env, log_handler)
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
   Finished compilation in   0.023 s
  Final compilation step
   Finished final compilation step in   0.484 s

Tests - module example:
  envtest1:              OK: 1213 runs in 50.135ms

All 1 tests passed (0.689s)

```

The test discovery finds env actor tests based on the name starting with `_test_` and has a function signature of `proc(action(?bool, ?Exception) -> None, Env, logging.Handler) -> None`.
