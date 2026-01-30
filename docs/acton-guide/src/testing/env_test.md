# Env tests

When you need to test functionality that accesses the environment, like files on disk or connect to something across the network, you need an env test. Do beware of errors related to test setup though, since you now depend on the external environment. TCP ports that you try to listen to might be already taken. Files that you assume exist might not be there.

Source:
```python
import logging
import testing

actor _TestWithEnv(t: testing.EnvT):
    log = logging.Logger(t.log_handler)
    def test():
        log.info("EnvTester.test() running, going to check with the env", {"worker_threads": t.env.nr_wthreads})
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
   Finished compilation in   0.023 s
  Final compilation step
   Finished final compilation step in   0.484 s

Tests - module example:
  envtest1:              OK: 1213 runs in 50.135ms

All 1 tests passed (0.689s)

```

The test discovery system finds environment tests by looking for *actors* that take a `testing.EnvT` parameter.

*Snapshot testing* can be enabled by providing an output of type *str* to the `.success(output: ?str)` function. The Acton test framework will take care about recognizing the test as a snapshot test and comparing its output to the expected *snapshot value*.
