# Testing

Testing your code is a really good idea! While Acton's type system allows interfaces to be precisely defined, it is imperative that the behavior of a function or actor is tested!

Test functions are automatically discovered by the compiler. Run tests with `acton test` which automatically builds the project first. Import the testing module and name your functions starting with `_test`. The type signature should match the test intended test category. Use the assertion functions available in the testing module. For example:

```
import testing

def _test_simple():
    foo = 3+4
    testing.assertEqual(7, foo)
```

```
$ acton test
Building project in /home/user/foo
  Compiling foo.act for release
   Finished compilation in   0.020 s
  Final compilation step
   Finished final compilation step in   0.022 s
Build done, running tests for modules:

Tests - module foo:
  simple:           OK (0.005020ms)

All 1 tests passed (0.004s)
```


There are 4 categories of tests:
- unit tests
  - type signature: `mut() -> None`
  - outcomes:
    - test function returns `None` = test success
    - test function raises `AssertionError` = test failure
    - test function raises other exception = test error
  - Functions in Acton always run on a single worker thread and are deterministic
  - The test runner will run as many unit tests in parallel as there are available CPU threads
- synchronous actor tests
  - type signature: `proc(log_handler=logging.Handler) -> None`
  - outcomes:
    - test function returns `None` = test success
    - test function raises `AssertionError` = test failure
    - test function raises other exception = test error
  - The test runner will attempt to run a fourth as many tests concurrently as there are CPU threads, e.g. with 16 CPU cores it will run 4 tests in parallel. This presumes that tests will use multiple actors, which can run in parallel.
- asynchronous actor tests
  - type signature: `proc(report_result: action(success: ?bool, exception: ?Exception) -> None, log_handler=logging.Handler) -> None`
  - The test function must report the outcome by calling the report_result() function
    - `report_result(True, None)` for successful tests
    - `report_result(False, exc)` failures / errors, where `exc` is the exception raised
  - The test runner will attempt to run a fourth as many tests concurrently as there are CPU threads, e.g. with 16 CPU cores it will run 4 tests in parallel. This presumes that tests will use multiple actors, which can run in parallel.
- environment tests
  - type signature: `proc(report_result: action(success: ?bool, exception: ?Exception) -> None, env: Env, log_handler=logging.Handler) -> None`
  - The test function must report the outcome by calling the report_result() function
    - `report_result(True, None)` for successful tests
    - `report_result(False, exc)` failures / errors, where `exc` is the exception raised
  - these tests have access to the full environment via the `env` argument and can thus communicate with the outside world
    - this is a source of non-determinism so be mindful of this and try to avoid non-deterministic functions to the largest degree possible
  - The test runner will attempt to run a fourth as many tests concurrently as there are CPU threads, e.g. with 16 CPU cores it will run 4 tests in parallel. This presumes that tests will use multiple actors, which can run in parallel.
  
Pure functions are deterministic and are thus preferable for tests. Since the Acton RTS is multi-threaded and actors are scheduled concurrently on worker threads, using actors imply a degree of non-determinism. For example, actor A might be schedule before or after actor B so if the test relies on ordering of the output, it could fail or succeed intermittently. Interacting with the surrounding environment by reading files or communicating over the network introduces even more sources of non-determinism. Avoid it if you can. When possible, strive to use unit tests rather than actor based tests and strive to avoid env tests.

## Test failures vs errors

The test runner differentiates between the concepts of test failures and test errors. Test failures are when the test ran as expected but had an unexpected result. All assertion functions lead to test failures. Test errors are when the test was not able to run, which could indicate a design issue or that the test environment is not as expected. For example, if a test attempts to retrieve `https://dummyjson.com/products/1` and check that the returned JSON looks a certain way, it would be a test failure if the returned JSON does not mach the expected value and it would be a test error if we don't have an Internet connection and are thus unable to retrieve the JSON. It's probably a bad idea to try to connect to something on the Internet in a test, so avoid that and other sources of non-determinism when possible.
