# Testing

Testing your code is a really good idea! While Acton's type system allows interfaces to be precisely defined, it is imperative that the behavior of a function or actor is tested!

Test functions and test actors are automatically discovered by the compiler. Run tests with `acton test`. Import the testing module and name your test functions or actors starting with `_test_`. The type signature should match the test intended test category. Use the assertion functions available in the testing module. Here is a simple unit test:

Source:
```python src/ut.act
import testing

def _test_simple():
    testing.assertEqual(1, 1)
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
   Finished final compilation step in   0.437 s

Tests - module example:
  simple:                OK: 1523 runs in 50.001ms

All 1 tests passed (0.604s)

```

There are 4 kinds of tests
- **unit tests**
  - small simple tests of pure functions
- **synchronous actor tests**
  - involving one or more actors, returning results synchronously
- **asynchronous actor tests**
  - involving actors but use asynchronous callbacks for return values
- **environment tests**
  - similar to async actor test in that a callback is used for the return value
  - these tests have access to the full environment via the `env` argument and can thus communicate with the outside world
    - this is a source of non-determinism so be mindful of this and try to avoid non-deterministic functions to the largest degree possible
  
When possible, strive to use unit tests rather than actor based tests and strive to avoid env tests.

For snapshot-based assertions, see [Snapshot testing](testing/snapshot.md).

## Cached test results

The Acton test runner caches test results which means that repeated invokations of `acton test` might not actually (re)run tests. Cached failures and errors are still shown by default, so you never miss a failing test. Cached successes are hidden unless you pass `--show-cached`. Pass `--no-cache` to force all selected tests to run, even if cached results exist.

This means that the developer experience for test driven development is great even for project with a very large amount of tests as the content hash driven test runner only recompiles and reruns tests that are actually affected by a change.

Note that test input need to be contained within .act source code files in order for the compiler to consider them part of the content hash. You cannot use external .txt files or similar as input to test functions, since the compiler won't consider those parts of the implementation. Also see [incremental compilation](compilation/incremental.md) for more details on content hashing and how it applies to testing.

## Module Filtering

You can run tests from specific modules using the `--module` flag:

```sh
acton test --module foo --module bar
```

This will only run tests from the `foo` and `bar` modules, skipping all other test modules.

## Capability-gated tests

Some tests depend on external capabilities (for example network services, hardware, or system setup). In tests that receive a test context argument (`t`), use `t.require(...)` and pass available capabilities with `--tag`:

```python
import testing

def _test_external_service(t):
    t.require("external-service")
    # test logic that depends on external-service being available
```

Run with capabilities:

```sh
acton test --tag external-service
```

If the required capability is not enabled, the test is marked as skipped.
Capabilities are runtime environment signals used by `t.require(...)`; they are not a pre-test selection/filter mechanism. This applies to test context objects like `SyncT`, `AsyncT`, and `EnvT`.

You can also skip explicitly:

```python
def _test_todo(t):
    t.skip("not implemented yet")
```
