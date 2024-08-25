# Testing

Testing your code is a really good idea! While Acton's type system allows interfaces to be precisely defined, it is imperative that the behavior of a function or actor is tested!

Test functions are automatically discovered by the compiler. Run tests with `acton test`. Import the testing module and name your test functions starting with `_test`. The type signature should match the test intended test category. Use the assertion functions available in the testing module. Here is a simple unit test:

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
