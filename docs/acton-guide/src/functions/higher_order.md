# Higher order functions

Acton supports higher order functions which means you can pass a function as an argument to another function.

Source:
```python
def multiply_with_3(a):
    print("Multiplying with 3")
    return 3*a

def multiply_with_42(a):
    print("Multiplying with 42")
    return 42*a

def compute(a, fun):
    """Compute value from a using function fun"""
    return fun(a)
    
actor main(env):
    print( compute(7, multiply_with_3) )
    print( compute(7, multiply_with_42) )
    env.exit(0)
```

Output:
```sh
Multiplying with 3
21
Multiplying with 42
294
```

Sometimes a separate named function is unnecessary. In those cases, you can use a lambda expression to define a small anonymous function inline. The syntax is `lambda args: expression`, and the value of the expression becomes the return value. For example, `compute(7, lambda a: 2 * a)` passes a function to multiply a value with 2 without introducing a separate `def`.

Lambda expressions are also useful when you need a small adapter in a callback:

In the example below, `run_compute()` accepts a callback of type `(str, int) -> None` because it wants to pass along the current label together with the computed result. The call to `c.compute()` inside `run_compute()` instead expects a callback of type `(int) -> None`, so the lambda bridges that mismatch by taking the computed integer result and attaching the caputred label, before forwarding to the `main.report` callback.

```python
actor Compute():
    def compute(a: int, cb: (int) -> None):
        cb(42 * a)

actor main(env):
    var label = "Computed result"
    c = Compute()

    def report(prefix: str, result: int):
        print("{prefix}: {result}")
        env.exit(0)

    def run_compute(value: int, cb: (str, int) -> None):
        current_label = label
        c.compute(value, lambda result: cb(current_label, result))

    run_compute(7, report)
    label = "Result"
```

Output:
```sh
Computed result: 294
```

Inside an actor, a lambda may capture ordinary local variables such as `current_label`, but not local actor variable attributes such as `label`, since those are actor state. That is why `run_compute()` first copies `label` into the local variable `current_label` and then captures that local in the lambda. The compiler disallows direct access to actor state from lambdas to avoid turning a simple callback adapter into an extra actor interaction just to sample the current state. Copy actor state to a local variable first if you need to include it in a lambda.
