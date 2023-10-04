# Actor methods

Actor methods are declared under an `actor` using the `def` keyword.

An actor method runs in the context of the actor and can access its private state. As Actors are sequential processes, calling other methods on the local actor or any function is going to be run sequentially.

Calling an actor method on the local actor can be done simply by calling it by its name, without any prefix such as `self.`.

All actor methods are public. Call a method on another actor by calling `actor_name.method_name()`. Calling methods on other actors can be done [synchronously](/actors/sync_method_call.md) or [asynchronously](/actors/async_method_call.md).

Source:
```python

def multiply(a, b):
    print("Multiplying", a, "with", b)
    return a*b
    
actor main(env):
    var secret = 42

    def compute(a):
        print("Computing result based on our secret", secret)
        res = multiply(a, secret)
        return res

    result = compute(3)
    print("Result:", result)
    env.exit(0)
```

Output:
```sh
Computing result based on our secret 42
Multiplying 3 with 42
Result: 126
```
