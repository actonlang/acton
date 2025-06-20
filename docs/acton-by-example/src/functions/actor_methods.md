# Actor methods

Actor methods are declared under an `actor` using the `def` keyword.

An actor method runs in the context of the actor and can access its private state. As Actors are sequential processes, calling other methods on the local actor or any function is going to be run sequentially.

Calling an actor method on the local actor can be done simply by calling it by its name, without any prefix such as `self.`.

All actor methods are public. Call a method on another actor by calling `actor_name.method_name()`. Calling methods on other actors can be done [synchronously](/actors/sync_method_call.md) or [asynchronously](/actors/async_method_call.md).

Source:
```python
actor Calculator():
    def multiply(a, b):
        print("Calculator multiplying", a, "with", b)
        return a * b

actor main(env):
    var secret = 42
    calc = Calculator()

    def compute(a):
        # Accessing local actor variable directly (no self. needed)
        print("Computing result based on our secret", secret)
        # Calling a method on another actor
        res = await async calc.multiply(a, secret)
        return res

    # Calling local method directly (no self. needed)
    result = compute(3)
    print("Result:", result)
    env.exit(0)
```

Output:
```sh
Computing result based on our secret 42
Calculator multiplying 3 with 42
Result: 126
```

## Use of `self` in actors

Actors have an implicit `self` that is bound to the external view of the current actor. This allows us to pass a reference to us to another actor that needs to interact with us by simply passing `self`. This is particularly useful when implementing callback patterns or when an actor needs to register itself with another actor.

This means that actor methods are defined without a `self` parameter. In fact, using `self` as a parameter name in actor methods or in the actor's initialization arguments will result in an error.

Here's an example demonstrating how an actor can pass a reference to itself:

```python
actor Worker():
    def do_work(manager, data):
        print("Worker processing:", data)
        result = data * 2
        # Call back to the manager (implicitly async)
        manager.receive_result(result)

actor Manager(env):
    def assign_work(worker):
        print("Assigning work to worker")
        # Pass 'self' so the worker can call us back when done
        worker.do_work(self, 42)

    def receive_result(result):
        print("Manager received result:", result)
        env.exit(0)

actor main(env):
    manager = Manager(env)
    worker = Worker()
    manager.assign_work(worker)
```

Output:
```sh
Assigning work to worker
Worker processing: 42
Manager received result: 84
```

In this example, the Manager actor passes `self` to the Worker, allowing the Worker to call back to the Manager when the work is complete.
