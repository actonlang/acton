# Actor methods

Actor methods are declared inside an `actor` with `def`.

An actor method runs in the context of that actor and can access its
private state.

<div class="beginner-content">
<p>An actor method is like a normal method with one extra idea: it runs
inside an actor that owns its own state and message flow. That is why a
method like <code>compute</code> can use <code>secret</code> without
receiving it as an argument each time.</p>
</div>

Local actor methods can call each other by name. Methods on other
actors are called through that actor's reference.

<div class="advanced-content">
<p>Because actors are sequential, local function calls and local method
calls run one step at a time inside that actor. Concurrency appears
when actors call each other. Whether a remote call is sync or async
changes ordering and waiting behavior, not just syntax.</p>
</div>

```python
def multiply(a, b):
    return a * b

actor main(env):
    var secret = 42

    def compute(a):
        return multiply(a, secret)

    result = compute(3)
    print("Result:", result)
    env.exit(0)
```

Actor methods are public by default. Calls to other actors can be
[synchronous](../actors/sync_method_call.md) or
[asynchronous](../actors/async_method_call.md).
