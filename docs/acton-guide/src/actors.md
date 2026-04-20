# Actors

Actors are Acton's primary unit for state and concurrency.

If you are looking for the Acton answer to Rust-style lifetime
concerns, this is it: actors own mutable state, capabilities are passed
explicitly, and the runtime handles ordinary object lifetime.

<div class="beginner-content">
<p>If classes feel like objects that own data, actors are a useful way
to think about objects that own state and participate in concurrent
work. The actor body runs once when the actor starts; its methods run
later in response to calls.</p>
</div>

An actor combines:

- private state owned by one actor
- sequential execution inside that actor
- method-based communication with other actors
- a place to put mutable program state

```python
actor Greeter(name):
    print("starting", name)

    def hello():
        print("hello from", name)

actor main(env):
    greeter = Greeter("Acton")
    await async greeter.hello()

    env.exit(0)
```

Code in the actor body runs once when the actor is created. That body is
where initialization happens, and it may define methods that operate on
the actor's private state.

<div class="advanced-content">
<p>Actors are also where several Acton-specific language pieces meet:
<code>var</code>, <code>await</code>, <code>async</code>,
<code>after</code>, capability passing, and effectful APIs. The main
guarantee is actor-local sequentiality: state changes happen one
handled message at a time inside that actor.</p>
</div>

## What to read next

- [Root Actor](actors/root.md) for the entrypoint pattern
- [Lifetime](actors/lifetime.md) for how actors stay alive
- [Attributes](actors/attributes.md) and [`self`](actors/self.md) for
  actor state
- [Actor methods](functions/actor_methods.md), [Sync Method
  calls](actors/sync_method_call.md), and [Async Method
  calls](actors/async_method_call.md) for communication
- [Concurrency](actors/concurrency.md), [Control flow in an async actor
  world](control_flow/actors.md), and [after / sleep](control_flow/after_sleep.md)
  for concurrent behavior
- [Cleanup](actors/cleanup.md) for best-effort finalization
