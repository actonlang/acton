# `self`

`self` is implicitly bound inside an actor. Use it when you need to pass
a reference to the current actor to another actor. Inside the actor,
`self` is usually unnecessary.

<div class="advanced-content">
<p><code>self</code> inside an actor is about identity and
communication, not ordinary attribute access. Passing
<code>self</code> hands out a callback path to the current actor, which
makes it central to request/reply and subscription-style protocols.</p>
</div>

Source:
```python
actor Pinger(ponger: Ponger):
    def pong(message: str):
        print("Pinger: Got pong:", message)

    print("Pinger: Sending ping to Ponger...")
    ponger.ping(self)

actor Ponger():
    def ping(pinger: Pinger):
        print("Ponger: Got ping!")
        # Call back to the pinger
        pinger.pong("Hello from Ponger!")

# Usage
actor main(env):
    ponger = Ponger()
    pinger = Pinger(ponger)

    env.exit(0)
```

Output:
```console
Pinger: Sending ping to Ponger...
Ponger: Got ping!
Pinger: Got pong: Hello from Ponger!
```

In this example, `Pinger` passes `self` to `Ponger` when pinging, allowing the ponger to send a pong back.
