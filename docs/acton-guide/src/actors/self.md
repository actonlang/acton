# `self`

`self` is implicitly bound inside an actor. Use `self` to pass a reference to the current actor to another actor. `self` is never needed to reference to the current actor within the current actor.

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
