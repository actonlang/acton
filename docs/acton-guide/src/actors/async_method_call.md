# Async Method calls

Async calls let an actor tell another actor to do something without
waiting for a return value.

A method call is asynchronous when the caller does not use the return
value.

<div class="beginner-content">
<p>An async call is closer to "send this message" than "call this
function and wait".</p>
</div>

```python
actor Worker(name):
    def say(msg):
        print(name, "received:", msg)

actor main(env):
    w1 = Worker("one")
    w2 = Worker("two")

    w1.say("hello")
    w2.say("world")

    def stop():
        env.exit(0)
    after 0.1: stop()
```

Here, `main` sends two messages and keeps going. It does not wait for
either worker to return a value.

## When to use async calls

- use them for fire-and-forget work
- use them when another actor should react independently
- use them to avoid blocking the current actor on a result
