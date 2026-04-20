# `after` and sleep

Use `after` when work should happen later.

`after 1: tick()` tells the runtime to schedule `tick()` to run about
one second later. Meanwhile, the actor is free to handle other
messages.

<div class="beginner-content">
<p>If you would normally reach for <code>sleep()</code> in another
language, first ask whether what you really want is "run this later".
In Acton, that usually means <code>after</code>. Read
<code>after 1: tick()</code> as "schedule this call for later", not
"pause here for one second".</p>
</div>

```python
actor main(env):
    var count = 0

    def tick():
        print("tick", count)
        count += 1
        if count >= 3:
            env.exit(0)
        else:
            after 1: tick()

    tick()
```

`after` is the normal tool for:

- timeouts
- retries
- pacing repeated work
- scheduling a follow-up action

## Why not `sleep`?

Normal actor code should avoid blocking waits. A delayed callback with
`after` lets the actor go idle and react to other messages in the
meantime.

<div class="advanced-content">
<p>There is a low-level sleep in the RTS for debugging and runtime
work, but it is not the idiomatic control tool for actor programs.
<code>after</code> keeps the actor schedulable, and the callback sees
whatever state the actor has when that later message is handled.</p>
</div>
