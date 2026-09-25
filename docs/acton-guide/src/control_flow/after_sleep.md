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

## `after` or `after now`

Plain `after` is built for periodic work. It counts the delay from when
the current round was due to run, not from when the `after` statement
runs, so the time a round spends working does not push the next one
back. A `tick()` that ends with `after 1: tick()` stays on a steady
one-second schedule as long as each round takes less than a second. If
it falls behind anyway, because a round ran long or the machine was
busy or suspended, the overdue rounds run back to back until it has
caught up.

`after now` is a plain delay: it counts from the moment the statement
runs. Use it when the wait itself is what matters, such as a backoff
before retrying something that failed, a pause between requests to a
remote service, or polling that should always leave a full interval
between rounds instead of catching up after a stall.

```python
def tick():
    sample()
    after 1: tick()         # every second, on a steady schedule

def on_error(error):
    after now 5: connect()  # wait five seconds, then try again
```

For a one-off delay set as soon as an event comes in, such as in a
callback for incoming data, the two behave the same. They differ once
the work has taken a while to reach the `after`, for example after an
`await`, a long computation or a wait in a busy actor's queue. Plain
`after` then fires that much sooner, and at once if the whole delay has
already gone by.

`now` is only a keyword directly after `after`, so a variable named
`now` still works as before: `after now: f()` uses it as the delay.

<div class="advanced-content">
<p>Plain <code>after</code> measures from the baseline of the message
being handled. A message for an I/O event, or the first message at
program start, gets the current time as its baseline. A message that an
actor sends inherits the baseline of the message that actor is
handling, so a chain of work started by one event shares one baseline.
<code>after d</code> schedules its call at that baseline plus
<code>d</code> and runs it with that time as its baseline, which is what
keeps periodic work on schedule. <code>after now d</code> schedules its
call at the current time plus <code>d</code>, and the call starts from
that new baseline.</p>
<p>The baseline falls behind the clock while a message waits in an
actor's queue, while this actor or earlier actors in the chain compute,
and during an <code>await</code>, since the code after an
<code>await</code> keeps the baseline of its message. A timer that fires
late passes the lateness on, because its call runs with the baseline it
was scheduled for; that is how periodic work catches up after a stall.
With a 2 second gap, <code>after 3.8:</code> fires 1.8 seconds later,
and with a gap over 3.8 seconds it fires at once, while
<code>after now 3.8:</code> always waits 3.8 seconds.</p>
<p>The same rule makes plain <code>after</code> fit a deadline that
should count from when a request arrived, even when it is set after an
<code>await</code>; <code>after now</code> would restart the count from
that point.</p>
</div>
