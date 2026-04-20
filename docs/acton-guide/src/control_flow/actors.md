# Control flow in an async actor world

Acton programs do not have only one simple top-to-bottom control flow.

Once an actor exists, it keeps living and reacting to incoming method
calls until it stops. That means control flow is closer to "react to
messages over time" than to "run one main function and finish
immediately".

<div class="beginner-content">
<p>If you are new to this style, start with one actor and a few
methods. Then add async calls and delayed work once the basic message
flow makes sense. Inside one actor, each message still runs
sequentially; the new part is thinking about what messages may arrive
next.</p>
</div>

<div class="advanced-content">
<p>This reactive model means message order belongs to each actor's own
mailbox and handling, not one global timeline for the whole program.
That same boundary is why callbacks, async calls, and actor lifetime
fit together in Acton.</p>
</div>

## A mental model

It helps to think in terms of actors reacting:

- an actor receives a method call
- it handles that work sequentially
- it may call other actors
- it may schedule more work with `after`
- then it becomes idle again until the next message arrives

This is why actor code often looks different from ordinary function
code. The interesting question is usually not just "what happens
next?", but also "what happens later, and in response to what message?"
