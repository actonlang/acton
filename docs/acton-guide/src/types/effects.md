# Effects (`pure`, `mut`, `proc`, `action`)

Acton tracks effects as part of function and callable typing.
An effect marker tells you what the callable is allowed to do, so it is
part of the type information you read and design with.

The four effect markers are:

- `pure`: no side effects
- `mut`: may update state
- `proc`: functions that call actors
- `action`: action or callback style code used heavily in actor APIs

```python
pure def square(x: int) -> int:
    return x * x

class Counter:
    value: int

    def __init__(self):
        self.value = 0

    mut def next(self) -> int:
        self.value += 1
        return self.value

actor Greeter():
    def hello(msg):
        print(msg)

proc def show_square(g: Greeter, x: int) -> None:
    g.hello("square: " + str(square(x)))

actor main(env):
    counter = Counter()
    greeter = Greeter()

    print("counter:", counter.next())
    n = square(7)
    print("n:", n)
    show_square(greeter, 9)

    action def stop() -> None:
        env.exit(0)

    after 0.1: stop()
```

In this example:

- `square` is `pure`
- `Counter.next` is `mut`
- `show_square` is `proc` because it calls an actor
- `stop` is an `action`
- the effect markers are part of the callable signatures, not notes

## Effect inference

If you omit the effect marker, Acton infers it from the body.

Use explicit annotations when you want an API to promise purity, make
mutation clear, or document that a callback or actor-facing entrypoint
has a particular effect. The effect is part of the contract just like
its argument and return types.

<div class="beginner-content">
<p>A useful first habit is to keep calculations pure and push printing,
I/O, and actor orchestration into a smaller layer of effectful code.
Read <code>pure</code> as "calculation only", <code>mut</code> as "may
update state", <code>proc</code> as "calls actors", and
<code>action</code> as "actor action or callback".</p>

<p>That division makes code easier to reason about. If a helper is pure,
you know it only depends on its inputs. If it is mut or proc, you know
it may change state or interact with actors, which makes its API more
specific.</p>
</div>

## When the four effects show up

- `pure` is common for ordinary calculations and helpers that should be
  easy to reuse anywhere
- `mut` is common on methods that update state or work with local
  mutable data
- `proc` is common for functions that orchestrate work by calling
  actors
- `action` often appears in actor methods, timers, cleanup hooks, and
  callback types such as `action(str) -> None`

Effects often explain why a signature looks the way it does. A function
may have a simple data type and still be effectful, and the effect marker
is what tells you whether it stays in pure computation or crosses into
stateful or actor-driven work.

<div class="advanced-content">
<p>Effects also appear in inferred signatures. As your code gets more
generic or callback-heavy, those effect annotations become part of how
you read and design APIs. In Acton, purity is a real constraint on what
a function may call, so effect annotations are part of the contract, not
just commentary.</p>
</div>

## Practical guidance

- Prefer `pure` for deterministic, test-friendly core logic.
- Use `mut` when a callable really updates state.
- Use `proc` for functions that orchestrate work by calling actors.
- Expect `action` in actor APIs, timers, cleanup hooks, and callbacks.
- Keep pure logic separated from actor-driven orchestration code.
- Read the effect marker together with the argument and return types.

<div class="advanced-content">
<p>A useful mental model is <code>pure &lt;= mut &lt;= proc</code>, with
<code>action &lt;= proc</code> on a separate branch. That is why pure
code can be used where a mutating or procedural callable is accepted,
and why actions participate in the broader effect system without being
the same thing as ordinary sequential procedures. When you design higher
order APIs, the effect on the callback is as important as its argument
types.</p>

<p>In practice, this means you should choose the weakest effect that
describes the callable accurately. That keeps more code reusable and
leaves the effect system useful as the codebase grows.</p>
</div>
