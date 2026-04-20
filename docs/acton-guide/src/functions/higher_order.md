# Higher order functions

Acton supports higher order functions, which means you can pass a
function as an argument to another function and choose behavior at the
call site.

That is Acton's nearest equivalent to the reusable part of Rust's
closure and iterator story. Acton does not have a Rust-style closure or
iterator-adapter path to learn first; use higher-order functions,
comprehensions, and explicit iteration instead.

<div class="beginner-content">
<p>A function can be treated like any other value. That makes it easy to
reuse one loop, one validation path, or one calculation with different
behaviors plugged in.</p>
</div>

```python
def apply_twice(fun, value):
    return fun(fun(value))

def double(n):
    return 2 * n

def square(n):
    return n * n

actor main(env):
    print(apply_twice(double, 3))
    print(apply_twice(square, 2))
    env.exit(0)
```

This prints:

```sh
12
16
```

Use this pattern when the operation is the thing that changes and the
overall shape of the work stays the same.

<div class="advanced-content">
<p>Higher order functions work best when the varying behavior is small,
stateless, and easy to describe as "apply this operation here". That
makes them a good fit for callbacks, adapters, validation hooks, and
small reusable transformation steps. Once the behavior needs evolving
state across calls, the design question changes: you are no longer just
passing behavior, you are passing behavior plus state.</p>

<p>At that point, an actor or a small object is often a better home than
trying to simulate closure-heavy code by threading more and more helper
arguments through every call. The same applies to collection work: if a
transformation is local and easy to read, a comprehension or short loop
usually expresses it more clearly than a stack of tiny callback-style
helpers. Reach for higher order functions when they clarify the reusable
variation, not just because the language allows them.</p>
</div>
