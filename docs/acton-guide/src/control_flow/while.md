# while

Use `while` when you want to keep looping for as long as a condition
stays true.

```python
actor main(env):
    var remaining = 3

    while remaining > 0:
        print("remaining:", remaining)
        remaining -= 1

    print("done")
    env.exit(0)
```

<div class="beginner-content">
<p>With <code>while</code>, make sure something in the loop body can
eventually make the condition false. If that condition depends on a
changing value in actor code, that value usually needs
<code>var</code>.</p>
</div>

`while` is useful when the number of iterations is not the main point.
What matters is the condition.

```python
actor main(env):
    var retries = 5

    while retries > 0:
        print("trying...")
        retries -= 1

    env.exit(0)
```

If you already have a collection or a simple numeric range, a
`for` loop is usually clearer than a `while` loop. Reach for `while`
when the condition really is the center of the logic.

<div class="advanced-content">
<p>A <code>while</code> loop makes state transitions explicit, which is
useful for retries and actor-local state machines. The tradeoff is that
you now own the loop invariant and termination condition, so it is more
error-prone than <code>for</code> when the iteration boundary is already
known.</p>
</div>

Typical uses include retry loops, waiting for a condition to change, and
small state machines where each pass updates the state before checking
again.
