# if / elif / else

Use `if` to run code only when a condition is true.

Use `elif` for more cases and `else` for the fallback case.

```python
def describe(n):
    if n < 0:
        return "negative"
    elif n > 0:
        return "positive"
    else:
        return "zero"

actor main(env):
    print(describe(-7))
    print(describe(0))
    print(describe(5))
    env.exit(0)
```

The conditions in `if`, `elif`, and `while` are expressions that
evaluate to `True` or `False`.

```python
n = 7

if n % 2 == 0:
    print("even")
else:
    print("odd")
```

<div class="advanced-content">
<p>Conditionals compose with Acton's expression model, so guards often
mix comparisons, membership tests, and short-circuiting in one place.
Once the branching logic starts encoding type or protocol decisions,
helper functions or explicit narrowing usually read better than a long
ladder.</p>
</div>
