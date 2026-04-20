# Sync Method calls

Acton lets one actor call another actor synchronously when it needs a
result back immediately.

A method call is synchronous when the caller uses the return value.

<div class="beginner-content">
<p>A synchronous actor call feels like an ordinary function call: ask
for a result, wait, then continue.</p>
</div>

```python
actor Calculator():
    def square(n):
        return n * n

actor main(env):
    calc = Calculator()

    answer = calc.square(7)
    print("The answer is", answer)

    env.exit(0)
```

Here, `main` waits for `calc.square(7)` to finish and then continues
with the returned value.

<div class="advanced-content">
<p>Sync calls suspend the current actor until the other actor replies.
As systems grow, it is usually better to keep sync chains short and
push longer work into asynchronous flows.</p>
</div>

## When to use sync calls

- use them when a result is needed right away
- prefer them for small, direct requests
- be careful with long chains of sync actor-to-actor calls
