# Errors and exceptions

Use exceptions when something is wrong and the current path should stop.
Raise an exception with `raise`, and handle it with `try` and `except`.

```python
def parse_port(text: str) -> int:
    port = int(text)
    if port < 0 or port > 65535:
        raise ValueError("port must be between 0 and 65535")
    return port

actor main(env):
    for text in ("8080", "70000"):
        try:
            port = parse_port(text)
        except ValueError as e:
            print("invalid input:", e)
        else:
            print("port:", port)

    env.exit(0)
```

<div class="beginner-content">
<p>Use an exception for a real error. If "no result" is expected and
normal, an optional value is often a better fit.</p>
</div>

## `try` structure

A `try` statement can contain these parts:

- `try` for the code that may fail
- `except` for handling specific exception types
- `else` for code that should run only when nothing failed
- `finally` for cleanup that should happen either way

```python
try:
    value = parse_port("9000")
except ValueError as e:
    print("bad input:", e)
else:
    print("ready to use:", value)
finally:
    print("done")
```

`except` runs only for matching exceptions. `else` runs only when the
`try` block completed without raising. `finally` runs whether the `try`
block succeeded or failed.

<div class="beginner-content">
<p>Keep <code>try</code> blocks narrow so it stays obvious which
operation can fail.</p>
</div>

<div class="advanced-content">
<p>Catch specific exceptions before broader ones, and keep exception
handling close to boundaries such as input parsing, file access, network
calls, and other integration points. Inside core logic, prefer domain
values or optionals when the situation is expected rather than using
exceptions as routine branching.</p>
</div>
