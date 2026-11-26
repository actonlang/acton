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

## Raise expressions

`raise` can also appear in an expression, including a declarative initializer:

```python
pure def checked_port(port: int) -> int:
    return port if 0 <= port <= 65535 else raise ValueError("invalid port")
```

A raise expression never produces a value, so it can be used wherever a value
of any type is expected. Raising itself adds no effect; evaluating the
exception expression can still have effects.

`raise` has low precedence: `raise first if condition else second` raises the
selected exception. To raise only in the first branch, write
`(raise error) if condition else value`.

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

## Unhandled actor exceptions

When an exception escapes an actor message and nobody is waiting for its
result, Acton prints it to standard error. The actor can process its next
message. Where the platform supports stack unwinding, the report includes
the native calls still on the worker's stack when the raise reached the
runtime, starting with the most recent call. The worker captures up to
32 return addresses. If a handler re-raises after unwinding the original
calls, the trace starts at that raise. An optimized build can omit frames.

Acton shows source lines when it can resolve them, and addresses
otherwise. On Linux, you can look up an address in the matching binary
with debug information using `addr2line -f -e ./my_program 0xADDRESS`.

An actor awaiting a failed message can catch the exception. If that actor
also leaves the exception unhandled, its report can show the original
native stack. Calls from earlier continuations have already returned by
the time the worker captures the stack.

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
