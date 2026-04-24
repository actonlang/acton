# Optionals and `None`

`None` is the value Acton uses for "nothing here".

An optional type `?T` means a value is either a `T` or `None`.
Use an optional when absence is part of the normal result.

<div class="beginner-content">
<p>`?str` means "a string or <code>None</code>". `?int` means "an
integer or <code>None</code>". The `?` belongs to the type, not the
value.</p>
</div>

```python
def lookup_name(users: dict[str, str], username: str) -> ?str:
    if username in users:
        return users[username]
    return None

actor main(env):
    users = {"alice": "Alice Andersson"}

    name = lookup_name(users, "bob")
    if name is not None:
        print("Found:", name)
        print("Upper:", name.upper())
    else:
        print("No match")

    env.exit(0)
```

Use `is None` and `is not None` to check whether an optional is
present. When you want to use the value as non-optional, put that code
inside the `is not None` branch.

<div class="beginner-content">
<p>Write the check in the direction of the code that needs the value.
Do not rely on an <code>if name is None</code> / <code>else</code>
shape to make <code>name</code> non-optional in the <code>else</code>
branch.</p>
</div>

Optional values are common in lookups, parsing, and APIs that may or
may not find a result. `None` is not a general placeholder for every
kind of empty value; use it when absence itself matters.
