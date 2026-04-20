# Tuples

Tuples group a fixed number of values into one value.

The fields in a tuple can have different types.

A tuple is a good first step for a small value with a fixed shape.
When the shape needs names but still behaves like plain data, use a
named tuple. When the value needs methods, invariants, or a lifecycle,
move to a class.

<div class="beginner-content">
<p>A tuple has a fixed shape. If you need a small value with exactly two
or three fields, a tuple is often a good fit. If you keep forgetting
what <code>.0</code> and <code>.1</code> mean, switch to a named tuple
so the fields explain themselves. If the data starts needing methods or
construction rules, switch to a class.</p>
</div>

```python
actor main(env):
    pair = ("Ada", 36)
    point = (x=3, y=4)

    print(pair.0)
    print(pair.1)
    print(point.x)
    print(point.y)

    env.exit(0)
```

Access positional tuple fields with `.0`, `.1`, and so on.

Named tuples use field names such as `.x` and `.y`.

## Returning tuples from functions

Tuples are handy when a function naturally returns a small fixed group
of values.

```python
def parse_result():
    return (ok=True, code=200)
```

Acton can infer the tuple shape here from the returned value.

<div class="advanced-content">
<p>Named tuples are the bridge between raw tuple positions and classes.
They keep the value lightweight while making the shape self-documenting.
Because the tuple shape is part of the type, changing field count or
names is an API change.</p>
</div>
