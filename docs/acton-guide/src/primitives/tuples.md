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

## Comparing tuples and using them as keys

Tuples whose components support it can be compared and hashed, so they
work as dictionary and set keys and can be sorted.

```python
def demo() -> str:
    d = {(1, 2): "a", (3, 4): "b"}       # tuples as dict keys
    positions = [(2, "b"), (1, "c")]
    print((1, "x") == (1, "x"))          # True
    print((1, 2) < (1, 3))               # True: comparison is lexicographic
    print(sorted(positions))             # [(1, 'c'), (2, 'b')]
    return d[(1, 2)]

actor main(env):
    print(demo())
    env.exit(0)
```

Directly in an actor's body, type inference currently needs a little
help pinning the tuple type for `<` and for dictionary keys: give the
dictionary an annotation such as `d : dict[(int, int), str]`, or put the
code in a function as above. Equality needs no such help.

Equality, ordering and hashing apply componentwise, so they require each
component to support the operation in turn: `(int, str)` can be compared
because `int` and `str` can, but a tuple containing a function cannot.

Both sides of a comparison must have the same shape: the same number of
components, and for named tuples the same field names in the same order.
Comparing a positional tuple with a named one is a type error. Named
tuples with reordered fields can be compared after converting to a
common annotated type:

```python
a : (x: int, y: int) = (y=2, x=1)        # reorders the fields
print((x=1, y=2) == a)                   # True
```

<div class="advanced-content">
<p>Named tuples are the bridge between raw tuple positions and classes.
They keep the value lightweight while making the shape self-documenting.
Because the tuple shape is part of the type, changing field count or
names is an API change.</p>
</div>
