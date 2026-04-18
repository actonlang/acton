# Optionals

An optional value is either a value of some type or `None`, usually
written as `?T`.

<div class="beginner-content">
<p>For example, <code>?str</code> means "a string or
<code>None</code>".</p>

```python
name: ?str = None
```

<p>A variable of type <code>?str</code> might hold a string such as
<code>"Ada"</code>, or it might hold <code>None</code>.</p>
</div>

To use a value of type `?T` as `T`, Acton must be able to statically see
that the value is present and not `None`. A test such as
`if x is not None` narrows `x` from `?T` to `T` inside that branch.
`if isinstance(x, SomeClass)` does the same while also refining the
type.

<div class="beginner-content">
<p>In other words, you can only use the value in ways that belong to the
actual <code>T</code> type after checking that it is not
<code>None</code>.</p>

<p>In the example below, "use <code>?str</code> as <code>str</code>"
means calling a string method on it. <code>upper()</code> is a method on
<code>str</code>, not on <code>None</code>, so the call is only valid
after a check that rules out the <code>None</code> case.</p>
</div>

```python
def upper_or_none(text: ?str) -> ?str:
    if text is not None:
        return text.upper()
    return None
```

Inside the `if` branch, `text` is treated as `str`.

## Nested data

For nested data, the same idea looks like this:

```python
class Residence():
    def __init__(self, rooms: int, name: ?str = None):
        self.rooms = rooms
        self.name = name

class Person():
    def __init__(self, name: str, residence: ?Residence):
        self.name = name
        self.residence = residence

def residence_name(person: ?Person) -> ?str:
    if person is not None:
        residence = person.residence
        if residence is not None:
            return residence.name
    return None
```

Each access is guarded by a test that establishes the non-`None` case
before the next access. Narrowing applies to variables, which is why
the example first binds `residence = person.residence` and then narrows
`residence`.

## Optional Chaining

For nested data, this pattern repeats. Optional chaining is a shorter
way to express the same thing when `None` should propagate through the
current expression:

```python
def residence_name(person: ?Person) -> ?str:
    return person?.residence?.name
```

If the value to the left of `?.` or `?[...]` is `None`, the chained
expression evaluates to `None`. Otherwise the nested access proceeds
normally.

Use `?.` for attribute access and method calls, and `?[...]` for
indexing and slicing. The `?.` applies to the lookup, while the `()`
after a method name are the ordinary call syntax.

```python
def loud_residence_name(person: ?Person) -> ?str:
    return person?.residence?.name?.upper()
```

This returns `None` if `person`, `residence`, or `name` is `None`.
Otherwise it calls `upper()` on the string and returns the result.

For indexing, `d.get("numbers")?[0]` returns `None` if the key is
missing and otherwise indexes the returned list.

The result of an optional chain is still optional. For example,
`person?.residence?.rooms` has type `?int`, not `int`.

Optional chaining only short-circuits the current expression. It does
not narrow the value for later statements. Use `is None` or
`is not None` when you need to branch on whether a value is present.

Dot-based calls can stay inside the chain. For example,
`person?.residence?.name?.upper()` is valid and still has type `?str`.

But the result of the whole chain is still optional. If you need to
pass it to a separate function that expects a non-optional value, you
still need a check:

```python
def describe(name: str) -> str:
    return "Residence: " + name

def describe_residence(person: ?Person) -> ?str:
    name = person?.residence?.name
    if name is not None:
        return describe(name)
    return None
```

## Forced Unwrapping

Sometimes `None` is not a normal outcome but a broken assumption. In
that case, use forced unwrapping to require a value to be present.

`!.` and `![...]` follow the same chain shapes as `?.` and `?[...]`, but
they raise `ValueError` if the value to the left is `None` instead of
returning `None`.

```python
def required_residence_name(person: ?Person) -> str:
    return person!.residence!.name!.upper()
```

This returns `str`, not `?str`. If `person`, `residence`, or `name` is
`None`, evaluation stops and raises a `ValueError`.

You can mix `?` and `!` in the same chain. Later steps still see the
result of earlier ones, so a later `!` will raise if an earlier `?`
already produced `None`.

```python
def first_port(config: ?dict[str, list[int]]) -> int:
    return config?.get("ports")![0]

def first_port_or_none(config: ?dict[str, list[int]]) -> ?int:
    return config!.get("ports")?[0]
```

In the first function, a missing `config` or missing `"ports"` key
reaches `![0]` and raises `ValueError`. In the second, `config` must be
present, but a missing `"ports"` key still yields `None`.

Use forced unwrapping when absence indicates a bug or broken invariant
and execution should stop immediately.

Use optional chaining when a missing value is part of normal control
flow and you want the current expression to stay optional without
repeating intermediate `None` checks. The chain does not handle the
`None` case by itself; it is just a shorter way to write nested access
before later conditions, matching, or other handling of the optional
result.
