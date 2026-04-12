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
indexing and slicing. For example, `d.get("numbers")?[0]` returns
`None` if the key is missing and otherwise indexes the returned list.

The result of an optional chain is still optional. For example,
`person?.residence?.rooms` has type `?int`, not `int`.

Optional chaining only short-circuits the current expression. It does
not narrow the value for later statements. Use `is None` or
`is not None` when you need to branch on whether a value is present.

If you need to use the result as a non-optional value, you still need a
check:

```python
    name = person?.residence?.name
    if name is not None:
        return name.upper()
```
