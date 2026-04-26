# Optionals

Before you can use an optional `?T` value as a plain `T` value, Acton
must be able to see that the value is present.

## Narrowing

`if x is not None` narrows `x` from `?T` to `T` inside that branch. Use
this form when the following code needs the value itself, not a value
that may still be `None`.

```python
def upper_or_none(text: ?str) -> ?str:
    if text is not None:
        return text.upper()
    return None
```

Inside the `if` branch, `text` is treated as `str`, so ordinary string
methods are available. `if isinstance(x, SomeClass)` can also narrow an
optional value while refining the concrete class.

Acton currently narrows variable names, not arbitrary attributes or
expressions. When you need several guarded accesses, bind the
intermediate value to a name and narrow that name explicitly.

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

Each access is guarded by a test that rules out `None` before the next
access.

## Optional chaining

Optional chaining is a shorter way to keep `None` flowing through a
single expression.

```python
def residence_name(person: ?Person) -> ?str:
    return person?.residence?.name
```

The chain above is the compact form of the explicit checks from the
previous section:

```python
def residence_name(person: ?Person) -> ?str:
    if person is not None:
        residence = person.residence
        if residence is not None:
            return residence.name
    return None
```

If the value to the left of `?.` is `None`, the whole expression
evaluates to `None`. If the value to the left of `?[...]` is `None`,
indexing or slicing is skipped and the result is `None`.

Use `?.` for attribute access and method calls, and `?[...]` for
indexing and slicing.

```python
def loud_residence_name(person: ?Person) -> ?str:
    return person?.residence?.name?.upper()

def first_port(config: ?dict[str, list[int]]) -> ?int:
    return config?.get("ports")?[0]
```

The result of an optional chain is still optional. For example,
`person?.residence?.rooms` has type `?int`, not `int`.

Optional chaining only affects the current expression. It does not
narrow the value for later statements, so use `is None` or `is not None`
when you need to branch on the result.

<div class="advanced-content">
<p>Optional chaining lifts each later access into optional context. Each
step only runs if the previous step produced a real value; otherwise the
whole expression settles to <code>None</code> immediately. That is why a
chain is good for one-pass extraction of a nested value: it preserves
absence without forcing you to invent a sentinel or write a stack of
temporary checks just to carry <code>None</code> through the expression.</p>

<p>That same property is also the limit of the feature. A chain tells
you only the final result, not which step failed or what should happen
next. Once you need branching, logging, recovery, or repeated use of an
intermediate value, stop chaining and narrow explicitly with
<code>is None</code> / <code>is not None</code>. Optional chaining is
best for compact extraction, not for complex control flow.</p>
</div>

## Forced unwrapping

Use forced unwrapping when surrounding logic has already established
that an optional value is present. If that assumption is wrong, Acton
raises `ValueError` instead of continuing with `None`.

Use `!` on an optional value to get the non-optional value inside it. If
`b` has type `?str`, then `b!` has type `str`.

```python
def required_name(name: ?str) -> str:
    return name!
```

You can think of forced unwrapping as this check:

```python
def unwrap[T](value: ?T) -> T:
    if value is not None:
        return value
    raise ValueError("unexpected None value")
```

You do not normally write that helper yourself. The `!` syntax is the
short form of "give me the value if it is present, otherwise raise
`ValueError`".

`!.` and `![...]` apply the same idea inside attribute access and
indexing chains. They follow the same shapes as `?.` and `?[...]`, but
raise `ValueError` if the value to the left is `None` instead of
returning `None`.

```python
def required_residence_name(person: ?Person) -> str:
    return person!.residence!.name!.upper()
```

This returns `str`, not `?str`. If `person`, `residence`, or `name` is
`None`, evaluation stops and raises `ValueError`.

You can mix `?` and `!` in the same chain. Later steps still see the
result of earlier ones, so a later `!` will raise if an earlier step
produced `None`.

```python
def first_port_required(config: ?dict[str, list[int]]) -> int:
    return config!.get("ports")![0]

def first_port_or_none(config: ?dict[str, list[int]]) -> ?int:
    return config?.get("ports")?[0]
```

Use forced unwrapping when absence indicates a bug or broken invariant
and execution should stop immediately.

<div class="advanced-content">
<p>Forced unwrapping is for invariants: states that should already have
been proven by surrounding logic. It is the right tool when a missing
value means the program state is wrong, not when absence is still part
of the normal control flow.</p>
</div>
