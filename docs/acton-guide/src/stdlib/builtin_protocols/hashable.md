# Hashable

The `Hashable` protocol defines how values participate in hashing.

It is required for dictionary keys and set elements.

<div class="beginner-content">
<p>If a collection needs to look up a value by hash, the value's type
must be hashable. That is why dictionary keys and set elements have this
constraint.</p>
</div>

## Protocol definition

`Hashable` extends `Eq`, which means hashable values must also support
equality:

```python
protocol Hashable (Eq):
    hash : (hasher) -> None
```

The built-in helper functions are:

```python
def hash(x: Hashable) -> u64
def seed_hash(seed: u64, x: Hashable) -> u64
```

## How hashing works

Hashing uses a two-part design:

1. A `hasher` object accumulates hash input.
2. A value's `hash` method feeds its data into that `hasher`.

When you call `hash(x)`, Acton creates a `hasher`, asks `x` to feed its
state into it, and then finalizes the result as a `u64`.

```python
p = Point(10, 20)
h = hash(p)
print("hash:", h)
```

## Implementing `Hashable`

To make a custom type hashable, define both equality and hashing so
they describe the same identity.

```python
class Point:
    x: int
    y: int

    def __init__(self, x: int, y: int):
        self.x = x
        self.y = y

extension Point(Hashable):
    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

    def hash(self, h):
        self.x.hash(h)
        self.y.hash(h)
```

Equal values must contribute the same data to the hasher. Choose the
fields and any normalization to agree with `__eq__`, and process them in a
consistent order. In particular, do not include a cache or other field
that can differ between values that compare equal.

Different values are allowed to have the same hash: this is a collision.
Including all fields used by equality usually improves the distribution,
but omitting one does not by itself violate correctness. Keep the state
used by equality and hashing unchanged while a value is a dictionary key
or set element.

<div class="advanced-content">
<p>The compiler checks the lifetime of the supplied hasher. It does not
check that hashing agrees with equality or that hashes are well distributed.</p>
</div>

## Hashing fields and tuples

A field can have any type implementing `Hashable`, including a user-defined
class whose hash implementation has passed the same checks. This works
across module boundaries and with generic `A(Hashable)` constraints.
It does not depend on the field having a built-in one-shot hash operation.

Tuples are hashable when every component is hashable. Components may
include user-defined classes and other tuples. Use the explicit protocol
call `Hashable.hash(value, h)` for a concrete tuple: `value.hash(h)` would
select a named tuple component called `hash` instead. The explicit form
also works for built-in values and user-defined classes.

For example, using `Point` from above:

```python
class Entry(object):
    point: Point
    extra: (Point, str)

    def __init__(self, point: Point, extra: (Point, str)):
        self.point = point
        self.extra = extra

extension Entry(Hashable):
    def __eq__(self, other):
        return self.point == other.point and self.extra == other.extra

    def hash(self, h):
        self.point.hash(h)
        Hashable.hash(self.extra, h)
```

Both calls contribute to the same supplied hasher. Tuple hashing forwards
that hasher to each component and also records component boundaries. For
example, the string components `("ab", "c")` and `("a", "bc")` have different
encodings despite having the same concatenation. An existing tuple field
can be hashed directly; constructing a temporary tuple just to hash several
fields may allocate an additional object.

The one-shot optimization belongs to the top-level `hash(value)` operation
for selected built-in types. A field's `hash(h)` operation feeds the existing
hasher, allowing it to compose with the other fields.

## Rules for using the supplied hasher

A `Hashable.hash` method returns `None`. It may use its `h` argument only
during that call. The compiler may keep the hasher on the stack, so it must
not remain reachable after the method returns.

The compiler accepts:

- Forwarding `h` to another resolved `Hashable.hash` implementation, as in
  `self.point.hash(h)` or `Hashable.hash(self.extra, h)`.
- Feeding custom bytes with the built-in `h.update(data)` operation.
- Local aliases such as `same_hasher = h`, subject to the same restrictions.
- Conditionals, loops and ordinary computations.

Sequential field calls are the usual implementation, but they are not the
only accepted form. For example, a method can hash a sequence's length and
then use a loop to hash its elements, even when the sequence type itself
does not implement `Hashable`.

Do not retain the hasher in an object or container that can outlive the
call, return it, capture it in a closure, or send it to asynchronous work.
The compiler conservatively rejects closure capture even if that particular
closure would remain local.

The check recognizes the resolved `Hashable` protocol operation, not merely
a method named `hash`. Only compiler-distributed built-in native hash
implementations are trusted without an Acton body.

The compiler currently also accepts the built-in `h.finalize()` operation,
but ordinary hash implementations leave finalization to the caller of
`hash(value)` or `seed_hash(seed, value)`.

These lifetime rules do not guarantee that the entire hash method allocates
nothing: temporary values, scalar boxing and custom computations can still
allocate.

## Using hashable values in collections

Once a type implements `Hashable`, values of that type can be used in
sets and as dictionary keys:

```python
def test_hashable_point():
    p1 = Point(1, 2)
    p2 = Point(3, 4)
    p3 = Point(1, 2)

    points = {p1, p2, p3}
    point_names = {p1: "origin", p2: "destination"}

    print("unique points:", len(points))
    print("point names:", point_names)
```

## Built-in types

Many built-in value types implement `Hashable`, including:

- `bool`
- `int`, `bigint`, and the fixed-width integer types
- `float`
- `complex`
- `str`
- `bytes`
- tuples, whose components must all implement `Hashable`
- `iset`, whose elements must also implement `Hashable`

An immutable set's hash depends on its elements, regardless of insertion
order. Immutable sets can therefore be dictionary keys or elements of
other sets.

Mutable `set` and `dict` do not implement `Hashable`. Their elements and
keys, respectively, must still implement `Hashable`.
