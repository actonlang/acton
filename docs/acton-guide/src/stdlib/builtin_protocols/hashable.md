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

The important rules are:

1. Hash all fields used by equality.
2. Hash them in a stable order.
3. Do not leave out part of the value's identity.

If two values compare equal, they must feed the same data into the
hasher.

<div class="advanced-content">
<p><code>Hashable</code> follows equality, not the other way around. If
two values compare equal but feed different data into the hasher, sets
and dictionaries can behave incorrectly in subtle ways even though the
program still typechecks.</p>
</div>

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

`dict` and `set` use `Hashable` for their key or element types, but are
not themselves documented as `Hashable` here.
