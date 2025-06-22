# The Hashable Protocol

The `Hashable` protocol defines how objects can be hashed, which is essential for using them as keys in dictionaries or elements in sets. It provides a flexible and composable approach to hashing custom types. All built-in types (`int`, `str`, `list`, `dict`, etc.) already implement this protocol.

## Protocol Definition

The `Hashable` protocol extends the `Eq` protocol, meaning any hashable type must also support equality comparison:

```python
protocol Hashable (Eq):
    hash : (hasher) -> None
```

## How It Works

The hashing system in Acton uses a two-part approach:

1. **The `hasher` object**: A stateful object that accumulates hash data
2. **The `.hash()` method**: Updates the hasher with the object's data

When you need a hash value, you use the built-in `hash()` function:

```python
p = Point(10, 20)
h = hash(p)  # Returns a u64 hash value
print("Hash of point: {h}")
```

## Implementing Hashable for Custom Types

To make a custom type hashable, you need to:

1. Implement the `__eq__` method (required by the `Eq` protocol)
2. Implement the `hash` method that updates the hasher

Here's a simple example:

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
        self.x.hash(h)  # Hash the x coordinate
        self.y.hash(h)  # Hash the y coordinate
```

It is important to:
1. **Hash all fields used in equality**: If two objects are equal according to `__eq__`, they must produce the same hash value.
2. **Order matters**: Always hash fields in the same order, as the hasher is stateful.
3. **Use all significant fields**: Include all fields that contribute to the object's identity.

## Example: Using Custom Types in Collections

Once your type implements `Hashable`, it can be used in sets and as dictionary keys:

```python
def test_hashable_point():
    p1 = Point(1, 2)
    p2 = Point(3, 4)
    p3 = Point(1, 2)
    
    # Use in a set
    points = {p1, p2, p3}  # Will contain only 2 points (p1 and p3 are equal)
    
    # Use as dictionary keys
    point_names = {p1: "origin", p2: "destination"}
    
    print("Unique points:", len(points))
    print("Point names:", point_names)
```

## Implementation Details

Under the hood, Acton uses the wyhash algorithm, which is very fast and provides good distribution properties.
