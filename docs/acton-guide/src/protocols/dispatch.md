# Protocol method dispatch

Protocol dispatch is about choosing which protocol implementation to
use.

For ordinary class methods, the answer is usually the method on the
actual class. For protocol methods, the result depends on the type the
program is observing at that point.

Here are two classes, the base class `Point` and the derived class
`Point3D`. Both implement `Eq`.

```python
class Point(object):
    def __init__(self, x: int, y: int):
        self.x = x
        self.y = y

extension Point (Eq):
    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

class Point3D(Point):
    def __init__(self, x: int, y: int, z: int):
        self.x = x
        self.y = y
        self.z = z

extension Point3D (Eq):
    def __eq__(self, other):
        return self.x == other.x and self.y == other.y and self.z == other.z

def comparator(a: Point, b: Point) -> bool:
    return a == b

actor main(env):
    p1 = Point3D(1, 2, 3)
    p2 = Point3D(1, 2, 4)

    print(p1 == p2)

    print(comparator(p1, p2))

    env.exit(0)
```

The first comparison uses `Point3D` as the observed type, so it uses the
`Eq` implementation for `Point3D`.

The second comparison goes through `comparator(a: Point, b: Point)`, so
the observed type has been forced to `Point`. That means protocol
dispatch uses the `Eq` implementation for `Point`, which ignores `z`.

<div class="beginner-content">
<p>If protocol dispatch feels surprising, first ask: what type is the
program actually seeing here? If you annotate values as a base type too
early, you can force dispatch to use the base-type implementation.</p>
</div>

In this case, the better API is usually a generic one that keeps the
full type:

```python
def generic_comparator[A(Eq)](a: A, b: A):
    return a == b
```

<div class="advanced-content">
<p>This is one reason generic constraints such as <code>[A(Eq)]</code>
often produce better behavior than prematurely forcing values into a
base-class type. Protocol dispatch follows the observed type, so type
annotations, container element types, and narrowing steps can all change
behavior without changing the value itself.</p>
</div>

The same issue can appear when values are stored in a collection typed
as the base class:

```python
actor main(env):
    ref_point = Point3D(1, 2, 4)
    p1 = Point3D(1, 2, 3)
    p2 = Point3D(1, 2, 4)
    my_points: list[Point] = [p1, p2]
    for point in my_points:
        if point == ref_point:
            print("Found the reference (compared as Point)", point)
        if isinstance(point, Point3D) and point == ref_point:
            print("Found the reference (compared as Point3D)", point)
```

Here, `isinstance` narrows the observed type back to `Point3D`, so the
more specific protocol implementation is used again.

## What to watch

- If you annotate values as a base type too early, protocol dispatch
  can use the base-type implementation.
- If you want behavior to follow the concrete value, keep the type
  generic for as long as possible.
- If you need a more specific implementation after narrowing, use the
  narrower type again before the protocol method call.
