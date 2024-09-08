# Protocol Method Dispatch and Method Resolution Order

Method dispatch and Method Resolution Order (MRO) is about picking a particular implementation of a method when there are multiple to pick from. For class inheritance, it is pretty straight forward, the method used will always be that of the actual class. For protocols, it is not as simple. Dispatch of methods for protocols is based on the observed type, which can be somewhat surprising.

Here are two classes, the base class `Point` and the dervied class `Point3D` (which inherits from `Point`). Each implement the `Eq` protocol.

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
    # p1 and p2 are actually different in z. If seen as a 2D point, they appear
    # equal since the x and y values of p1 and p2 are the same
    p1 = Point3D(1, 2, 3)
    p2 = Point3D(1, 2, 4)

    # False, because we see p1 and p2 as Point3D objects and thus dispatch the Eq
    # extension __eq__ method to Point3D which correctly compares x, y and z
    print(p1 == p2)

    # True, because comparator takes (Point, Point) and thus we see p1 and p2
    # as Point objects and thus dispatch the Eq extension __eq__ method to Point
    # which only compares x and y and thus returns True despite the z difference
    print(comparator(p1, p2))

    env.exit(0)
```

The `comparator` function takes arguments of the base class `Point` type, which means that when dispatching, they will use the `Eq` protocol of `Point` rather than that of `Point3D`. Naive use typically dispatches to the protocol implementation of the derived class, which is what we intuitively want. It's only when we have forced the type to be observed, as here, that we end up with the "wrong" protocol implementation. Note that there are also cases where this is the desired behavior.

In this particular example, the natural and better solution is to write the comparator function as requiring the Eq type:

```python
def generic_comparator[A(Eq)](a: A, b: A):
    return a == b
```

Another common scenario is when the type has been forced by storing items in a list or dict, which in turn is typed as storing a base class.

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
            print("Found the reference (compared to Point3D)", point)
```

As shown in the above example, we can force the comparison to use the `Eq` protocol implemention of `Point3D` by using `isinstance` to check and thus coerce the observed type of `point` to be `Point3D`.
