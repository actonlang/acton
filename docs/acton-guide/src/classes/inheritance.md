# Class inheritance

Inheritance lets one class specialize another.

Use it when there is a real "is a" relationship between the two types.
The derived class inherits behavior from the base class and can add or
override methods of its own.

<div class="advanced-content">
<p>Use inheritance sparingly. It couples representation, lifecycle, and
method lookup, so a base class becomes part of the derived type's public
contract. When the shared need is only behavior, a protocol is often a
better fit. When the shared need is only state reuse, composition is
often clearer.</p>
</div>

```python
class Shape(object):
    def area(self) -> float:
        raise NotImplementedError("subclasses must implement area()")


class Circle(Shape):
    def __init__(self, radius):
        self.radius = radius

    def area(self):
        return 3.14 * self.radius ** 2


class Square(Shape):
    def __init__(self, side):
        self.side = side

    def area(self):
        return self.side ** 2


actor main(env):
    circle = Circle(3.14)
    square = Square(3.14)
    print(circle.area() + square.area())
    env.exit(0)
```

In this example:

- `Shape` defines a common interface
- `Circle` and `Square` inherit from `Shape`
- each subclass provides its own `area()` implementation
- code written against `Shape` can still call `area()` on either value

<div class="beginner-content">
<p>Read <code>class Circle(Shape):</code> as "Circle is a kind of
Shape". If that sentence feels wrong, inheritance is usually the wrong
tool.</p>
</div>
