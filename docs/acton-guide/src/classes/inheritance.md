# Class Inheritance

Inheritance is a way of creating a new class from an existing class. The new class is called the derived class and the existing class from which we inherit is called the base class. The derived class inherits all the attributes and methods of the base class. The derived class can extend the base class by adding more attributes or methods. It is also possible to override methods to create more specific functionality.

We add the `area()` to `Circle` to get the area and realize that the unlike the diameter, area is common for all gemetric shapes. Thus we create a base class `Shape` that defines the `area()` method, but does not implement it since there is no generic way to compute the area for all shapes. Each concrete class, like `Circle` and `Square`, should implement `area()`.

Source:
```python
class Shape(object):
    def area(self) -> float:
        raise NotImplementedError("This method should be overridden by subclasses")


class Circle(Shape):
    radius: float

    def __init__(self, radius):
        self.radius = radius

    def diameter(self):
        return self.radius * 2

    def area(self):
        return 3.14 * self.radius ** 2


class Square(Shape):
    def __init__(self, side: float):
        self.side = side

    def area(self) -> float:
        return self.side ** 2


actor main(env):
    circle = Circle(3.14)
    square = Square(3.14)
    print(circle.area() + square.area())
    env.exit(0)
```

Inheritance is one of the primary methods, if not *the* primary method, of structuring programs in the object oriented paradigm.

`Shape` is an abstract class because it has no `__init__` method.
