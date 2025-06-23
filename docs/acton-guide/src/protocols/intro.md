# Protocols

Protocols defines functionality in an abstract way that can be implemented by many classes.

Class and class inheritance is a good tool for structuring data and behavior. However, sometimes there is no clear parent / child relationship between classes, in which case using a protocol might be a more suitable choice.

Perhaps the simplest example is the `Eq` protocol, that defines equality between two objects, from the Acton builtins. Classes do not need to have any common inheritance in order to support equality.

```python
protocol Eq:
    @staticmethod
    __eq__       : (Self,Self) -> bool
```

The `__eq__` method takes two arguments, itself and the other object to compare with and returns `True` or `False`. Both arguments are of the same type `Self`, which means we can only compare two objects that are of the same type. It can be any type, as long as its the same type.

```python
extension Circle (Eq):
    def __eq__(self, other):
        return self.radius == other.radius
```

Protocols allows us to express the requirement for a (generic) type to implement some particular functionality.

Let's say we have a function that compares two objects.

```python
def comparator(a, b):
    print("Things:", a, b)
    print("Are they equal?", a == b)
```

We know that `a` and `b` must implement the `Eq` protocol and they must be of the same type. Let's see what the compiler constraint solver says:
```console
acton --sigs generic.act


== sigs: generic ================================

comparator : [A(Eq)] => (a: A, b: A) -> None
=================================================
```

The type has been inferred to a generic type `A` that must implement the `Eq` protocol, which is written as `A(Eq)`. We can write this explicitly:

```python
def comparator[A(Eq)](a: A, b: A):
    print("Things:", a, b)
    print("Are they equal?", a == b)
```
