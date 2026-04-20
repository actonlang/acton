# Classes and objects

Classes let you name a concept, keep its data together, and attach the
behavior that belongs with that data. If a tuple or named tuple starts
to feel too anonymous, a class is usually the next step.

Use a class when a value should have:

- a clear name instead of a bundle of positions
- related pieces of data that travel together
- methods that operate on that data
- construction rules or invariants that matter to callers

```python
class Circle(object):
    def __init__(self, radius):
        self.radius = radius

    def diameter(self):
        return self.radius * 2

actor main(env):
    circle = Circle(3.14)
    print(circle.diameter())
    env.exit(0)
```

## What a class gives you

In the example above:

- `radius` is an attribute stored on each `Circle`
- `diameter()` is a method that uses that attribute
- `self` refers to the object the method is running on

<div class="beginner-content">
<p>If a tuple starts to feel anonymous or unclear, that is usually a
sign that the value wants to become a class. Inside methods,
<code>self</code> is the current object, so <code>self.name</code> means
"this object's name".</p>
</div>

Attributes are often introduced by assignments in `__init__`, but you
can also declare them explicitly in the class body when the shape should
be obvious up front.

```python
class Person(object):
    name: str

    def __init__(self, name, age):
        self.name = name
        self.age = age
```
