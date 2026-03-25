# Actor Attributes & Constants

For actor attributes, mutability and visibility are separate concerns. Actors can define three kinds of top-level attributes:

- `var foo = ...` defines a local actor variable attribute, that is, actor state. It is mutable and only visible inside the actor.
- `_foo = ...` defines a private constant. After initialization, it cannot be reassigned and is only visible inside the actor.
- `foo = ...` defines a public constant. After initialization, it cannot be reassigned and is visible to other actors.

All three can be accessed directly from methods on the same actor. Note how `self.` is not needed.

Note: constant actor attributes are constant after initialization, not necessarily during initialization. The top-level code in an actor runs when the actor instance is created, and during that phase the name may be rebound. The final binding is then the constant value seen by methods and, for public constants, by other actors.

Source:
```python
actor Act():
    var something = 40  # local actor variable attribute, i.e. actor state
    _step = 2           # private constant
    fixed = 0           # public constant
    fixed = 1234        # public constant redefinition
    
    def hello():
        # Local actor attributes are accessed directly, no self. needed
        something += _step
        print("Hello, I'm Act & value of 'something' is: " + str(something))

actor main(env):
    actor1 = Act()
    await async actor1.hello()
    print("Externally visible constant: ", actor1.fixed)
    # These would give errors, try uncommenting them
    # print(actor1.something)
    # print(actor1._step)

    env.exit(0)
```

Compile and run:
```sh
acton attrs.act
```

Output:
```sh
Hello, I'm Act & value of 'something' is: 42
Externally visible constant:  1234
```

The important difference is that `var foo` and `_foo` are both private to the actor, but only `var foo` is actor state and remains mutable after initialization. A name without `var` becomes constant once initialization has completed, and a leading underscore keeps that constant private. In other words, `_foo` is how you declare a constant value that belongs to the actor and is still hidden from other actors. Without the underscore, the constant is public and can be read from other actors.
