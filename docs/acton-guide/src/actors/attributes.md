# Actor Attributes & Constants

Actors typically contain some private state. We define variable attributes at the top level in the actor using the `var` keyword and can then access them from any method within the local actor. Note how `self.` is not needed. Private variables are not visible from other actors.

Source:
```python
actor Act():
    var something = 40  # private actor variable attribute
    fixed = 1234        # public constant
    
    def hello():
        # We can access local actor variable attributes directly, no need for 
        # self.something or similar
        something += 2
        print("Hello, I'm Act & value of 'something' is: " + str(something))

actor main(env):
    actor1 = Act()
    await async actor1.hello()
    print("Externally visible constant: ", actor1.fixed)
    # This would give an error, try uncommenting it
    # print(actor1.something)

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

Without the `var` keyword, an actor attribute is a constant. As constants are not mutable, it is safe to make it visible to other actors and it can be accessed like an attribute on an object.
