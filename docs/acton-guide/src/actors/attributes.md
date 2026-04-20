# Actor Attributes & Constants

Actors usually keep their state in top-level attributes. Use `var` for
mutable actor-local state and a plain binding for a constant.

<div class="advanced-content">
<p>The split between <code>var</code> and plain top-level bindings is
one of the key boundaries around state. Mutable actor data stays
private to the actor, while constant attributes can be read through an
actor reference without shared mutable memory.</p>
</div>

Source:
```python
actor Act():
    var something = 40  # private actor variable attribute
    fixed = 1234        # public constant
    
    def hello():
        # Local state is accessed directly inside the actor.
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

Without `var`, an actor attribute is a constant. Constants are safe to
share with other actors because they do not expose mutable state.
