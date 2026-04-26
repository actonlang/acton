# Actor Attributes & Constants

Actor attributes are used to store the state of an actor.
The important distinctions are whether an attribute is mutable state or
a constant, and whether it is private or public.

An actor can define three kinds of top-level attributes:

- `var foo = ...` defines private mutable actor state
- `_foo = ...` defines a private constant
- `foo = ...` defines a public constant

<div class="advanced-content">
<p>The important boundary is not just "mutable or constant", but also
"private or public". <code>var</code> stays private because mutable
actor state must not be exposed directly across actor boundaries. Plain
constant attributes can be read from other actors because they do not
create shared mutable memory, and a leading <code>_</code> keeps such a
constant private to the actor.</p>

<p>Actor constants are observed as constants. The top-level code in the
actor body runs during actor creation, and a constant name may be
assigned more than once while that final value is being established.
Once initialization is complete, the final binding is the constant
value seen by methods and, for public constants, by other actors.</p>
</div>

For example:
```python
actor Act():
    var something = 40
    _step = 2
    fixed = 0
    fixed = 1234
    
    def hello():
        something += _step
        print("Hello, I'm Act & value of 'something' is: " + str(something))

actor main(env):
    actor1 = Act()
    await async actor1.hello()
    print("Externally visible constant: ", actor1.fixed)
    # print(actor1.something)
    # print(actor1._step)

    env.exit(0)
```

Here, `something` is private mutable state, `_step` is a private
constant, and `fixed` is a public constant.

The repeated assignment to `fixed` happens during actor creation. The
final binding, `1234`, is the constant value methods and other actors
observe after initialization has completed.

Inside the actor, all three kinds of attributes can be used directly
from methods without writing `self.`.

Without `var`, an actor attribute is a constant. If the name starts
with `_`, that constant stays private to the actor. Without the leading
underscore, other actors can read the constant through an actor
reference.

In this example, `main` can read `actor1.fixed`, but it cannot read
`actor1.something` or `actor1._step`. The first is private mutable
state, and the second is a private constant.
