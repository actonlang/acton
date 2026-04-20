# Modeling data and interfaces

Once functions and collections stop expressing a problem clearly, you
usually need a more explicit model.

In Acton, that usually means moving through a simple progression:

- tuples for small anonymous values
- named tuples for small structured values with readable fields
- classes for values with methods or invariants
- protocols for shared behavior across unrelated types

<div class="beginner-content">
<p>Start with a tuple when the value is small and anonymous. Move to a
named tuple when the shape deserves field names but not behavior. Move
to a class when the data needs methods, a constructor, or rules that
should stay close to the value. Use a protocol when the real question
is whether several different types can be used in the same place.</p>
</div>

Use this section when you need to decide how a domain concept should be
represented, how an object becomes valid, and how different types can
share the same behavior without sharing the same class.

<div class="advanced-content">
<p>Classes and protocols solve different problems. Classes define the
shape and lifecycle of a value. Protocols define an observed interface
that multiple concrete types can satisfy. That choice affects API shape,
initialization, dispatch, and type inference, not just code
organization.</p>
</div>

- [Tuples](../primitives/tuples.md)
- [Classes and objects](../classes/intro.md)
- [Protocols](../protocols/intro.md)
