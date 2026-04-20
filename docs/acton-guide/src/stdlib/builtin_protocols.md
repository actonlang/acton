# Built-in protocols

Built-in protocols are defined in `__builtin__`.

They show up in generic constraints, operator behavior, and the APIs of
built-in collections. This section is reference material for the
protocols that come with the language.

<div class="advanced-content">
<p>Built-in protocol constraints are part of the type system, not just
documentation. They affect which operators are available, which generic
functions typecheck, and how protocol methods are resolved.</p>
</div>

The built-in protocols are grouped here as:

- [General protocols](builtin_protocols/general.md) for iteration,
  comparison, operators, and hashing
- [Numeric protocols](builtin_protocols/numeric.md) for number-like
  types
- [Collection protocols](builtin_protocols/collection.md) for
  collection-shaped APIs

<div class="beginner-content">
<p>Read a header such as <code>protocol Ord (Eq)</code> as "Ord
extends Eq". A type that implements <code>Ord</code> must also satisfy
the requirements of <code>Eq</code>.</p>
</div>

For how protocols work as a language feature, read
[Protocols](../protocols/intro.md).
