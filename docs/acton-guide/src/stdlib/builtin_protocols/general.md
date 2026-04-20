# General protocols

These protocols cover iteration, comparison, operators, and hashing.

## Iteration

- `Iterable[A]`: values that can produce an iterator with `__iter__`
  and can therefore be used in `for` loops and other iteration-based
  APIs

## Identity and comparison

- `Identity`: identity comparison with `is` and `is not`
- `Eq`: equality comparison with `==` and `!=`
- `Ord (Eq)`: ordering with `<`, `<=`, `>`, and `>=`

<div class="beginner-content">
<p>If you see a constraint such as <code>A(Ord)</code> in a type
signature, it means values of type <code>A</code> support ordering.</p>
</div>

## Operator families

- `Logical`: `&`, `|`, `^`, and their in-place forms
- `Plus`: `+`, `+=`, and `__zero__`
- `Minus`: `-` and `-=`
- `Times[A] (Plus)`: `*` and `*=`, with right-hand operand type `A`
- `Div[A]`: `/` and `/=`, with result type `A`

## Hashing

- `Hashable (Eq)`: values that can feed data into a `hasher`; required
  for dictionary keys and set elements

<div class="beginner-content">
<p>If you see a constraint such as <code>A(Hashable)</code>, values of
type <code>A</code> can be used where hashing is required.</p>
</div>

See [Hashable](hashable.md) for the full protocol reference and an
implementation example.
