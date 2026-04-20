# Collection protocols

These protocols describe the shared APIs behind built-in collection
types.

<div class="beginner-content">
<p>These protocols describe behavior, not concrete storage. They let the
language talk about "something indexable" or "something iterable"
without naming one specific collection type.</p>
</div>

## Core collection protocols

- `Indexed[A(Eq), B]`: lookup, assignment, and deletion with `[]`
- `Sliceable[A] (Indexed[int, A])`: slicing with `start:stop:step`
- `Collection[A] (Iterable[A])`: construction from an iterable and
  `len`
- `Container[A(Eq)] (Collection[A])`: membership with `in` and `not in`

## Specialized collection protocols

- `Sequence[A] (Sliceable[A], Collection[A], Times[int])`: ordered,
  sliceable collections with repetition and mutating operations such as
  `append`, `insert`, and `reverse`
- `Mapping[A(Eq), B] (Container[A], Indexed[A, B])`: key/value
  collections with `get`, `pop`, `keys`, `values`, and `items`
- `Set[A(Eq)] (Container[A], Ord, Logical, Minus)`: set operations,
  ordering, membership, and mutating updates such as `add` and
  `discard`

Built-in types make use of these protocols:

- `list[A]` implements `Sequence[A]`
- `dict[A(Hashable), B]` implements `Mapping[A, B]`
- `set[A(Hashable)]` implements `Set[A]`

<div class="advanced-content">
<p>The protocol surface is sometimes looser than a concrete type's full
requirements. For example, <code>Mapping[A, B]</code> talks about
key/value behavior in general, while <code>dict</code> specifically
requires hashable keys.</p>
</div>
