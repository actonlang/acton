# Generics

Generics let you write code that works for many types without throwing
away type safety. They are how you describe a pattern once and reuse it
for every concrete type that fits the pattern.

## A simple generic function

```python
def first[A](items: list[A]) -> A:
    return items[0]

actor main(env):
    print(first([1, 2, 3]))
    print(first(["a", "b", "c"]))
    env.exit(0)
```

`[A]` introduces a type variable named `A`. In this function:

- `items` is a `list[A]`
- the return value is also `A`
- calling `first` on a `list[int]` returns an `int`
- calling it on a `list[str]` returns a `str`
- the compiler checks each call with the concrete type it sees there

<div class="beginner-content">
<p>If the brackets feel abstract at first, read them as "for any type
named A". Each call picks a concrete type for <code>A</code>, so
<code>first([1, 2, 3])</code> uses <code>int</code> while
<code>first(["a"])</code> uses <code>str</code>. That is how one
definition stays reusable without giving up compile-time checking.</p>
</div>

## Constrained generics

Sometimes a generic function needs more than "any type". It may require
that the type supports some operation or protocol.

```python
def bigger[A(Ord)](a: A, b: A) -> A:
    if a > b:
        return a
    return b
```

Here, `A(Ord)` means `A` must implement the `Ord` protocol so the
function can compare `a` and `b`. The constraint is part of the type
information, not an implementation detail. Without it, the compiler
would not know that `>` is valid for `A`.

<div class="advanced-content">
<p>Acton can often infer generic parameters and protocol constraints for
you. Using <code>--sigs</code> is a good way to see what the compiler
understood before you decide whether to write the generic signature
explicitly. That matters when a helper starts being reused widely,
because the inferred constraints determine both flexibility and dispatch
behavior.</p>
</div>

## Generics on classes

Built-in collection types use the same syntax.

```python
class list[A] (object):
    ...
```

That means a `list[int]` and a `list[str]` have the same generic shape
but different element types.

The same idea applies to your own classes and records. If a container or
wrapper stores values without caring which concrete type they are, make
that type parameter explicit.

## When to add constraints

Add a constraint when a type parameter must support a particular
operation:

- comparison, as in `Ord`
- equality or hashing, if the code depends on it
- a protocol, if the function calls methods from that protocol

Do not add a constraint just because it looks formal. The compiler only
needs the bounds that the body actually uses.
