# Working with types

Acton is statically typed. The compiler checks types when your program
is compiled, but it can infer many of them for you. In practice, that
means you usually write the shape of the code and let the compiler fill
in the rest.

This section explains how to work with that type system deliberately:
how to read inferred signatures, when to write explicit annotations, and
how generics, constraints, and effects show up in real code.

Acton does not have Rust-style lifetimes. Mutable state lives inside
actors, access to the outside world is passed explicitly as capabilities,
and ordinary object lifetime is handled by the runtime. See
[Actors](../actors.md), [Lifetime](../actors/lifetime.md), and
[Environment and capabilities](../language/environment_capabilities.md)
for the Acton model.

Optional types are introduced earlier under
[Missing values and failures](../language/missing_values_failures.md)
because they come up quickly in everyday code, but they are also part of
the broader type story.

```python
def describe(value):
    if value > 0:
        return "positive"
    return "zero or negative"

actor main(env):
    n = 3
    text = describe(n)
    print(text)
    env.exit(0)
```

In this example, Acton can infer the types without any annotations.
`n` is an `int`, `describe` returns `str`, and `text` is also a `str`.
That is often the most pleasant way to write small programs: start with
plain code, then add annotations only where they improve clarity.

<div class="beginner-content">
<p>You do not need to annotate every value. A small helper, a local
temporary, or a short private function can often stay inferred without
hurting readability. Add types where they explain intent, not as
decorations.</p>
</div>

## Reading inferred signatures

Use `--sigs` when you want the compiler to show the types it inferred.

```sh
acton types.act --sigs
```

<div class="advanced-content">
<p>Acton's inferred signatures include more than argument and return
types. You will see generic binders, protocol constraints, optional
types, tuple rows, and effect markers. Reading a signature means
reading both the data shape and the callable behavior. The compiler's
output is often the clearest summary of what an API actually promises,
so <code>--sigs</code> is a useful first step before deciding whether to
make that promise explicit.</p>
</div>

This is especially useful when:

- you want to understand what type a helper function ended up with
- you are learning how generic constraints are written
- you want to turn an inferred signature into an explicit API
- you need to see effect markers or optionality in a callable type

## What this section covers

- [Explicit types](explicit.md) when you want to state an API, narrow a
  local value, or make inference easier to follow
- [Generics](generics.md) when one definition should work for many
  concrete types without losing safety
- [Effects (`pure`, `mut`, `proc`, `action`)](effects.md) when you want
  to treat side effects as part of the type information
- [Optionals](optionals.md) when you want the deeper type-system view of
  values that may be absent
