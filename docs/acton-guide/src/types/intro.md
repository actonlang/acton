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

In a project, use `acton sig` when you want signatures by import path
instead of by source file path:

```sh
acton sig foo.bar
```

The target is resolved like an import path. Acton first looks for a
module named `foo.bar`. If that module does not exist, it treats the
target as the public name `bar` inside module `foo` and prints only that
signature.

This is useful when a type error mentions an imported module or name,
especially one that comes from a dependency. `acton sig` uses the
project's normal build resolution, so it reads `Build.act`, honors
dependency overrides such as `--dep name=path`, fetches missing
dependencies, and compiles the module interfaces it needs before
printing the signatures. It does not perform the final executable build.

For example:

```sh
acton sig collections.list
acton sig mylib.parser.parse
```

<div class="advanced-content">
<p>Acton's inferred signatures include more than argument and return
types. You will see generic binders, protocol constraints, optional
types, tuple rows, and effect markers. Reading a signature means
reading both the data shape and the callable behavior. The compiler's
output is often the clearest summary of what an API actually promises,
so <code>acton sig</code> and <code>--sigs</code> are useful first
steps before deciding whether to make that promise explicit.</p>
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
- [Troubleshooting type errors](troubleshooting.md) when you want to
  inspect imported signatures and compare them with an error
- [Optionals](optionals.md) when you want the deeper type-system view of
  values that may be absent
