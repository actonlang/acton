# Variables, constants, and scope

Names let you store values and use them again later.

<div class="beginner-content">
<p>Programmers often use <em>variable</em> to mean "a named place where a
value is stored". In Acton, the more precise question is whether that
name is a <em>constant</em> binding or a <em>mutable</em> one.</p>

<p>A constant binding keeps the same value for its lifetime. A mutable
binding is one whose value may be updated later. In everyday English,
<em>mutable</em> just means "able to change".</p>
</div>

Where a name is defined determines where it can be used. This is called
its *scope*.

<div class="beginner-content">
<p>Scope means "the part of the program where a name is visible". A name
defined inside a function or method is only available there.
Module-level names live across the module. Actor bodies have their own
rules, which are covered below.</p>
</div>

```python
greeting = "hello"    # module-level constant

def show_local():
    greeting = "hi"   # local name that shadows the module-level greeting
    print("local greeting:", greeting)

def show_global():
    print("global greeting:", greeting)

actor main(env):
    show_local()
    show_global()

    env.exit(0)
```

In this example:

- The module-level `greeting` is a constant.
- The `greeting` inside `show_local` is a different local name.

## Module-level names

Names defined at module level are constants. They are useful for helper
functions, reusable values, and definitions that the rest of the module
shares.

```python
port = 9000

def address():
    return "127.0.0.1:" + str(port)
```

<div class="beginner-content">
<p>Here <code>port</code> is a constant: after it is defined, you use it,
but you do not update it. That is a good default for names that describe
configuration, helper values, and definitions shared across a module.</p>
</div>

## Local names

Names defined inside a function or method are local to that body.

```python
def greet(name):
    message = "Hello " + name
    print(message)
```

Here, `message` only exists inside `greet`.

## Names in actors

Acton treats names at the top level of an actor body differently from
names inside a function or method.

```python
actor Counter():
    var remaining = 3
    label = "counter"
    _unit = "items"

    def tick():
        print(label, remaining, _unit)
        remaining -= 1
```

In this example:

- `remaining` is private mutable actor state.
- `label` is a public constant attribute.
- `_unit` is private to the actor because its name starts with `_`.

<div class="beginner-content">
<p>Read <code>var</code> as "this actor-local name will change over
time". If you plan to update a value in actor code, make that explicit
up front.</p>

<p>A plain name at the top level of an actor body is not an ordinary
local variable. It becomes a constant actor attribute instead. If the
name starts with <code>_</code>, it stays private to the actor. Without
the leading underscore, other actors can read that constant through an
actor reference.</p>
</div>

<div class="advanced-content">
<p>Acton keeps module-level bindings constant and pushes mutable shared
state into actors. That changes how you structure larger programs:
refactoring stateful logic usually means introducing an actor boundary,
not another mutable top-level name.</p>
</div>

## Shadowing

Shadowing means introducing a new local name with the same spelling as
an existing one. The outer name still exists, but the inner one is the
one used in that scope.

```python
name = "Acton"

def show():
    name = "local"
    print(name)

show()
print(name)
```

Here, `show` introduces a local `name` that shadows the module-level
`name`. Inside `show`, `name` means `"local"`. Outside `show`, it still
means `"Acton"`.

Shadowing is sometimes useful, but overusing it makes code harder to
read. Prefer distinct names when the two values mean different things.
