# Type aliases

A type alias gives an existing type another name. Use one to explain
what a value represents or to avoid repeating a long type expression.

Declare an alias at module level with `type`, a name, and the type
expression it stands for:

```python
type Point = (float, float)

def translate(point: Point, offset: Point) -> Point:
    return (point.0 + offset.0, point.1 + offset.1)

actor main(env):
    start: Point = (3.0, 4.0)
    print(translate(start, (1.0, 2.0)))
    env.exit(0)
```

Here, `Point` means `(float, float)`. The tuple literal creates the
value, and the alias makes the parameter, return, and local annotations
easier to read. The tuple still has its usual `.0` and `.1` fields.

<div class="beginner-content">
<p>An alias is shorthand for its right-hand side. It does not define a
new class or change the value's representation. You can pass an ordinary
pair of floats wherever <code>Point</code> is expected.</p>
</div>

Aliases do not make otherwise identical types distinct. If both
`UserId` and `OrderId` alias `int`, the compiler treats both as `int`.
Use a [class](../classes/intro.md) when you need a separate type with its
own methods or initialization rules.

## Parameterized aliases

An alias can take type parameters, using the same brackets as
[generics](generics.md):

```python
type Pair[V] = (V, V)

def swap[V](pair: Pair[V]) -> Pair[V]:
    return (pair.1, pair.0)

actor main(env):
    names: Pair[str] = ("Ada", "Grace")
    numbers: Pair[int] = (1, 2)
    print(swap(names))
    print(swap(numbers))
    env.exit(0)
```

`Pair[str]` expands to `(str, str)`, and `Pair[int]` expands to
`(int, int)`. Each use substitutes its type argument for `V` in the
alias's right-hand side.

## Constraints on parameters

Include protocol constraints when the underlying type requires them.
For example, dictionary keys must support `Hashable`:

```python
type Point = (float, float)
type Plot[K(Hashable)] = dict[K, Point]

def lookup[K(Hashable)](landmarks: Plot[K], key: K) -> Point:
    return landmarks[key]

actor main(env):
    landmarks = {"origin": (0.0, 0.0), "home": (3.0, 4.0)}
    print(lookup(landmarks, "home"))
    env.exit(0)
```

`Plot[str]` means `dict[str, Point]`, which in turn means
`dict[str, (float, float)]`. The `K(Hashable)` constraint ensures that
each key type satisfies the dictionary's requirement. An alias must
declare the constraints needed by its definition; it cannot hide them.

## Sharing aliases between modules

Import an alias through its module just like other public names.
For example, put this in `src/geometry.act`:

```python
type Point = (float, float)
```

Then use it from another module in the same project:

```python
import geometry

actor main(env):
    origin: geometry.Point = (0.0, 0.0)
    print(origin)
    env.exit(0)
```

You can also use `from geometry import Point` and write `Point` directly
in annotations. See [Modules](../modules.md) for the import forms and
project layout.

## Recursive definitions

An alias can refer to a class or actor that refers back to the alias.
For example, a tree can have a list of child trees:

```python
type Branches = list[Tree]

class Tree():
    def __init__(self, children: Branches):
        self.children = children

actor main(env):
    root = Tree([Tree([]), Tree([])])
    print(len(root.children))
    env.exit(0)
```

`Branches` expands to `list[Tree]`. The reference to `Tree` is valid
even though the class is declared after the alias.

<div class="advanced-content">
<p>A recursive definition must pass through at least one class or
actor. A cycle made entirely of aliases, such as
<code>type Loop = list[Loop]</code>, is rejected: expanding the alias
would never reach a complete type definition.</p>
</div>
