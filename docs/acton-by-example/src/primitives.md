# Variable data types

Acton supports a plethora of primitive data types.

- `int` integers, like `1`, `2`, `123512` or `-6542`
- `float` floats, like `1.3` or `-382.31`
- `bool` boolean, like `True` or `False`
- `str` strings, like `foo`
  - strings support unicode characters
- [lists](primitives/lists.md) like `[1, 2, 3]` or `["foo", "bar"]`
- [dictionaries](primitives/dicts.md) like `{"foo": 1, "bar": 3}`
- [tuples](primitives/tuples.md) like `(1, "foo")`
- [sets](primitives/sets.md) like `{"foo", "bar"}`

In Acton, mutable state can only be held by actors. You can define a global variable at the top level in your program, but it becomes constant. Assigning to the same name in an actor will overshadow the global variable.

Source:
```python
foo = 3    # this is a global constant and cannot be changed

def printfoo():
    print("global foo:", foo)  # this will print the global foo

actor main(env):
    # this sets a local variable with the name foo, shadowing the global constant foo
    foo = 4
    print("local foo, shadowing the global foo:", foo)

    printfoo()

    env.exit(0)
```

Output:
```sh
local foo, shadowing the global foo: 4
global foo: 3
```
