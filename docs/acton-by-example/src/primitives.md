# Variable data types

Acton supports a plethora of primitive data types.

- `int` integers, like `1`, `2`, `123512`, `-6542` or `1267650600228229401496703205376`
  - `int` is arbitrary precision and can grow beyond machine word sizes
  - `i16` is a fixed size signed 16 bit integer
  - `i32` is a fixed size signed 32 bit integer
  - `i64` is a fixed size signed 64 bit integer
  - `u16` is a fixed size unsigned 16 bit integer
  - `u32` is a fixed size unsigned 32 bit integer
  - `u64` is a fixed size unsigned 64 bit integer
- `float` 64 bit float, like `1.3` or `-382.31`
- `bool` boolean, like `True` or `False`
- `str` strings, like `foo`
  - strings support Unicode characters
- [lists](primitives/lists.md) like `[1, 2, 3]` or `["foo", "bar"]`
- [dictionaries](primitives/dicts.md) like `{"foo": 1, "bar": 3}`
- [tuples](primitives/tuples.md) like `(1, "foo")`
- [sets](primitives/sets.md) like `{"foo", "bar"}`

In Acton, mutable state can only be held by actors. Global definitions in modules are constant. Assigning to the same name in an actor will shadow the global variable.

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

    a = u16(1234)
    print("u16:", a)

    env.exit(0)
```

Output:
```sh
local foo, shadowing the global foo: 4
global foo: 3
u16: 1234
```
