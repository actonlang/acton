# Data Types

Every value in Acton is of a certain *type*. The *type* specifies what kind of data is being specified and how to work with that data. Acton has a number of built-in data types.

- [integers](primitives/integers.md), like `1`, `2`, `123512`, `-6542` or `1267650600228229401496703205376`
  - `int` is arbitrary precision and can grow beyond machine word sizes
  - `i16`, `i32`, `i64`, `u16`, `u32`, `u64` are fixed size integers
- [float](primitives/float.md) 64 bit float, like `1.3` or `-382.31`
- [complex](primitives/complex.md) complex numbers like `1+2j`
- `bool` boolean, like `True` or `False`
- `str` strings, like `foo`
  - strings support Unicode characters
- [tuples](primitives/tuples.md) for structuring multiple values, like `(1, "foo")` or `(a="bar", b=1337)`

In Acton, mutable state can only be held by actors. Global definitions in modules are constant. Assigning to the same name in an actor will shadow the global variable.

<table class="side-by-side-code">
<tr>
<td>
module level constant
</td>
<td>

```python
foo = 3
```
</td>
</tr>
<tr>
<td>
this will print the global foo
</td>
<td>

```python
def printfoo():
    print("global foo:", foo)
```

</td>
</tr>
<tr>
<td>
set a local variable with the name foo, shadowing the global constant foo
</td>
<td>

```json
actor main(env):
    foo = 4
```

</td>
</tr>
<tr>
<td>
print local foo, then call printfoo()
</td>
<td>

```python
    print("local foo:", foo)
    printfoo()

    a = u16(1234)
    print("u16:", a)

    env.exit(0)
```

</td>
</tr>
</table>

Output:
```sh
local foo, shadowing the global foo: 4
global foo: 3
u16: 1234
```

<!--
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
-->
