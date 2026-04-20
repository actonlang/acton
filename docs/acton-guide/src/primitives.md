# Built-in types and literals

Every value in Acton has a type.

Many values can be written directly in source code. These are called
*literals*.

<div class="beginner-content">
<p><code>42</code>, <code>True</code>, and <code>"hello"</code> are all
literals: the value is written directly in the program instead of being
computed somewhere else.</p>
</div>

## Common built-in types

| Type | Example | Notes |
| --- | --- | --- |
| `int` | `42` | 64-bit signed integer |
| `bigint` | `123456789012345678901234567890` | arbitrary-precision integer |
| `i8`, `i16`, `i32`, `u1`, `u8`, `u16`, `u32`, `u64` | `u16(42)` | explicitly sized integers |
| `float` | `3.14` | 64-bit floating-point number |
| `complex` | `complex.from_real_imag(1.0, 2.0)` | complex number |
| `bool` | `True` | `True` or `False` |
| `str` | `"hello"` | Unicode text |
| tuple | `(1, "two")` | fixed-size group of values |

```python
actor main(env):
    whole = 42
    huge = bigint(123456789012345678901234567890)
    ratio = 3.5
    truth = True
    name = "Acton"
    point = (x=3, y=4)
    z = complex.from_real_imag(2.0, 3.0)

    print(whole, huge, ratio, truth, name, point, z)
    env.exit(0)
```

<div class="advanced-content">
<p>Integer literals are not all the same internally. Small whole-number
literals usually fit in <code>int</code>, while very large literals may
need <code>u64</code> or <code>bigint</code>. If the exact type matters,
write it explicitly.</p>
</div>

## Choosing a type

Use:

- `int` for ordinary whole numbers
- `bigint` when whole numbers may grow beyond the normal `int` range
- `float` for fractional values
- `bool` for yes/no conditions
- `str` for text
- tuples for small fixed-size groups of values

Reach for fixed-size integers when width or sign matters, and for
`complex` when you need real and imaginary parts together.

Lists, dictionaries, and sets are covered in [Collections](collections.md).

## More detail

- [Integers](primitives/integers.md)
- [Floating-point numbers](primitives/float.md)
- [Complex numbers](primitives/complex.md)
- [Tuples](primitives/tuples.md)
