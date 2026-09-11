# Integers

Integers are whole numbers such as `0`, `42`, and `-7`.

Acton has three groups of integer types:

- `int` for the normal 64-bit signed integer type
- `bigint` for integers that must grow beyond the `int` range
- explicitly sized signed and unsigned integers such as `i32` and `u16`

`bigint` lets values grow arbitrarily large ensuring correct program behavior
when you are uncertain about the exact size needed. However, as `bigint` is
significantly slower than the bounded integer types, do not default to `bigint`
out of convenience.  For the vast majority of normal use cases, `int` is large
enough and considerably faster. Use exact-width integers when you specifically
need their bit width.

<div class="advanced-content">
<p>Bounded integer types can often be compiled in an unboxed form, which avoids
boxing overhead and can make arithmetic much faster, several orders of
magnitude, than <code>bigint</code> in tight code. That is another reason to
prefer <code>int</code> or an exact-width integer when the bounded range is the
right fit, and reserve <code>bigint</code> for values that truly need arbitrary
precision.</p>
</div>

<div class="beginner-content">
<p>If you are not sure which integer type to use, start with
<code>int</code>. Move to <code>bigint</code> when values may get very
large, and use the exact-width types when you need to match a protocol,
file format, or external API.</p>
</div>

<table>
    <tr>
        <th>Type</th>
        <th>Min</th>
        <th>Max</th>
    </tr>
    <tr>
        <td><pre>i8</pre></td>
        <td>-128</td>
        <td>127</td>
    </tr>
    <tr>
        <td><pre>i16</pre></td>
        <td>-32768</td>
        <td>32767</td>
    </tr>
    <tr>
        <td><pre>i32</pre></td>
        <td>-2147483648</td>
        <td>2147483647</td>
    </tr>
    <tr>
        <td><pre>u1</pre></td>
        <td>0</td>
        <td>1</td>
    </tr>
    <tr>
        <td><pre>u8</pre></td>
        <td>0</td>
        <td>255</td>
    </tr>
    <tr>
        <td><pre>u16</pre></td>
        <td>0</td>
        <td>65535</td>
    </tr>
    <tr>
        <td><pre>u32</pre></td>
        <td>0</td>
        <td>4294967295</td>
    </tr>
    <tr>
        <td><pre>u64</pre></td>
        <td>0</td>
        <td>18446744073709551615</td>
    </tr>
    <tr>
        <td><pre>int</pre></td>
        <td>-9223372036854775808</td>
        <td>9223372036854775807</td>
    </tr>
    <tr>
        <td><pre>bigint</pre></td>
        <td>arbitrary</td>
        <td>arbitrary</td>
    </tr>
</table>

## Basic use

```python
actor main(env):
    count = 42
    port = u16(5000)
    huge = bigint(123456789012345678901234567890)

    print("count:", count)
    print("port:", port)
    print("huge:", huge)
    print("widened:", int(port))

    env.exit(0)
```

Use `int` for everyday counting and arithmetic. Use `bigint` when a
value may exceed the normal machine-sized range. Use exact-width types
when the bit pattern matters.

## Integer division and remainder

`a // b` computes an integer quotient and `a % b` computes the remainder.
The rounding rule depends on the integer type:

- `int`, `i32`, `i16`, and `i8` round the quotient toward zero. A nonzero
  remainder has the same sign as the dividend (`a`).
- `bigint` rounds the quotient toward negative infinity (floor division).
  A nonzero remainder has the same sign as the divisor (`b`).
- `u1`, `u8`, `u16`, `u32`, and `u64` round the quotient toward zero,
  which is also rounding down for non-negative operands. For a nonzero
  divisor, the remainder satisfies `0 <= a % b < b`. For example,
  `u32(8) // u32(3)` and `u32(8) % u32(3)` both give `2`.

For example:

| `a` | `b` | `int`: `a // b` | `int`: `a % b` | `bigint`: `a // b` | `bigint`: `a % b` |
| --- | --- | --- | --- | --- | --- |
| `8` | `3` | `2` | `2` | `2` | `2` |
| `-8` | `3` | `-2` | `-2` | `-3` | `1` |
| `8` | `-3` | `-2` | `2` | `-3` | `-1` |
| `-8` | `-3` | `2` | `-2` | `2` | `-2` |

For a nonzero divisor and a quotient that fits the type, both conventions
satisfy `a == (a // b) * b + (a % b)`. The magnitude of the remainder is
less than the magnitude of the divisor. `divmod(a, b)` returns the same
quotient and remainder as the tuple `(a // b, a % b)`.

Converting operands to `bigint` can therefore change the quotient and
remainder even when the operand values fit in `int`.

### Why the rounding rules differ

Bounded integers use truncation to match efficient hardware integer
division. For signed types, when the quotient `q = a // b` fits,
truncation also keeps the product `q * b` in range. With `i8` operands
`-128` and `3`, truncation gives `q = -42` and `q * b = -126`.
Rounding down would give `q = -43` and `q * b = -129`, which is outside
the `i8` range.

`bigint` has no fixed-width overflow limit. Its floor division gives
useful properties for modular arithmetic: for a positive divisor `b`,
the remainder is always in `0` through `b - 1`, even when the dividend
is negative. Numbers that differ by a multiple of `b` have the same
remainder. For example, with `bigint` operands, `-8`, `1`, and `4` all
leave remainder `1` when divided by `3`. This agrees with Euclidean
division for positive divisors; for negative divisors, `bigint` can
have a negative remainder.

## Converting integers

Convert by calling the target type as a constructor.

```python
int(42)
bigint(42)
u16(255)
```

Widening to a larger type is straightforward:

```python
small = u16(255)
widened = int(small)
```

Converting to a narrower type checks that the value fits:

```python
safe = u16(12345)
# u16(70000) would raise ValueError
```

<div class="advanced-content">
<p>Large integer literals are inferred by size. Values above the normal
<code>int</code> range may infer as <code>u64</code> or
<code>bigint</code>. When you care about the exact type, annotate it or
use an explicit constructor.</p>
</div>
