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
