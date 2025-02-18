# Integers

An integer is a number without a fractional component, like `42` or `-123`. Acton has multiple integer types, which are used to represent different ranges of numbers. Signed numbers, whose type start with `i` followed by the number of bits, can represent both positive and negative numbers, while unsigned numbers, starting with `u` can only represent positive numbers.

<table>
    <tr>
        <th>Type</th>
        <th>Min</th>
        <th>Max</th>
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
        <td><pre>i64</pre></td>
        <td>-9223372036854775808</td>
        <td>9223372036854775807</td>
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
        <td>arbitrary</td>
        <td>arbitrary</td>
    </tr>
</table>

`int` provides arbitrary precision, meaning it can represent any integer value, regardless of its size. This is useful when you need to work with very large numbers, but it comes with a significant performance cost compared to the fixed size integer. If you don't need arbitrary precision, it's better to use a fixed-size integer type.

You can convert between integer types by calling the instructor for a particular type, like `i16(42)`. This is safe when converting to a larger integer type. When converting to a smaller integer type, the value must be within the range of the smaller type, otherwise, an error will be thrown. Ensure that your values are small enough to fit in the target type. For example, you can convert an `i32` to an `i16` if the value is between `-32768` and `32767`.

<table class="side-by-side-code">
<tr>
<td>

Assign the value `123456789` to `a` of type `u16`.
</td>
<td>

```python
a: u32 = 123456789
```
</td>
</tr>
<tr>
<td>

Convert to an `i64` by calling the `i64()` constructor.
</td>
<td>

```python
b = i64(a)
```
</td>
</tr>
<tr>
<td>

Ensure the value is within the range of a `u16` through an `if` statement or default to `42`.
</td>
<td>

```python
c = u16(a) if a < 65536 else u16(42)
```
</td>
</tr>
<tr>
<td>

Attempt to convert to a `u16`, which will fail because `123456789` is too large. A `ValueError` will be thrown.
</td>
<td>

```python
c = u16(a)
```
</td>
</tr>
<tr>
<td>

Convert to a `u16` by first using modulo to ensure the value is within the range of a `u16` and discarding the rest.
</td>
<td>

```python
c = u16(a % (2 ** 16))
```
</td>
</tr>
</table>
