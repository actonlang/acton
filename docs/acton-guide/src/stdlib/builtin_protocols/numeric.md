# Numeric protocols

These protocols describe the operations available on built-in numeric
types.

## Protocol hierarchy

- `Number (Times[Self], Minus)`: core numeric behavior such as `+`,
  `-`, `*`, `**`, unary `+` and `-`, `abs`, `real`, `imag`, and
  `conjugate`
- `Real (Number)`: float-style conversion and rounding operations such
  as `__float__`, `trunc`, `floor`, `ceil`, and `round`
- `RealFloat (Real)`: floating-point real numbers
- `Rational (Real)`: values with `numerator` and `denominator`
- `Integral (Rational, Logical)`: integer-style operations such as
  `//`, `%`, shifts, bit operations, and indexing

## Built-in implementations

- `int`, `bigint`, and the fixed-width signed and unsigned integer types
  implement `Integral`
- `float` implements `RealFloat`
- `complex` implements `Number`

Use the narrowest constraint that matches the operations you need:

<div class="beginner-content">
<p>Choose <code>Number</code> when you need ordinary arithmetic.
Choose <code>Integral</code> when you need integer-only operations such
as floor division, modulo, or bit shifts.</p>
</div>

```python
def square[A(Number)](x: A) -> A:
    return x * x

def bucket[A(Integral)](x: A, size: A) -> A:
    return x // size
```

<div class="advanced-content">
<p>Numeric constraints affect both operator availability and result
types. For example, <code>Div[A]</code> separates the type of the result
from the type of the operands, which is why integer division through
<code>/</code> can return a different type than floor division through
<code>//</code>.</p>
</div>
