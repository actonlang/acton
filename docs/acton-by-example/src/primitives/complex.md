# Complex Numbers

Complex numbers in Acton provide support for mathematical operations involving both real and imaginary components. Complex numbers implement the `Number` protocol and include operations for arithmetic, comparison, and hashing. Complex also implement the `Div[Eq]`, `Eq` and `Hashable` protocols.

## Construction

Complex numbers can be created using the `from_real_imag` constructor method:

```python
# Create a complex number with real part 3.0 and imaginary part 4.0
c = complex.from_real_imag(3.0, 4.0)
```

## Properties

Complex numbers have two main properties accessible through methods:

- `real()`: Returns the real part as a float
- `imag()`: Returns the imaginary part as a float

```python
c = complex.from_real_imag(3.0, 4.0)
r = c.real()  # 3.0
i = c.imag()  # 4.0
```

## Arithmetic Operations

Complex numbers support standard arithmetic operations:

### Addition and Subtraction
```python
a = complex.from_real_imag(1.0, 2.0)
b = complex.from_real_imag(3.0, 4.0)
sum = a + b    # 4.0 + 6.0i
diff = b - a   # 2.0 + 2.0i
```

### Multiplication
Complex multiplication follows the rule (a + bi)(c + di) = (ac - bd) + (ad + bc)i
```python
# (1 + 2i)(3 + 4i) = (1×3 - 2×4) + (1×4 + 2×3)i = -5 + 10i
prod = a * b   # -5.0 + 10.0i
```

### Division
Complex division is performed by multiplying both numerator and denominator by the complex conjugate of the denominator:
```python
# (1 + 2i)/(1 + i) = (1 + 2i)(1 - i)/(1 + i)(1 - i) = (3 + i)/2
quotient = a / b
```

### Power Operation
```python
c = complex.from_real_imag(1.0, 1.0)
squared = c ** 2  # 0.0 + 2.0i
```

## Special Operations

### Complex Conjugate
The complex conjugate of a + bi is a - bi:
```python
c = complex.from_real_imag(1.0, 2.0)
conj = c.conjugate()  # 1.0 - 2.0i
```

### Absolute Value (Magnitude)
The absolute value or magnitude of a complex number is the square root of (a² + b²):
```python
c = complex.from_real_imag(3.0, 4.0)
magnitude = abs(c)  # 5.0
```

## Comparison and Hashing

Complex numbers can be compared for equality and can be used as dictionary keys:

```python
a = complex.from_real_imag(1.0, 2.0)
b = complex.from_real_imag(1.0, 2.0)
c = complex.from_real_imag(2.0, 1.0)

a == b  # True
a != c  # True

# Can be used as dictionary keys
d = {a: "value"}
```

## Edge Cases and Limitations

### Division by Zero
Attempting to divide by zero raises a `ZeroDivisionError`:
```python
zero = complex.from_real_imag(0.0, 0.0)
# a / zero  # Raises ZeroDivisionError
```

### Numerical Limits
Complex numbers use floating-point arithmetic and are subject to the same limitations:
- Very large numbers may result in infinity
- Very small numbers may underflow to zero
- Floating-point arithmetic may introduce small rounding errors

## Protocols

Complex numbers implement multiple protocols that define their behavior:

### Number Protocol
The `Number` protocol provides:
- Basic arithmetic operations (+, -, *)
- Power operation (**)
- Negation (-x)
- Properties for real and imaginary parts
- Absolute value (magnitude)
- Complex conjugate

### Div[complex] Protocol
The `Div[complex]` protocol adds:
- Division operation (/)
- In-place division operation (/=)

### Eq Protocol
The `Eq` protocol provides:
- Equality comparison (==)
- Inequality comparison (!=)

### Hashable Protocol 
The `Hashable` protocol (which extends `Eq`) enables:
- Hash computation via `__hash__`
- Use as dictionary keys or set elements

## Best Practices

1. Use appropriate tolerance when comparing results of complex arithmetic due to floating-point rounding:
```python
if abs(result.real() - expected_real) < 1e-10 and abs(result.imag() - expected_imag) < 1e-10:
    # Numbers are equal within tolerance
```

2. Handle potential exceptions when performing division:
```python
try:
    result = a / b
except ZeroDivisionError:
    # Handle division by zero
```

3. Consider numerical stability when working with very large or very small numbers:
```python
# Check for overflow/underflow
if result.real() == float('inf') or result.imag() == float('inf'):
    # Handle overflow
```
