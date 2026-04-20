# Complex numbers

Complex numbers combine a real part and an imaginary part.

Create them with `complex.from_real_imag(real, imag)`.

<div class="beginner-content">
<p>If you have not used complex numbers before, think of them as two
floating-point values with arithmetic rules built in. Most programs do
not need them, but they are useful in math-heavy domains.</p>
</div>

```python
actor main(env):
    a = complex.from_real_imag(1.0, 2.0)
    b = complex.from_real_imag(3.0, 4.0)

    print("sum:", a + b)
    print("product:", a * b)
    print("real part:", a.real())
    print("imag part:", a.imag())
    print("conjugate:", a.conjugate())
    print("magnitude:", abs(a))

    env.exit(0)
```

## Arithmetic

Complex numbers support the usual arithmetic operations:

- `+` and `-`
- `*`
- `/`
- `**`

They also support equality testing with `==` and `!=`.

```python
a = complex.from_real_imag(1.0, 2.0)
b = complex.from_real_imag(1.0, 2.0)

print(a == b)
```

<div class="advanced-content">
<p>Complex numbers are part of Acton's numeric world and work with the
same floating-point realities as <code>float</code>: rounding and
precision limits still matter. Any algorithm that wants a notion of
ordering has to state it explicitly, such as by comparing magnitude,
rather than relying on the usual ordered-number intuition.</p>
</div>

## Errors

Division by zero raises `ZeroDivisionError`.

```python
zero = complex.from_real_imag(0.0, 0.0)
# a / zero
```
