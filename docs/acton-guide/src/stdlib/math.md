# math

The `math` module provides the constant `pi` and common floating-point
functions.

Source:
```python
import math

actor main(env):
    angle = math.pi / 4.0

    print(math.sin(angle))
    print(math.sqrt(9.0))

    env.exit(0)
```

<div class="beginner-content">
<p>Import the module first with <code>import math</code>, then call its
functions as <code>math.sqrt(...)</code>, <code>math.sin(...)</code>,
and so on.</p>
</div>

The module currently exposes:

- `pi`
- `sqrt`, `exp`, `log`
- `sin`, `cos`, `tan`
- `asin`, `acos`, `atan`
- `sinh`, `cosh`, `tanh`
- `asinh`, `acosh`, `atanh`
- `ldexp(x, exp)`, which returns `x * 2**exp`

All functions return `float`. `ldexp` also takes an `int` exponent.
