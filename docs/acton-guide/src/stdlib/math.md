# std.math

The `std.math` module provides the constant `pi` and common floating-point
functions.

Source:
```python
import std.math

actor main(env):
    angle = std.math.pi / 4.0

    print(std.math.sin(angle))
    print(std.math.sqrt(9.0))

    env.exit(0)
```

<div class="beginner-content">
<p>Import the module first with <code>import math</code>, then call its
functions as <code>std.math.sqrt(...)</code>, <code>std.math.sin(...)</code>,
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
