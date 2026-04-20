# Floating-point numbers

`float` is Acton's 64-bit floating-point type.

Use it for values with a fractional part, such as measurements, ratios,
and scientific calculations.

<div class="beginner-content">
<p>A decimal point usually means a <code>float</code> literal. Use
floats for measurements and ratios, and do not be surprised by small
rounding artifacts in the last digits.</p>
</div>

```python
actor main(env):
    distance = 12.5
    time = 4.0
    speed = distance / time

    print("speed:", speed)
    print("rounded:", round(speed, 2))
    print("formatted: %.2f" % speed)

    env.exit(0)
```

Floating-point arithmetic is approximate, not exact.

```python
a = 0.1
b = 0.2
print(a + b)
```

<div class="advanced-content">
<p>Floats trade exactness for range and speed. They are usually the
right tool for physical measurements and approximate calculations, but
not for values where exact decimal behavior is required. Equality on
computed floats is often brittle, so treat exact comparison with care
once values have gone through prior arithmetic.</p>
</div>
