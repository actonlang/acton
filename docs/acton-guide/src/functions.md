# Functions

Functions package reusable logic behind a name. Declare a function with
`def`.

Functions are values too, so you can pass behavior into another
function when the call site should choose the variation. See
[Higher order functions](functions/higher_order.md) for that pattern.

```python
def clamp(n, low, high):
    if n < low:
        return low
    if n > high:
        return high
    return n

actor main(env):
    print(clamp(3, 0, 5))
    print(clamp(-2, 0, 5))
    print(clamp(9, 0, 5))
    env.exit(0)
```

Function arguments are local names inside the function body.

<div class="advanced-content">
<p>Type inference makes local helpers cheap to write, but it does not
make their types unimportant. As soon as a function is used across a
module boundary, callers depend on whatever the compiler inferred for
its arguments, return value, effects, and generic constraints. Those
facts are part of the callable contract even when no explicit signature
appears in the source.</p>

<p>That has a practical consequence: changing a function from
<code>pure</code> to <code>proc</code>, tightening a generic bound, or
making a result optional is not just an implementation tweak. It can
force changes at call sites. That is why public functions deserve a
higher bar than local helpers. It is often fine to let inference handle
small local code, but API-facing functions should be read as typed
interfaces whether the signature is written down or not.</p>
</div>

## Arguments

Arguments are available only inside the function where they are defined.
That makes them useful for small, self-contained pieces of logic.

<div class="beginner-content">
<p>Think of a function as a small named tool. You give it input values,
it does some work, and it may give a result back. Calling a function is
just another expression, so you can store or pass along its result.</p>

<p>When the behavior itself should vary, pass a function instead of
branching in every caller. That keeps the shared work in one place.</p>
</div>

## Returning values

Use `return` to send a value back to the caller.

```python
def square(n):
    return n * n
```

If a function reaches the end without a `return`, the result is `None`.

<div class="beginner-content">
<p><code>return</code> ends the function immediately. Any code after it
in the same block does not run.</p>
</div>

```python
def greet(name):
    print("Hello", name)
```

## Practical guidance

- Keep functions focused on one job.
- Prefer returning values over printing inside helper functions.
- Give functions names that describe what they compute or do.
