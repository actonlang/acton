# Expressions and operators

Acton evaluates expressions to values. Operators combine values into new
expressions.

## Common operators

- Arithmetic: `+`, `-`, `*`, `/`, `//`, `%`, `**`
- Comparison: `==`, `!=`, `<`, `<=`, `>`, `>=`
- Boolean: `and`, `or`, `not`
- Membership: `in`, `not in`

```python
actor main(env):
    a = 10
    b = 3
    word = "Acton"
    point = (3, 4)

    print("sum:", a + b)
    print("product:", a * b)
    print("floor division:", a // b)
    print("remainder:", a % b)
    print("power:", b ** 3)

    print("comparison:", a > b)
    print("boolean:", a > 0 and b > 0)
    print("membership:", "ct" in word)

    print("indexing:", word[0], point[1])
    print("slicing:", word[1:4])

    env.exit(0)
```

Method calls, function calls, indexing, and slicing are expressions too.
They all produce values.

```python
name = "Acton"
first = name[0]
upper = name.upper()
```

## Reading expressions

Read an expression from the values upward. Start with the pieces that
already have values, then apply the operator or call.

<div class="beginner-content">
<p>When reading an expression, start with the smallest parts that
already have values and then see how the operator or call combines them.
If you have to stop and think about precedence, add parentheses.</p>
</div>

## Precedence

Acton follows standard operator precedence rules. When in doubt, use
parentheses to make intent explicit.

```python
x = 2 + 3 * 4      # 14
y = (2 + 3) * 4    # 20
```

## Boolean logic

`and` and `or` are useful when the right-hand side only makes sense if
the left-hand side has already passed a check.

```python
if user_is_known and user_is_enabled:
    print("welcome")
```

<div class="advanced-content">
<p><code>and</code> and <code>or</code> short-circuit. That means Acton
only evaluates the right-hand side when it is needed to determine the
result. That matters not just for speed, but for semantics: you can use
short-circuiting to guard operations that would otherwise fail or do
unnecessary work. Calls, indexing, slicing, and later optional
operations all participate in the same expression model.</p>

```python
a = 0

if a != 0 and 10 / a > 2:
    print("large enough")
```
</div>
