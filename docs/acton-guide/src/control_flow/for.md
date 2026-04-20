# for

Use `for ... in ...` to go through each value in something like a
string, tuple, or `range(...)`.

```python
actor main(env):
    names = ("Ada", "Grace", "Linus")

    for name in names:
        print("Hello", name)

    for n in range(3):
        print("n =", n)

    env.exit(0)
```

<div class="beginner-content">
<p>A <code>for</code> loop is the usual choice when you want to go
through each item in a collection or repeat something a known number of
times. The loop variable is a new local name that takes on each value in
turn.</p>
</div>

`range(stop)` counts from `0` up to, but not including, `stop`.

```python
for n in range(5):
    print(n)
```

You can also use `range(start, stop, step)`.

```python
for n in range(2, 10, 2):
    print(n)
```

The loop variable is local to the loop body. A common pattern is to use
`for` with a collection when the values matter, and `range(...)` when the
count matters.

<div class="advanced-content">
<p>In Acton, <code>for</code> is usually the clearest way to consume an
iterable because it avoids manual index state and keeps the element type
front and center. When you need both the index and the value, prefer
<code>enumerate(...)</code> over <code>range(...)</code>. Use
<code>range(...)</code> when the numbers themselves matter, not as a
default substitute for iterating a collection.</p>

```python
for i, name in enumerate(names):
    print(i, name)
```
</div>
