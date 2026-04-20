# Control flow

Control flow decides which code runs, when it runs, and how often it
runs.

Acton does not have a Rust-style `match` expression. Use
`if`/`elif`/`else` for branching, and combine that with optionals or
exceptions when the question is about absence or failure.

Start with these three everyday tools:

- [`if` / `elif` / `else`](control_flow/if_else.md) for branching
- [`for`](control_flow/for.md) for iterating over items
- [`while`](control_flow/while.md) for repeating while a condition stays
  true

Loops also support a few extra tools:

- `break` to stop the loop early
- `continue` to skip to the next iteration
- `else` to run code after a loop finishes normally, without a `break`

<div class="beginner-content">
<p>If you are new to programming, start with <code>if</code>,
<code>for</code>, and <code>while</code> and treat loop
<code>else</code> as optional. Most code uses the first three tools far
more often.</p>
</div>

```python
for n in range(5):
    if n == 2:
        continue
    if n == 4:
        break
    print(n)
else:
    print("finished without break")
```

<div class="advanced-content">
<p>Loop <code>else</code> is tied to <code>break</code>, not to "zero
iterations". It runs whenever the loop finishes normally, which makes it
useful for search-style logic but easy to misread if used casually.</p>
</div>

Acton also has actor-specific control patterns such as
[`after`](control_flow/after_sleep.md). Those matter once you start
thinking about actors, timers, and concurrency.
