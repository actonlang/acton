# Interactive stdin

Interactive programs do not usually want line-buffered input. A text
editor, a terminal UI, or a game often needs individual key presses as
they happen.

By default, stdin is in canonical mode. That means the terminal buffers
input and usually handles line editing before your program sees
anything. If you want raw key presses, switch stdin to non-canonical
mode.

```python
actor main(env):
    def interact(input):
        print("Got some input:", input)

    # Set non-canonical mode so we get each key press directly.
    env.set_stdin(canonical=False)
    # Turn off terminal echo.
    env.set_stdin(echo=False)
    env.stdin_install(interact)
```

Canonical mode is the right default for ordinary command line tools.
Non-canonical mode is for programs that need to manage the terminal
themselves.

The runtime copies the terminal settings on startup and restores them
on exit, so you do not need to restore echo manually in the common
case.

<div class="advanced-content">
<p>Interactive stdin changes the terminal contract for the whole
process, not just one helper function. That is why the runtime restores
settings on exit for you, and why these programs should be careful and
intentional about when they enter non-canonical mode.</p>
</div>

## Common patterns

Switch to non-canonical mode only when you need it, and keep that code
close to the part that depends on raw input. That makes the terminal
state easier to reason about.

Disable echo when raw input should not be shown back to the user, such
as when reading passwords or handling single-key commands.
