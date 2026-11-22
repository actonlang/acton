# Interactive stdin

Interactive programs do not usually want line-buffered input. A text
editor, a terminal UI, or a game often needs individual key presses as
they happen.

By default, stdin is in canonical mode. That means the terminal buffers
input and usually handles line editing before your program sees
anything. If you want key presses as they happen, switch stdin to
non-canonical mode.

```python
actor main(env):
    def interact(input):
        print("Got some input:", input)

    # Set non-canonical mode so we get each key press directly.
    env.stdio.set_stdin(canonical=False)
    # Turn off terminal echo.
    env.stdio.set_stdin(echo=False)
    env.stdio.stdin_install(interact)
```

Canonical mode is the right default for ordinary command line tools.
Non-canonical mode is for programs that need to manage the terminal
themselves.

## Raw mode

In non-canonical mode the terminal still acts on some keys before your
program sees them. Ctrl-C ends the program, Ctrl-Z suspends it, Ctrl-S
stops output until Ctrl-Q, and Enter arrives as a newline. A program
that handles every key itself, like a line editor with its own key
bindings, puts stdin in raw mode instead. Then every key arrives as its
bytes: Ctrl-C arrives as the byte 3 and Enter as a carriage return, the
byte 13.

```python
actor main(env):
    def on_key(key: bytes):
        # Ctrl-C arrives as the byte 3
        if 3 in key:
            print("Ctrl-C, bye")
            env.exit(0)
        else:
            print("Got key:", key)

    if env.stdio.stdin_is_tty():
        env.stdio.set_stdin(raw=True)
    env.stdio.stdin_install(on_stdin_bytes=on_key)
```

Raw mode does not change output, so a newline that the program writes
still starts a new line. `set_stdin(raw=False)` puts back the settings
from before `set_stdin(raw=True)`, for example to let a command that
the program runs read whole lines.

`env.stdio.stdin_is_tty()`, `env.stdio.stdout_is_tty()` and
`env.stdio.stderr_is_tty()` tell whether stdin, stdout and stderr are
terminals. When stdin is a pipe or a file, `set_stdin` does nothing.

The runtime copies the terminal settings on startup and restores them
on exit, so you do not need to restore echo or raw mode manually in the
common case. This includes an exit through a signal such as SIGTERM,
SIGHUP or SIGQUIT and a crash, so a program in raw mode does not leave
the terminal without Ctrl-C.

<div class="advanced-content">
<p>Interactive stdin changes the terminal contract for the whole
process, not just one helper function. That is why the runtime restores
settings on exit for you, and why these programs should be careful and
intentional about when they enter non-canonical or raw mode.</p>
</div>

## Terminal size

`env.stdio.term_size()` returns the size of the terminal on stdout as
`columns` and `rows`. It returns `None` when stdout is not a terminal,
for example when the output goes to a pipe or a file, or when the
terminal reports no size.

```python
actor main(env):
    columns = 80
    size = env.stdio.term_size()
    if size is not None:
        columns = size.columns
    print("-" * columns)
    env.exit(0)
```

The size changes when the user resizes the terminal window, so read it
again before laying out output that depends on it.

## One stdin handler

A process has one reader of stdin. Calling `stdin_install` again
replaces the handler, and the input that arrives after that goes to the
new handler only. A program can read a password with one handler and
then install another one for its command line.

## Common patterns

Switch to non-canonical or raw mode only when you need it, and keep
that code close to the part that depends on raw input. That makes the
terminal state easier to reason about.

Disable echo when raw input should not be shown back to the user, such
as when reading passwords or handling single-key commands.
