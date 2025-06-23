# Interactive stdin

For interactive programs, like a text editor, input is not fed into the program
line by line, rather the program can react on individual key strokes.

The default stdin mode is the *canonical* mode, which implies line buffering and
that there are typically line editing capabilities offered that are implemented
external to the Acton program. By setting stdin in non-canonical mode we can
instead get the raw key strokes directly fed to us.

```python
actor main(env):
    def interact(input):
        print("Got some input:", input)

    # Set non-canonical mode, so we get each key stroke directly
    env.set_stdin(canonical=False)
    # Turn off terminal echo
    env.set_stdin(echo=False)
    env.stdin_install(interact)
```

We can also disable the echo mode with the echo option.

The Acton run time system will copy the stdin terminal settings on startup and
restore them on exit, so you do not need to manually restore terminal echo for
example.
