# Child processes

Start other programs through `env.process`. There are two ways to do
it, depending on whether the program's output is needed as it arrives
or only once the program has finished.

`env.process.run` starts a program and calls `on_exit` once it has
finished, with everything it wrote to stdout and stderr. `on_error` is
called instead if the program cannot be started.

```python
actor main(env):
    def on_exit(p, exit_code, term_signal, stdout_buf, stderr_buf):
        print("Exit code:", exit_code)
        print("Output:", stdout_buf.decode())
        env.exit(0)

    def on_error(p, error):
        print("Could not run the program:", error)
        env.exit(1)

    env.process.run(["echo", "hello"], on_exit, on_error)
```

`env.process.spawn` starts a program and calls `on_stdout` and
`on_stderr` with its output as it arrives. They get `None` when the
stream is closed. `spawn` returns the `Process`, which takes input with
`write` and `done_writing`.

```python
actor main(env):
    def on_stdout(p, data):
        if data is not None:
            print("Output:", data.decode())

    def on_stderr(p, data):
        if data is not None:
            print("Error output:", data.decode())

    def on_exit(p, exit_code, term_signal):
        print("Exit code:", exit_code)
        env.exit(0)

    def on_error(p, error):
        print("Could not run the program:", error)
        env.exit(1)

    p = env.process.spawn(["cat"], on_stdout, on_stderr, on_exit, on_error)
    p.write(b"hello\n")
    p.done_writing()
```

Both take three optional arguments:

- `workdir`, the working directory of the program, by default the
  current one
- `env`, a `dict[str, str]` with the environment of the program, by
  default the current environment. When it has no `PATH`, the program
  gets the current `PATH`.
- `timeout`, the number of seconds after which the program is stopped

A running `Process` also has `pid()`, `signal(n)`, `terminate()`, which
sends SIGTERM, `kill()`, which sends SIGKILL, and `stop()`, which sends
SIGTERM and, one second later, SIGKILL.

## Common patterns

Use `run` when the program's output is only needed once it has
finished, as for most commands. Use `spawn` when the program runs for a
long time, reads input, or produces output that should be handled as it
arrives.

`run` and `spawn` are actor calls, so a call whose result is not used
returns before the program has started. Use the result, as in
`p = env.process.spawn(...)`, when the code that follows needs the
program to be running.

<div class="advanced-content">
<p>Starting processes is authority over the outside world, and it comes
from <code>env.process</code>, an <code>EnvProcess</code>. A helper that
starts programs can take <code>env.process</code> instead of the whole
<code>env</code>, which keeps it from also reading environment variables
or stdin.</p>
</div>
