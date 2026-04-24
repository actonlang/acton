# Environment

The environment is your program's practical link to the outside world.
Most programs meet it first as the `env` argument passed to the root
actor.

The root actor receives the builtin environment actor `Env` as its
`env` parameter. `Env` is the runtime-provided handle for outside-world
access.

```python
actor main(env):
    print("args:", env.argv)
    env.exit(0)
```

<div class="beginner-content">
<p>You can think of <code>env</code> as the handle your program receives
for talking to the world around it. It is passed into <code>main</code>;
it is not a hidden global that every function can reach automatically.
If a helper only needs one small piece of that power, pass the smaller
piece instead of the whole environment.</p>
</div>

`Env` provides, among other things:

- `env.argv` for command-line arguments
- `env.cap` for the root `WorldCap` capability
- `env.getenv`, `env.setenv`, and `env.unsetenv` for environment
  variables
- `env.stdin_install`, `env.set_stdin`, `env.stdout_write`, and
  `env.is_tty` for terminal and stdin handling
- `env.exit(n)` to terminate the program

- [Environment variables](environment/variables.md)
- [Reading stdin input](environment/stdin.md)
- [Interactive stdin input](environment/interactive_stdin.md)

Use `env` when code needs process arguments, environment variables,
standard input, or terminal configuration. Pass it only to the code
that actually needs that access.

<div class="advanced-content">
<p><code>env</code> is best understood as a bundle of process-level
capabilities, not as one ordinary argument. It carries authority over
things such as arguments, environment variables, standard input, and
terminal configuration. If that whole bundle gets threaded through many
layers, those layers quietly become coupled to process concerns even
when they only need one small part of it.</p>

<p>In larger programs, treat the root actor as the place where broad
authority is received, then pass inward only the narrower capability or
value a helper actually needs. That keeps APIs honest about what they
depend on, makes tests easier to fake, and prevents a small utility
from accidentally gaining much wider access to the outside world than
its job requires.</p>
</div>
