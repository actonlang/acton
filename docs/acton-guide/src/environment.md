# Environment

The environment of an Acton application is the outside world. Any useful application typically needs to interact with the environment in some way, like reading arguments or taking input from stdin and printing output.

The root actor receives the builtin environment actor `Env` as its `env` parameter. This is the primary way the root actor, and other actors it enables, access the outside world.

The builtin `Env` actor provides, among other things:

- `env.argv` for command-line arguments, see [Program Arguments](/hello/args.md)
- `env.cap` for the root `WorldCap` capability, see [Capabilities](/security/capabilities.md)
- `env.syscap` for low-level RTS/system access used by some runtime-facing libraries
- `env.getenv`, `env.setenv`, and `env.unsetenv` for environment variables, see [Environment Variables](/environment/variables.md)
- `env.stdin_install`, `env.set_stdin`, `env.stdout_write`, and `env.is_tty` for terminal I/O, see [Reading stdin input](/environment/stdin.md) and [Interactive stdin input](/environment/interactive_stdin.md)
- `env.exit(n)` to terminate the program
- `env.nr_wthreads` for the number of RTS worker threads
