# Environment variables

Environment variables are a common way to pass configuration into a
program. Acton lets you read, set, and unset them through `env`.

The string-based helpers are the most convenient:

- `env.getenv(name)` returns a `str` or `None`
- `env.setenv(name, value)` stores a string value
- `env.unsetenv(name)` removes a variable

They assume UTF-8 text, which is practical for most programs. When you
need exact byte-level control, use the byte-oriented variants instead:

- `env.getenvb`
- `env.setenvb`
- `env.unsetenvb`

<div class="advanced-content">
<p>The string helpers are deliberately opinionated. They make the common
case easy, but they also make the text boundary explicit. If your code
needs to preserve the original bytes, avoid round-tripping through
<code>str</code> until you have chosen a decoding strategy yourself.</p>
</div>

```python
actor main(env):
    env_user = env.getenv("USER")
    if env_user is None:
        env_user = "unknown"

    print("User:", env_user)

    if env.getenv("FOO") is None:
        env.setenv("FOO", "bar")

    foo_env = env.getenv("FOO")
    if foo_env is not None:
        print("FOO:", foo_env)

    env.unsetenv("LANG")
    env.exit(0)
```

Output:
```sh
User: myuser
FOO: bar
```

## Common patterns

Read a variable once, check whether it is missing, and choose a safe
default. Do not assume configuration is always present.

Use environment variables for configuration values that are naturally
textual, such as names, paths, flags, and addresses. If the value is
binary or needs exact decoding rules, use the byte-oriented APIs and
decode in your own code.

When a program sets or unsets variables, it is changing process state,
not just a local dictionary. Keep that in mind when passing `env`
through helpers.
