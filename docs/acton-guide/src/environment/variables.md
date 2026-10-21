# Environment variables

Environment variables are a common way to pass configuration into a
program. Acton lets you read, set, and unset them through `env.vars`.

The string-based methods are the most convenient:

- `env.vars.getenv(name)` returns a `str` or `None`
- `env.vars.setenv(name, value)` stores a string value
- `env.vars.unsetenv(name)` removes a variable

They assume UTF-8 text, which is practical for most programs. When you
need exact byte-level control, use the byte-oriented variants instead:

- `env.vars.getenvb`
- `env.vars.setenvb`
- `env.vars.unsetenvb`

<div class="advanced-content">
<p>The string methods are deliberately opinionated. They make the common
case easy, but they also make the text boundary explicit. If your code
needs to preserve the original bytes, avoid round-tripping through
<code>str</code> until you have chosen a decoding strategy yourself.</p>
</div>

```python
actor main(env):
    env_user = env.vars.getenv("USER")
    if env_user is None:
        env_user = "unknown"

    print("User:", env_user)

    if env.vars.getenv("FOO") is None:
        env.vars.setenv("FOO", "bar")

    foo_env = env.vars.getenv("FOO")
    if foo_env is not None:
        print("FOO:", foo_env)

    env.vars.unsetenv("LANG")
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
not just a local dictionary. Keep that in mind when passing `env.vars`
to helpers.

A helper that only works with environment variables can take
`env.vars`, an `EnvVar`, instead of the whole `env`. Give its parameter
the type `EnvVar`: a parameter without a type that calls `getenv` is
taken to be an `Env`.

```python
proc def user_name(vars: EnvVar) -> str:
    user = vars.getenv("USER")
    if user is not None:
        return user
    return "unknown"

actor main(env):
    print("User:", user_name(env.vars))
    env.exit(0)
```
