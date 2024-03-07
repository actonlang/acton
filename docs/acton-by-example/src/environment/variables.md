# Environment variables

It is possible to read, set and unset environment variables. The standard functions `env.getenv`, `env.setenv` and `env.unsetenv` all assume `str` input and output, which is a convenience based on the assumption that all data is encoded using UTF-8. POSIX systems really use binary encoding for both environment names and variables. To access the environment as bytes and handle decoding explicitly, use `env.getenvb`, `env.setenvb` and `env.unsetenvb`.

Source:
```python
actor main(env):
    env_user = env.getenv("USER")
    if env_user is not None:
        print("User:", env_user)
    env.setenv("FOO", "bar")
    env.unsetenv("LANG")
    foo_env = env.getenv("FOO")
    if foo_env is not None:
        print("FOO:", foo_env)
    env.exit(0)
```

Output:
```sh
User: myuser
FOO: bar
```
