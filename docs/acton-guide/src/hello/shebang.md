# Shebang

While Acton is a compiled language and the `acton` compiler produces an executable binary, script style execution is also possible through the use of a shebang line.

Source:
```python
#!/usr/bin/env runacton

actor main(env):
    print("Hello World!")
    env.exit(0)
```

Ensure the executable bit is set and run your .act file directly:
```console
chmod a+x hello.act
./hello.act
```

Output:
```console
Hello World!
```

