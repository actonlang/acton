# Shebang

While Acton is a compiled language and the actonc compiler produces an executable binary, script style execution is also possible through the use of a shebang line.

Source:
```python
#!/usr/bin/env runacton

actor main(env):
    print("Hello World!")
    await async env.exit(0)
```

Ensure the executable bit is set and run your .act file directly:
```sh
chmod a+x hello.act
./hello.act
```

Output:
```sh
Hello Johan!
```

