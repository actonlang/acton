# Program Arguments

Program arguments are available as the attribute `argv` on the `env` actor. `env.argv` is a list where the first element contains the name of the shell command and the second element being the first proper argument.

We can rewrite our program to print a user supplied name to greet rather than the world.

Source:
```python
actor main(env):
    print("Hello " + env.argv[1] + "!")
    env.exit(0)
```

Compile and run, with argument:
```sh
acton hello.act
./hello Johan
```

Output:
```sh
Hello Johan!
```
