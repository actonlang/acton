# Hello World

We follow tradition and introduce Acton with the following minimal example

Source:
```python
# This is a comment, which is ignored by the compiler.

# An actor named 'main' is automatically discovered and recognized as the root 
# actor. Any .act file with a main actor will be compiled into a binary 
# executable and the main actor becomes the starting point.
actor main(env):
    print("Hello World!")
    env.exit(0)
```

Compile and run:
```console
acton hello.act
./hello
```

Output:
```console
Hello World!
```


## Description

When an Acton program runs, it really consits of a collection of actors that interact with each other. In the above example, we have just a single actor, which has been given the name `main` and that acts as the *root actor* of our system. The *root actor* of a system takes a parameter `env`, which represents the execution environment. `env` has methods for accessing command line arguments and carries a reference to the capabilities of the surrounding world, `WorldCap`, for accessing the environment, e.g. reading from and writing to keyboard/screen and files, working with sockets etc.
