# Hello World

We follow tradition and introduce Acton with the following minimal example

Source:
```python
# This is a comment, which is ignored by the compiler.

# This is the main actor, which we will tell the compiler to use as the root
# actor by setting `--root main` when compiling the program
actor main(env):
    print("Hello World!")
    await async env.exit(0)
```

Compile and run:
```sh
actonc hello.act
./hello
```

Output:
```sh
Hello World!
```


## Description

An executing Acton program consists of a collection of interacting actors. Here we have just a single actor, which has been given the name `main` and that acts as the *root actor* of our system. The *root actor* of a system takes a parameter `env`, which represents the execution environment. `env` has methods for accessing command line arguments and carries a security token, `WorldAuth`, for accessing the environment, e.g. reading from and writing to keyboard/screen and files, working with sockets etc. When the program is executed, the run time system creates the root actor, hands it the `env` and runs its initialization code, which here is just a single command, to print a message on screen.

The careful reader may ask why `print` is not a method of `env`. The answer is that we see `print` as such a ubiquitous function that it should be possible to use without having to thread an `env` parameter to all sub-units.
