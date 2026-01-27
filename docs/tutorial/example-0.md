# Example 0 - Hello World

We follow tradition and introduce Acton programming with the following minimal example

```py
actor main(env):
    print("Hello, World!")
```

We already mentioned that an executing Acton program consists of a
collection of interacting actors. Here we have just a single actor,
which has been given the name `main`. By convention, the
***root actor*** of a system takes a parameter `env` which
represents the execution environment, i.e. `env` has fields and
methods for accessing command line arguments, reading from and writing
to keyboard/screen and files, working with sockets etc. When the
program is executed, the runtime system creates the root actor,
hands it an `env` object and runs its initialization code, which here is
just a single command, to print a message on screen.

The careful reader may ask why `print` is not a method of
`env`. The answer is that we see `print` as a useful tool
for tracing program execution, and this should be
possible without having to thread an `env` parameter to all subunits.

To compile the program, assuming that it is saved in
`world.act`, the command is

``` shell
$ acton world.act --root main
```

This creates an executable file `world` in the same directory,
which can be run to see the familiar message.

Finally, let's modify the program to greet not the world, but a name
given on the command line:

```py
actor main(env):
    print("Hello, " + env.argv[1] + "!")
```

The field `env.argv` is the list of command line arguments
(with element 0 the name of the shell command, and element 1 the first proper
argument).
