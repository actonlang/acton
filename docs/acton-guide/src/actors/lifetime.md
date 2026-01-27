# Lifetime

The main function in most imperative and functional programming languages start at the top and when they reach the end of the function, the whole program exits. Actors exist as long as another actor has a reference to it and will idle, passively waiting for the next method call. Actors without references will be garbage collected. The root actor of a program will always exist even without other references.

This means that a simple program like this modified helloworld (the `env.exit()` call has been removed) will run indefinitely. You need to deliberately tell the run time system to stop the actor world and exit, via `env.exit()`, in order to exit the program.

Source:
```python
actor main(env):
    print("Hello world!")
```

Compile and run:
```sh
acton noexit.act
```

Output:
```sh
$ ./noexit
<you will never get your prompt back>
```
