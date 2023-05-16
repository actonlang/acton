# Lifetime

The main function in most imperative and functional programming languages start at the top and when they reach the end of the function, the whole program exits. Actors do not exit on their own. Once an actor has been created, it runs the initialization code contained in the body of the actor, after which it will wait for incoming messages in the form of actor methods calls.

This means that a simple program like this modified helloworld (the `env.exit()` call has been removed) will run indefinitely.

Source:
```python
actor main(env):
    print("Hello world!")
```

Compile and run:
```sh
actonc noexit.act
```

Output:
```sh
$ ./noexit
<you will never get your prompt back>
```
