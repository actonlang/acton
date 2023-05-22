# Actors

**Actors** is a key concept in Acton. Each actor is a small sequential process with its own private state. Actors communicate with each other through messages, in practice by calling methods on other actors or reading their attributes.

Source:
```python
# An actor definition
actor Act(name):

    # Top level code in an actor runs when initializing an actor instance, like
    # __init__() in Python.
    print("Starting up actor " + name)
    
    def hello():
        # We can directly access actor arguments, like `name`
        print("Hello world from " + name)

actor main(env):
    # Create an actor instance a of Act
    a = Act("FOO")
    # Call the actor method hello
    await async a.hello()

    await async env.exit(0)
```

Compile and run:
```sh
actonc actors.act
./actors
```

Output:
```sh
Starting up actor FOO
Hello world from FOO
```
