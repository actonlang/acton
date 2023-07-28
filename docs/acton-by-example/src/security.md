# Security

Security is an important part of application development and is best considered throughout the entire design and development time of an application rather than as an bolted-on after-thought.

In Acton, the separation of actors offers the primary means of security. Access to actors (like being able to call their methods) requires a reference to the relevant actor. Anyone with a reference can access the actor in question. It is not possible to forge a reference.

This is similar to the [object capability (OCAP) model](https://en.wikipedia.org/wiki/Object-capability_model).

Since there are no global variables, the only reachable state is local to an actor or reachable via a reference to another actor. This means you cannot reach something out of thin air. You have to be explicitly passed a reference to anything you need to access.

The security model based on capability references extends for [accessing the world outside](security/capabilities.md) of the Acton system.

```python
actor Foo():
    def foo():
        print("foofoo")
    
actor Bar():
    # Without a reference to f we cannot call its foo() function
    
actor main(env):
    f = Foo()
    f.foo()
    b = Bar()
```
