# Security

Acton's security model starts from a simple rule: code can only use
what it has a reference to.

Actors are isolated from each other. To call an actor, read from it, or
otherwise interact with it, code must already hold a reference to that
actor. There is no ambient authority hiding behind a module import or a
global variable.

That is close to the [object capability
model](https://en.wikipedia.org/wiki/Object-capability_model).

<div class="beginner-content">
<p>A useful mental model is: no ambient authority. If code can reach
something, that access had to come from somewhere concrete. In
practice, give each function or actor only the references it actually
needs. That keeps the code easy to reason about and makes accidental
access paths harder to create.</p>
</div>

Because there are no mutable globals, reachable state is either:

- local to the current actor
- reachable through references that were explicitly passed in

```python
actor Vault():
    def read():
        print("secret")

actor Reader(vault):
    def show():
        await async vault.read()

actor main(env):
    vault = Vault()
    reader = Reader(vault)
    await async reader.show()
    env.exit(0)
```

`Reader` can call `Vault` only because the reference was passed in
explicitly. If the reference is not available, the access is not
available.

<div class="advanced-content">
<p>The key point is not only "there are no mutable globals". It is that
authority itself becomes something code can pass, withhold, or narrow.
If a function or actor never receives a filesystem, network, or process
capability, it cannot perform those actions by accident or by hidden
convention. That makes ordinary API boundaries double as security
boundaries.</p>

<p>This is why explicit capability passing matters so much in Acton. A
reference is not just a way to reach a value; it is also the way
authority enters a piece of code. That applies equally to actor
references inside the program and to outside-world capabilities such as
files, networking, environment access, and terminal control.</p>
</div>
