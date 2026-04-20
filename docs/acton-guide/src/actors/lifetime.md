# Lifetime

In many languages, reaching the end of `main` ends the program. Acton is
different. Actors stay alive as long as another actor keeps a reference
to them, and they sit idle until they receive more work. Actors without
references can be garbage collected. The root actor stays alive until
the program exits.

That means a program like this will keep running even though the root
actor body reaches its end. You must explicitly tell the runtime to stop
the actor world with `env.exit()`.

<div class="advanced-content">
<p>Actor lifetime is reference-based. Non-root actors disappear when no
live actor keeps a reference to them, while the root actor persists
until the program exits. This is why shutdown in Acton is explicit
rather than an implicit fall-through from the end of <code>main</code>.</p>
</div>

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
