# Root Actor

Every Acton program has a root actor. A binary executable must have
one, and by default Acton uses an actor named `main` when it has a
root-eligible signature. In ordinary programs that means a single
environment parameter, usually written as `actor main(env):` or
explicitly as `actor main(env: Env):`. You can choose a different root
with `--root`.

Given this Acton program:
```python
actor main(env):
    print("Hello World!")
    env.exit(0)
```

The following acton commands will all produce the same output.
```sh
acton hello.act
acton hello.act --root main
acton hello.act --root hello.main
```

The first invocation relies on the default rule of using an actor named
`main`. The second explicitly selects `main` as the root actor. The
third uses a qualified name that includes both the module name and the
actor name. Qualified names are useful when a project contains several
actors that could otherwise be mistaken for the entrypoint.

The `env` parameter an actor reference to the builtin environment actor of type
`Env`. The environment actor is constructed by the runtime system and passed to
the root actor when the program starts.

A normal Acton program consists of many actors arranged in a tree. The
root actor is at the top of that tree and starts the rest of the program
directly or indirectly. The Acton runtime bootstraps the root actor and
passes it `Env`, which is the entrypoint for process arguments,
capabilities, standard input, environment variables, and program exit.

<div class="advanced-content">
<p>The root actor is the boundary where process-level capabilities
enter the actor world. That is why <code>main(env)</code> keeps showing
up throughout the guide: it is both the conventional entrypoint and the
place where outside-world access begins.</p>
</div>

```bob
              .────.
             ( root )
              `----'
             /      \
            /        \
           V          V
         .─.          .─.
        ( A )        ( B )---+
         `-'          `-'     \
        /  \          |  \     \
       /    \         |   \     \
      V      V        |    \     \
    .─.      .─.      V     V     V
   ( 1 )    ( 2 )    .─.    .─.    .─.
    `─'      `─'    ( 3 )  ( 4 )  ( 5 )
                     `─'    `─'    `─'
```

Any executable Acton program must have a root actor. Acton libraries
that are imported into another program do not.

See [Environment](../environment.md) for the practical guide to `Env`
and the operations it makes available.
