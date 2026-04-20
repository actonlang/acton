# Root Actor

Every Acton program has a root actor. A binary executable must have
one, and by default Acton uses an actor named `main` if it finds one in
the source file. You can choose a different root with `--root`.

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

A normal Acton program consists of many actors arranged in a tree. The
root actor is at the top of that tree and starts the rest of the program
directly or indirectly. The Acton runtime bootstraps the root actor and
passes it the process-level capability object.

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
