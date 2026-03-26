# Root Actor

Every Acton program has a root actor. To compile a binary executable, there must be a root actor. Per default, if a source (`.act`) file contains an actor named `main` with a single `Env` type parameter, it will be used as the root actor. You can specify which actor should be the root using the `--root` argument. While the convention is to call the root actor `main`, you are free to name it anything.

The `env` parameter is not an arbitrary actor reference, it is the builtin environment actor `Env`. The Acton run time system (RTS) constructs this actor and passes it to the root actor when the program starts. In guide examples this is usually written as `actor main(env):`, but you can also write the type explicitly as `actor main(env: Env):`.

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

The first invocation relies on the default rule of using an actor called `main`. The second invocation explicitly specifies that we want to use `main` as the root actor while the third uses a qualified name which includes both the actor name (`main`) as well as the module name (`hello`). Using qualified names can be particularly useful when building executable binaries in projects.

A normal Acton program consists of many actors that are structured in a hierarchical tree. The root actor is at the root of the tree and is responsible for starting all other actors directly or indirectly. The Acton Run Time System (RTS) will bootstrap the root actor.

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

Any executable Acton program must have a root actor defined. Acton libraries (that are included in an another Acton program), do not need a root actor.

Because the root actor receives `Env`, it is also where access to the builtin environment actor begins. See [Environment](/environment.md) for an overview of `Env` and links to the relevant guide pages for arguments, capabilities, environment variables, and stdin handling.
