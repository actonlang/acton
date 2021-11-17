# Root Actor

Like C has a main() function, Acton has a root actor. To compile a binary executable, the root actor must be specified using `--root` when invoking `actonc`. While the convention is to call the root actor `main`, you are free to name it anything.

```sh
actonc --root main myprogram.act
```

A normal Acton program consists of many actors that are structured in a hierarchical tree. The root actor is at the root of the tree and is responsible for starting all other actors directly or indirectly. The Acton Run Time System (RTS) will bootstrap the root actor.

Any executable Acton program must have a root actor defined, i.e. must be compiled with `--root ROOT`. Acton libraries (that are included in an another Acton program), do not need a root actor.
