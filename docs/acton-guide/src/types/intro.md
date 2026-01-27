# Types

Every value in Acton is of a certain *data type*, which tells Acton what kind of data is being specified so it knows how to work with the data. Acton is a statically typed language, which means the type of all values must be known when we compile the program. Unlike many traditional languges like Java or C++, where types must be explicitly stated everywhere, we can write most Acton programs without types. The Acton compiler features a powerful type inferencer which will infer the types used in the program.

This program does not have any explicit types specified.

Source:
```python
def foo(a, b):
    if a > 4:
        print(len(b))

actor main(env):
    i1 = 1234      # inferred type: int
    s1 = "hello"   # inferred type: str
    foo(i1, s1)

    env.exit(0)
```

To see the inferred types of an Acton program, use the `--sigs` option of the compiler. As the name suggests, this will print out the type signatures for functions, actors and their attributes and methods in the specified Acton module.

Print type signatures with `--sigs`:
```sh
acton types.act --sigs
```

Output:
```sh
#################################### sigs:

foo : [A(Ord, Number)] => (A, Collection[B]) -> None

actor main (Env):
    i1 : int
    s1 : str
```

Acton implements the Hindley-Milner (HM) type system, which is common in languages with static types, like Haskell and Rust. Acton extends further from HM to support additional features.

<!-- TODO: what do we extend from HM? -->
