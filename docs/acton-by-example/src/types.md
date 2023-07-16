# Types

Acton is a statically typed language. Unlike traditional languages like C or Java, where the type of every variable must be explicitly stated, we can write most Acton programs without types. The Acton compiler features a powerful type inferencer which will then infer the type.


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

    await async env.exit(0)
```

To see the inferred types of an Acton program, use the `--sigs` option of the compiler. As the name suggests, this will print out the type signatures for functions, actors and their attributes and methods in the specified Acton module.

Print type signatures with `--sigs`:
```sh
actonc types.act --sigs
```

Output:
```sh
#################################### sigs:

foo : [A(Ord, Number)] => (A, Collection[B]) -> None

actor main (Env):
    i1 : int
    s1 : str
```
