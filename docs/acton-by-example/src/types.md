# Types

Acton is a statically typed language. Unlike traditional languages like C or Java, where the type of every variable must be explicitly stated, we can write most Acton programs without types. The Acton compiler features a powerful type inferencer which will then infer the type.

It is possible to specify the types of variables or arguments in a function through type hints. The syntax is similar to type hints in Python.

Source:
```python
# 'a: int' means the first argument `a` should be of type int
# 'b: str means the second argument `b` should be of type str
# The function returns nothing
def foo(a: int, b: str) -> None:
    print(a, b)
    
# A functions type signature can also be written on a separate line
bar : (int, str) -> None
def bar(a, b):
    print(a, b)

actor main(env):
    # i1 is explicitly specified as an integer while s1 is a str
    i1 : int = 1234
    s1 : str = "hello"
    foo(i1, s1)
    bar(i1, s1)

    # The literal value 1234 is an integer, so when we assign it to i2, the
    # compiler can easily infer that the type of i2 is int. Similarly for s2
    # since the literal value "foo" is clearly a string
    i2 = 1234
    s2 = "hello"
    foo(i2, s2)
    bar(i2, s2)
    
    await async env.exit(0)
```

Compile and run:
```sh
actonc types.act
./types
```

Output:
```sh
1234 hello
1234 hello
1234 hello
1234 hello
```

Try changing the type of `i1` or `s1` and you will find that the compiler complains that it cannot solve the type constraints of the program.
