# if / elif / else

Acton supports the `if` / `elif` / `else` construct - the corner stone of programming control flow.

The conditionals evaluated by `if` / `elif` / `else` are expressions.

Source:
```python

def whatnum(n):

    if n < 0:
        print(n, "is negative")
    elif n > 0:
        print(n, "is positive")
    else:
        print(n, "is zero")


def inrange(n):
    if n < 10 and n > 5:
        print(n, "is between 5 and 10")
    else:
        print(n, "is outside of the range 5-10")

actor main(env):

    whatnum(5)
    whatnum(1337)
    whatnum(-7)
    whatnum(0)
    
    inrange(3)
    inrange(-7)
    inrange(7)

    env.exit(0)
```

Compile and run:
```sh
acton if_else.act
```

Note that the output is random and you could get a different result.

Output:
```sh
5 is positive
1337 is positive
-7 is negative
0 is zero
3 is outside of the range 5-10
-7 is outside of the range 5-10
7 is between 5 and 10
```
