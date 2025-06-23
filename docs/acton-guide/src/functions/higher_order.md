# Higher order functions

Acton supports higher order functions which means you can pass a function as an argument to another function.

Source:
```python
def multiply_with_3(a):
    print("Multiplying with 3")
    return 3*a

def multiply_with_42(a):
    print("Multiplying with 42")
    return 42*a

def compute(a, fun):
    """Compute value from a using function fun"""
    return fun(a)
    
actor main(env):
    print( compute(7, multiply_with_3) )
    print( compute(7, multiply_with_42) )
    env.exit(0)
```

Output:
```sh
Multiplying with 3
21
Multiplying with 42
294
```
