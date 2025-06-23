# Functions

Functions are declared using the `def` keyword.

Use `return foo` to return variable `foo`. If no `return` keyword is used or a lone `return` without argument is given, the function will return `None`.

Source:
```python
def multiply(a, b):
    print("Multiplying", a, "with", b)
    return a*b
    
actor main(env):
    result = multiply(3, 4)
    print("Result:", result)
    env.exit(0)
```

Output:
```sh
Multiplying 3 with 4
Result: 12
```
