# Scalars

Source:
```python
actor main(env):
    i = 42       # an integer
    f = 13.37    # a float
    
    print(i)
    print(f)

    s = "Hello"  # a string
    print(s)
    # a slice of a string
    print(s[0:1])
    
    b = True     # a boolean
    print(b)
    
    await async env.exit(0)
```

Compile and run:
```sh
actonc scalars.act --root main
./scalars
```

Output:
```sh
42
13.37
Hello
H
True
```
