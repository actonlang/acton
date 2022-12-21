# Tuples

Source:
```python
actor main(env):
    # Items in a tuple can be of different types
    t = ("foo", 42)
    # Items are accessed liked attributes on a object (this is so we can find 
    # out type of items at compile time).
    print(t)
    print(t.0)
    print(t.1)
    
    await async env.exit(0)
```

Compile and run:
```sh
actonc sets.act
./sets
```

Output:
```sh
(foo, 42)
foo
42
```
