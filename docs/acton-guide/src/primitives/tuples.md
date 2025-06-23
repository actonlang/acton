# Tuples

Tuples allow structuring multiple values of different type into one value.

Source:
```python
actor main(env):
    # Items in a tuple can be of different types
    t = ("foo", 42)
    # Fields are accessed by their index and using field / attribute selection style:
    print(t)
    print(t.0)
    print(t.1)
    
    # Tuples can use named fields
    nt = (a="bar", b=1337)
    print(nt)
    print(nt.a)
    print(nt.b)
    
    r = foo()
    if r.b:
        print(r.c)

    env.exit(0)
    
def foo() -> (a: str, b: bool, c: int):
    """A function that returns a tuple with fields name a and b
    """
    return (a = "hello", b=True, c=123)
```

Compile and run:
```sh
actonc sets.act
./sets
```

Output:
```sh
('foo', 42)
foo
42
('bar', 1337)
bar
1337
123
```

- fields in a tuple can be of different types
- tuples have a fixed fields
- tuples with named fields is like an anonymous data class, i.e. the data type itself has no name
- tuples with named fields can be used like a simple record type
