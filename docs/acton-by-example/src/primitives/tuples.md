# Tuples

Source:
```python
actor main(env):
    # Items in a tuple can be of different types
    t = ("foo", 42)
    # Items are accessed liked attributes on a object (this is so we can find 
    # out type of items at compile time), not like the index in a list.
    print(t)
    print(t.0)
    print(t.1)
    
    # Named tuples
    nt = (a="bar", b=1337)
    print(nt)
    print(nt.a)
    print(nt.b)

    await async env.exit(0)
```

Compile and run:
```sh
actonc sets.act
./sets
```

Output:
```sh
("foo", 42)
foo
42
("bar", 1337)
bar
1337
```

- tuples allow storing multiple items in one variable
- tuples are fixed length, unlike lists which can be appended and shortened
- fields in a tuple can be of different types
- named tuples are similar to records in other languages
