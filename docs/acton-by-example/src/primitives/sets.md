# Sets

Source:
```python
actor main(env):
    # Duplicate entries
    s = {"foo", "foo"}
    print("Set without duplicate 'foo':", s)
    s.add("bar")
    print("Set after adding 'bar':", s)
    print("Entries in set:", len(s))
    s.discard("foo")
    print("Set after discarding 'foo':", s)

    await async env.exit(0)
```

Compile and run:
```sh
actonc sets.act
./sets
```

Output:
```sh
Set without duplicate 'foo': {"foo"}
Set after adding 'bar': {"bar", "foo"}
Entries in set: 2
Set after discarding 'foo': {"bar"}
```
