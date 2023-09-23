# Sets

Source:
```python
actor main(env):
    s = {"foo", "bar"}
    print("Set content:", s)
    if "foo" in s:
        print("'foo' is in the set")
    if "a" not in s:
        print("'a' is not in the set")
    # Adding an item that is already in the set does nothing
    s.add("foo")
    print("Set without duplicate 'foo':", s)
    s.add("a")
    print("Set after adding 'a':", s)
    if "a" in s:
        print("'a' is in the set now")
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
Set content: {'bar', 'foo'}
'foo' is in the set
'a' is not in the set
Set without duplicate 'foo': {'bar', 'foo'}
Set after adding 'a': {'bar', 'a', 'foo'}
'a' is in the set now
Entries in set: 3
Set after discarding 'foo': {'bar', 'a'}
```
