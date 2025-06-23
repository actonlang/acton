# Sets

Sets in Acton are unordered collections of unique elements. They do not preserve insertion order, so when iterating over a set or printing it, elements may appear in any order. Sets can only contain values of one type at a time.

Source:
```python
actor main(env):
    # set syntax is similar to dicts using {} but without keys
    s = {"foo", "bar"}

    # Although for an empty set, {} cannot be used as it means an empty dict
    # (and it is impossible to distinguish between the two from syntax alone).
    # Use set() to create an empty set.
    empty_set = set()

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
    
    # Add all items from another set or some other iterable
    s.update({"x", "y", "z"})
    print(s)

    env.exit(0)
```

Compile and run:
```sh
acton sets.act
./sets
```

Output:
```sh
Building project in /home/user/foo
  Compiling example.act for release
   Finished compilation in   0.032 s
  Final compilation step
   Finished final compilation step in   0.701 s
Set content: {'bar', 'foo'}
'foo' is in the set
'a' is not in the set
Set without duplicate 'foo': {'bar', 'foo'}
Set after adding 'a': {'bar', 'a', 'foo'}
'a' is in the set now
Entries in set: 3
Set after discarding 'foo': {'bar', 'a'}
{'x', 'bar', 'a', 'y', 'z'}
```
