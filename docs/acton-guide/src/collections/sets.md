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

## Set Comprehensions

Set comprehensions provide a concise way to create sets from sequences or other iterables.

Source:
```python
actor main(env):
    # Basic set comprehension
    squares = {x * x for x in range(5)}
    print("Squares set:", squares)
    
    # With condition
    evens = {x for x in range(10) if x % 2 == 0}
    print("Even numbers set:", evens)
    
    # From a list with duplicates
    numbers = [1, 2, 2, 3, 3, 3, 4, 4, 4, 4]
    unique = {n for n in numbers}
    print("Unique numbers:", unique)
    
    # String manipulation
    words = ["hello", "world", "hello", "acton"]
    unique_words = {w for w in words}
    print("Unique words:", unique_words)
    
    # Filtering and transforming
    long_caps = {w.upper() for w in words if len(w) > 4}
    print("Long words in caps:", long_caps)
    
    # Creating from calculations
    remainders = {x % 3 for x in range(10)}
    print("Remainders when divided by 3:", remainders)
    
    # Combining conditions
    special = {x for x in range(20) if x % 2 == 0 if x % 3 == 0}
    print("Numbers divisible by both 2 and 3:", special)

    env.exit(0)
```

Output:
```
Squares set: {0, 1, 4, 9, 16}
Even numbers set: {0, 2, 4, 6, 8}
Unique numbers: {1, 2, 3, 4}
Unique words: {'hello', 'world', 'acton'}
Long words in caps: {'HELLO', 'WORLD', 'ACTON'}
Remainders when divided by 3: {0, 1, 2}
Numbers divisible by both 2 and 3: {0, 6, 12, 18}
