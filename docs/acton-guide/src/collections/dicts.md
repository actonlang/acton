# Dictionaries

Dictionaries in Acton are ordered collections that map keys to values. They maintain insertion order, so when iterating over keys, values, or items, they will be returned in the order they were first inserted. All keys must be of the same type, and all values must be of the same type.

Source:
```python
actor main(env):
    d = {"foo": 1, "bar": 2, "x": 42}
    d["cookie"] = 3

    print("Dict:", d)
    print("len :", len(d))
    print("item foo:", d["foo"])
    print("item foo:", d.get("foo"))
    print("get value when key nonexistent:", d.get("foobar"))
    print("get_def value when key nonexistent:", d.get_def("foobar", "DEF_val"))
    
    d["bar"] = 42
    del d["foo"]
    print("Dict:", d)
    
    print("Dict keys: " + str(list(d.keys())))
    print("Dict values: " + str(list(d.values())))

    print("Dict items:")
    for k, v in d.items():
        print("  dict key " + k + "=" + str(v))

    print("Pop item with key 'bar':", d.pop("bar"))
    print("Pop item:", d.popitem())
    print("Dict after .popitem():", d)
    
    # Update dict items from items in other dict
    d.update({"x": 1337}.items())
    print("Dict after .update():", d)

    # Use setdefault to set a value if it does not already exist in the dict
    print("setdefault for existing key 'x' returns:", d.setdefault("x", "DEF_val"))
    print("setdefault for new key 'new' returns:", d.setdefault("new", "DEF_val"))

    # Get a shallow copy of a dict. Note the use of .items() to get keys and values
    new_d = dict(d.items())

    env.exit(0)
```

Compile and run:
```sh
actonc dicts.act
./dicts
```

Output:
```sh
Building project in /home/user/foo
  Compiling example.act for release
   Finished compilation in   0.122 s
  Final compilation step
   Finished final compilation step in  13.381 s
Dict: {'foo':1, 'bar':2, 'x':42, 'cookie':3}
len : 4
item foo: 1
item foo: 1
get value when key nonexistent: None
get_def value when key nonexistent: DEF_val
Dict: {'bar':42, 'x':42, 'cookie':3}
Dict keys: ['bar', 'x', 'cookie']
Dict values: [42, 42, 3]
Dict items:
  dict key bar=42
  dict key x=42
  dict key cookie=3
Pop item with key 'bar': 42
Pop item: ('cookie', 3)
Dict after .popitem(): {'x':42}
Dict after .update(): {'x':1337}
setdefault for existing key 'x' returns: 1337
setdefault for new key 'new' returns: DEF_val
```

## Dictionary Comprehensions

Dictionary comprehensions provide a concise way to create dictionaries from sequences or other iterables.

Source:
```python
actor main(env):
    # Basic dict comprehension
    squares = {x: x * x for x in range(5)}
    print("Squares dict:", squares)
    
    # With condition
    even_squares = {x: x * x for x in range(10) if x % 2 == 0}
    print("Even squares:", even_squares)
    
    # Creating from a list of tuples
    pairs = [(1, "one"), (2, "two"), (3, "three")]
    num_to_word = {k: v for (k, v) in pairs}
    print("Number to word:", num_to_word)
    
    # Swapping keys and values
    word_to_num = {v: k for (k, v) in pairs}
    print("Word to number:", word_to_num)
    
    # String manipulation
    words = ["hello", "world", "acton"]
    word_lengths = {w: len(w) for w in words}
    print("Word lengths:", word_lengths)
    
    # Filtering and transforming
    long_words = {w.upper(): w for w in words if len(w) > 4}
    print("Long words map:", long_words)
    
    # Using enumerate
    indexed = {i: v for (i, v) in enumerate(words)}
    print("Indexed words:", indexed)

    env.exit(0)
```

Output:
```
Squares dict: {0: 0, 1: 1, 2: 4, 3: 9, 4: 16}
Even squares: {0: 0, 2: 4, 4: 16, 6: 36, 8: 64}
Number to word: {1: 'one', 2: 'two', 3: 'three'}
Word to number: {'one': 1, 'two': 2, 'three': 3}
Word lengths: {'hello': 5, 'world': 5, 'acton': 5}
Long words map: {'HELLO': 'hello', 'WORLD': 'world', 'ACTON': 'acton'}
Indexed words: {0: 'hello', 1: 'world', 2: 'acton'}
