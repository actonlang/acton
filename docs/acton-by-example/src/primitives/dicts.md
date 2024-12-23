# Dictionaries

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
   Finished compilation in   0.112 s
  Final compilation step
   Finished final compilation step in  15.626 s
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
```
