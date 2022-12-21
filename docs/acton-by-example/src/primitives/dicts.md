# Dictionaries

Source:
```python
actor main(env):
    d = {"foo": 1, "bar": 2}
    d["cookie"] = 3

    print("Dict: " + str(d))
    print("len : " + str(len(d)))
    print("item foo: " + str(d["foo"]))
    
    d["bar"] = 42
    del d["foo"]
    print("Dict: " + str(d))
    
    print("Dict keys: " + str(list(d.keys())))
    print("Dict values: " + str(list(d.values())))

    print("Dict items:")
    for k, v in d.items():
        print("  dict key " + k + "=" + str(v))

    await async env.exit(0)
```

Compile and run:
```sh
actonc dicts.act
./dicts
```

Output:
```sh
Dict: {foo:1, bar:2, cookie:3}
len : 3
item foo: 1
Dict: {bar:42, cookie:3}
Dict keys: [bar, cookie]
Dict values: [42, 3]
Dict items:
  dict key bar=42
  dict key cookie=3
```
