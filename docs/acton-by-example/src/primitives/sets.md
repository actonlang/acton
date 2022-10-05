# Sets

Source:
```python
actor main(env):
    s = {"foo", "foo"}
    print(s)
    s.add("bar")
    print(s)
    print(len(s))
    print(s)

    await async env.exit(0)
```

Compile and run:
```sh
actonc sets.act
./sets
```

Output:
```sh
{foo}
{bar, foo}
2
{bar, foo}
```
