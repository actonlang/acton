# Lists

Source:
```python
actor main(env):
    l = ["foo", "foo"]
    l.append("bar")
    l.insert(0, "Firsty")
    l.append("banana")

    # Indexing starts at 0
    print("First item: " + l[0])
    # Negative index counts from the end
    print("Last item : " + l[-1])

    # Note how we need to explicitly cast the list to str before printing
    print("List items: " + str(l))
    # We can take a slice of a list with [start:stop] where the start index is
    # inclusive but the stop index is exclusive, just like in Python.
    # A slice of a list is also a list, so cast to str.
    print("A slice   : " + str(l[2:4]))

    await async env.exit(0)
```

Compile and run:
```sh
actonc lists.act --root main
./lists
```

Output:
```sh
First item: Firsty
Last item : banana
List items: [Firsty, foo, foo, bar, banana]
A slice   : [foo, bar]
```


- All items in a list must be of the same type
  - It is not allowed to mix, like `["foo", 1]` leads to a compiler error
