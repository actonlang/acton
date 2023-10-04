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
    
    print("Pop first item:", l.pop(0))
    print("Pop last item:", l.pop(-1))
    print("List items:", str(l))
    
    # Extend a list by adding another list to it
    l.extend(["apple", "orange"])
    print("List items:", str(l))
    
    unsorted_list = [9, 5, 123, 14, 1]
    sl = sorted(unsorted_list)
    print("Sorted list", str(sl))

    # Reverse a list inplace
    l.reverse()
    print("Reversed:", l)
    
    # Get a shallow copy of the list
    l2 = l.copy()
    print("Copy:", l2)
    
    # Clear list
    l.clear()
    print("List after clear:", str(l))

    env.exit(0)
```

Compile and run:
```sh
actonc lists.act
./lists
```

Output:
```sh
First item: Firsty
Last item : banana
List items: ['Firsty', 'foo', 'foo', 'bar', 'banana']
A slice   : ['foo', 'bar']
Pop first item: Firsty
Pop last item: banana
List items: ['foo', 'foo', 'bar']
List items: ['foo', 'foo', 'bar', 'apple', 'orange']
Sorted list [1, 5, 9, 14, 123]
Reversed: ['orange', 'apple', 'bar', 'foo', 'foo']
Copy: ['orange', 'apple', 'bar', 'foo', 'foo']
List after clear: []
```

- All items in a list must be of the same type
  - It is not allowed to mix, like `["foo", 1]` leads to a compiler error
