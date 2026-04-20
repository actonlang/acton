# Lists

Lists are ordered, mutable sequences. Use them when position matters or
when you want to keep adding and removing items over time.

<div class="beginner-content">
<p>Think of a list as a numbered row of slots. Slot <code>0</code> is
the first item, slot <code>1</code> is the second, and so on. A list
holds one element type, so a <code>list[str]</code> is for strings
only.</p>
</div>

## Creating lists

```python
fruits = ["apple", "banana", "orange"]
tasks: list[str] = []
numbers: list[int] = [1, 2, 3]
```

Use a literal when you already have values. Use an empty list when you
plan to fill it later. If the compiler cannot infer the element type
from context, add an annotation.

<div class="advanced-content">
<p>Lists are dynamic arrays. Appending is cheap most of the time because
storage grows in chunks, not one item at a time.</p>
</div>

## Reading values

```python
items = ["first", "second", "third", "fourth"]

print(items[0])
print(items[-1])
print(items[1:3])
print(len(items))
print("second" in items)
print(items.index("third"))
```

Indexing starts at `0`. Negative indexes count from the end. Slices use
the familiar `start:stop` form and include the start but exclude the
stop.

<div class="beginner-content">
<p>If you are coming from a one-based indexing language, the first item
is still at position <code>0</code> here.</p>
</div>

## Updating lists

```python
items = ["first", "second", "third"]

items.append("fourth")
items.insert(1, "new")
items.extend(["fifth", "sixth"])

print(items)
print(items.pop())
print(items.pop(0))
del items[1]
```

`append()` adds one item to the end. `insert()` places an item at a
specific index. `extend()` adds several items from another iterable.
`pop()` removes and returns an item, and `del items[i]` removes by
index without returning anything.

<div class="advanced-content">
<p>Appends are amortized O(1). Inserting or deleting near the front of a
long list is O(n) because elements need to shift. `pop()` is O(1) at the
end and O(n) at other positions.</p>
</div>

## Common utilities

```python
values = [9, 5, 123, 14, 1, 5]

print(sorted(values))
values.reverse()
print(values)
print(values.count(5))

copy_of_values = values.copy()
values.clear()
print(copy_of_values)
print(values)
```

`sorted()` returns a new list. `reverse()` changes the existing list in
place. `count()` scans the whole list and counts matches. `copy()` makes
a shallow copy, which is enough when the elements themselves are simple
values.

<div class="advanced-content">
<p>If the list contains mutable values, a shallow copy only duplicates
the outer list. The items inside are still shared.</p>
</div>

## Iterating over lists

```python
names = ["Ada", "Bjarne", "Grace"]

for name in names:
    print(name)

for i, name in enumerate(names):
    print(i, name)
```

Iteration gives you each item in order. Use `enumerate()` when you also
need the current index.

## List comprehensions

```python
numbers = [1, 2, 3, 4, 5]
squares = [n * n for n in numbers]
evens = [n for n in numbers if n % 2 == 0]
```

List comprehensions are the compact way to build a new list from an
existing iterable. Read them as "make a list of this expression for
each item that matches the condition".

## Type safety

All items in a list must be of the same type. Mixing types like
`["foo", 1, True]` will not compile.

```python
strings = ["foo", "bar", "baz"]
numbers = [1, 2, 3, 4, 5]
```

When a list is empty, give it a type if the surrounding code does not
make the element type obvious.
