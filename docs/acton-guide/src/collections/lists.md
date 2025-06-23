# Lists

Lists in Acton are ordered mutable sequences<span class="beginner-inline">, similar to a to-do list where items have a specific order</span>. Lists can only contain values of one type at a time<span class="advanced-inline"> due to Acton's static type system</span>.

<div class="beginner-content">

Think of a list like a row of boxes, where each box can hold one item. All boxes must hold the same type of thing - you can't mix apples and hammers!

</div>

## Creating Lists

<table class="side-by-side-code">
<tr>
<td>
Create a list with initial values
</td>
<td>

```python
fruits = ["apple", "banana", "orange"]
```

</td>
</tr>
<tr>
<td>
Create an empty list<span class="advanced-inline"> - type will be inferred when you add the first item</span>
</td>
<td>

```python
numbers = []
```

</td>
</tr>
</table>

<div class="advanced-content">

Lists in Acton are implemented as dynamic arrays with amortized O(1) append operations. The underlying storage grows geometrically to maintain performance characteristics.

</div>

## Basic Operations

<table class="side-by-side-code">
<tr>
<td>
Create a list and add items
</td>
<td>

```python
l = ["foo", "foo"]
l.append("bar")
l.insert(0, "Firsty")
l.append("banana")
```

</td>
</tr>
<tr>
<td>
Access items by index<span class="beginner-inline"> (counting starts at 0)</span>
</td>
<td>

```python
# Indexing starts at 0
print("First item: " + l[0])          # First item: Firsty
# Negative index counts from the end
print("Last item : " + l[-1])         # Last item : banana
```

</td>
</tr>
<tr>
<td>
Print the entire list<span class="beginner-inline"> - must convert to string first</span>
</td>
<td>

```python
# Note: explicitly cast list to str before printing
print("List items: " + str(l))
# List items: ['Firsty', 'foo', 'foo', 'bar', 'banana']
```

</td>
</tr>
<tr>
<td>
Get a slice of the list
</td>
<td>

```python
# [start:stop] where start is inclusive, stop is exclusive
print("A slice   : " + str(l[2:4]))
# A slice   : ['foo', 'bar']
```

</td>
</tr>
</table>

<div class="beginner-content">

**Common mistake:** Remember that indexing starts at 0, not 1! So the first item is at position 0, the second at position 1, and so on.

**Slicing:** The slice `l[start:stop]` includes the item at `start` but excludes the item at `stop`. This is the same behavior as Python.

</div>

## Modifying Lists

<table class="side-by-side-code">
<tr>
<td>
Add items to the end<span class="advanced-inline"> - O(1) amortized time</span>
</td>
<td>

```python
l.append("new item")
```

</td>
</tr>
<tr>
<td>
Insert at a specific position<span class="advanced-inline"> - O(n) time complexity</span>
</td>
<td>

```python
l.insert(0, "first")  # 0 = first position
```

</td>
</tr>
<tr>
<td>
Remove and return an item
</td>
<td>

```python
print("Pop first item:", l.pop(0))    # Pop first item: Firsty
print("Pop last item:", l.pop(-1))    # Pop last item: banana
print("List items:", str(l))          # List items: ['foo', 'foo', 'bar']
```

</td>
</tr>
<tr>
<td>
Extend a list with another list
</td>
<td>

```python
l.extend(["apple", "orange"])
print("List items:", str(l))
# List items: ['foo', 'foo', 'bar', 'apple', 'orange']
```

</td>
</tr>
</table>

## List Utilities

<table class="side-by-side-code">
<tr>
<td>
Sort a list (returns a new list)
</td>
<td>

```python
unsorted_list = [9, 5, 123, 14, 1]
sl = sorted(unsorted_list)
print("Sorted list", str(sl))         # Sorted list [1, 5, 9, 14, 123]
```

</td>
</tr>
<tr>
<td>
Reverse a list in-place
</td>
<td>

```python
l.reverse()
print("Reversed:", l)
# Reversed: ['orange', 'apple', 'bar', 'foo', 'foo']
```

</td>
</tr>
<tr>
<td>
Copy a list<span class="advanced-inline"> - creates a shallow copy</span>
</td>
<td>

```python
l2 = l.copy()
print("Copy:", l2)
# Copy: ['orange', 'apple', 'bar', 'foo', 'foo']
```

</td>
</tr>
<tr>
<td>
Clear all items from a list
</td>
<td>

```python
l.clear()
print("List after clear:", str(l))    # List after clear: []

env.exit(0)
```

</td>
</tr>
</table>

<div class="advanced-content">

**Performance characteristics:**
- `append()` - O(1) amortized (may occasionally resize)
- `insert()` - O(n) as elements need to be shifted
- `pop()` - O(1) for last element, O(n) for arbitrary index
- `extend()` - O(k) where k is the length of the added list
- `reverse()` - O(n) in-place operation
- `copy()` - O(n) creates new list

</div>

## List Comprehensions

List comprehensions provide a concise way to create lists based on existing sequences or ranges<span class="beginner-inline">. They're like a recipe for making a new list from an existing one!</span>

<div class="beginner-content">

List comprehensions might look complex at first, but they follow a simple pattern:
`[expression for item in sequence]` - "give me expression for each item in sequence"

</div>

<table class="side-by-side-code">
<tr>
<td>
Create a list of squares<span class="beginner-inline"> (multiply each number by itself)</span>
</td>
<td>

```python
squares = [x * x for x in range(5)]
# Result: [0, 1, 4, 9, 16]
```

</td>
</tr>
<tr>
<td>
Filter with a condition
</td>
<td>

```python
evens = [x for x in range(10) if x % 2 == 0]
# Result: [0, 2, 4, 6, 8]
```

</td>
</tr>
<tr>
<td>
Multiple iterators<span class="advanced-inline"> (cartesian product)</span>
</td>
<td>

```python
pairs = [(x, y) for x in range(3) for y in range(2)]
# Result: [(0,0), (0,1), (1,0), (1,1), (2,0), (2,1)]
```

</td>
</tr>
</table>

### Advanced Comprehensions

<table class="side-by-side-code">
<tr>
<td>
Nested list comprehension
</td>
<td>

```python
matrix = [[i * j for i in range(4)] for j in range(3)]
print("Matrix:")
for row in matrix:
    print("  ", row)
# Matrix:
#    [0, 0, 0, 0]
#    [0, 1, 2, 3]
#    [0, 2, 4, 6]
```

</td>
</tr>
<tr>
<td>
Transform existing list
</td>
<td>

```python
words = ["hello", "world", "acton"]
lengths = [len(w) for w in words]
print("Word lengths:", lengths)        # Word lengths: [5, 5, 5]
```

</td>
</tr>
<tr>
<td>
Filter and transform
</td>
<td>

```python
long_caps = [w.upper() for w in words if len(w) > 4]
print("Long words in caps:", long_caps)
# Long words in caps: ['HELLO', 'WORLD', 'ACTON']
```

</td>
</tr>
</table>

<div class="advanced-content">

**Performance tip:** When using multiple iterators with filtering, place filters as early as possible:
```python
# Good - filters early, reduces iterations
result = [x + y for x in big_list if condition(x) for y in small_list]

# Bad - filters late, wastes computation
result = [x + y for x in big_list for y in small_list if condition(x)]
```

</div>

## Type Safety

All items in a list must be of the same type. Mixing types like `["foo", 1]` will result in a compiler error.

<table class="side-by-side-code">
<tr>
<td>
Valid: homogeneous list
</td>
<td>

```python
strings = ["foo", "bar", "baz"]
numbers = [1, 2, 3, 4, 5]
```

</td>
</tr>
<tr>
<td>
Invalid: mixed types
</td>
<td>

```python
# This will not compile!
mixed = ["foo", 1, True]
```

</td>
</tr>
</table>