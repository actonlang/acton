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

## Joining and repeating lists

```python
a = [1, 2]
b = [3]

print(a + b)    # [1, 2, 3]
print(a * 2)    # [1, 2, 1, 2]
```

`+` and `*` return a new list and do not change their operands.

The augmented forms `+=` and `*=` change the list on the left in place, as in
Python. Every name that refers to that list sees the change:

```python
items = ["a"]
alias = items
items += ["b", "c"]
items *= 2
print(alias)    # ['a', 'b', 'c', 'a', 'b', 'c']
```

<div class="advanced-content">
<p><code>items += more</code> copies the elements of <code>more</code>, like
<code>items.extend(more)</code>, so its cost does not grow with the length of
<code>items</code>. <code>items *= n</code> writes only the added copies.</p>
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

## Generator expressions

A generator expression uses parentheses to produce an `Iterator[A]`.
It computes values as they are requested, while a list comprehension
builds the whole list immediately.

```python
numbers = [1, 2, 3, 4, 5]
squares = (n * n for n in numbers if n % 2 == 0)

print(list(squares))    # [4, 16]
print(list(squares))    # []
```

Generators are single-pass: a `for` loop or a consumer such as `list()`
or `sum()` advances the iterator. Once exhausted, it produces no more
values. Construct another generator if you need to repeat the traversal.

When a generator is the only argument to a call, its extra parentheses
can be omitted. Keep them when passing other arguments:

```python
print(sum(n * n for n in numbers))         # 55
print(sum((n * n for n in numbers), 10))    # 65
```

Like comprehensions, generator expressions can have multiple `for` and
`if` clauses. Clauses run from left to right, with each inner loop
traversed for the current outer value:

```python
pairs = ((x, y) for x in range(2) for y in range(3) if x != y)
print(list(pairs))    # [(0, 1), (0, 2), (1, 0), (1, 2)]
```

### Evaluation and captures

The outermost source expression is evaluated and converted to an iterator
when the generator is constructed. Filters, nested source expressions,
and the result expression are evaluated as iteration advances. Errors in
the outer source therefore occur at construction; errors in deferred
expressions occur when those expressions are reached during iteration.

The deferred expressions must be [pure](../types/effects.md). They can
compute values and raise exceptions, but cannot call functions requiring
`mut`, `proc`, or `action` effects. Use an explicit `for` loop when each
step needs those effects.

Local variables and actor state referenced from the surrounding scope
are captured by value when the generator is constructed, as with Acton
lambdas:

```python
factor = 2
scaled = (factor * n for n in [1, 2, 3])
factor = 3
print(list(scaled))    # [2, 4, 6]
```

Capturing a mutable object keeps a reference to that object; it does not
copy its contents. A generator does not take a snapshot of its source
collection.

The compiler can combine generator production and consumption into a
single loop, avoiding an intermediate iterator pipeline while preserving
these evaluation rules.

### Flattening with `flatmap`

`flatmap(f, items)` returns a lazy iterator that calls `f` for each input
and yields all values from the returned iterator before moving to the
next input. Empty inner iterators contribute no values.

```python
groups = [[1, 2], [], [3]]
flattened = flatmap(lambda group: iter(group), groups)
print(list(flattened))    # [1, 2, 3]
```

The input can be any `Iterable[A]`. The callback must be pure and return
an `Iterator[B]`; wrap collections in `iter()` inside the callback. The
result is a single-pass `Iterator[B]`, just like a generator expression
with nested `for` clauses.

## Type safety

All items in a list must be of the same type. Mixing types like
`["foo", 1, True]` will not compile.

```python
strings = ["foo", "bar", "baz"]
numbers = [1, 2, 3, 4, 5]
```

When a list is empty, give it a type if the surrounding code does not
make the element type obvious.
