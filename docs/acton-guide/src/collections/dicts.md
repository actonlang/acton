# Dictionaries

Dictionaries map keys to values. They keep insertion order, so when you
iterate over a dictionary you get keys back in the order they were first
added.

<div class="beginner-content">
<p>Use a dictionary when you want to look something up by name, id, or
some other key. If you keep searching through a list for a matching
value, a dictionary is often the better shape.</p>
</div>

## Creating dictionaries

```python
counts = {"apples": 2, "bananas": 4}
empty_counts: dict[str, int] = {}
```

The key type and value type are fixed for a given dictionary. When a
dictionary is empty, give it a type if the surrounding code does not
make that obvious.

## Looking up values

```python
counts = {"apples": 2, "bananas": 4}

print(counts["apples"])
print(counts.get("pears"))
print(counts.get_def("pears", 0))
print("apples" in counts)
print(len(counts))
```

Direct indexing says the key must already exist. `get()` returns `None`
when a key is missing. `get_def()` lets you provide a default value
instead of handling `None` later.

<div class="advanced-content">
<p>`None` is a real value, so `get()` is best when missing keys are
normal and uninteresting. If `None` means something in your data,
`get_def()` or an explicit membership test is clearer.</p>
</div>

## Updating entries

```python
counts = {"apples": 2, "bananas": 4}

counts["bananas"] = 5
counts["pears"] = 1
del counts["apples"]

print(counts)
```

Assigning to a key adds it if it is new or replaces the old value if it
already exists.

## Removing entries

```python
counts = {"apples": 2, "bananas": 4, "pears": 1}

print(counts.pop("bananas"))
print(counts.pop_def("missing", 0))
print(counts.popitem())
```

`pop()` removes a key and returns its value. `pop_def()` does the same
thing with a fallback when the key is absent. `popitem()` removes and
returns one key/value pair.

## Iterating over dictionaries

```python
counts = {"apples": 2, "bananas": 4, "pears": 1}

for key in counts:
    print(key)

for key, value in counts.items():
    print(key, value)

print(list(counts.keys()))
print(list(counts.values()))
print(list(counts.items()))
```

Iterating over a dictionary yields keys. Use `items()` when you need
both the key and the value.

## Updating from other data

```python
counts = {"apples": 2}
more_counts = {"bananas": 4, "pears": 1}

counts.update(more_counts.items())
counts.setdefault("apples", 0)
counts.setdefault("grapes", 3)
```

`update()` merges entries from another iterable of key/value pairs.
`setdefault()` is useful when you want to add a missing key only once
and keep the existing value otherwise.

## Dictionary comprehensions

```python
words = ["hello", "world", "acton"]
lengths = {word: len(word) for word in words}
filtered = {word: len(word) for word in words if len(word) > 4}
indexed = {i: word for (i, word) in enumerate(words)}
```

Dictionary comprehensions are the concise way to build a dictionary
from an iterable. They are useful when the new keys and values come
from the old data directly.
