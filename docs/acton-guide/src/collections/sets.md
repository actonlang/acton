# Sets

Sets store unique values. Use them when you care about membership or
deduplication more than order.

<div class="beginner-content">
<p>A set is like a bag of distinct items. Adding the same value twice
does not create a duplicate. If you only need to know whether something
is present, a set usually fits better than a list.</p>
</div>

## Creating sets

```python
tags = {"docs", "guide", "acton"}
empty_tags: set[str] = set()
```

Use `{...}` for a non-empty set. Use `set()` for an empty one, because
`{}` means an empty dictionary.

## Checking and updating

```python
tags = {"docs", "guide"}

print("docs" in tags)
print("api" not in tags)

tags.add("api")
tags.add("docs")
tags.discard("guide")
tags.update({"reference", "tutorial"})

print(tags)
print(len(tags))
```

`add()` inserts one value. `discard()` removes a value if it is present
and does nothing if it is not. `update()` adds values from another set
or any iterable of values.

## Removing values

```python
tags = {"docs", "guide", "acton"}

print(tags.pop())
print(tags)
```

`pop()` removes and returns one element. On an empty set it raises an
exception, so check before calling it if the set may be empty.

## Iteration and order

```python
tags = {"docs", "guide", "acton"}

for tag in tags:
    print(tag)

print(sorted(tags))
```

Sets do not preserve order. Do not rely on the printed order when the
exact sequence matters. If you need stable display order, sort the set
first.

## Set comprehensions

```python
words = ["hello", "world", "hello", "acton"]
unique_words = {word for word in words}
long_words = {word.upper() for word in words if len(word) > 4}
remainder_classes = {n % 3 for n in range(10)}
```

Set comprehensions are a compact way to build a set while automatically
removing duplicates.
