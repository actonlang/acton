# Set Comprehensions

Set comprehensions create sets containing unique elements from iterables, automatically eliminating duplicates.

Source:
```python
actor main(env):
    # Basic set comprehension
    numbers = {x for x in range(5)}
    print("Numbers:", str(numbers))
    
    # Transform elements (unique values only)
    remainders = {x % 3 for x in range(10)}
    print("Remainders:", str(remainders))
    
    # Filter with condition  
    even_squares = {x * x for x in range(10) if x % 2 == 0}
    print("Even squares:", str(even_squares))
    
    # Remove duplicates from list
    duplicated = [1, 2, 2, 3, 3, 3, 4, 4, 4, 4]
    unique = {x for x in duplicated}
    print("Unique elements:", str(unique))
    
    # Multiple for clauses with uniqueness
    products = {x * y for x in range(1, 4) for y in range(1, 4)}
    print("Products:", str(products))
    
    # Character set from strings
    text = "hello world"
    chars = {c for c in text if c != ' '}
    print("Characters:", str(chars))
    
    # Pattern matching
    pairs = [(1, 2), (3, 4), (1, 5), (3, 6)]
    first_elements = {a for (a, b) in pairs}
    print("First elements:", str(first_elements))

    env.exit(0)
```

Compile and run:
```sh
actonc set-comprehensions.act
./set-comprehensions
```

Output:
```sh
Numbers: {0, 1, 2, 3, 4}
Remainders: {0, 1, 2}
Even squares: {0, 4, 16, 36, 64}
Unique elements: {1, 2, 3, 4}
Products: {1, 2, 3, 4, 6, 8, 9, 12}
Characters: {'d', 'e', 'h', 'l', 'o', 'r', 'w'}
First elements: {1, 3}
```

## Implementation and Effects

Set comprehensions are internally rewritten to loop-style code with set operations, ensuring both efficiency and automatic deduplication. Like other comprehensions, the effect is determined by the expressions used - pure expressions result in pure comprehensions, while expressions with `mut` effects make the entire comprehension `mut`.

```python
# Pure set comprehension
pure_set = {x * 2 for x in range(5)}

# The effect depends on what's called within the comprehension
# mut_set = {process_item(x) for x in data}  # mut if process_item is mut
```