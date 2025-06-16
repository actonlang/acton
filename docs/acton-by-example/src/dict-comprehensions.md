# Dict Comprehensions

Dict comprehensions create dictionaries by specifying key-value pairs based on iterables.

Source:
```python
actor main(env):
    # Basic dict comprehension
    squares = {x: x * x for x in range(5)}
    print("Squares:", str(squares))
    
    # String keys
    str_numbers = {str(x): x for x in range(3)}
    print("String keys:", str(str_numbers))
    
    # Transform both keys and values
    doubled = {x * 2: x * 3 for x in range(1, 4)}
    print("Doubled keys, tripled values:", str(doubled))
    
    # Filter with condition
    even_cubes = {x: x * x * x for x in range(10) if x % 2 == 0}
    print("Even cubes:", str(even_cubes))
    
    # From existing data structures
    words = ["apple", "banana", "cherry"]
    word_lengths = {word: len(word) for word in words}
    print("Word lengths:", str(word_lengths))
    
    # Invert a dictionary
    original = {"a": 1, "b": 2, "c": 3}
    inverted = {v: k for k, v in original.items()}
    print("Inverted:", str(inverted))
    
    # Multiple for clauses
    coord_sums = {f"{x},{y}": x + y for x in range(2) for y in range(2)}
    print("Coordinate sums:", str(coord_sums))
    
    # Pattern matching with tuples
    data = [("name", "Alice"), ("age", 30), ("city", "Stockholm")]
    info = {key: value for (key, value) in data}
    print("Info dict:", str(info))
    
    # Variable capture
    base = 10
    powers = {x: base ** x for x in range(4)}
    print("Powers of 10:", str(powers))

    env.exit(0)
```

Compile and run:
```sh
actonc dict-comprehensions.act
./dict-comprehensions
```

Output:
```sh
Squares: {0: 0, 1: 1, 2: 4, 3: 9, 4: 16}
String keys: {'0': 0, '1': 1, '2': 2}
Doubled keys, tripled values: {2: 3, 4: 6, 6: 9}
Even cubes: {0: 0, 2: 8, 4: 64, 6: 216, 8: 512}
Word lengths: {'apple': 5, 'banana': 6, 'cherry': 6}
Inverted: {1: 'a', 2: 'b', 3: 'c'}
Coordinate sums: {'0,0': 0, '0,1': 1, '1,0': 1, '1,1': 2}
Info dict: {'name': 'Alice', 'age': 30, 'city': 'Stockholm'}
Powers of 10: {0: 1, 1: 10, 2: 100, 3: 1000}
```

## Implementation and Effects

Dict comprehensions are internally rewritten to loop-style code with dictionary operations. The comprehension's effect is based on the key and value expressions - if both are pure, the comprehension is pure. If either expression calls functions with `mut` effects, the entire comprehension will have a `mut` effect.

```python
# Pure dict comprehension
pure_dict = {x: x * 2 for x in range(5)}

# Effect depends on expressions used in keys or values
# mut_dict = {process_key(x): process_value(x) for x in data}  # mut if either function is mut
```