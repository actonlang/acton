# List Comprehensions

List comprehensions provide a concise way to create lists based on existing iterables.

Source:
```python
actor main(env):
    # Basic list comprehension
    numbers = [x for x in range(5)]
    print("Numbers:", str(numbers))
    
    # Transform elements
    squares = [x * x for x in range(5)]
    print("Squares:", str(squares))
    
    # Filter with condition
    evens = [x for x in range(10) if x % 2 == 0]
    print("Even numbers:", str(evens))
    
    # Multiple for clauses
    pairs = [x + y for x in range(3) for y in range(3)]
    print("Pairs:", str(pairs))
    
    # Pattern matching with tuples
    data = [(1, 2), (3, 4), (5, 6)]
    firsts = [a for (a, b) in data]
    print("First elements:", str(firsts))
    
    # Variable capture from outer scope
    multiplier = 10
    scaled = [x * multiplier for x in range(3)]
    print("Scaled:", str(scaled))
    
    # Nested list comprehensions
    matrix = [[i * j for i in range(3)] for j in range(3)]
    print("Matrix:", str(matrix))
    
    # Complex expression
    words = ["hello", "world", "acton"]
    lengths = [len(word) for word in words]
    print("Word lengths:", str(lengths))

    env.exit(0)
```

Compile and run:
```sh
actonc list-comprehensions.act
./list-comprehensions
```

Output:
```sh
Numbers: [0, 1, 2, 3, 4]
Squares: [0, 1, 4, 9, 16]
Even numbers: [0, 2, 4, 6, 8]
Pairs: [0, 1, 2, 1, 2, 3, 2, 3, 4]
First elements: [1, 3, 5]
Scaled: [0, 10, 20]
Matrix: [[0, 0, 0], [0, 1, 2], [0, 2, 4]]
Word lengths: [5, 5, 5]
```

## Implementation and Effects

List comprehensions are internally rewritten to loop-style code, making them as fast and efficient as traditional loops. The comprehension's effect is determined by the expressions within it - if all expressions are pure, the comprehension is pure. If any expression has a `mut` effect, the entire comprehension will have a `mut` effect.

```python
# Pure comprehension - no side effects
pure_squares = [x * x for x in range(5)]

# Mut comprehension - if calling functions with mut effects
# mut_results = [some_mut_function(x) for x in data]
```