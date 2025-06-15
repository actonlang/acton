# Comprehensions

Comprehensions provide a concise way to create lists, sets, and dictionaries in Acton. They offer a more readable and often more efficient alternative to using loops for creating collections.

## List Comprehensions

Source:
```python
actor main(env):
    # Basic list comprehension
    squares = [x * x for x in range(5)]
    print("Squares:", squares)
    
    # List comprehension with condition
    even_squares = [x * x for x in range(10) if x % 2 == 0]
    print("Even squares:", even_squares)
    
    # Multiple for clauses
    pairs = [(i, j) for i in range(3) for j in range(2)]
    print("Pairs:", pairs)
    
    # Nested list comprehension
    matrix = [[i * j for i in range(3)] for j in range(3)]
    print("Matrix:", matrix)
    
    # Using outer variables
    multiplier = 5
    multiples = [x * multiplier for x in range(4)]
    print("Multiples of 5:", multiples)
    
    env.exit(0)
```

Compile and run:
```sh
actonc list_comprehensions.act
./list_comprehensions
```

Output:
```sh
Squares: [0, 1, 4, 9, 16]
Even squares: [0, 4, 16, 36, 64]
Pairs: [(0, 0), (0, 1), (1, 0), (1, 1), (2, 0), (2, 1)]
Matrix: [[0, 0, 0], [0, 1, 2], [0, 2, 4]]
Multiples of 5: [0, 5, 10, 15]
```

## Set Comprehensions

Source:
```python
actor main(env):
    # Basic set comprehension
    unique_squares = {x * x for x in range(5)}
    print("Unique squares:", unique_squares)
    
    # Set comprehension with condition
    even_numbers = {x for x in range(10) if x % 2 == 0}
    print("Even numbers:", even_numbers)
    
    # Remove duplicates using set comprehension
    words = ["hello", "world", "hello", "acton"]
    unique_lengths = {len(word) for word in words}
    print("Unique word lengths:", unique_lengths)
    
    env.exit(0)
```

Compile and run:
```sh
actonc set_comprehensions.act
./set_comprehensions
```

Output:
```sh
Unique squares: {0, 1, 4, 9, 16}
Even numbers: {0, 2, 4, 6, 8}
Unique word lengths: {5, 4}
```

## Dictionary Comprehensions

Source:
```python
actor main(env):
    # Basic dictionary comprehension
    square_dict = {x: x * x for x in range(5)}
    print("Square dict:", square_dict)
    
    # Dictionary comprehension with condition
    even_square_dict = {x: x * x for x in range(10) if x % 2 == 0}
    print("Even square dict:", even_square_dict)
    
    # Creating dictionary from two lists
    keys = ["a", "b", "c"]
    values = [1, 2, 3]
    combined = {k: v for k, v in zip(keys, values)}
    print("Combined dict:", combined)
    
    # String transformations
    words = ["hello", "world", "acton"]
    word_lengths = {word: len(word) for word in words}
    print("Word lengths:", word_lengths)
    
    env.exit(0)
```

Compile and run:
```sh
actonc dict_comprehensions.act
./dict_comprehensions
```

Output:
```sh
Square dict: {0: 0, 1: 1, 2: 4, 3: 9, 4: 16}
Even square dict: {0: 0, 2: 4, 4: 16, 6: 36, 8: 64}
Combined dict: {'a': 1, 'b': 2, 'c': 3}
Word lengths: {'hello': 5, 'world': 5, 'acton': 5}
```

## Pattern Matching in Comprehensions

Source:
```python
actor main(env):
    # Destructuring tuples in comprehensions
    data = [(1, 2), (3, 4), (5, 6)]
    
    # Extract first elements
    firsts = [a for (a, b) in data]
    print("First elements:", firsts)
    
    # Extract second elements
    seconds = [b for (a, b) in data]
    print("Second elements:", seconds)
    
    # Sum of pairs
    sums = [a + b for (a, b) in data]
    print("Sums:", sums)
    
    env.exit(0)
```

Compile and run:
```sh
actonc pattern_comprehensions.act
./pattern_comprehensions
```

Output:
```sh
First elements: [1, 3, 5]
Second elements: [2, 4, 6]
Sums: [3, 7, 11]
```

## Complex Example

Source:
```python
actor main(env):
    # Comprehensive example combining different features
    
    # Generate prime numbers using comprehension
    def is_prime(n: int) -> bool:
        if n < 2:
            return False
        for i in range(2, int(n ** 0.5) + 1):
            if n % i == 0:
                return False
        return True
    
    # Find prime numbers up to 30
    primes = [n for n in range(2, 31) if is_prime(n)]
    print("Primes up to 30:", primes)
    
    # Create a dict mapping each prime to its square
    prime_squares = {p: p * p for p in primes if p < 20}
    print("Prime squares < 20:", prime_squares)
    
    # Flatten nested structure with comprehensions
    matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    flattened = [item for row in matrix for item in row]
    print("Flattened matrix:", flattened)
    
    # Filter and transform in one step
    positive_even_squares = [x * x for x in range(-10, 11) if x > 0 and x % 2 == 0]
    print("Positive even squares:", positive_even_squares)
    
    env.exit(0)
```

Compile and run:
```sh
actonc complex_comprehensions.act
./complex_comprehensions
```

Output:
```sh
Primes up to 30: [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
Prime squares < 20: {2: 4, 3: 9, 5: 25, 7: 49, 11: 121, 13: 169, 17: 289, 19: 361}
Flattened matrix: [1, 2, 3, 4, 5, 6, 7, 8, 9]
Positive even squares: [4, 16, 36, 64, 100]
```

## Key Features

- **List comprehensions**: `[expr for item in iterable]` - Create lists
- **Set comprehensions**: `{expr for item in iterable}` - Create sets (automatically handle duplicates)  
- **Dict comprehensions**: `{key: value for item in iterable}` - Create dictionaries
- **Conditional filtering**: Add `if condition` to filter items
- **Multiple iterations**: Use multiple `for` clauses to iterate over multiple sequences
- **Pattern matching**: Destructure tuples and other structures directly in the comprehension
- **Variable capture**: Access variables from the enclosing scope
- **Nested comprehensions**: Use comprehensions inside other comprehensions

Comprehensions are generally more efficient and readable than equivalent loop-based code for creating collections.