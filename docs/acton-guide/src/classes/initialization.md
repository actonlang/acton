# Class Initialization

All class attributes must be initialized in the `__init__` method to ensure objects are always in a valid state after construction.

We can think of `__init__` as having two parts:
1. **Constructor part**: Must fully initialize the object. Ends when `self` escapes (is passed externally)
2. **Post-constructor part**: Can perform other logic once the object is fully initialized

## The Constructor Part

The constructor part continues until `self` escapes - that is, until we pass a reference to `self` externally. At that point, the object must be fully initialized. Examples of `self` escaping:
- Passing `self` to a function: `register(self)`
- Calling a method on `self`: `self.validate()`
- Passing a method reference: `callback(self.on_event)`
- Returning `self` or raising with `self`

During the constructor part, you can access already-initialized attributes:

```python
class Config(object):
    def __init__(self, base_value: int):
        self.base = base_value
        self.doubled = self.base * 2        # OK: self.base is initialized
        self.quadrupled = self.doubled * 2  # OK: self.doubled is initialized
```

Note that accessing `self.attribute` where the attribute is uninitialized will stop the constructor part, as it indicates an error in initialization order.

## The Post-Constructor Part

Once `self` escapes, we enter the post-constructor part where all attributes must already be initialized:

```python
class BankAccount(object):
    def __init__(self, owner: str, initial_deposit: float):
        # Constructor part: Initialize all attributes
        self.owner = owner
        self.balance = initial_deposit
        self.transaction_log = []

        # Post-constructor: self escapes here, all attributes must be initialized
        self.log_transaction("Account opened")  # Calls method on self
        register_account(self)                  # Passes self to function
        if initial_deposit > 10000:
            flag_for_review(self)
```

## Control Flow in the Constructor Part

### Conditional branches

Conditional branches (if/elif/else) work as long as all branches that complete normally initialize all attributes. Branches that `raise` exceptions don't need to initialize since they abort object creation:

```python
class Rational(object):
    num: int
    denom: int

    def __init__(self, num: int, denom: int):
        if denom == 0:
            raise ValueError("Denominator cannot be zero")
        else:
            if denom > 0:
                self.num = num
                self.denom = denom
            else:
                self.num = -num
                self.denom = -denom
```

### Try/except blocks

Try/except blocks work as long as all paths that complete normally initialize all attributes. The `else` clause executes only when no exception occurs and is part of the normal path. Exception handlers that raise new exceptions don't need to initialize attributes.

### Loops

Loops can be used in the constructor part as long as they don't leak `self`:

```python
class Example(object):
    def __init__(self, data: list[int]):
        # OK: Loop doesn't reference self
        total = 0
        for item in data:
            total += item

        # Initialize attributes after the loop
        self.values = data
        self.computed = total

        # Now can use self in loops (post-constructor)
        for item in data:
            self.process(item)  # This escapes self!
```

## Common Pitfall: Method References

Passing a method reference like `self.method` also causes `self` to escape, even without calling the method:

```python
class Handler(object):
    callback: Callback
    data: int

    def __init__(self):
        # This escapes self! The method reference captures self
        self.callback = Callback(self.on_event)

        # ERROR: This won't be seen as initialized
        # because self already escaped on the line above
        self.data = 42

    def on_event(self):
        pass
```

## Special Cases

Methods with `NotImplemented` bodies are implemented in C and can be called during initialization since they are trusted to not access uninitialized attributes.

To initialize parent class attributes, call the parent's `__init__`:

```python
class Account(object):
    account_id: str
    created_date: str

    def __init__(self, account_id: str):
        self.account_id = account_id
        self.created_date = current_date()

class BankAccount(Account):
    owner: str
    balance: float

    def __init__(self, account_id: str, owner: str, initial_deposit: float):
        Account.__init__(self, account_id)  # Initialize parent attributes
        self.owner = owner
        self.balance = initial_deposit
```
