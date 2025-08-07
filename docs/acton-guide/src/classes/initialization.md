# Class Initialization

All class attributes must be initialized in the `__init__` method. This ensures that objects are always in a valid state after construction.

We can think of `__init__` as having two parts:
1. **Constructor part**: Must fully initialize the object through unconditional simple logic
2. **Post-constructor part**: Can perform other logic once the object is fully initialized, calling methods on `self` or passing references to `self`

```python
class BankAccount(object):
    owner: str
    balance: float
    transaction_log: list[str]

    def __init__(self, owner: str, initial_deposit: float):
        # Constructor part: Initialize all attributes first
        self.owner = owner
        self.balance = initial_deposit
        self.transaction_log = []

        # Post-constructor part: Now we can use methods and pass self
        self.log_transaction("Account opened")
        register_account(self)  # OK to pass self - object is fully initialized
        if initial_deposit > 10000:
            flag_for_review(self)

    def log_transaction(self, msg: str):
        self.transaction_log.append(msg)
```

## Conditional branches and `raise`

if / elif / else are valid as long as all branches initialize all attributes. `raise` will not create the new object and is thus seen as en exception to the unconditional initialization check.

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

## Loops are not allowed

Loops (`for` and `while`) cannot be used in the constructor part because we cannot determine at compile time whether they will execute:

```python
class BadExample(object):
    values: list[int]
    first: int

    def __init__(self, data: list[int]):
        self.values = data

        # ERROR: Loop might not execute if data is empty
        for item in data:
            self.first = item
            break
```

The fix: initialize attributes unconditionally, then use loops in the post-constructor part:

```python
class GoodExample(object):
    values: list[int]
    first: int

    def __init__(self, data: list[int]):
        # Constructor part
        self.values = data
        self.first = data[0] if data else 0  # Unconditional initialization

        # Post-constructor part can use loops
        for item in data:
            self.process_item(item)
```

## Parent Class `__init__`

Initialize parent attributes by calling the parent's `__init__`:

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
