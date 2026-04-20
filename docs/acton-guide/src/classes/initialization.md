# Class initialization

Use `__init__` to leave the object valid. Before `self` escapes, every
required attribute must already be set.

Read the rule as two steps:

1. Build the object locally.
2. Let `self` out only after construction is complete.

<div class="beginner-content">
<p>Think of <code>__init__</code> as the boundary between "not ready"
and "ready". If another function needs the object, wait until every
required field is assigned.</p>
</div>

`self` escapes when you:

- pass `self` to another function
- call a method on `self`
- capture a bound method such as `self.on_event`
- return `self`
- store `self` somewhere that outlives the constructor

<div class="advanced-content">
<p>The important invariant is not "all assignments happen early" but
"<code>self</code> must not escape before the object is fully
initialized". Once you read the rules that way, the branch, loop,
callback, and base-class cases all follow the same rule.</p>
</div>

## Build the object first

During construction, use local variables for intermediate values and
assign to attributes once the values are ready. If you read from an
attribute before assigning it, the initialization order is wrong.

```python
class Config(object):
    def __init__(self, base_value: int):
        self.base = base_value
        self.doubled = self.base * 2
        self.quadrupled = self.doubled * 2
```

## Let `self` out last

Once the object is complete, you can call helper methods or register the
instance with the rest of the system.

```python
class BankAccount(object):
    def __init__(self, owner: str, initial_deposit: float):
        self.owner = owner
        self.balance = initial_deposit
        self.transaction_log = []

        self.log_transaction("Account opened")
        register_account(self)
        if initial_deposit > 10000:
            flag_for_review(self)
```

## Control flow

Branches and loops are fine as long as every normal path leaves the
object complete.

### Branches

Conditional branches work when every branch that completes normally
initializes the same required attributes. Branches that `raise`
exceptions do not need to finish construction.

```python
class Rational(object):
    num: int
    denom: int

    def __init__(self, num: int, denom: int):
        if denom == 0:
            raise ValueError("Denominator cannot be zero")
        if denom > 0:
            self.num = num
            self.denom = denom
        else:
            self.num = -num
            self.denom = -denom
```

### Try/except

`try`/`except` works the same way. The `else` branch is part of the
normal path, so it must also leave the object initialized.

### Loops

Loops are fine as long as they do not leak `self` before the object is
ready.

```python
class Example(object):
    def __init__(self, data: list[int]):
        total = 0
        for item in data:
            total += item

        self.values = data
        self.computed = total

        for item in data:
            self.process(item)
```

## Common mistake

Passing a method reference like `self.method` also makes `self` escape,
even if the method is not called immediately.

```python
class Handler(object):
    callback: Callback
    data: int

    def __init__(self):
        self.callback = Callback(self.on_event)
        self.data = 42

    def on_event(self):
        pass
```

## Parent classes

If a base class owns state, initialize that state before exposing the
derived object. Call the parent `__init__` as part of your own
construction.

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
        Account.__init__(self, account_id)
        self.owner = owner
        self.balance = initial_deposit
```
