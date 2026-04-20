# Protocols

Protocols describe behavior that different types can share.

Use a protocol when your main question is "what operations does this
value support?" rather than "what class does it inherit from?"

```python
protocol Processable[T]:
    process : () -> T

class Message(object):
    def __init__(self, text):
        self.text = text

extension Message(Processable[str]):
    def process(self):
        return self.text.upper()
```

In this example:

- `Processable[T]` defines a capability
- `Message` is an ordinary class
- the `extension` says that `Message` implements that protocol

<div class="beginner-content">
<p>Inheritance says one class is a kind of another class. A protocol
says a type supports a certain behavior, whether or not there is any
inheritance relationship. Read
<code>extension Message(Processable[str])</code> as "Message implements
the Processable[str] protocol".</p>
</div>

Protocols are useful at API boundaries because they let you depend on a
capability instead of a concrete class. Several unrelated types can
offer the same behavior, and one type can offer several unrelated
behaviors.

<div class="advanced-content">
<p>Protocols matter both for programming style and for type inference.
They let you describe the shape of an API without committing to a
concrete hierarchy. In Acton, protocols can also be implemented by
extensions after a class is defined, which makes them useful for
retrofitting shared behavior onto existing types.</p>
</div>

## Protocols in practice

```python
protocol Printable:
    print : () -> None

def render(item: Printable):
    item.print()
```

Here, `render` only cares that the value can be printed. It does not
care which class the value comes from.

## Protocols and generic constraints

Protocols also show up in generic type signatures.

```python
def bigger[A(Ord)](a: A, b: A) -> A:
    if a > b:
        return a
    return b
```

Here, `A(Ord)` means the type `A` must implement the `Ord` protocol so
that `>` is available.

Built-in protocols such as `Ord`, `Hashable`, `Iterable`, and `Mapping`
are documented in the reference section under [Built-in
protocols](../stdlib/builtin_protocols.md).
