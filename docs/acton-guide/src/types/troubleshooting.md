# Troubleshooting type errors with signatures

When a type error mentions a value from another module, first check the
interface that the compiler sees. `acton sig` prints inferred signatures
by import path, so you can compare the error against the imported API
instead of searching for generated files or guessing from memory.

Use this workflow:

1. Read the type error and find the module or type name involved.
2. Run `acton sig` for that module or public name.
3. Compare the available attributes and methods with the failing code.
4. Fix the source and rebuild.

## Example: singular vs plural attribute

Suppose one module defines a contact value:

```python
# src/directory.act
class Contact(object):
    display_name: str
    email_addresses: list[str]

    def __init__(self, display_name: str, email_addresses: list[str]):
        self.display_name = display_name
        self.email_addresses = email_addresses
```

Another module imports the class and uses the right concept, but the
wrong singular form of the attribute name:

```python
# src/main.act
from directory import Contact

def email_count(contact: Contact) -> int:
    return len(contact.email_address)
```

Running `acton build` points at the attribute selection:

```text
[error]: Attribute not found email_address
     +--> main.act@4:24-4:37
     |
   4 |     return len(contact.email_address)
     :                        ^------------
     :                        `- Attribute not found email_address
-----+
```

The error is in `main`, but the shape of `Contact` is defined in
`directory`. Ask the compiler for the imported type:

```sh
acton sig directory.Contact
```

`acton sig` first looks for a module named `directory.Contact`. If no
such module exists, it treats the target as the public name `Contact`
inside module `directory`. The output shows the public interface for the
class:

```python
class Contact (object, value):
    G_init : () -> None
    @property
    display_name : str
    @property
    email_addresses : list[str]
    __init__ : (display_name: str, email_addresses: list[str]) -> None
```

The `@property` entries are the readable part for this error. They say
the class has `email_addresses`, not `email_address`. That is not
obvious from the local `main.act` file, but it is clear from the
compiler-visible interface. The fix is in the selecting code:

```python
def email_count(contact: Contact) -> int:
    return len(contact.email_addresses)
```

This same approach works for dependencies. If `directory` comes from a
package dependency, `acton sig directory.Contact` still uses the
project's normal build resolution: it reads `Build.act`, fetches missing
dependencies, compiles the interface files it needs, and skips the final
executable build.

If you are testing a local checkout of a dependency, pass the same
override you would pass to `acton build`:

```sh
acton sig --dep directory=../directory directory.Contact
```
