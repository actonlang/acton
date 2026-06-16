# Troubleshooting type errors with signatures

When a type error mentions a value from another module, first check the
interface that the compiler sees. `acton sig` prints inferred signatures
by import path, so you can compare the error against the imported API
instead of searching for generated files or guessing from memory.

Use this workflow:

1. Read the type error and find the module or type name involved.
2. Run `acton sig` for the smallest exact module or public name.
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

This same approach works for dependencies. Dependency signatures use the
same names as dependency imports. If `directory` is a dependency and
`Contact` is defined in that dependency's `src/lib.act`,
`acton sig directory.Contact` uses the project's normal build
resolution: it reads `Build.act`, fetches missing dependencies,
compiles the interface files it needs, and skips the final executable
build. If `Contact` is defined in `src/models.act`, ask for
`acton sig directory.models.Contact` instead.

If you are testing a local checkout of a dependency, pass the same
override you would pass to `acton build`:

```sh
acton sig --dep directory=../directory directory.Contact
```

## Example: selecting attributes on the wrong value

Some type errors mention an "unknown type" even when the value has a
known expected type. This can happen when Acton checks a method body and
collects constraints from dot selections before it has simplified the
whole constraint set.

The important part is not the unknown type name alone. Read the whole
error and look for the known type that must satisfy those constraints.

Suppose one module defines an item type, a context type, and a base class
whose method takes both:

```python
# pipeline.act
class WorkItem(object):
    title: str
    tags: set[str]
    output_path: str

    def __init__(self, title: str, tags: set[str], output_path: str):
        self.title = title
        self.tags = tags
        self.output_path = output_path

class RunContext(object):
    run_id: str
    user: str

    def __init__(self, run_id: str, user: str):
        self.run_id = run_id
        self.user = user

class Step(object):
    def apply(self, item: WorkItem, ctx: RunContext):
        raise NotImplementedError()
```

A subclass can omit the local annotations because the inherited method
shape already gives `item` and `ctx` their expected types:

```python
# steps.act
from pipeline import Step

class DraftStep(Step):
    def apply(self, item, ctx):
        print("processing {ctx.title}")

        if "draft" in ctx.tags:
            return ctx.output_path

        raise ValueError("not a draft")
```

The method uses `ctx.title`, `ctx.tags`, and `ctx.output_path`, but the
inherited signature says `ctx` is a `pipeline.RunContext`. The resulting
error has a common "simultaneous constraints" shape:

```text
[error]: Cannot satisfy the following simultaneous constraints for the unknown types
     +--> steps.act@4:5-10:1
     |
   4 | +>     def apply(self, item, ctx):
   5 | |          print("processing {ctx.title}")
     : |                             ^--------
     : |                             `- The type of the indicated expression (which we call t0) must have an attribute title with type t4; no such type is known.
   6 | |
   7 | |          if "draft" in ctx.tags:
     : |             ^----------^-------
     : |             |          |- The type of the indicated expression (which we call t0) must have an attribute tags with type t1; no such type is known.
     : |             |          `- The type of the indicated expression (which we call t1) must be a subtype of t2
     : |             |- The type of the indicated expression (inferred to be __builtin__.str) must be a subtype of t3
     : |             `- The type of the indicated expression (which we call t2) must implement __builtin__.Container[t3]
   8 | |              return ctx.output_path
     : |                     ^--------------
     : |                     `- The type of the indicated expression (which we call t0) must have an attribute output_path with type None; no such type is known.
     : |                        The type of the indicated expression (which we call t4) must be a subclass of ?__builtin__.value
   9 | |
  10 | |>         raise ValueError("not a draft")
     : |
     : `- pipeline.RunContext must be a subclass of t0
-----+
```

Do not read this as "Acton cannot infer the type of `ctx`". In this
example, the inherited method signature already gives `ctx` the expected
type `pipeline.RunContext`. The unknown `t0` is the type variable that
collected the requirements introduced by the dot selections on `ctx`.
The final line says that `pipeline.RunContext` must satisfy those
collected requirements.

The next step is to inspect the exact known type named in the error:

```sh
acton sig pipeline.RunContext
```

That is usually better than asking for the whole module, because a large
module can produce a lot of unrelated output. The signature shows the
compiler-visible fields:

```python
class RunContext (object, value):
    @property
    run_id : str
    @property
    user : str
    __init__ : (run_id: str, user: str) -> None
```

Now compare the failing selections with the signature. The code asked
for `title`, `tags`, and `output_path`, but `RunContext` has only
`run_id` and `user`. When none of the selected attributes are present,
the code may be selecting on the wrong value. In this example, those
attributes belong to `WorkItem`, so inspect that type too:

```sh
acton sig pipeline.WorkItem
```

The fix is not to add a redundant annotation to `ctx`. The fix is to use
the value that actually has the fields:

```python
class DraftStep(Step):
    def apply(self, item, ctx):
        print("processing {item.title}")

        if "draft" in item.tags:
            return item.output_path

        raise ValueError("not a draft")
```

This pattern also appears with dependency types. If the known type comes
from a dependency, still run `acton sig` for the exact public name shown
in the error. For a dependency named `package`, a type in
`src/module.act` is addressed through the dependency prefix:

```sh
acton sig package.module.TypeName
```

If the type is in the dependency's `src/lib.act`, the package root
module is the dependency name itself:

```sh
acton sig package.TypeName
```

If you are using a local dependency override, pass the same override to
`acton sig` that you pass to `acton build`:

```sh
acton sig --dep package=../package package.module.TypeName
```

The compiler already sees the dependency signatures when it reports the
error. Running `acton sig` does not make those signatures visible to the
compiler; it makes them visible to you, so you can compare the API Acton
is checking against the attributes and methods your code selects.
