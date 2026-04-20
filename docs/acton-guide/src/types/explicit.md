# Explicit types

Acton can infer many types, but explicit annotations are still useful.
They are the way to say "this value must stay this shape" when the code
would otherwise leave room for interpretation.

```python
def repeat(text: str, count: int) -> str:
    return text * count

describe_port : (int) -> str
def describe_port(port):
    return "port " + str(port)

actor main(env):
    port: int = 9000
    print(repeat("ha", 3))
    print(describe_port(port))
    env.exit(0)
```

<div class="beginner-content">
<p>Read <code>name: Type</code> as "name has type Type" and
<code>-&gt; Type</code> as "returns Type". A separate signature line can
be easier to scan when the implementation is long or the signature is
part of the public surface of a module.</p>
</div>

You can annotate:

- function parameters
- return values
- local names
- class and actor attributes
- separate signature lines for named APIs
- effect markers on callables

## When explicit types help

Write annotations when:

- you want an API to be clear to readers
- inference becomes hard to understand
- you want the compiler to reject the wrong shape earlier
- a value could otherwise be inferred more loosely than you want
- you are defining a reusable helper and want its contract visible

<div class="beginner-content">
<p>A useful default is to annotate the things other people will read
first: public functions, methods, actor fields, and data structures.
Leave short local expressions inferred unless the type is surprising or
important to the code around it.</p>
</div>

<div class="advanced-content">
<p>Explicit annotations also control generalization. If inference would
make a helper more polymorphic than you want, a written signature can
pin the API down and keep later changes from widening it by accident.
That is especially useful for callback types, actor-facing entrypoints,
and shared utility code where the signature is the real contract.</p>
</div>
