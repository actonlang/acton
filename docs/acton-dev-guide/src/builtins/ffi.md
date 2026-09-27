# FFI hooks

## Raising exceptions

`$RAISE` stores an exception pointer in the current worker's catch frame. It does
not attach a traceback or otherwise mutate the exception. Exceptions inherit
from `value`, so an instance whose complete payload is fixed can be shared by
all calls and workers.

Define these instances at file scope in the C module that raises them:

```c
static struct B_ValueError invalid_input_error =
    STATIC_EXCEPTION(B_ValueError, "Invalid input");

// In the error path:
RAISE_EXC(&invalid_input_error);
```

`STATIC_EXCEPTION` requires an ASCII string literal. It initializes both the
exception and its message in static storage, without heap allocation or lazy
initialization. Any additional exception fields must also have fixed values;
for example, omitted optional pointer fields are `None`. Arithmetic macros in
headers refer to instances defined in their corresponding C modules.

In Acton modules, use private module constants for fixed exceptions. Construct
them after their exception class is defined and before the declarations that
use them. They allocate once during module initialization.

`RAISE_EXC(instance)` raises an existing instance and accepts any exception
subclass. `RAISE(Type, args...)` constructs an exception and raises it, for
example `RAISE(B_IndexError, index, actStrFromCString("Index out of range"))`. Both evaluate
their arguments once and hide the cast to `BaseException`.

Keep constructing exceptions that carry per-call data: indices, keys, filenames,
formatted input values, system errors, and parser source locations. A fixed
message alone does not make an exception shareable. The AON parser, for example,
adds its current source, line, and column even to fixed messages. User exception
constructors and deserialization still create separate values.

Use the byte length when formatting an Acton `str`, `bytes`, or `bytearray` in
C. `%.*s` bounds the read but still stops at NUL; use Acton string concatenation
when embedded NUL must be preserved. Only true C strings may rely on a NUL
terminator. Keep existing exception types, constructors, and diagnostic fields
when optimizing their allocation.

Catch frames may still allocate when entering a `try` block. Sharing exceptions
removes their construction cost; it does not remove all allocation from error
handling. The internal `$SEQ`, `$BRK`, and `$CNT` control-flow exceptions are also
shared, while `$RET` retains its per-call return value.
