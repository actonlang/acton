# FFI hooks

## Raising exceptions

Use `RAISE(Type, args...)` to construct and raise an exception, for example
`RAISE(B_IndexError, index, actStrFromCString("Index out of range"))`.
Use `RAISE_EXC(instance)` to raise an existing instance of any exception
subclass. Both evaluate their arguments once and hide the cast to `BaseException`.

Raising stores an exception pointer in the current worker's catch frame without
attaching a traceback or otherwise mutating the exception.

Exceptions inherit from `value`. Attribute assignment outside initialization
requires an `object` receiver, and these exception classes do not implement
`Identity` for `is` and `is not`. Acton callers therefore cannot reassign their
fields or observe instance identity through those operations. An exception can
be shared by all calls and workers when its complete payload is immutable and
fixed.

C code must preserve that immutability. Explicit `__init__` calls on existing
values remain a compiler loophole; do not use them to reinitialize shared
exceptions.

Define these instances at file scope in the C module that raises them:

```c
static struct B_ValueError invalid_input_error =
    STATIC_EXCEPTION(B_ValueError, "Invalid input");

// In the error path:
RAISE_EXC(&invalid_input_error);
```

`STATIC_EXCEPTION` requires an ASCII string literal. Literal concatenation rejects
pointer variables at compile time. ASCII remains the caller's responsibility:
the byte and character counts both use the literal's byte length. The macro
initializes both the exception and its message in static storage, without heap
allocation or lazy initialization. Any additional exception fields must also
have fixed values; for example, omitted optional pointer fields are `None`.
Arithmetic macros in headers refer to instances defined in their corresponding
C modules.

In Acton modules, use private module constants for fixed exceptions. Construct
them after their exception class is defined and before the declarations that
use them. They allocate once during module initialization.

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
