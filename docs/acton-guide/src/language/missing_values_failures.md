# Missing values and failures

Programs usually need to represent two different cases:

- a value is genuinely absent
- an operation failed and execution should stop or recover

Acton uses different tools for those cases. `None` and optional types
model ordinary absence. Exceptions model failures.

<div class="beginner-content">
<p>Use an optional when absence is part of the normal result. Use an
exception when the operation could not finish as intended. A caller can
branch on an optional; an exception means the current path should stop
unless it is explicitly handled.</p>
</div>

There is no Rust-style `match` path for this in Acton. Use ordinary
branching, optional checks, and exception handling instead.

Start here when you need to decide whether a function should return
`None`, raise an exception, or let an expression keep propagating an
optional result.

<div class="advanced-content">
<p>Optional chaining and forced unwrapping are expression-level tools.
They do not replace the type system or exception handling; they make the
common cases shorter. Keep absence in the type when it is expected, and
reserve exceptions for broken assumptions, invalid input, and other
failures that should not be treated as routine control flow.</p>
</div>

- [Optionals and `None`](optionals_none.md) covers the basic meaning of
  `None` and optional return values
- [Optionals](../types/optionals.md) explains narrowing, optional
  chaining, and forced unwrapping
- [Errors and exceptions](errors_exceptions.md) covers `raise`,
  `try`, `except`, `else`, and `finally`
