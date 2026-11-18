# Actor startup migration

These patterns were used to migrate acton-ssh on the `actor-startup` branch.
They describe the compiler on that branch, while the actor syntax and effect
changes are being developed.

Keep initialization declarative wherever possible. Move communication that
requires waiting, native `proc` setup, and other operational work into
`__startup__`. Actor constructors run without CPS and allow `pure` and `com`
computations. Startup is scheduled automatically with interface
`action() -> None`; its implementation can use `proc` operations.

Construction publishes startup messages only if the construction transaction
succeeds. Startup execution itself is outside that transaction: each actor
handles its own startup failures.

## Validation and configuration

Replace an actor-body `if` that raises with a conditional initializer. This
keeps validation errors in declarative construction, where the parent can
handle them:

```acton
var _keys = (list(keys) if len(keys) > 0 else raise ValueError("empty keys")) if keys is not None else None
```

Use an explicit outer `is not None` condition when narrowing an optional
value. A condition using `is None or ...` did not narrow the value in its
second operand in the SSH migration.

Likewise, combine defaults and conditional reassignment:

```acton
cycles = int(env.argv[1]) if len(env.argv) > 1 else 2000
```

Preserve the order of validations when several inputs can be invalid.

## Duplicate bindings and callback cycles

Replace a placeholder followed by a second declarative assignment with one
initializer where name availability permits:

```acton
var server: ?Server = Server(...)
```

Retain the optional type if methods later assign `None`.

The current compiler requires fields to be defined before methods refer to
them, and callbacks to be defined before an initializer uses them. When these
requirements form a cycle, retain the early placeholder and move construction
into startup:

```acton
var server: ?Server = None

# Callback methods that use server go here.

proc def __startup__():
    server = Server(...)
```

This source-level workaround avoids adding forward signatures or changing the
compiler's treatment of declaration groups. It is unnecessary when earlier
methods do not use the field.

## Operational setup

Move stray helper calls into startup, preserving their order. In SSH, native
affinity pinning must precede native client/server initialization:

```acton
proc def __startup__():
    _pin_affinity()
    _init()
```

Environment queries and file reads belong in startup, followed by any child
construction that depends on their results. Validate command-line argument
counts before indexing the argument list. Values computed locally in startup
can be passed to actor methods through callback lambdas instead of adding
actor state solely to retain them.

A declarative initializer cannot read an earlier mutable actor field. Use an
immutable initial value for both initializers when appropriate:

```acton
initial_start = now()
var start = initial_start
var engine = Engine(initial_start)
```

## Avoid unnecessary local mutation

Incrementally constructing a set with `add` can introduce `mut`, propagating
through helpers and class constructors until a declarative actor initializer
rejects it. Prefer set comprehensions for construction; retain separate
validation as needed to preserve error messages and validation order.

Repeated byte concatenation can also become a declarative expression:

```acton
payload = bytes([i for i in range(256)]) * 512
```

Check inferred effects after each rewrite. Removing one mutation does not
guarantee that all remaining operations fit `com`.

## Tests and verification

Run test requirements and configure timeouts in startup before creating child
test actors. Otherwise a skipped test can still launch work.

Explicitly await asynchronous writes and closes when completion is required
before process exit. Queuing a write followed by `env.exit` does not ensure
that the write completes; an empty output file can result.

Validate with both a normal project build and its test suite. Test builds can
type-check example modules without checking every executable's root interface.
Use local `--dep name=path` overrides to exercise migrated dependency branches.
`acton test --no-cache` disables test-result reuse; it does not force every
module to be recompiled. Report gated tests as skipped, not executed.
