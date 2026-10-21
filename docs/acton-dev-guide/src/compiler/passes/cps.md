# CPS

Implementation lives in `compiler/lib/src/Acton/CPS.hs`.

The CPS pass turns potentially suspending procedure calls into calls with an
explicit continuation. Loops and branches can introduce additional local
continuations to represent the code that follows them. Lambda lifting later
turns continuations used as values into closure objects.

## Forwarding sequence continuations

A common pattern is a loop followed by a return of a local value:

```acton
value = await async source()
while value % prime == 0:
    value = await async source()
return value
```

The ordinary CPS conversion introduces a sequence continuation that ignores
`None` and returns `value` to the caller. A loop exit then constructs that
continuation and resumes it with `None`. The CPS pass simplifies this immediate
jump to a return directly to the caller:

```text
# Before, schematically:
continuation k(ignore):
    return R_CONT(caller, value)
...
return R_CONT(closure(k, caller, value), None)

# After:
return R_CONT(caller, value)
```

The forwarding body must consist of exactly one return through `R_CONTc` or
`R_CONT`, and must ignore the sequence continuation's result parameter. Its
target and value must be known local variables or literals. Property reads,
module variable reads, calls, and reads of the cells used for volatile locals
retain the ordinary continuation. Additional statements, including exception
frame cleanup, also prevent this simplification.

Only immediate generated jumps are simplified. A continuation passed to a
potentially suspending call remains in place: that call still has to finish
before the procedure can return. Loop state parameters retain the same names
at the definition and jump; the simplifier checks this before reusing the body.
Forwarding information does not cross source function or class boundaries.

The result still goes through the RTS trampoline using an `R_CONT` return
record. It does not invoke the caller's continuation recursively in C. This
preserves bounded C stack use when a long chain of suspended calls completes.
No escape analysis is required, and the closure representation and serialization
of the remaining continuations are unchanged. A now-unused local function may
remain in a CPS dump; it no longer needs a closure class if it has no remaining
uses as a value.

The `cps_forwarding` CPS fixture covers direct returns and conservative
fallbacks. The corresponding native regression covers loop-carried values,
branch joins, suspension, effects, exception cleanup and deep recursion, in
debug and release builds.
