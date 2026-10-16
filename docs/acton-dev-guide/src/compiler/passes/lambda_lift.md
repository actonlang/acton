# Lambda lift

Implementation lives in `compiler/lib/src/Acton/LambdaLifter.hs`.

`closureConvert` interns equivalent generated closure classes within a module.
The comparison ignores source locations but includes the effect family, type
quantification, capture names and types, and complete method bodies. It is
conservative: it does not rename captures or merge bodies that merely have the
same physical state layout. Each constructor call still creates a fresh object.

A monomorphic CPS continuation with a `None` parameter that ignores its result
and returns a captured value to a captured continuation can use the existing
serializable `$ConstCont`. Recognition is restricted to local variable captures
so it cannot change the timing of property or global reads. Its lifted helper
remains available for direct calls.

Both transformations can change generated names and class registration order.
They do not provide compatibility with persistent snapshots from an older build.
