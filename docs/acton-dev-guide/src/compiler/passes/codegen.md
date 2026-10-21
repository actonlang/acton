# Codegen

Implementation lives in `compiler/lib/src/Acton/CodeGen.hs`.

`Acton.CodeGen.generate` receives the hash string that should be stamped into
the generated output. The file backend stamps the same hash on `.c`, `.h`,
and generated support fragments:

```c
/* Acton impl hash: ... */
```

For normal modules, `Acton.Compile` passes the module implementation hash. For
DBP modules, it passes the DBP codegen hash derived from the module
implementation hash plus the selected top-level names. The label is therefore
historical: in DBP output it is a selection-sensitive generated-code hash, not
just the raw module implementation hash.

File builds put generated closure implementations, global initialization chunks,
and method-table initialization in `out/types/<module-directory>/generated/<module>.inc`.
The corresponding module `.c` includes that fragment in the same translation
unit, after its ordinary definitions. This keeps declarations, external C
extensions, source line directives, and initialization dependencies intact.
`--cgen` continues to show a complete listing without an external fragment.
Incremental freshness checks include the fragment hash; deleted modules also
have their owned fragments pruned.

Generated closures retain their typed capture structs and separate class
identity for each distinct body. A static `$ClosureLayout` describes their size,
method table, and capture fields using C `offsetof` and `sizeof`. Field
entries distinguish object references from raw scalar class IDs. Tiny typed
serializer and deserializer wrappers call shared runtime walkers that preserve
ordinary graph back-references and external actor references. Deserialization
registers newly allocated objects before restoring their captures, supports the
allocation-only phase, and can fill an already allocated object. Raw scalar
fields are copied at their actual width; padding is never serialized.

Within a module, closures with the same ordered capture representations share
one field array. Object reference types and capture names may differ; raw scalar
class IDs stay distinct even when their widths match. Generated C static
assertions verify each reused field's actual offset and size against the first
closure using that array. Each closure keeps its own layout, object size, method
table, and class identity. Empty closures need no field array.

Closure constructors allocate and assign captures directly. Their unused
`__init__` slot is null. For Proc closures, `__exec__` reuses `__call__` when the
actual C slot signatures match. Pure and Mut `__call__` and `__exec__` share
one adapter; boxed evaluation results are forwarded without unboxing and
allocating another box. Action retains separate adapters: `__call__` waits for
the action result, whereas `__exec__` returns its message to the continuation.

Within a module, capture-independent adapters are pooled by their generated C
signature and body after canonicalizing parameter names and boxed pointers to
the RTS's existing `$WORD` representation. Arity, effect family, exact raw
scalar representations, and any remaining conversions stay part of the key.
Each helper dispatches through the primitive closure header; generated static
assertions verify the target method-table slot's offset and size. Adapter
helpers are private to the translation unit and have no per-class prototypes
in the header. Typed captures, method tables, serialization identities, and
object allocation remain as before, with no extra pointer in each object.

Scalar boxing also reuses an expression's existing box when its generated C
representation is boxed and its Acton type exactly matches the requested scalar
type. Identity lambda bodies therefore return their argument directly. Raw
computed values and numeric conversions retain the required boxing.

The shared walkers add a loop and descriptor reads to serialization, while
removing repeated field-walking code. This is a code-size tradeoff, not a claim
that persistence runs faster. Any binary-size or runtime improvement should be
measured on representative programs.
