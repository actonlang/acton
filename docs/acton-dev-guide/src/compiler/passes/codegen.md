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
and method-table initialization in `<module>.closures.inc`, next to the module's
`.c` in `out/types`. The `.c` includes that fragment in the same translation
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

## Automatic storage for builtin loop iterators

For a direct `for` loop over a known builtin collection, CodeGen can place the
loop's iterator in the C function's automatic storage. For example:

```acton
def total(xs: list[int]) -> int:
    result = 0
    for value in xs:
        result += value
    return result
```

The generated function declares a `struct B_IteratorD_list`, initializes it
with `$listD_U_iter_init(&storage, xs)`, and uses the ordinary non-throwing
`__next__` method through a `B_Iterator` pointer. No iterator object is allocated
on the heap. The source collection and the values yielded by the iterator keep
their usual representations.

The optimization has three requirements:

- Normalizer created the iterator variable privately for a `for` loop. User
  variables containing iterators are outside this optimization, even when
  they happen to be local to the function.
- CodeGen recognizes the builtin iterator factory, either as a direct call or
  through a compiler-created local witness whose builtin origin is known.
  Generic witness parameters and user-defined iteration use the ordinary path.
- The generated C function is synchronous. Functions returning the RTS
  continuation result (`$R`) keep heap iterators, since a continuation may use
  the iterator after the current C activation returns.

The supported families are list/ilist, array, bitarray, bitset, dict/idict
(keys, values and items), set/iset, str, bytes and bytearray. An inline
`iter(...)` wrapper around a recognized factory is also eligible. Reverse
iteration and iterator combinators keep their existing construction behavior.
Range iteration has a separate, existing optimization.

Backing structs are declared at function scope. This is necessary because
normalization may initialize a loop iterator inside a conditional while
hoisting the iterator pointer outside that conditional. The storage therefore
outlives all uses of the pointer. A recursive call gets its own storage, and
repeated entry into a loop reinitializes its struct. This reserves one small
struct per eligible loop in the C frame; it does not grow with the number of
iterations. A source reference in that storage may conservatively remain live
until the function returns.

The native helpers share initialization logic with heap iterators and keep the
ordinary class pointer and source reference. The conservative GC scans C
stacks and registers, so temporary source collections remain reachable during
iteration. The helpers preserve mutation behavior by using the existing
`__next__` implementations. Iterator layouts and serialization methods are
unchanged. This does not remove allocations made while producing elements,
such as tuples from dictionary items or boxed byte values.

This local proof does not depend on general escape analysis. Extending it to
named iterators or helper calls would require proving both that the iterator
cannot escape and that every use stays within the lifetime of the same C
activation; a no-capture parameter summary alone is insufficient.

### Verification and motivating measurement

The compiler fixtures `stack_iterators.act` and
`stack_iterators_fallback.act` check recognized families and fallback paths.
The native regression `test/regression_auto/stack_iterators.act` runs in both
debug and fast modes and covers nesting, empty loops, mutation, early exits,
exceptions, recursion, forced GC with a temporary source, and suspension.

For the motivating `~/acton-local/bench/nbody.act`, matched builds from main
`14e0fe60` and this change used `--fast --always-build` and ran with
`--rts-wthreads 1`. The source SHA-256 was
`8a6375f660ed80b7e67a8a63559f9c5c873b94968ad4c39a102306700b0510c3`.
The RTS reported cumulative heap allocation, including startup:

| Iterations | Main | Automatic loop iterators |
| ---: | ---: | ---: |
| 0 | 269,040 bytes | 269,040 bytes |
| 1,000 | 432,736 bytes | 285,424 bytes |
| 5,000,000 | 480,332,416 bytes | 285,424 bytes |

Both builds printed identical energies for all three counts. At five million
iterations these were `-0.169075164` initially and `-0.169083134` finally.
The two list iterators in the advance loop previously accounted for 96 bytes
per iteration. The revised build has no iteration-dependent allocation in
that loop. These figures measure allocation volume, not peak resident memory;
small startup totals can vary between runs.
