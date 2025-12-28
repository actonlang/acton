# Boxing

Implementation lives in `compiler/lib/src/Acton/Boxing.hs`.

Values in Acton are mostly boxed. The boxing pass can unbox values in certain situations, which can greatly improve performance by avoiding unnecessary boxing operations. The more we can unbox, the better, but it is at odds with generics and polymorphism.

- Inserts explicit `Box`/`UnBox` nodes to model conversions between boxed runtime
  objects and unboxed machine values.
- Introduces unboxed temporaries for numeric primitives (ints/uints/floats) so
  codegen can emit efficient C operations.
- Rewrites selected builtin operations (arithmetic/comparisons, math functions,
  and some builtin methods) to operate on unboxed values and then re-box results.

The pass keeps a map from boxed names to their unboxed counterparts in its
environment and generates internal names using the `BoxPass` prefix. It also
preserves boxed values where needed so later stages still see the expected
surface semantics.

## Debugging

```sh
actonc --box path/to/file.act
```

## Thoughts

- Push unboxing further without changing the type system by operating late,
  after type-checking and lowering, where the AST is simpler and richly typed.
- Treat unboxable primitives (bounded ints, floats, bool) as unboxed in locals,
  params, returns, and class fields, and only box/unbox at polymorphic
  boundaries.
- Keep polymorphic code boxed; allow monomorphic code to be unboxed, with boxing
  inserted at call sites when crossing module boundaries or calling polymorphic
  functions.
- Extend the pass to cover classes and cross-module calls; current behavior is
  limited to function bodies and initializers.
