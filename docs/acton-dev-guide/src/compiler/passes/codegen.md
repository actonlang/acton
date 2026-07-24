# Codegen

Implementation lives in `compiler/lib/src/Acton/CodeGen.hs`.

`Acton.CodeGen.generate` receives the hash string stamped into
the generated output. It writes that hash as the first line of both `.c` and
`.h` files:

```c
/* Acton codegen hash: ... */
```

A whole-module back pass combines the compiler-binary identity, interface
version, line-emission mode, module implementation hash, and raw source hash.
The raw bytes and line mode are part of the generated-code key because codegen
emits `#line` directives and other source mappings that can change without a
semantic implementation change.

For selective output, one whole-program projection hash fingerprints the exact
global selection, every materialized module and projected type environment,
the exact selected names at opaque boundaries, the compiler identity, and the
interface version. Each module's codegen hash
combines that projection hash with its module name. A deferred module forced
whole by a native provider closure starts with the ordinary whole-module hash
and additionally binds it to the public hashes in the captured interface
snapshots for that batch. Those generation-coherent snapshots also retain the
ordered source imports used to rebuild each module environment. A change in
consumer interest therefore invalidates affected selective provider output even
when the provider's source is unchanged, while a lazy interface change cannot
leave forced-whole output stale.

The snapshot binding covers every interface in the captured lazy back-pass
environment. This deliberately conservative guard covers codegen's witness
forwarding and descendant, attribute, and extension index callbacks, whose
exact query buckets do not yet have persisted fingerprints. A public-interface
change anywhere in that closure can therefore invalidate selective output even
when no content row from that interface was selected. This does not materialize
unrelated rows; implementation-only changes outside the projection remain
outside the semantic codegen hash.
