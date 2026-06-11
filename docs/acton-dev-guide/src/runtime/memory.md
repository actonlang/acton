# Memory and GC

Acton processes contain two heaps with fundamentally different rules:

- **The GC heap**, managed by Boehm GC (`libgc`). Collection is conservative:
  the collector scans the GC heap, all thread stacks and registers, the data
  segment, and `GC_malloc_uncollectable` blocks for anything that looks like a
  pointer into the GC heap. `ALL_INTERIOR_POINTERS` is enabled, so a pointer
  into the middle of an object keeps the whole object alive. Conservatism cuts
  both ways: an `int64_t` actor field holding a cast pointer value pins the
  object it points to, even though the type system thinks it is an integer.
- **The libc heap**, managed by `malloc`/`free`. The collector never scans
  libc memory. A GC-heap object whose only reference is stored inside a
  libc-heap allocation is unreachable as far as the collector is concerned and
  will be collected while still in use.

That second bullet is the single most important rule when working on the
runtime or on `.ext.c` modules: **a GC pointer stored in libc memory does not
keep its referent alive.**

## The Acton allocator

Acton objects (actors, builtin values, everything the compiler generates
allocations for) go through the `acton__allocator` function table
(`base/rts/common.c`), reached via `acton_malloc`/`acton_calloc`/etc. `main()`
re-points the table during startup (`base/rts/rts.c`):

1. After `GC_INIT()`, the table points at the GC (`GC_malloc`,
   `GC_malloc_atomic`, ... with `acton_noop_free`).
2. During module constant initialization it briefly points at libc, so that
   module-level constants live outside the GC heap.
3. Before the worker loops start it points back at the GC for the lifetime of
   the process.

`free` is deliberately a no-op in the GC regime: the collector reclaims
memory, and explicit frees of GC memory would be double-management.

## Finalizers

Actors that override `__cleanup__` get a GC finalizer: code generation emits a
guarded `$InstallFinalizer` (a thin wrapper around `GC_register_finalizer`,
`base/rts/rts.h`) in the actor constructor. Declaring `__cleanup__` as an
`action` means the finalizer merely enqueues a message; the cleanup body then
runs on the actor's affinity-pinned worker thread like any other message. The
message resurrects the actor for the duration of the cleanup; Boehm finalizers
are one-shot, so after the cleanup message has been processed and the last
reference dropped, the actor is collected without the finalizer running again.

## IO and native libraries

The native IO stack (libuv, tlsuv, mbedtls and the native objects owned by
`.ext.c` modules) spans the boundary between the two heaps, and getting object
lifetimes right across that boundary has its own design, hazard catalogue and
ownership patterns. See [IO memory management](io_memory.md).
