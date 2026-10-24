# Memory and GC

Acton processes contain two heaps with fundamentally different rules:

- **The GC heap**, managed by Boehm GC (`libgc`). Collection is conservative:
  the collector scans the GC heap, all thread stacks and registers, the data
  segment, and `GC_malloc_uncollectable` blocks for anything that looks like a
  pointer into the GC heap. `ALL_INTERIOR_POINTERS` is enabled, so a pointer
  into the middle of an object keeps the whole object alive. Conservatism cuts
  both ways: an `int64_t` actor field holding a cast pointer value pins the
  object it points to, even though the type system thinks it is an integer.
  Objects allocated pointer-free (`acton_malloc_atomic`) are not scanned at
  all: boxed scalars (`int`, `float`, `complex`, the fixed-width integers),
  ranges, slice bounds, bigint digits and string and byte data. A pointer
  stored as the value of a boxed `int` does not keep its object alive.
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

## Write barrier

Builds with `gc_dirty_tracking_backend=write_barrier` (the default) compile
the collector with its manual dirty-bit mode only and the program with a
write barrier (`base/rts/wbar.h`). In generational mode
(`ACTON_GC_MODE=generational`) the collector reads those bits instead of
protecting pages or reading the kernel's dirty bits:

- a minor collection rescans the marked objects of the dirty blocks, pushes
  the roots and marks to the end;
- a collection marked in slices reads the bits at its first stop (minor) or
  clears them (full), marks in world-stopped parallel steps, and at its final
  stop rescans the marked objects of the blocks dirtied since the first,
  pushes the roots and marks to the end. Steps push no roots and scan no
  stacks.

The collector marks only with every thread stopped, and reads and clears the
bits only with every thread stopped. So the barrier needs no memory fence.

**The invariant.** At every read of the bits, every marked object that holds
a pointer to an unmarked object lies in a dirty block, or in a block of a kind
that every read rescans (uncollectable objects, `acton_malloc_rescanned`).

**The barrier.** After `*slot = v` in object `obj`: set the bit of the slot's
block, with an atomic OR only if it is clear, and keep `v` in a register until
then. A thread stopped between the store and the bit has `v` in a register,
so that stop marks it, and the bit set after the restart serves the next
read. `ACTON_STORE_ATTR` (generated code) and `ACTON_STORE` evaluate the
value first, then the receiver, as clang does for the plain assignment.

**Rule F: stores that need no barrier.** A store into an object that the
storing code has held in a local variable continuously since the object was
allocated. The collector makes this true:

- the allocator returns an unmarked object, or one in a block that is dirty
  for the next read (the blocks of thread-local free lists, whose entries a
  collection marks, are dirtied at every root push; so is the block of a
  marked object that `GC_free` releases);
- every stop that pushes roots dirties, for the next read, the block of every
  object a stack or register references, and every block of a large one;
- while a collection is in progress, every block the allocator hands out is
  dirty for its final read.

Rule F covers `__init__` and `G_init` stores into `self` of classes (not
actors; it relies on `__init__` running only on a new object), closure and
continuation constructors, and C constructors. It does not cover an object
reached through the held one (`B_listD_new(n)->data`), an object loaded back
from the heap (the `--db` restore fills objects allocated in an earlier pass,
so every deserializer fill has a barrier), or the result of realloc.

**Moves and bulk copies.** `ACTON_WBAR_RANGE` over the destination, before
the first move: a step can scan a large object in pieces between two moves,
and the values in flight are only in registers or a temporary buffer. A
barrier after the copy is not enough for sliced marking. Values held on the
stack do not make a store safe either, since steps do not scan stacks.

**C libraries.** libuv writes pointers into the uv loops, handles and
requests and into memory it allocates with `realloc(NULL, n)`: in generational
mode those come from the rescanned kind (`acton_malloc_rescanned`,
`base/rts/common.c`). libuv's other allocations are uncollectable, which every
read rescans. The worker context, which `SET_SELF` writes for every
continuation, is rescanned too. Each allocation from the kind takes the
collector's allocator lock, so ordinary mode, which never reads the dirty
bits, allocates ordinary GC memory instead.

**Checking.** `ACTON_GC_CHECK=1` (`acton test --gc-check`) runs the
collector's checker at every read: it reports a marked object in a clean block
that points to an unmarked object, and exits with status 70. It finds a
missing barrier only when nothing else dirtied the same block in that run, so
it needs many collections (small budget, 1 ms steps) and many runs.

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
