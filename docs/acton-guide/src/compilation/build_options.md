# Application build options

`build_options` is an optional dictionary of string names and string values in
`Build.act`. It supplies options to the application's Zig build, with each entry
passed as `-Dname=value`. Values are literal arguments, not Zig expressions.
An unknown option or a value of the wrong type fails the build.

The root application's options govern its build, including test executables.
Options in a dependency's `Build.act` apply when building that dependency as a
project itself; they do not override the consuming application's choices.
Changing options rebuilds the affected artifacts and invalidates cached test
results. Performance recordings retain the selected options so comparisons
can measure configuration changes.

Compiler options such as target, CPU, optimization, database support and
threading remain controlled by their existing command-line flags. Their Zig
option names (`target`, `cpu`, `ofmt`, `dynamic-linker`, `optimize`, `db`, `no_threads`,
`cpedantic` and names beginning with `acton_`) are reserved and cannot appear in `build_options`.

## C allocator

Applications on Linux and macOS can replace ordinary C allocation with mimalloc:

```python
build_options = {
    "malloc": "mimalloc",
}
```

The default is `"libc"`. Removing the option or selecting `"libc"` restores the
system allocator on the next build. Unsupported targets and unknown values
fail the build. The choice applies to application and test executables.

This replaces the C `malloc`/`calloc`/`realloc`/`free` family, including calls
from native dependencies. It does not replace Boehm GC or change Acton object
lifetimes. In particular, libuv's explicitly configured GC allocations stay
scanned and uncollectable, because they retain references to Acton actors.
Libraries with their own allocation hooks continue to use those hooks.

Acton links one mimalloc object into each final executable, so applications
do not need an additional shared allocator library at deployment. The selected
allocator is active from process startup and cannot be changed mid-run.
`MIMALLOC_VERBOSE=1` prints mimalloc's startup diagnostics when launching a
mimalloc executable; leave it unset during timing runs.

Compare identical application workloads with each setting, keeping compiler,
optimization, worker count and GC settings fixed. XML parsing and TLS connection
churn exercise ordinary C allocation. Acton collections and JSON conversions
mostly use GC allocation and are useful controls. Measure throughput, latency
and process RSS; the performance runner's allocated-byte count covers GC
allocation, not all memory obtained through C `malloc`.

## GC defaults

Acton turns on these collector behaviours, which BDWGC leaves off. Each can
be turned off, as its section below describes:

| Behaviour | Turn off in `build_options` | Turn off when starting the program |
|---|---|---|
| [Range stealing](#range-stealing) by parallel markers | `"gc_mark_range_stealing": "false"` | not possible |
| [Old copies of objects moved by `GC_realloc`](#gc-objects-moved-by-realloc) are left to the collector | `"gc_realloc_no_free": "false"` | `GC_REALLOC_NO_FREE=0` |
| A new thread uses its own free lists [without a warm-up](#gc-thread-local-warm-up) | `"gc_no_thread_local_warmup": "false"` | `GC_NO_THREAD_LOCAL_WARMUP=0` |

## GC mark layout

Applications can opt into a different BDWGC mark representation:

```python
build_options = {
    "gc_use_mark_bits": "true",
    "gc_mark_bit_per_object": "true",
}
```

`gc_use_mark_bits` packs marks into bits instead of using the collector's
default representation. With parallel marking, the default is byte marks;
without parallel marking, BDWGC already uses bits. `gc_mark_bit_per_object`
indexes marks by object instead of allocation granule. The options are
independent, so either can be enabled on its own.

Both default to `"false"`, preserving BDWGC's existing choices. Packed marks
reduce metadata size but may increase contention between marking threads.
Per-object indexing changes the work needed to find an object's mark.
Measure both on the application's workload before choosing them.

These are compile-time settings shared by the application, its Acton
dependencies, the standard library and database support. They preserve
parallel marking support and do not select incremental or generational
collection. They cannot be changed by a running application.

## GC transparent huge pages

On Linux, an application can keep GC memory on ordinary pages:

```python
build_options = {
    "gc_disable_thp": "true",
}
```

This defaults to `"false"`, leaving the operating system's transparent huge
page (THP) policy unchanged. When enabled, the runtime applies
`MADV_NOHUGEPAGE` to memory obtained by the collector, starting with its initial
allocation. It does not change the policy for the rest of the process. The
setting persists when the collector releases physical pages and reuses them.

Ordinary pages can reduce dirty-page tracking work for incremental or
generational collection. Huge pages can improve address translation and
memory access performance, so measure both choices for the application.
This option does not enable incremental or generational collection itself.

Enabling it for a non-Linux target fails the build. If the kernel rejects
`MADV_NOHUGEPAGE`, the application exits with an error instead of continuing
with an ineffective setting.

## GC dirty tracking backend

Linux applications can select the collector's dirty-page tracking backend:

```python
build_options = {
    "gc_dirty_tracking_backend": "soft_dirty",
}
```

The choices are `"auto"` (the default), `"soft_dirty"` and `"userfaultfd"`.
Automatic selection preserves BDWGC's platform choices and fallback behavior.
Explicit choices require a Linux GNU target. `userfaultfd` additionally requires
x86, x86_64 or aarch64 and a glibc target of 2.34 or newer. Both explicit backends
can therefore be compared using `--target x86_64-linux-gnu.2.36`.

This is a build setting, shared by the application and its dependencies,
including database support. It does not enable incremental or generational
collection. For example, `GC_ENABLE_INCREMENTAL=1` enables that machinery when
launching the executable; `GC_PAUSE_TIME_TARGET=999999` selects generational
collection without a marking time limit.

An explicit choice compiles out the `mprotect` fallback. If incremental
collection is requested through `GC_ENABLE_INCREMENTAL` at startup and the
selected backend cannot be activated, the runtime exits with an error. An
ordinary collection run does not require the backend to be active. Native code
that enables incremental collection later must check `GC_get_actual_vdb()`;
this build option does not add a live backend-switching API.

On macOS, arm64 builds track dirty pages with `mprotect`. x86_64 macOS builds
leave it out, because incremental collection with it hangs under Rosetta 2;
there, incremental and generational mode treat every heap page as dirty.

## GC page-hash table size

Applications with large heaps can experiment with a larger page-hash table:

```python
build_options = {
    "gc_page_hash_table_log2": "23",
}
```

The value is the base-two logarithm of the number of entries. Values from 1
through 30 are accepted; `"0"` (the default) keeps the collector's usual size.
This is a build setting and is shared by all application dependencies,
including database support.

In Acton's 64-bit large-heap configuration the default is 21: 256 KiB per table.
With 4 KiB collector blocks, addresses 8 GiB apart share an entry. Setting 23
uses 1 MiB per table and moves that address interval to 32 GiB. Larger tables
can reduce unnecessary rescanning when clean pages share entries with dirty
pages. They cost more memory and clearing/copying work. Several tables exist,
including conservative-pointer blacklisting tables, so this also affects
ordinary collection. It is independent of object mark-bit layout.

Measure the application's workload before choosing a larger table. Increasing
this value does not remove dirty-tracking faults or guarantee fewer collections.

## GC heap growth

Programs whose live data grows to gigabytes can let the collector expand the
heap in larger steps:

```python
build_options = {
    "gc_heap_growth_divisor": "16",
}
```

Normally one automatic heap expansion adds at most a fixed amount (16 MiB in
Acton's configuration), and the collector runs a full collection before
expanding again. A heap that grows from nothing to 4 GiB therefore needs
hundreds of full collections, each marking all live data. With a divisor of
`N`, one expansion may add up to the heap size divided by `N` when that is
larger than the fixed amount, so the number of these collections grows with
the logarithm of the heap size instead. Building a 1 GiB list of small
objects took 78 collections with the default, 53 with 16 and 39 with 8.

The cost is a larger heap: a few percent with 16, more with smaller values.
The free space divisor still bounds each expansion, so divisors at or below
it (3 by default) all behave alike. The setting has no effect until the heap
is larger than `N` times the fixed amount (256 MiB for 16).

The default, `"0"`, keeps the fixed increment. Values must be non-negative
integers. The `GC_HEAP_GROWTH_DIVISOR` environment variable overrides the
build setting when the program starts; `0` there turns the scaling off.
This is a build setting shared by the application and its dependencies,
including database support.

## GC allocation budget

By default the collector starts a collection after the program has allocated
an amount derived from the previous collection: twice the pointer-containing
live data, plus a quarter of the pointer-free live data, plus the roots (with
thread stacks counted twice), all divided by the free space divisor (3 by
default). A program whose live data is mostly strings, byte buffers or numbers
therefore collects often compared with its heap size. The allocation budget
instead makes the amount a percentage of all live data and roots, like Go's
`GOGC`:

```python
build_options = {
    "gc_alloc_budget_percent": "100",
}
```

With `"100"`, a collection starts after the program has allocated as much as
survived the previous collection, so the heap settles at about twice the live
data. Larger values collect less often and use more memory. In incremental and
generational mode the amount is halved, as with the default policy. On the
`gc_heap` benchmark on macOS, 100 used 26-30% less CPU time and 28% more heap
than the default.

The budget is an upper bound. The heap still grows in steps set by the free
space divisor and `gc_heap_growth_divisor`, and the collector collects before
it grows the heap a second time since the previous collection. When the budget
exceeds the free heap plus one growth step, collections therefore come
earlier.

The default, `"0"`, keeps the free space divisor policy. The
`GC_ALLOC_BUDGET_PERCENT` environment variable overrides the build setting
when the program starts; `0` there restores the default policy. Under either
policy, `GC_MIN_BYTES_ALLOCD` sets the smallest amount allocated between
collections, in bytes with an optional `K`, `M` or `G` suffix. This is a build
setting shared by the application and its dependencies, including database
support.

## GC heap block size

The collector manages its heap in blocks. Each block holds objects of one size,
and a thread refilling its free list of a size takes the allocator lock once
per block. Larger blocks mean fewer lock acquisitions and fewer blocks to visit
in each collection:

```python
build_options = {
    "gc_block_size": "16384",
}
```

Valid values are powers of two from `"4096"` to `"65536"`; `"0"` (the default)
keeps the collector's 4 KiB blocks. Heap growth steps stay the same in bytes.
On Apple silicon, 16 KiB is the page size. On the `gc_heap` benchmark on macOS,
16 KiB blocks used 13% less CPU time with the same heap size, and 64 KiB blocks
13-18% less.

Objects larger than half a block get whole blocks, so larger blocks round
large objects up more coarsely: with 64 KiB blocks, a 40 KiB array occupies
64 KiB. A conservative false pointer into a free block also keeps more memory
from reuse. When blocks are larger than pages, incremental and generational
collection track dirty memory per block instead of per page. Measure memory
use as well as time before choosing a size.

`"65536"` cannot be combined with `gc_mark_bit_per_object`. This is a build
setting shared by the application and its dependencies, including database
support.

## GC mark stack

The collector marks live objects by pushing references to be scanned onto a
mark stack. With parallel marking, each marker thread works from a local stack
and shares work through a global one.

### Range stealing

With range stealing, a marker takes work from the global mark stack by
claiming a range of entries, sized to share the stack among the markers, with
one atomic compare-and-swap, so that each entry is taken by one marker. It is
on by default on targets with threads and can be turned off:

```python
build_options = {
    "gc_mark_range_stealing": "false",
}
```

Without it, a marker scans the global mark stack for a few entries at a time,
without a lock. The markers rescan entries that others have already taken,
and more than one marker can take the same entry and scan its object. This is
slow when the stack holds many small entries, as in generational collection,
where the markers start from the objects on dirty pages. Acton's collector
marks generational collections with all its markers, where upstream BDWGC
marks them on one thread. In measurements on a 32-thread Linux machine,
without range stealing these collections took 1.25 times the wall time and
2.5 times the CPU time of marking on one thread with 4 markers, and 1.9 to
2.4 times the wall time and 18 to 27 times the CPU time with 16 markers; the
longest pause was 2.6 to 6.6 times as long. With range stealing they took
0.83 to 0.93 times the wall time of marking on one thread, and the longest
pause was 0.5 to 0.8 times as long. Full collections were neither faster nor
slower with it.

No environment variable turns it off: the setting changes how the collector
is compiled. On a target without threads the collector has no parallel
markers, the default is `"false"`, and `"true"` fails the build.

### Initial size

The global mark stack starts with as many entries as a heap block has bytes:
4096 with the default blocks. When marking overflows it, the collector drops
entries, later scans the heap for marked objects to recover the dropped work,
and doubles the stack for the next collection. With large and wide data
structures, overflows can recur for several collections and add seconds to
pauses. The stack can start bigger:

```python
build_options = {
    "gc_initial_mark_stack_size": "1048576",
}
```

The value is a number of entries: a power of two of at least 4096 whose size
fills whole heap blocks, which holds for every power of two from 4096 on
64-bit targets. `"0"` (the default) keeps the initial size above. Each entry
takes 16 bytes on 64-bit targets, so 1048576 entries use 16 MiB for the life
of the process. In a multi-actor service with a heap of about 24 GB,
1048576 entries removed the mark stack overflows seen with the default size
and cut the total pause time from 50.6 to 20.0 seconds and the longest pause
from 11.9 to 2.3 seconds.

Both are build settings shared by the application and its dependencies,
including database support.

## GC object end padding

The collector treats a pointer into the interior of an object as a reference
to it. So that a pointer just past the end of an object also keeps the object
alive, the collector adds a byte to every allocation. With 16-byte size steps,
the padding turns a 16-byte object into 32 bytes and a 32-byte object into 48.
The padding can be turned off:

```python
build_options = {
    "gc_no_end_padding": "true",
}
```

Without padding, a pointer just past the end of an object points at the next
object and keeps that one alive instead, so an object that is referenced only
through such a pointer can be freed while still in use. Acton's runtime
allocates strings and byte buffers with room for a terminating NUL, so its end
pointers stay inside the object. Turn padding off only if the C extensions and
C libraries in the application never keep an object alive through its end
pointer alone. On the `gc_heap` benchmark, the heap and resident memory were
9.5% smaller. The default is `"false"`. This is a build setting shared by the
application and its dependencies, including database support.

## GC thread-local allocation size

Each thread allocates small objects from its own free lists, one list per size
step (16 bytes on 64-bit targets), and takes the collector's allocator lock
only to refill a list. Objects above a size limit do not use these lists:
every allocation of one takes the lock. By default the limit is 384 bytes on
64-bit targets. When several threads allocate many objects above it, they wait
for the lock. The limit can be raised:

```python
build_options = {
    "gc_thread_local_size_limit": "2048",
}
```

The value is an object size in bytes, including the end padding byte, so with
padding on a limit of 2048 covers allocations of up to 2047 bytes. It must be
a multiple of 16 and at most half the heap block size: 2048 with the default
4 KiB blocks, 8192 with 16 KiB blocks. `"0"` (the default) keeps the
collector's limit. A higher limit makes each thread's table of free lists
larger (by about 3 KB at 2048 and 64 KB at 32768), and each thread can hold
partly used blocks of more sizes. This is a build setting shared by the
application and its dependencies, including database support.

## GC thread-local warm-up

In BDWGC, a new thread does not use its own free list of a size until it has
allocated about a heap block of objects of that size: until then, each of its
allocations of that size takes the allocator lock. This keeps a thread that
allocates only a few objects of a size from holding a list of them, but every
new thread takes the lock once per object for each size it uses at first, and
threads that start together contend for it. In a 16-thread service with a
collector tuned to take the lock less often, and with the old copies of
[moved objects](#gc-objects-moved-by-realloc) left to the collector, these
allocations were a quarter of the lock acquisitions of one phase of its work.

By default, Acton skips this warm-up: a thread takes its own free list of a
size at its first allocation of that size, which takes the lock once. The cost
is memory: each thread can hold up to a heap block of free objects of every
size it has allocated at least once, separately for objects with and without
pointers. With the default blocks and [size limit](#gc-thread-local-allocation-size)
that is at most 200 KiB per thread, and more with larger blocks or a higher
limit. The warm-up can be turned back on:

```python
build_options = {
    "gc_no_thread_local_warmup": "false",
}
```

The `GC_NO_THREAD_LOCAL_WARMUP` environment variable overrides the build
setting when the program starts: `0` turns the warm-up back on and `1` skips
it, without rebuilding. On a target without threads there are no thread-local
free lists, the default is `"false"`, and `"true"` fails the build.

## GC objects moved by realloc

When a list or a bytearray outgrows its storage, Acton's runtime doubles the
capacity with `GC_realloc`. If the storage cannot grow in place, `GC_realloc`
allocates a new object, copies the contents and, in BDWGC, frees the old copy
with `GC_free`, which takes the collector's allocator lock. The new object
usually comes from the thread's own free lists without the lock, so threads
that grow lists take the lock for nearly every move only to free the old
copy. In a 16-thread service whose collector was tuned to take the lock less
often, these frees were 66% and 93% of the lock acquisitions in two phases of
its work. Leaving the old copies to the collector cut the acquisitions 3 and
15 times, with the same number of collections and the same peak memory.

By default, Acton leaves a small old copy to the collector, which reclaims it
like any other unreachable object. Old copies larger than half a heap block
(2 KiB with the default blocks) are still freed at once: such a copy occupies
whole heap blocks, which freeing returns to the heap right away, at the cost of
one acquisition of the allocator lock. The behaviour can be turned off, so that
every old copy is freed:

```python
build_options = {
    "gc_realloc_no_free": "false",
}
```

The `GC_REALLOC_NO_FREE` environment variable overrides the build setting
when the program starts: `0` turns the behaviour off and `1` turns it on,
without rebuilding.

The cost is memory reuse: an old copy's memory is not reused before the next
collection, and it counts toward the allocation that starts a collection. A
program that grows many small lists can therefore collect more often. An
Acton program on macOS (arm64) that keeps 4096 lists of up to 256 integers and
keeps replacing them with new lists built by `append` collected 19% more often
on one thread and used 11% more CPU time in 2% more wall time. With 32768
lists, or with lists of up to 4096 integers, it collected 3% to 5% more often
and used 3% to 8% more CPU time. With four actors building lists in parallel,
it collected 13% more often but took 14% less wall time and 7% less CPU time:
with fewer frees taking the allocator lock, its system time fell from 3.9 to
2.2 seconds. In a C benchmark whose allocations are almost all such arrays,
one thread collected up to 2.1 times as often and took up to 1.4 times the
wall time. Measure a program that grows many small lists on one thread both
ways.

## Inspecting the collector

An application can inspect its current collector configuration:

```python
import acton.rts

actor main(env):
    info = acton.rts.get_gc_info(env.syscap)
    print(info.mode)
    print(info.configured_backend)
    print(info.backend)
    env.exit(0)
```

`configured_backend` is the build choice; `backend` is the active mechanism,
using the same `soft_dirty` and `userfaultfd` names. In ordinary mode the active
backend is `none`. `supported_backends` lists compiled capabilities, not a
promise that the host kernel permits them.

The result also reports `page_hash_table_log2`, `block_size`, `end_padding`,
`thread_local_size_limit`, `no_thread_local_warmup`, `realloc_no_free`,
available `markers` (including the initiating thread), `mark_range_stealing`,
`initial_mark_stack_size`, `pause_target_ms`, `free_space_divisor`,
`full_frequency`, `heap_growth_divisor`, `alloc_budget_percent`, `heap_size`,
`free_bytes` and `unmapped_bytes`. The pause target is `None` for ordinary and
unlimited generational collection, and is not a guaranteed maximum pause.
Available markers need not participate in every incremental marking attempt.
Settings that an environment variable can override are reported as in effect,
after the override.

Heap sizes are bytes; both `heap_size` and `free_bytes` include unmapped
capacity. Their difference approximates occupied GC heap, not resident memory
or deployment memory. The query takes a consistent snapshot and does not
change collector policy or force a collection.
