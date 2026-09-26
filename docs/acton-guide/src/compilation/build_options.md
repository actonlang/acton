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

By default a marker scans the global mark stack for a few entries at a time,
without a lock. The markers rescan entries that others have already taken,
and more than one marker can take the same entry and scan its object. This is
slow when the stack holds many small entries, as in generational collection,
where the markers start from the objects on dirty pages. With range stealing,
a marker instead claims a range of entries, sized to share the stack among the
markers, with one atomic compare-and-swap, so that each entry is taken by one
marker:

```python
build_options = {
    "gc_mark_range_stealing": "true",
}
```

Full collections were neither faster nor slower with it in measurements. The
default is `"false"`. It requires a target with threads.

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

The result also reports `page_hash_table_log2`, `block_size`, available
`markers` (including the initiating thread), `mark_range_stealing`,
`initial_mark_stack_size`, `pause_target_ms`, `free_space_divisor`,
`full_frequency`, `heap_growth_divisor`, `alloc_budget_percent`, `heap_size`,
`free_bytes` and `unmapped_bytes`. The pause target is `None` for ordinary and
unlimited generational collection, and is not a guaranteed maximum pause.
Available markers need not participate in every incremental marking attempt.

Heap sizes are bytes; both `heap_size` and `free_bytes` include unmapped
capacity. Their difference approximates occupied GC heap, not resident memory
or deployment memory. The query takes a consistent snapshot and does not
change collector policy or force a collection.
