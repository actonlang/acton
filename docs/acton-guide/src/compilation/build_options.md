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

## Worker threads

The runtime starts one worker thread per CPU, and at least 4. By default it
supports at most 255 worker threads, so a machine with more CPUs gets 255. An
application can set a different limit:

```python
build_options = {
    "max_wthreads": "1023",
}
```

The value must be from 4 to 65535. On a machine with more CPUs than the limit,
the runtime starts as many worker threads as the limit allows. A program run
with `--rts-wthreads` above the limit exits with an error at startup.

The runtime keeps the state of each worker thread in tables sized by the limit
when the program is built. The limit is therefore shared by the application,
its Acton dependencies and the standard library, and it cannot be changed by a
running program. The tables take about 1 KiB of memory for each worker thread
that the limit allows, whether or not the program starts it. Each running
worker thread also has a libuv loop with file descriptors of its own, two on
macOS. The runtime raises its [open file limit](../rts.md#open-files) at
startup, so only a low hard limit stops a program with many worker threads.
Programs built with `--no-threads` ignore the option.

## GC defaults

Acton changes these defaults of BDWGC. Each change can be turned off, as
its section below describes:

| Behaviour | Turn off in `build_options` | Turn off when starting the program |
|---|---|---|
| [Range stealing](#range-stealing) by parallel markers | `"gc_mark_range_stealing": "false"` | not possible |
| A mark phase gets [one marker per 4 MiB](#gc-markers-per-mark-phase) it is estimated to scan instead of all of them | `"gc_bytes_per_marker": "0"` | `GC_BYTES_PER_MARKER=0` |
| [Old copies of objects moved by `GC_realloc`](#gc-objects-moved-by-realloc) are left to the collector | `"gc_realloc_no_free": "false"` | `GC_REALLOC_NO_FREE=0` |
| A new thread uses its own free lists [without a warm-up](#gc-thread-local-warm-up) | `"gc_no_thread_local_warmup": "false"` | `GC_NO_THREAD_LOCAL_WARMUP=0` |
| A refill of a thread's free list takes [up to 16 heap blocks](#gc-thread-local-refills) under the allocator lock instead of one | `"gc_thread_local_refill_blocks": "1"` | `GC_TL_REFILL_MAX=1` |
| The global mark stack [starts with 4194304 entries](#initial-size), 1048576 on Windows, instead of 16384 with the default blocks | `"gc_initial_mark_stack_size": "0"` | not possible |
| A collection starts after the program has allocated [twice the live data](#gc-allocation-budget), pointer-free data included | `"gc_alloc_budget_percent": "0"` | `GC_ALLOC_BUDGET_PERCENT=0` |
| One automatic heap expansion may add up to [a quarter of the heap](#gc-heap-growth) instead of 16 MiB | `"gc_heap_growth_divisor": "0"` | `GC_HEAP_GROWTH_DIVISOR=0` |
| The heap is managed in [blocks of 16 KiB](#gc-heap-block-size) instead of 4 KiB | `"gc_block_size": "4096"` | not possible |
| Threads allocate objects of up to [2048 bytes](#gc-thread-local-allocation-size) from their own free lists, instead of 384 | `"gc_thread_local_size_limit": "0"` | not possible |
| The collector keeps its marks as [bits](#gc-mark-layout) instead of bytes | `"gc_use_mark_bits": "false"` | not possible |

## GC mark layout

The collector keeps the marks of a heap block in the block's header, one mark
per allocation granule (16 bytes on 64-bit targets). With parallel marking,
BDWGC stores each mark in a byte. Acton stores the marks as bits by default.
With 16 KiB blocks a header then takes 208 bytes instead of 1104, and the
headers add 1.3% to the heap instead of 6.7%. Clearing the marks before each
collection clears about an eighth as many bytes, and the walk over all headers
at the start of the sweep steps about a fifth as far from one header to the
next. Setting a mark bit takes an atomic operation, since parallel markers may
set bits of the same word, where setting a mark byte is a plain store.

On an M1 with 4 markers and on an AMD Ryzen 9 5950X (Linux) with 16, each
workload ran 8 rounds with either layout: the `gc_heap` and `fleet` benchmarks,
binarytrees, merkletrees, a build of 4 million records and fleetmgr loading
10,000 and 50,000 devices. In the geometric mean over the rounds, mark bits used
2 to 14% less CPU time than mark bytes, 2 to 10% less wall time and 3 to 6% less
peak resident memory in each workload. The CPU time was lower in every round but
one, of the `gc_heap` benchmark on the Ryzen. The wall time was lower in 7 or 8
of the 8 rounds of each workload except fleetmgr loading 10,000 devices on the
Ryzen, where it was lower in 4 rounds and the same in 4. World-stopped marking
took 4 to 21% less time in the workloads whose peak resident memory was 390 MB
or more with mark bytes. Loading 50,000 devices, fleetmgr used 14% less CPU time
and 19% less time in world-stopped marking. Binarytrees and merkletrees, whose
peak resident memory was 105 and 84 MB with mark bytes, gained the least: they
took as long to mark with either layout, and gained only through the cheaper
clearing and sweep start.

Mark bytes, BDWGC's default with parallel marking, can be selected instead:

```python
build_options = {
    "gc_use_mark_bits": "false",
}
```

Without parallel marking, as on WebAssembly, BDWGC uses mark bits whatever
`gc_use_mark_bits` says. `gc_mark_bit_per_object` indexes marks by object
instead of allocation granule, which changes the work needed to find an
object's mark. It defaults to `"false"`. The options are independent, so
either can be set on its own. Measure on the application's workload before
changing them.

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
With Acton's 16 KiB collector blocks, addresses 32 GiB apart share an entry (8
GiB with 4 KiB blocks). Setting 23 uses 1 MiB per table and moves that address
interval to 128 GiB. Larger tables can reduce unnecessary rescanning when clean
pages share entries with dirty pages. They cost more memory and clearing/copying
work. Several tables exist, including conservative-pointer blacklisting tables,
so this also affects ordinary collection. It is independent of object mark-bit
layout.

Measure the application's workload before choosing a larger table. Increasing
this value does not remove dirty-tracking faults or guarantee fewer collections.

## GC heap growth

Programs whose live data grows to gigabytes need the collector to expand the
heap in larger steps. Acton sets the heap growth divisor to 4 by default:

```python
build_options = {
    "gc_heap_growth_divisor": "4",
}
```

Without it, one automatic heap expansion adds at most a fixed amount (16 MiB
in Acton's configuration), and the collector runs a full collection before
expanding again. A heap that grows from nothing to 4 GiB therefore needs
hundreds of full collections, each marking all live data. With a divisor of
`N`, one expansion may add up to the heap size divided by `N` when that is
larger than the fixed amount, so the number of these collections grows with
the logarithm of the heap size instead. Building a 1 GiB list of small
objects took 78 collections with the fixed increment, 53 with 16 and 39 with
8.

The cost is a larger heap. On macOS, with 4 KiB blocks and an allocation budget
of 67%, the divisor 4 made the fleetmgr load of 50,000 devices use 45% less CPU
time and settle 30% sooner, at 32% more peak memory (4.1 to 5.5 GiB). The fleet
load benchmark used 7% less CPU time at 63% more peak memory (316 MiB with the
fixed increment). Larger divisors are not much cheaper in memory: 16 still cost
28% more peak memory on fleetmgr. The free space divisor bounds each expansion,
so divisors at or below it (3 by default) all behave alike. The setting has no
effect until the heap is larger than `N` times the fixed amount (64 MiB for 4).

`"0"` keeps the fixed increment. Values must be non-negative integers. The
`GC_HEAP_GROWTH_DIVISOR` environment variable overrides the build setting when
the program starts; `0` there turns the scaling off. This is a build setting
shared by the application and its dependencies, including database support.

## GC allocation budget

BDWGC starts a collection after the program has allocated an amount derived
from the previous collection: twice the pointer-containing live data, plus a
quarter of the pointer-free live data, plus the roots (with thread stacks
counted twice), all divided by the free space divisor (3 by default). A program
whose live data is mostly strings, byte buffers or numbers therefore collects
often compared with its heap size: pointer-free data counts an eighth as much as
pointer-containing data. The allocation budget instead makes the amount a
percentage of all live data and roots, like Go's `GOGC`. Acton uses a budget of
200% by default, and the build setting changes it:

```python
build_options = {
    "gc_alloc_budget_percent": "100",
}
```

With 200%, the collector collects rather than grow the heap once the program has
allocated twice as much as survived the previous collection, so the heap holds
about three times the live data or more. Larger values collect less often and
use more memory, smaller values collect more often and use less. Pointer-free
data, such as the contents of strings and byte buffers, counts like other data
instead of an eighth as much. In incremental and generational mode the amount is
halved, as with the default policy.

The budget does not fix when collections come. A collection comes only when
the allocator needs heap blocks that it does not have free. The collector then
collects if the program has allocated the budget since the previous collection
or if the heap has already grown once since then, and otherwise grows the heap
by one step (see `gc_heap_growth_divisor`). While the live data grows,
collections therefore follow the growth steps and come before the budget is
used. With steady live data the heap settles at the first step whose free part
covers the budget, so peak memory changes in steps of a quarter of the heap. A
heap that grew larger earlier, for example while the program built its data,
keeps its size, and a budget below its free part changes nothing.

Compared with 100%, 200% gave these medians of interleaved rounds, on an AMD
Ryzen 9 5950X running Linux with 16 markers and mark bits (8 rounds), and on
an Apple M6 with 4 worker threads, 4 markers and mark bytes (12 rounds):

| Workload | Machine | CPU | Wall | Peak memory |
|---|---|---|---|---|
| Fleet manager, 10,000 devices | Ryzen | -29% | -19% | +51% |
| Fleet manager, 50,000 devices | Ryzen | -28% | -14% | +25% |
| GC heap cold inventory, a million records | Ryzen | -30% | -8% | +25% |
| GC heap build, 4 million records | Ryzen | -21% | +1% | +56% |
| Fleet load, 10,000 devices, 16 workers | Ryzen | -5% | -5% | +25% |
| GC heap updates, a million records | M6 | -23% | -12% | +56% |
| GC heap cold inventory, a million records | M6 | -18% | -8% | +56% |
| Fleet load, 10,000 devices, 4 workers | M6 | -8% | -8% | +25% |
| Binary trees | M6 | 0% | 0% | 0% |
| Merkle trees | M6 | +1% | +1% | 0% |

The binary and merkle trees benchmarks keep the heap they grow at start-up,
whose free part already exceeds the budget. Against 200%, 300% saved the fleet
manager another 15 and 14% CPU time for 24 and 25% more peak memory, and the
GC heap cold inventory on the Ryzen 23% for 56%. The other workloads saved at
most 7% for 25% more peak memory, and the GC heap build none. 150% gave 44% to
all of the CPU saving of 200% for at most 25% more peak memory than 100%.

A program short of memory can lower the budget, in `build_options` as above
or, without a rebuild, with the `GC_ALLOC_BUDGET_PERCENT` environment variable
when it starts, for example `GC_ALLOC_BUDGET_PERCENT=100`. In the eight
workloads above whose heap the budget changed, 100% peaked at 0.64 to 0.80
times the memory of 200% for 1.06 to 1.43 times the CPU time, and 67% at 0.51
to 0.78 times the memory for 1.12 to 2.05 times the CPU time.

`"0"` selects the free space divisor policy. The `GC_ALLOC_BUDGET_PERCENT`
environment variable overrides the build setting when the program starts; `0`
there selects the free space divisor policy. Under either policy,
`GC_MIN_BYTES_ALLOCD` sets the smallest amount allocated between collections,
in bytes with an optional `K`, `M` or `G` suffix. This is a build setting
shared by the application and its dependencies, including database support.

## GC heap block size

The collector manages its heap in blocks. Each block holds objects of one size,
and a thread refilling its free list of a size takes the allocator lock once
per [refill](#gc-thread-local-refills) of one block or more. Larger blocks mean
fewer lock acquisitions and fewer blocks to visit in each collection. Acton uses
16 KiB blocks by default, where BDWGC uses 4 KiB. The size can be changed:

```python
build_options = {
    "gc_block_size": "4096",
}
```

Valid values are powers of two from `"4096"` to `"65536"`; `"0"` also keeps
the collector's 4 KiB blocks. Heap growth steps stay the same in bytes. On
Apple silicon, 16 KiB is the page size.

On an M1 with 8 worker threads, an allocation budget of 67% and the heap growth
divisor at 4, compared with 4 KiB blocks, 16 KiB blocks took 12 to 31% of the
CPU time off the `gc_heap` benchmark and 13 to 18% off the `fleet` load
benchmark, which also ran in half the wall time. The fleetmgr load of 50,000
devices used 37% less CPU time and settled 53% sooner. An M4 Pro and an M6 gave
similar results, except that the `fleet` load benchmark used about the same CPU
time on the M6, in half the wall time. Actor message passing used 21% more CPU
time on the M1, in 8% less wall time, and 1 to 9% less on the other two.

The cost is memory where heaps are small. Every program starts with about 1
MiB more (a hello world program 3.5 to 4.6 MiB on the M1), the tests of the
standard library peaked at 15% more each, and benchmarks with heaps of 6 to 20
MiB at 23 to 42% more. Larger heaps changed less: the `gc_heap` benchmark's
shrank by 1.5% and the `fleet` load benchmark's by 1 to 24%, and fleetmgr
peaked at 12% more (5.5 to 6.1 GiB).

Objects larger than half a block get whole blocks, so larger blocks round
large objects up more coarsely: with 16 KiB blocks, a 9 KiB array occupies 16
KiB, and with 64 KiB blocks a 40 KiB array occupies 64 KiB. A conservative
false pointer into a free block also keeps more memory from reuse. When blocks
are larger than pages, incremental and generational collection track dirty
memory per block instead of per page. Measure memory use as well as time
before choosing a size.

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

When the global mark stack overflows while the collecting thread marks alone
(from the roots, in a mark phase with one marker, or from the objects that are
about to be finalized), the collector moves its entries to a larger stack and
goes on, without dropping any. In a parallel mark phase with range stealing the
global stack is a ring: the slots of entries that markers have taken are
reused, so it overflows only when the entries waiting at one time do not fit. A
marker that then finds no room drops its local stack, and after the phase the
collector doubles the stack and scans the whole heap again for marked objects,
on one thread, while normal marking runs on all marker threads. One such
overflow can make a collection several times longer. BDWGC starts the stack
with as many entries as a heap block has bytes, 16384 with Acton's default
blocks; Acton starts it with 4194304 entries by default, and with 1048576 on
Windows. The size can be changed:

```python
build_options = {
    "gc_initial_mark_stack_size": "16777216",
}
```

The value is a number of entries: a power of two of at least 4096 whose size
fills whole heap blocks, which holds for every power of two from 4096 on
64-bit targets. `"0"` gives BDWGC's initial size. Each entry takes 16 bytes on
64-bit targets, so 4194304 entries take 64 MiB and 1048576 entries 16 MiB, for
the life of the process. On Linux and macOS this is address space: a page of
the stack becomes resident only when marking first writes it, although under
strict overcommit (`vm.overcommit_memory=2`) Linux counts the whole stack
against the commit limit. Windows commits the whole stack when the program
starts, against the system's commit limit, which is why the default is smaller
there. Each marker thread also has a local stack of as many entries as a heap
block has bytes: 256 KiB with the default blocks. No environment variable
changes the size: the setting changes how the collector is compiled.

In a multi-actor service on a 32-thread Linux machine, 262144 entries
removed the collections that overflowed the stack with initial heaps of 1
and 4 GiB: the longest collection went from 1739 to 527 ms and from 2041 to
484 ms, and the total collection time fell by 18% and 48%; CPU time and
resident memory did not change. With a 32 GiB initial heap and about 6 GB
of pointer data, 262144 entries still overflowed once while the service
loaded its data: that collection took 10.6 to 12.6 seconds, 9 to 11 of
them spent rescanning the heap on one thread, against 1.4 seconds for the
other collections. With 1048576 entries and no other change, the overflow
did not happen, and the load took 197 to 202 seconds instead of 211 to
227.

How many entries a collection uses depends on the shape of the live data and,
with parallel markers, on how much work the markers pass through the global
stack in one mark phase: the stack holds all of it, including the entries that
markers have already taken. Programs with heaps of up to 3.4 GiB, the
benchmarks among them, used at most 9001 entries. The service used up to
290917 entries while it loaded 50,000 devices and up to 731573, 70% of
1048576, while it loaded 100,000. With 262144 entries, 8 markers and the other
settings at their defaults, it overflowed: an overflowing collection took 2.3
to 4.2 seconds against 0.4 to 0.8 seconds for the others, and at 100,000
devices the stack, with less than half the entries it needed, overflowed twice
before it fit. 4194304 entries leave 5.7 times the service's deepest use. They
cost nothing measurable but address space: against 1048576 entries, 4194304,
16777216 and 67108864 entries added 48, 240 and 1008 MiB to the virtual size
of a process, while peak resident memory, page faults, startup time and CPU
time stayed within the noise, on Linux and on macOS.

A larger stack moves the point where a program overflows it, but no size
removes it. A linked list whose payload field comes before its link field
needs one entry per node: a list of 1,000,000 such nodes used 995603 entries,
and a list of 5,000,000 nodes, 455 MiB at its peak, overflowed 4194304
entries. With the link field first, a list of 2,000,000 nodes used 59 entries.
After an overflow the stack keeps its doubled size, and the pages that marking
wrote stay resident, for the life of the process. A program with a very large
heap can set a larger size; on Linux and macOS it costs address space until
marking needs the entries.

Both are build settings shared by the application and its dependencies,
including database support.

## GC markers per mark phase

With parallel marking, the collector has up to 16 marker threads, by default
one per processor (hardware thread) that is not an efficiency core, since
efficiency cores mark the same work more slowly. At least 40% of all the
processors, rounded up, are counted, though, with as many efficiency cores as
that takes. An Apple M4 Pro, with 10 performance and 4 efficiency cores, has
10 markers, and an Apple M1, with 4 of each, has 4. An Intel processor with 2
two-thread performance cores and 10 efficiency cores has 6: its 4 processors
that are not efficiency cores are fewer than 40% of its 14 processors (5.6),
so 2 efficiency cores are counted too.

The efficiency cores are those of the macOS performance levels named
"Efficiency", on Linux, when an Intel hybrid processor lists processors in both
`/sys/devices/cpu_core/cpus` and `/sys/devices/cpu_atom/cpus`, those not in the
first list, and otherwise, as on arm64, the cores of the lowest capacity in
`/sys/devices/system/cpu/cpu<N>/cpu_capacity` if it is less than 3/4 of the
highest. On Linux, the collector counts every processor that `/proc/stat` lists,
however long the file is. With `GC_PRINT_STATS` set, the collector logs the
count when it leaves out efficiency cores, for example "Counting 10 of 14
processors for markers: efficiency cores fully excluded". The `GC_MARKERS`
environment variable sets the number of markers. `GC_NPROCS` sets the number of
processors, and the default number of markers then follows it with no efficiency
cores left out.

BDWGC marks every mark phase with all its markers. Waking up and coordinating
them costs more than they save when the phase has little to scan, and in a
phase of medium size most of them add CPU time without shortening the pause
much. Acton gives a mark phase one marker per 4 MiB it is estimated to scan,
rounded up: a phase estimated at under 4 MiB is marked by the collecting
thread alone, one estimated at 10 MiB by three markers, and a phase of a large
heap by all of them. The estimate is the pointer-containing data found live by
the previous collection, plus all that the program allocated since then, plus
the roots and the thread stacks, so it also grows with the allocation budget
and the initial heap size. The value can be changed:

```python
build_options = {
    "gc_bytes_per_marker": "8388608",
}
```

The value is a number of bytes that fits in the target's `size_t`. `"0"`
gives every mark phase all the markers, as BDWGC does. The
`GC_BYTES_PER_MARKER` environment variable overrides the setting when the
program starts, and accepts a `k`, `M` or `G` suffix:

```sh
GC_BYTES_PER_MARKER=0 ./out/bin/myapp
```

On a Linux machine with an AMD Ryzen 9 5950X and 16 markers, with
Acton's default 16 KiB blocks, heap growth divisor 4 and allocation
budget 100, 4 MiB per marker against all the markers gave these medians
of 12 interleaved rounds:

| Workload | Mark estimate | Markers | CPU | Wall |
|---|---|---|---|---|
| GC heap cold inventory, 14,000 records, 8 workers | 15 MiB | 4 | -34% | +1% |
| Fleet load, 400 devices, 8 workers | 19 MiB | 5 | -2% | +1% |
| Actor message passing, 16 workers | 7 MiB | 2.5 | 0% | 0% |

The mark estimate is the median per phase and the markers the mean per phase.
The wall times are within the noise: the cold inventory took more wall time
than with all the markers in 7 of 12 rounds. Larger values save a little more
CPU time and cost wall time: with 8 MiB the cold inventory used 38% less CPU
time and 8% more wall time (more in 11 of 12 rounds), and with 16 MiB, where
one thread marks it, 45% less CPU time and 16% more wall time. Fleet load with
10,000 devices, fleet upgrade, cold inventory of a million records, a GC heap
build of 4 million records and a fleet manager loading 10,000 devices marked
with fewer markers only in their first collections, while the heap was small,
and with 12 to 14 of the 16 on average. They changed by 1% or less, except the
CPU time of the fleet upgrade, which was 3% lower, with single rounds from 7%
lower to 8% higher. Peak memory did not depend on the setting.

The CPU saving grows with the number of markers. On an Apple M4 Pro with 14
markers, 4 KiB blocks, heap growth divisor 4, allocation budget 100 and 8
workers, 4 MiB per marker took 46% of the CPU time and 10% of the wall time off
the cold inventory in 6 rounds, 7% of the CPU time off the fleet load and 6%
off the actor message passing. On an Apple M6 with 6 markers, 4 KiB blocks,
heap growth divisor 4, allocation budget 67, 6 workers and a collector that
refills thread-local free lists from up to 16 blocks, it took 5% of the CPU
time off the cold inventory at the same wall time, and 8 MiB took 11% for 5%
more wall time.

The setting has no effect on a target without threads, where
`get_gc_info` reports 0. This is a build setting shared by the application
and its dependencies, including database support.

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
every allocation of one takes the lock. BDWGC's limit is 384 bytes on 64-bit
targets. When several threads allocate many objects above it, they wait for
the lock. With blocks of 16 KiB or more, Acton raises the limit to 2048 bytes
by default. The limit can be changed:

```python
build_options = {
    "gc_thread_local_size_limit": "4096",
}
```

The value is an object size in bytes, including the end padding byte, so with
padding on a limit of 2048 covers allocations of up to 2047 bytes. It must be
a multiple of 16 and at most half the heap block size: 8192 with the default
16 KiB blocks, 2048 with 4 KiB blocks. `"0"` keeps the collector's limit,
which is also the default with blocks smaller than 16 KiB. A higher limit
makes each thread's table of free lists larger (by about 3 KB at 2048 and 64
KB at 32768), and each thread can hold partly used blocks of more sizes: with
the default blocks, up to 4 MiB per thread at 2048 against 800 KiB at 384.

On an M1 with 8 worker threads and 16 KiB blocks, 2048 bytes against 384 took 8
to 9% of the CPU time off the `fleet` load benchmark and 13% off `pidigits`,
which peaked at 43% more memory. The `gc_heap` benchmark and the fleetmgr load
of 50,000 devices changed by less than 1.5%. Limits of 4096 and 8192 saved no
more CPU time on the `fleet` load and `gc_heap` benchmarks, and `pidigits`
peaked at 77% and 127% more memory than with 384. With 4 KiB blocks, 2048 bytes
made the `fleet` load benchmark with 400 and 10,000 devices take 6 to 14% more
wall time on the M1 and on an M6, so the default follows the block size.

This is a build setting shared by the application and its dependencies,
including database support.

## GC thread-local refills

A thread refills its free list of a size, when the list is empty, under the
collector's allocator lock. In BDWGC one refill takes at most one heap block
of objects, so a thread that allocates many small objects takes the lock
often, and with many allocating threads the lock decides how fast they
allocate. Acton lets one refill take up to 16 blocks under one acquisition of
the lock, and the collector builds their free lists after releasing it. A free
list gets one block at its first refill after a collection, and twice as many
at each further refill before the next collection, up to 16, so a size that a
thread allocates only now and then still takes one block at a time. The value
can be changed:

```python
build_options = {
    "gc_thread_local_refill_blocks": "4",
}
```

The value is from 1 to 32. `"1"` takes one block per refill, as BDWGC does.
The `GC_TL_REFILL_MAX` environment variable overrides the setting when the
program starts:

```sh
GC_TL_REFILL_MAX=1 ./out/bin/myapp
```

The cost is memory. A thread can hold up to that many blocks of free objects on
each of its free lists: 256 KiB per list with 16 of the default [16 KiB
blocks](#gc-heap-block-size), and 64 KiB with 4 KiB blocks. With the default
blocks and [size limit](#gc-thread-local-allocation-size), a thread that
allocates much of every size between two collections could hold up to 64 MiB,
16 times as much as with one block per refill. The objects on a thread's lists
count as live data at every collection. The first refill by a thread after a
collection empties its lists that it has not refilled since the collection
before the latest one, so that the next collection reclaims their objects. A
thread that stops allocating keeps its lists until it allocates again or exits.

On an AMD Ryzen 9 5950X (16 cores, 32 threads, 16 markers) and an Apple M6
(6 workers, 6 markers), with 16 KiB blocks, a thread-local size limit of 2048
bytes, an allocation budget of 100%, a heap growth divisor of 4 and all
markers in every mark phase, 16 blocks per refill gave these medians of 12
interleaved rounds (fleet manager: 6) against 1 block per refill:

| Workload | CPU | Wall |
|---|---|---|
| Fleet manager loading 50,000 devices, 8 workers, Ryzen | -2.4% | -4.3% |
| Fleet load, 8 / 16 / 32 workers, Ryzen | +0.5 / -1.3 / -3.1% | -0.1 / +0.5 / +0.4% |
| Fleet load, 6 workers, M6 | +1.4% | +1.5% |
| Fleet upgrade, 32 workers, Ryzen | -5.2% | -0.1% |
| GC heap cold inventory, a million records, Ryzen / M6 | -2.8 / -10.0% | -0.9 / -4.9% |
| GC heap cold inventory, 14,000 records, M6 | -14.8% | -8.0% |
| GC heap build of 4 million records, Ryzen | -0.9% | -2.5% |
| Scheduling pairs, Ryzen / M6 | +0.1 / -0.4% | -0.1 / -1.2% |
| `binarytrees` 18, Ryzen / M6 | -5.2 / -3.7% | -2.7 / -2.8% |
| `secp256k1`, Ryzen / M6 | -14.8 / -6.4% | -6.2 / -5.8% |

The fleet upgrade's wall time is fixed by the workload. With the fewest
workers the fleet load collected 10 to 11% more often (8 workers on the
Ryzen, 4 on the M6). Heaps of up to about 40 MB peaked 8 to 44% higher and
collected 23 to 48% less often: the scheduling pairs, the fleet load of 400
devices, the small cold inventory, `coro-prime-sieve`, `secp256k1` and
`fasta`. On larger heaps the peak did not change beyond its spread between
runs.

The setting has no effect on a target without threads, where `get_gc_info`
reports 0. This is a build setting shared by the application and its
dependencies, including database support.

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
that is at most about 4 MiB per thread, and more with larger blocks or a
higher limit. The warm-up can be turned back on:

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
(8 KiB with the default blocks) are still freed at once: such a copy occupies
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
`thread_local_size_limit`, `no_thread_local_warmup`,
`thread_local_refill_blocks`, `realloc_no_free`, available `markers` (including
the initiating thread), `bytes_per_marker`, `mark_range_stealing`, `mark_bits`,
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
