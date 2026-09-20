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

The result also reports `page_hash_table_log2`, available `markers` (including
the initiating thread), `pause_target_ms`, `free_space_divisor`,
`full_frequency`, `heap_size`, `free_bytes` and `unmapped_bytes`. The pause
target is `None` for ordinary and unlimited generational collection, and is
not a guaranteed maximum pause. Available markers need not participate in
every incremental marking attempt.

Heap sizes are bytes; both `heap_size` and `free_bytes` include unmapped
capacity. Their difference approximates occupied GC heap, not resident memory
or deployment memory. The query takes a consistent snapshot and does not
change collector policy or force a collection.
