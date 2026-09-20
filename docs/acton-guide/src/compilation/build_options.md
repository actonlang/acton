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
