# Builtin and collection benchmarks

Run these modules with a compiler that supports `t.scale()` and `t.loop()`:

```sh
acton test list --module builtin_functions --module collection_iterators
acton test perf --module builtin_functions --module collection_iterators
```

For the collection modules, scale means **input element count**. Each loop body
performs one operation on that input. For example, `sum` at scale 100000 sums a list of 100,000 elements;
`filter_all` creates and fully consumes one filter iterator over that list.
The report gives time and allocation per operation. Empty inputs stay empty at
every scale, and their controls measure one empty operation per body.

Read `t.scale()` to construct the input before `t.loop()`. Scale stays fixed
throughout an invocation. Calibration, warmup and measured invocations each
prepare their own inputs. Within an invocation, the runner repeats loop bodies
as needed for the time budget. Ordinary `acton test` runs a body once at scale 1
and checks its result.

Performance mode builds optimized code and runs one benchmark process at a time.
To use the representative input size from the earlier fixed-size benchmarks:

```sh
acton test perf --module builtin_functions --module collection_iterators --scale 1024
```

To explore the growth curve over a chosen range:

```sh
acton test scale --module builtin_functions --name filter_all --start-scale 1 --end-scale 100000
```

Empty-input controls and operations that can return immediately need not get
slower with scale. They are useful controls, but do not exercise a growing scan.
Use `acton test perf --scale 1` for fixed empty-operation measurements.

Collection inputs, mutable targets and accumulators are prepared before
`t.loop()`. Range inputs are consuming iterators, so `sum_range` and `zip_mixed`
create a fresh range inside each body. Result checks run after the loop and
tolerate no bodies when setup exhausts the time budget. Checksums and consumed
counts cover all completed bodies; collection tests check accumulated lengths
and final contents.

Constructors include output allocation. Dict updates start with all keys present
and measure value replacement. Set updates start with all elements present and
measure duplicate insertion. List and bytearray slice assignments start with a
target of the matching size. These cases measure the same kind of operation
from their first body, including when a scaling study measures just one body.

The builtin tests cover numeric sum variants, extrema with and without defaults,
filter selectivity and mixed zip iterators. The shared integer-list inputs contain
every integer from 0 to scale minus one, with odd values first, then positive
even values, and zero last. This permutation works at arbitrary positive scales
and keeps the minimum away from the first element when the input has more than
one element. Lazy iterators are consumed with ordinary loops so another builtin
under comparison cannot hide their cost. The `control` case traverses the input directly to help
assess measurement noise. Enumeration retains its C implementation and provides
another iterator control.

The collection tests cover list, dict and set construction, slice assignment,
dict and set updates, and set disjointness. Disjointness cases cover a full scan
of two size-N sets, immediate overlap, a size-N set against an empty set, and a
one-element set against N elements. The last case exercises the smaller-set path
when N is greater than one. Bytearray slice inputs cycle through the byte values
0 to 255, covering every byte value once scale reaches 256.

The disjoint and empty-set cases and both bytearray slice cases expose iterator
exhaustion bugs fixed in #3104. Before that fix, `StopIteration` escapes the
operation and the completion checks fail. Report a failed baseline rather than
treating the failure time as an operation latency or speedup.

## Exceptions

`exceptions` measures a fixed builtin error, a fixed Acton error, and a formatted
error that carries its input. Each loop body raises and catches one exception;
inputs are prepared before the loop. These are fixed workloads, so use scale 1:

```sh
acton test list --module exceptions
acton test perf --module exceptions --scale 1 --time 3s
```

Compare allocated bytes per operation as well as time. Catch frames and work
before the error can still allocate even when the exception and message are
shared. Allocation counts measure heap traffic, not retained memory or RSS.

## String operations

`string_operations` covers decoding, comparison, prefix and suffix checks,
hashing, and iteration. Scale is the UTF-8 byte count of the prepared input;
each loop body performs one operation or comparison group. Mixed and Unicode
inputs contain complete codepoints at every size. Prefix comparisons use
strings of N and N+1 bytes; affix checks use an N-byte affix and N+4-byte text.
The two short decode cases keep their fixed inputs as controls.

```sh
acton test perf --module string_operations --scale 65536 --time 3s --record
acton test scale --module string_operations --name equal_strings --start-scale 2 --end-scale 65536
```

Equal strings use separate decoded buffers. Empty and one-byte ASCII strings
can share cached objects, so start at scale 2 to measure byte comparisons.
All timed strings contain ordinary text; embedded NUL and malformed UTF-8
are covered by `test/builtins_auto/bytes_decode.act`.

## JSON conversion

`json_strings` covers object and array encoding and decoding with long ASCII
and Unicode keys and values. Scale is the object field count, or the number
of record/string pairs in an array. Each loop body converts one complete
prepared document. Fixture construction and full content checks run outside
`t.loop()`; conversion, output allocation and result-length accumulation are timed.

```sh
acton test perf --module json_strings --scale 64 --time 3s --record
acton test scale --module json_strings --start-scale 1 --end-scale 256
```

The inputs contain ordinary text without escapes. Expected JSON text is
assembled independently of the encoder. Embedded NUL and malformed input are
covered by the functional JSON tests. When comparing JSON implementations,
keep the string implementation and benchmark sources the same in both runs.

## GC pressure over a retained inventory

`gc_heap` retains an inventory of small records, each holding an integer key,
a list and four freshly allocated strings. `cold_inventory` keeps these
records unchanged; `updates` replaces groups spread throughout the inventory.

Scale is the retained record count. Each measured body creates four times
that many temporary records, rounded up to complete 4,096-record chunks:
`4096 * ceil(4 * scale / 4096)`. At scale 1,000,000, one body creates
4,001,792 temporary records. The updates case also replaces four groups of
up to 32 live records per chunk. Replaced groups become garbage, while the
retained record count stays fixed. Larger bodies spread collection costs
across more allocation work, including in `acton test scale`, which measures
only one body per sample.

On the Linux x86-64 build used for these experiments, one million records
retain roughly 308 MiB of GC objects (about 11 million separate allocations).
Four million records retain roughly 1.2 GiB. These estimates exclude runtime
and collector overhead; a configured heap allowance is not the live size.

A local bank of 32 payload patterns keeps the mix of string sizes unchanged
as records are replaced. Each record still allocates its own four payload
strings. Verification compares every retained payload against precomputed
expected values, avoiding the previous full inventory's worth of temporary
comparison strings. It also checks keys, group sizes and total record count.
Setup and final inventory verification are outside `t.loop()`. Numeric checks
avoid boxed comparison values and enumeration tuples. The complete final
traversal keeps the full inventory reachable throughout measurement. After
verification, each group and then the outer list is cleared, limiting how
much of a previous inventory stale conservative roots can retain between
invocations.
Each body checks its own checksum, and temporary numeric labels cycle within
a bounded range so prolonged runs do not accumulate an overflowing checksum.

A coprime stride distributes replacements over every group at arbitrary
positive scales. At small scales, one chunk can replace a group more than
once. Ordinary tests run one 4,096-record chunk at scale 1. The fixture uses
normal automatic collection, without forced collections between bodies.

```sh
acton test --module gc_heap
acton test perf --module gc_heap --scale 100000 --time 10s
acton test perf --module gc_heap --scale 1000000 --time 20s
acton test perf --module gc_heap --scale 4000000 --time 60s
acton test scale --module gc_heap --name cold_inventory --start-scale 1024 --end-scale 1000000 --max-memory 2GiB
```

Choose an explicit scale for repeatable comparisons. Scale now increases
both the retained heap and the work per body; timings are not directly
comparable to the earlier fixed-4,096-record version of this fixture.
Compare the two compiler builds using the same current source snapshot.

Large fixtures need time for construction, warmup and verification as well
as measured bodies. All consume the overall budget. Increase `--time` if
preparation leaves few or no measured bodies. "Performance time budget
exhausted during preparation" means the process finished without a usable
measurement. A body already in progress can finish after its time slice.
Peak RSS includes preparation and runtime overhead, not just live payload.

Read whole-body wall time, allocated bytes and total CPU together. Confirm
that the run includes measured GC work; neither a positive record count nor
a large allocation multiplier alone guarantees collection. The reported GC
counter is not a pause distribution or completed-collection count, and does
not measure all incremental work.

To compare compiler-runtime `memset` or `memcmp` changes, use the toolchain
comparison utility from the repository root:

```sh
utils/perf-compare origin/main --module gc_heap --scale 1000000 --time 20s
```

It builds both Acton revisions with `make` and gives both compilers the same
snapshot of the current `test/perf/Build.act` and `src`. The baseline therefore
does not need to contain `gc_heap`. `main` selects the local branch;
`origin/main` selects the fetched remote-tracking revision. The frontend's
`acton test perf --compare git:REF` uses the same current compiler and runtime
for both application revisions, so it does not compare these patches.

To compare GC mark layouts with one compiler, set `build_options` in this
project's `Build.act`:

```python
build_options = {
    "gc_use_mark_bits": "true",
    "gc_mark_bit_per_object": "true",
}
```

Then compare against a revision with the same benchmark source and the baseline
`Build.act` settings:

```sh
acton test perf --module gc_heap --name cold_inventory --scale 1000000 \
  --time 20s --compare git:REF
```

Each checkout uses its own root options, so this
comparison can measure the layout change with the current compiler. Test the
options separately as well as together; removing them restores defaults. Perf
and scale recordings retain the options, and ordinary test caches are
invalidated when they change. `utils/perf-compare` gives both compilers the same
`Build.act`, so use it for changes to the compiler or collector implementation.

For a controlled one-million-record comparison, keep the initial and maximum
GC heap equal and explicitly set marking parallelism:

```sh
GC_INITIAL_HEAP_SIZE=2G GC_MAXIMUM_HEAP_SIZE=2G GC_MARKERS=4 \
  utils/perf-compare origin/main --module gc_heap --scale 1000000 --time 20s
```

`2G` means 2 GiB. The equal sizes prevent ordinary heap growth during the
comparison; they do not cap total process RSS. Four GC markers include the
thread initiating collection. This controls GC parallelism while preserving
Acton's normal runtime worker configuration. Both revisions inherit identical
settings. These are benchmark controls, not proposed production defaults.
Larger inventories may need a larger heap allowance, and a cap that is too
small can cause allocation failure. Keep the environment with your results:
the comparison JSON does not record GC environment variables. Repeat the
comparison in fresh processes to check that results remain consistent.

## Comparing C allocators

Keep benchmark sources identical and change only the root `Build.act`:

```python
build_options = {"malloc": "mimalloc"}
```

Omit this option or use `"libc"` for the baseline. Linux and macOS are supported.
As with the GC options above, `acton test perf --compare git:REF` compares each
revision's root build options using the same compiler, and recordings retain
the chosen options. Run existing GC-heavy modules as controls: a replacement
for C malloc need not speed up Acton list, string or JSON allocation.

Useful application experiments include parsing and releasing representative
XML documents, repeated TLS handshakes, and steady HTTP traffic with realistic
connection reuse and concurrency. Check completed work and output correctness
in both builds. For servers, measure throughput and median/p95/p99 request
latency at the same offered load, plus CPU time and peak/steady process RSS.
Include a long run to observe memory retained after bursts or connections close.
GC heap and GC allocated-byte measurements do not account for the C heap.

Build both variants before timing, use the same release mode, worker count,
GC environment and fixtures, and alternate libc/mimalloc runs on a quiet
machine. Warm up each process and repeat several fresh-process pairs; report
the spread as well as the central result. Keep allocator diagnostics disabled
during timing. A short malloc/free microbenchmark can confirm allocator cost,
but only the application's measured result establishes an application speedup.

## Tuple hashing

`tuple_hashing` covers the hash path: single values (including all bounded
integer types, bool, float and complex), two- and three-component tuples, a
nested tuple, immutable integer sets, and dict construction and update with
those keys. Scale is the number of keys, and each loop body hashes every key
once, so
per-operation figures are per key hashed. Keys are prepared before `t.loop()`;
only hashing, table work and output allocation are timed. The single-value
cases hash the same payloads without a tuple around them, which separates a
composite hashing change from a change to the underlying str, int, u64 or float
hash. The nested case hashes the triple's strings as `((str, str), str)`, so its
difference from the triple is the cost of nesting. Each immutable-set key
contains 16 consecutive integers; the sets overlap but are distinct, and their
construction is outside the timed loop.

```sh
acton test perf --module tuple_hashing --scale 1024 --time 3s --record
acton test scale --module tuple_hashing --name hash_str_pair --start-scale 1 --end-scale 65536
```

Allocation is the sharpest signal here, because it is exact from run to run
while wall time and instruction counts on these short bodies move with GC
scheduling. A composite hash that allocates per component shows up as
allocation growing with tuple arity; hashing that copies its input shows up as
allocation growing with `string_operations`' scale.

## Fleet manager workload

`fleet` measures the runtime's scheduling cost with many actors, timers,
request and reply chains through shared actors, bursts and idle gaps. Its
actor and message shape follows a network fleet manager that onboards
devices and upgrades their software in campaigns; the module docstring in
`src/fleet.act` lists the actors. The work per operation is nearly fixed,
so CPU time per body should not grow with the number of worker threads;
where it does, the growth is runtime overhead. Measure at several worker
counts:

```sh
acton test perf --module fleet --name load --scale 10000 --time 30s --rts-wthreads 16
acton test perf --module fleet --name upgrade --scale 480 --time 60s --rts-wthreads 16 --show-log
```

- `load` creates scale device handles before `t.loop()`. Each body
  connects all of them at once: every device creates its five session
  actors, makes two requests through its in-process connection, commits a
  transaction to the shared store and renders its configuration three
  times. The body ends when the last device reports back, and the previous
  body's session actors become garbage.
- `upgrade` creates and connects a fleet of scale times 125/6 devices
  before `t.loop()`, 10,000 at scale 480. Each body is a campaign that upgrades
  scale of them, half at a time. An upgrade runs seven operations over
  about two seconds and polls the device about 57 times. After status
  changes, the campaign waits briefly, builds a decision over all its
  members and releases more devices; it also reads the campaign state every
  0.1 s. Each campaign takes the next devices in the fleet.

Measurements are per body: divide by scale for CPU time per device or per
upgrade. A campaign's wall time is mostly spent waiting for timers, so read
the CPU rows for the upgrade. `--show-log` prints one line per campaign,
including warmup campaigns, with the polls per upgrade, the number of
decisions and state reads, the upgrade latency and percentiles of the
round-trip time of requests through the in-process connection, in
milliseconds. Polls end when the device reports an operation done, so
slower round trips mean slightly fewer polls per upgrade.

Calibration does not suit `upgrade`. Its wall time is set by timers: about
two seconds for one upgrade and about 4.3 seconds for any larger campaign.
The scale that calibration picks therefore depends on `--time` rather than
on the work, and a budget below about five seconds fails before any
measurement. Choose the scale explicitly and give each campaign time to
repeat. `utils/perf-compare` gives each process five seconds by default,
enough for one upgrade campaign at scale 1.

Time is compressed about 30 times against the upgrade the workload is
modelled on (60 seconds with a poll every 0.73 s), with the same number of
polls, and each message does less work (JSON instead of XML and YANG). The
runtime's share of the CPU time is therefore larger than in a real fleet
manager, and messages arrive at a higher rate.

## Actor scheduling patterns

`scheduling` has one test per pattern of work between actors, so that a
change to the runtime's scheduler can be judged across patterns rather than
on one workload such as `fleet`. Scale counts operations per body; divide
CPU time per body by scale for the cost per operation. An operation is one
message for the rings and `pairs`, a call and its reply for `hot_server`, a
message and about 20 µs of work for `fan_out`, 9 deliveries for `pipeline`
(8 stages and the sink), and a round trip for `latency_under_load` and
`reply_behind_work`. Measure at several worker counts:

```sh
acton test perf --module scheduling --time 10s --rts-wthreads 8
acton test perf --module scheduling --name latency_under_load --scale 1000 --time 10s --rts-wthreads 2 --show-log
```

- `ring` passes one token around a ring of 1,000 actors, scale hops per
  body. Every hop hands over to an actor with an empty mailbox and nothing
  runs in parallel, so more worker threads can only add overhead.
- `ring_tokens` passes 64 tokens around the same ring, scale hops in total.
- `pairs` has 64 pairs of actors, each passing its own token back and
  forth, scale hops in total. Handoffs run in parallel as in `ring_tokens`,
  but no two tokens ever visit the same actor.
- `hot_server` has 64 clients make scale synchronous calls in total to one
  server actor, whose mailbox stays deep.
- `fan_out` sends scale work items of about 20 µs each at once, round robin
  to 64 worker actors, and each worker reports its count once.
- `pipeline` sends scale messages at once through a chain of 8 stages.
- `latency_under_load` keeps twice as many actors as worker threads busy:
  each always has another 100 µs message to run. Meanwhile a probe sends a
  round trip through an idle actor every millisecond, scale round trips per
  body. `--show-log` prints the round-trip percentiles once per body,
  including calibration and warmup bodies; read those rather than CPU time,
  which only shows that every worker is busy.
- `reply_behind_work` sends a round trip every 3 ms to a server, followed by
  a job of 1 ms for the same server, scale round trips per body. The reply
  makes the probe runnable while the server's worker goes on with the job;
  unless another worker takes the probe, the round trip includes the job.
  Read the round-trip percentiles as for `latency_under_load`.

`xlang/` has Go and Tokio ports of these tests, except `reply_behind_work`,
and `src/xlang.act` is a program that runs all three runtimes on one machine;
see [xlang/README.md](xlang/README.md).

## Timer precision

`timer_precision` measures how closely timers fire to their due time. Each
loop body covers 1000 consecutive 1 ms slots. The first slot is due after a
lead of 20 ms plus 2 microseconds per timer, which leaves time to arm every
timer before it is due.

- `spread` arms one-shot timers with `after now`: scale timers per slot, so
  scale 1 is 1000 timers 1 ms apart and scale 100 is 100,000 timers, 100 per
  millisecond. The timers are shared by 16 receiver actors, so the test
  measures timer delivery rather than one actor's queue.
- `periodic` runs scale actors, each with a 1 ms timer re-armed with plain
  `after`, 1000 ticks each. Plain `after` keeps a drift-free schedule, so
  tick k is due exactly k ms after the first.

Lateness is printed once per body as the minimum, median, 90th and 99th
percentile and maximum in milliseconds, against the monotonic clock:

```sh
acton test perf --module timer_precision --name spread --scale 1 --time 10s --show-log
acton test perf --module timer_precision --name periodic --scale 100 --time 10s --show-log
acton test scale --module timer_precision --name spread --start-scale 1 --end-scale 1000
```

The runner's wall time per body is the lead, 1000 ms of slots, the last
timer's lateness and the time to collect and sort the lateness values,
which grows with scale. Its CPU time per body covers arming and delivering
the timers, including any time the runtime spends waiting for them, and the
test's own work: recording each timer's lateness and sorting the values for
the percentiles. A body fails if a timer fires early, if a timer is queued
after it is already due, or if the body has not finished 10 seconds after
its last slot. That deadline is itself a timer, so a timer thread that no
longer wakes up makes the run hang instead. A timer counts as early when it
fires more than 20 microseconds plus 500 ppm of its delay before its due
time. The 20 microseconds cover the runtime's clock and delays being
truncated to microseconds. The 500 ppm cover a difference in rate on macOS,
where the runtime's clock is mach continuous time, which time
synchronization does not adjust, while `time.monotonic_ns()` follows its
frequency corrections. Read lateness together with CPU time per body: a
runtime can make timers precise by waiting for them actively, and that
shows up as CPU time. On Linux and macOS the runtime sleeps until a kernel
timer wakes it at the next timer's due time, so lateness below a
millisecond costs little CPU time. The macOS kernel timer is less precise:
on an M4 Pro timers fire with a median lateness near 0.07 ms and a 99th
percentile of 0.2 to 0.9 ms, against about 0.015 ms and 0.02 to 0.05 ms on
Linux on a Ryzen 5950X. Elsewhere the runtime wakes at the next whole
millisecond or later.

## Comparing implementations

To compare Acton itself, run the repository utility:

```sh
utils/perf-compare main --module builtin_functions --name sum
utils/perf-compare main --module collection_iterators --scale 100000 --time 2s
utils/perf-compare main --project ../my-app --module benchmarks
```

The launcher can be called from any directory. By default it uses `test/perf`
in the Acton checkout containing the utility. Use `--project PATH` to select
another benchmark project containing `Build.act` and `src`. Relative paths
are resolved from the directory where you invoke the utility:

```sh
cd /path/to/my-app
/path/to/acton/utils/perf-compare main --project . --name lookup
```

The revision defaults to the local `main` branch. The utility builds that revision
and the current Acton checkout with `make`, including local changes to builtins,
the runtime or compiler. Both installations compile the same snapshot of the
selected project's `Build.act` and `src`, with separate generated outputs. The
old compiler must support these benchmarks and their performance measurement format.
The benchmark project must be self-contained: files outside `Build.act` and
`src`, including relative local dependencies, are not copied into the snapshot.

Each benchmark runs in four pairs of fresh processes. Two pairs run baseline
first and two run current first, in shuffled order. Without `--scale`, an excluded
baseline pilot chooses the shared workload scale. Each process performs the
normal warmup. `--time` is the budget for each process, defaulting to five
seconds as in `acton test perf`: roughly 40 seconds per benchmark plus the pilot,
builds and process startup.
Slow invocations can overrun the budget. Cached build checks still occur between
processes; the initial builds finish before measurements begin.

The summary gives each process mean equal weight and estimates the wall-time
change from the four paired differences. An interval spanning zero is
inconclusive. Extra inner loop samples do not count as independent processes.
The machine, workload scale, build settings and measurement scope must match.

Results live under the selected project's `out/perf_compare/<timestamp>/`:
build logs, the benchmark source snapshot, each process's JSON output, and `comparison.json`
with execution order, provenance and the summary. Results are saved as commands
complete. Temporary benchmark builds are removed on completion or a reported
error; the project's `perf_data` and existing test build output stay in place.
Ctrl-C uses the runtime's default behavior and may leave child processes or
temporary builds behind. Command logs are written when each process finishes.

The baseline checkout and compiler build are retained at
`~/.cache/acton/worktrees/<repository-id>/compiler-<commit>/` for reuse. The
repository ID hashes the shared Git directory's path. The utility refuses a
cached checkout with source changes and serializes utility runs on the machine.
To reclaim a cached build, use `git worktree remove --force <path>` while the
utility is idle. The utility is written in Acton. Its launcher compiles it with
`dist/bin/acton`, or an installed `acton` if the local compiler is missing. It
requires the normal Acton build tools on Linux or macOS; it adds no compiler
options.

From `utils/perf_compare`, build the utility with `../../dist/bin/acton build
--release`, then run its checks with `../../dist/bin/acton test --release
--jobs 1`. These use temporary Git repositories and fake compiler commands,
without live benchmarks.

For a manual comparison, record the first implementation, then run the second
against the same reference:

```sh
acton test perf --module builtin_functions --module collection_iterators --scale 1024 --record
# Build the other implementation, keeping these benchmark sources and perf_data.
acton test perf --module builtin_functions --module collection_iterators --json > comparison.json
```

A compatible baseline supplies its chosen scale. Keep that scale fixed between
implementations; independently calibrated scales measure different input sizes.
Use `--scale N` to choose an exact input size and `--time 10s` to extend the budget:

```sh
acton test perf --module builtin_functions --name '(control|filter_all)' --scale 100000 --time 10s
```

Keep the compiler, measurement code, machine, build options and worker count
the same when isolating a builtin change. Rebuild with `make` and regenerate
`test/perf/out` when switching implementations. Performance mode bypasses saved
test results, but that does not replace rebuilding generated code. On macOS,
both installations need the elapsed GC clock fix from #3107.

Create new recordings after this migration. Scale previously meant repetitions
of an operation on a fixed 1,024-element input; it now means input element count,
and each body performs one operation. The old measurements describe different
workloads even when their numeric scales match. The permutation and initial
mutable-target states have also changed.

Repeat comparisons in alternating process order, retain JSON reports and inspect
controls, CPU work and allocation alongside wall time. Confidence markers describe
variation within one process, and do not capture variation between processes.
Peak RSS includes setup, calibration and warmup and is not an allocation or leak
verdict.

See the [performance guide](../../docs/acton-guide/src/testing/performance.md)
and [baseline workflow](../../docs/acton-guide/src/testing/perf_record.md) for
measurement definitions and compatibility rules.
