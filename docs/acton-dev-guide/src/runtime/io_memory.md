# IO memory management

This page describes how memory is managed across the native IO stack: which
allocator each library uses, why, and the ownership patterns `.ext.c` modules
must follow. The design was established when the TLS stack moved off the GC
heap, but it generalizes to all IO.

The background rule from [Memory and GC](memory.md) drives everything here:
Boehm GC does not scan libc-heap memory, so **a GC pointer stored in a libc
allocation does not keep its referent alive**, and a libc pointer stored in a
GC object is invisible to the collector in the harmless direction (the
collector never frees libc memory) but creates an ownership obligation —
somebody must `free()` it on every path.

## Allocator regimes

| Component | Allocator | Freed by |
|---|---|---|
| Acton objects (actors, builtins) | GC via `acton__allocator` | collector |
| uv handles/requests allocated by Acton code (TCP path) | GC (`acton_malloc`) | collector |
| libuv internals (watcher arrays, request buffers, ...) | `GC_malloc_uncollectable` | libuv itself, via `GC_free` |
| tlsuv (streams' engines, write queue, connectors, ...) | libc `malloc`/`free` | tlsuv itself |
| mbedtls (TLS state, X.509, bignums, ...) | libc `calloc`/`free` | mbedtls itself |
| TLS-path native objects in `net.ext.c` (streams, requests, state structs, payload copies) | libc `malloc`/`free` | `net.ext.c`, in close/completion callbacks |
| DNS `uv_getaddrinfo_t` requests in `net.ext.c` | libc `malloc`/`free` | the resolve callback |
| Read buffers (`alloc_buffer`, `base/rts/io.c`) | `GC_malloc_atomic` | collector |

The wiring lives in `acton_init_alloc()` (`base/rts/common.c`), called from
`main()` right after `GC_INIT()` and before any uv/tlsuv/mbedtls allocation.
Order matters: `tlsuv_set_allocator()` also re-points libuv's and mbedtls's
allocators (tlsuv `src/alloc.c`), so it runs first and `uv_replace_allocator()`
then overrides libuv. mbedtls needs no explicit setup: with
`MBEDTLS_PLATFORM_MEMORY` its allocator hooks default to `calloc`/`free`,
which is exactly the regime we want — and nothing may re-point them mid-run,
because mbedtls allocates and frees on widely separated code paths, so an
allocator change frees objects through the wrong regime.

### Why tlsuv and mbedtls are on libc

A TLS connection's engine (mbedtls session state) is allocated by tlsuv and
referenced only from the `tlsuv_stream_t`. The stream struct is owned by
`net.ext.c` and is libc memory. If the engine were GC-allocated, its only
reference would live inside libc memory: the collector would see it as
unreachable and collect it mid-connection — a use-after-free deep inside
mbedtls. This is not hypothetical; it was reproduced and proven (9/10 crash
rate under churn with the collector enabled, 0/10 with the collector pinned)
during the conversion. Keeping the whole native TLS chain on one heap ties its
lifetime to explicit close calls, with GC finalizers as the safety net.

### Why libuv is uncollectable, not libc

libuv's internal allocations might look like a candidate for plain libc too,
but they are **load-bearing GC roots**. The loop's `watchers` array (one entry
per registered fd, holding interior pointers into the handles) is, for an idle
connection that the application retains only through its callbacks, the *only*
reference chain that keeps the GC-allocated uv handle — and through
`handle->data`, its actor — alive. Moving libuv internals to libc severs that
chain and the collector frees handles whose fds are still registered with the
kernel: a use-after-free inside `uv_run` on the next poll event.

`GC_malloc_uncollectable` gives all three properties at once:

- blocks are **scanned**, so the watchers array keeps rooting handles;
- blocks are **never collected**, so libuv allocations referenced only from
  libc tlsuv structs (e.g. the hostname buffer inside a connector's embedded
  `uv_getaddrinfo_t`) cannot disappear mid-operation;
- blocks are freed **explicitly** via `GC_free` — libuv pairs every internal
  allocation with a free, so nothing leaks.

### Why frees used to be no-ops, and what that masked

Historically everything (uv, tlsuv, mbedtls) was GC-allocated with
`acton_noop_free`, so the whole IO loop was traceable from the root set and
`free()` could never be wrong — because it did nothing. The price was that the
no-op regime *masked* every lifetime bug: double frees, frees of objects still
in use, leaks of caller-owned outputs. Real `free()` converts each of those
into heap corruption or RSS growth. When converting a library to real frees,
expect to surface latent bugs in both the library and our usage of it; the
tlsuv fork's `harden` branch exists for exactly this reason.

## Hazard catalogue

When writing or reviewing `.ext.c` IO code, these are the failure classes to
check for. Each one was found (and is now guarded against) in the TLS stack.

1. **GC object referenced only from libc memory.** The referent gets
   collected while in use. Instances: a TLS engine hanging off a libc stream;
   a `B_bytes` write payload referenced only from tlsuv's libc write queue; a
   GC `uv_write_t` referenced only from that same queue; a GC DNS request
   whose only reference is libuv's threadpool work queue once libc nodes
   interleave in the chain. Remedies: move the object to libc with explicit
   ownership, copy the data (write payloads), or use
   `GC_malloc_uncollectable` when the object must stay GC-visible (see the
   DNS callback data below).
2. **Actor backref dereferenced after collection.** `handle->data` pointing
   at an actor does not keep the actor alive once the handle is libc memory.
   Any callback that runs after the actor's last message completes may
   dereference a dangling pointer. Remedy: nullable backrefs in a state
   struct, detached *before* an actor-initiated asynchronous close (see
   patterns below).
3. **Wrong-regime free.** `free()` on GC memory, `GC_free` on libc memory, or
   an allocator re-pointed between an object's allocation and its free.
   The mbedtls hook is the cautionary tale: it used to be re-pointed three
   times during startup.
4. **Use-after-free unmasked by real free.** Late callbacks (cancelled
   writes, poll events after close) touching objects that an earlier callback
   already freed. Remedy: free only in the terminal callback of an object's
   lifecycle, guard close paths for idempotence.
5. **Leaks of caller-owned outputs.** Library functions that transfer
   ownership of a buffer to the caller (PEM writers, error strings) used to be
   "freed" by the no-op; under real frees every path must free them.
6. **Zero-initialization assumptions.** `acton_malloc` zeroes (it is
   calloc-based) and `GC_malloc` zeroes; plain `malloc` does not. Code moved
   from the GC regime to libc must use `calloc` where it relied on zeroed
   memory.

## Ownership patterns for `.ext.c` IO code

The TLS implementation in `base/src/net.ext.c` is the reference
implementation for these patterns.

### State structs with nullable actor backrefs

Do not store an actor pointer directly in `handle->data`. Store a small libc
state struct whose `actor` field may be NULL:

```c
struct tls_client_state {
    netQ_TLSConnection actor;   // nullable backref, GC pointer in libc memory
    ...
};
```

Every callback fetches the state, checks `actor != NULL`, and no-ops
otherwise. The actor does not own the state; the native object's lifecycle
does — the state is freed in the handle's close callback.

### Detach before actor-initiated close

When the *actor* initiates an asynchronous close (explicit `close()` or
`__cleanup__`), it must NULL the backref synchronously, before the close
completes:

```c
self->_stream = -1LL;
state->actor = NULL;           // detach: callbacks after this message ends
tls_listener_close_stream(stream);   // must not reach for the actor
```

The close and cancelled-write callbacks run after the current message
completes, by which time nothing may be keeping the actor alive. When the
close is initiated *by the network* (EOF, read error), the backref stays set:
the error/remote-close message enqueued to the actor keeps it alive past the
close callback.

### Never free a uv handle before its close callback

`uv_close()` is asynchronous; libuv touches the handle until the close
callback runs. All frees of handles, their state structs and embedded
resources belong in the close callback (`uv_handle_free_on_close` is the
trivial helper for plain handles).

### Copy write payloads

A `B_bytes` passed to a write must not be referenced only from a libc queue
while the write is pending. Copy the payload into the (libc) write request
state and free it in the write callback — on every status, including
`UV_ECANCELED` (which means the stream's close path flushed the queue; treat
it as teardown, not as an error to report).

### Refcount shared native resources

When several native objects alias one resource — the TLS listener's
`tls_context`/key/cert are wired into every accepted connection's engine —
give the resource an atomically refcounted owner struct. The listener holds
one reference, each in-flight handshake and each accepted connection holds
one; the last release frees in dependency order. Make every error path
release exactly what it retained; an unpaired release (or a missing one) is a
heap corruption or a permanent leak under real frees.

### `GC_malloc_uncollectable` for GC-visible bridge data

When a struct must hold GC pointers (callback closures) but is itself
referenced only from libc memory or from nothing the collector scans,
allocate it with `GC_malloc_uncollectable` and free it with `GC_free` in its
terminal callback. The DNS lookup callback data is the canonical example: the
uv request is libc (it sits in the threadpool work queue), but its
`on_resolve`/`on_error` closures must stay alive, so they live in an
uncollectable block the collector scans.

### `__cleanup__` finalizers as the safety net

Explicit `close()` is the primary lifecycle; `action def __cleanup__` is the
safety net for actors dropped without closing. Because it is an `action`, the
GC finalizer only enqueues a message and the body runs on the actor's pinned
worker thread — the thread that owns the uv loop — so it may safely call
libuv/tlsuv functions. Cleanup bodies must be idempotent with explicit close
(guard on the `-1` handle sentinel), detach backrefs, NULL the callback
fields, and release owner references.

### Pin actors to the loop that owns their handles

libuv objects are single-threaded; every callback runs on the loop's worker
thread, and actor methods that touch the handle must run there too. Actors
call `pin_actor_affinity()` in `__init__`. For natively-created objects
adopted by a new actor (accepted connections), capture the worker id where
the native object lives (`get_wtid()`) and pass it through the constructor so
the actor pins to the owning loop's thread, not to whichever worker happens
to run its `__init__`.

## Testing and debugging

The TLS test modules (`test/stdlib_tests/src/test_net_tls.act`,
`test_net_tls_gc.act`) are written to attack the hazards above: byte-exact
queued-write roundtrips, listener drops with forced GC under live
connections, and actor-drop churn that exercises the finalizer path. Useful
instruments when hunting lifetime bugs in this area:

- **Stress mode**: `acton test stress --module test_net_tls
  --stress-workers 8` runs each test concurrently from drift-calibrated
  workers in one process; it is the most effective interleaving sweep.
- **Churn loops**: repeatedly running a test binary (`for i in $(seq 40); do
  ./out/bin/.test_test_net_tls test; done`) catches crashes that single runs
  miss; count exit codes above 128.
- **The GC discriminator**: run the reproducer with `GC_DONT_GC=1
  GC_INITIAL_HEAP_SIZE=1500000000`. A crash that disappears when the
  collector is pinned is a GC-visibility bug, not a memory-corruption bug.
- **MallocScribble**: `MallocScribble=1 MallocPreScribble=1` (macOS) poisons
  freed/fresh libc memory and converts silent use-after-free into crashes.
- **RSS plateau**: sustained connection churn must show flat RSS; growth
  means a leak that the no-op-free era would have hidden.
- **macOS crash reports**: in-process observers (instrumentation, attached
  debuggers) perturb these races. The non-perturbing method: remove stale
  reports from `~/Library/Logs/DiagnosticReports` (they deduplicate), run
  with `--rts-no-bt`, and read the fresh `.ips` report's faulting-thread
  backtrace and registers.
- ASan is not usable together with Boehm GC.

## Direction

The end state this design works toward is stricter per-actor heap management,
where memory that escapes a single actor turn (anything a native library may
touch asynchronously) cannot live on an actor-scoped heap at all. Moving IO
lifetimes from GC reachability to explicit ownership is the prerequisite:
synchronous, non-escaping native calls can keep using GC memory, while
everything with an asynchronous lifetime is owned and freed deterministically.
