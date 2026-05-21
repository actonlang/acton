# RTS Sync Pause

The RTS sync pause is a cooperative stop-the-world primitive for worker
threads. It is intended for runtime operations that must publish process-wide
state while no other worker is executing Acton continuations, for example
dynamic library code reload. It is not a general blocking API for arbitrary
threads; callers must be RTS workers.

The public C entry points are declared in `base/rts/common.h`:

```c
int acton_sync_pause_begin(void);
void acton_sync_pause_end(void);
```

`acton_sync_pause_begin()` may only be called after the worker pool has started
and before shutdown. It returns `0` for the worker that owns the pause and `-1`
if the caller is not a worker, shutdown has started, or another pause is already
active. There is no queue of pause requests; callers that race with an active
pause fail and must decide at the higher layer whether to retry or report an
error. `acton_sync_pause_end()` releases the pause only when called by the
worker that owns it.

## Protocol

The pause owner records its worker id, clears the per-worker parked bitmap, and
wakes all worker-thread event loops with `wake_all_wt()`. The wake is just a
libuv poke; sending it to the owner's own loop is harmless because async
callbacks are not run inline and wake notifications may be coalesced.

Workers run `maybe_sync_pause()` at the top of `wt_work_cb()` before dequeuing
the next actor continuation. A non-owner
worker that sees an active pause marks itself parked exactly once, increments
`sync_pause_parked_count`, signals the condition variable, and waits until the
owner releases the pause. The owner waits until `sync_pause_parked_count`
reaches `num_wthreads`, which is the number of workers other than the owner in
the `0..num_wthreads` worker-id range.

The primitive is therefore cooperative. It does not interrupt a continuation
that is already running; the pause is established only once every other worker
has returned to the worker-loop pause check.

## Shutdown

Both the owner and parked workers use a short timed condition-variable wait
rather than an unbounded `pthread_cond_wait()`. This keeps the pause responsive
to `rts_exit` even if shutdown starts while a worker is waiting and no further
condition broadcast arrives. If shutdown is observed while the owner is waiting,
the owner clears the pause state, broadcasts to parked workers, and fails the
pause request.

## Non-Threaded Builds

When Acton is built without RTS threads, `acton_sync_pause_begin()` returns
success and `acton_sync_pause_end()` is a no-op. There are no peer workers to
park in that configuration.
