# Messages and futures

This note explains how an asynchronous call is represented in the runtime, and
why the representation is split into two distinct objects: a short-lived
**transport message** (the envelope, `B_Msg`) and a longer-lived **future**
(`B_Future`, the C type behind the surface type `Future[A]`).

The split matters because the runtime is moving towards a **per-actor** memory
model: each actor has its own heap and is garbage-collected independently (see
[Memory and GC](memory.md)). Under per-actor heaps an actor may only write objects
on its *own* heap; the two roles a message plays — being delivered-and-run versus
being awaited-for-a-result — have different owners and different lifetimes, so
conflating them into one object makes the ownership story impossible. Splitting
them is what makes the per-actor model expressible.

Implementation lives in `base/rts/rts.c`, `base/rts/rts.h`, and `base/rts/q.c`.

## Background: how an async call works

When actor A calls a method on actor B, the call does not run on A. It is turned
into a message delivered to B, which runs it during one of B's turns. A may want
the result later — that is what `await` retrieves. Two things therefore have to
exist:

1. something to **carry the call to B and drive it** while B runs it, and
2. something to **hold the eventual result** so A (or whoever holds the handle)
   can read it.

These are distinct jobs with different owners and different lifetimes: the
envelope belongs to the machinery delivering one call and dies with it, while
the result handle belongs to whoever awaits it and lives as long as they hold
it. The runtime therefore represents them as two objects.

## The two objects

### The message envelope (`B_Msg`)

The envelope is an RTS-internal type (a hand-written runtime object like `$Actor`
and `$Cont`, not exposed to Acton source; its C type is `B_Msg`). It is the message
in flight to an actor and the actor's **activation frame** while that message runs.

```c
struct B_Msg {
    struct B_MsgG_class *$class;
    B_Msg     $next;        // mailbox / outgoing / timer linkage
    $Actor   $to;          // recipient actor
    $Cont    $cont;        // activation: continuation to run
    time_t   $baseline;    // logical delivery time (normal vs timer)
    $WORD    value;        // activation: continuation argument
    B_Future $fut;         // the future this envelope fulfills
    $long    $globkey;     // identity (used for DB persistence)
};
```

The scheduler loop (`wt_work_cb` in `rts.c`) takes the actor's current envelope
`current->$msg`, and runs `m->$cont(m->value)`. A continuation step (`$RCONT`)
rewrites `$cont`/`value` and re-runs the same envelope. On `await` of a pending
future (`$RWAIT`) the envelope is *parked*: it stays at the mailbox head, holding
the suspended continuation, until the result arrives and the turn eventually
finishes. Only `$RDONE` (or an unhandled `$RFAIL`) consumes (dequeues) it. So an
envelope lives for exactly one call — but that call may span an await.

### The future (`B_Future`)

The future is the runtime object behind the surface type `Future[A]` (its C type is
`B_Future`). It is the promise/result cell: produced by an async call, completed
once, read by whoever holds the handle.

```c
struct B_Future {
    struct B_FutureG_class *$class;
    $Actor $waiting;      // head of the waiting-actor list
    $Lock  $wait_lock;    // protects $waiting
    $int64 $state;        // FUT_PENDING / FUT_VALUE / FUT_EXCEPTION
    $WORD  value;         // the result (or the raised exception)
    $long  $globkey;      // identity (used for DB persistence)
};
```

Result state is an explicit enum:

```c
#define FUT_PENDING    0
#define FUT_VALUE      1
#define FUT_EXCEPTION  2
```

A future lives as long as something holds a reference to it — potentially much
longer than the envelope that produced it, which is exactly why it must be a
separate object with its own lifetime.

> **Note — naming.** A *message* is the thing in transit; a *future* is the
> result you await. The envelope is RTS-internal (`B_Msg`, invisible to Acton
> source, like `$Actor`); the future is the builtin behind the surface type
> `Future[A]` (`B_Future`). Generated code treats the future opaquely — it only
> ever calls the `$ASYNC`/`$AWAIT`/`$AFTER` primitives and never inspects the
> object's fields — and compiler-emitted actor headers declare the mailbox
> slots `$msg`/`$msg_tail`/`$outgoing` as `B_Msg` (an opaque pointer type to
> generated code).

## How a call is built: `$ASYNC` and `$AFTER`

An async call allocates **one of each** and links them. `$ASYNC` (in `rts.c`):

```c
B_Future $ASYNC($Actor to, $Cont cont) {
    $Actor self = GET_SELF();
    B_Future fut = B_FutureG_new();       // the future, returned to the caller
    B_Msg env = B_MsgG_newXX(to, cont, 0, &$Done$instance);
    env->$fut = fut;                      // envelope -> the future it fulfills
    ...   // buffer env on the caller's outgoing queue (flushed to `to`'s
    ...   // mailbox when the caller's turn ends); return fut

}
```

The envelope goes to the callee `to`; the future is returned to the caller. The
link `env->$fut` is how the running envelope knows which future to complete. When
the callee's turn ends, the result is frozen into `env->$fut` and any waiting
actors are woken.

`$AFTER` is the same shape for timer messages, with the envelope addressed back to
`self` and a future-dated `$baseline`.

## DB persistence

With `--db` (the distributed backend), actors and in-flight messages are persisted
so a node can recover. The split is reflected in the persistence layer:

- The envelope and the future are distinct serializable classes with their own
  preassigned class ids (`MSG_ID` / `FUTURE_ID`). Both kinds of row live in
  `MSGS_TABLE`; each row carries its class id, so recovery
  (`deserialize_system`) allocates the correct struct for each.
- `serialize_msg` persists an envelope and, via `serialize_future`, the future it
  fulfills; `serialize_actor` persists the actor's outgoing envelopes. No waiter
  state is persisted: an actor parked on an `await` is recovered by replaying its
  whole turn from the parked envelope at its mailbox head, so `$waitsfor` is
  rebuilt by re-execution rather than stored.

This path is validated by `make test-rts-db`.

> **Note — future direction.** A later transport layer is expected to carry
> messages in a pooled mailbox whose envelopes are not themselves serializable.
> Persisting in-flight messages for DB recovery will then need a separate
> serializable record (or snapshotting only at turn boundaries). This is a known
> seam for that work, not a property of the current split.

## Direction

The landed split is deliberately mechanical: the runtime still completes a
future with a direct write and wakes waiters via `ADD_waiting`/`FREEZE_waiting`,
which mutate objects across actor boundaries. Under per-actor heaps those
cross-actor writes have to become messages between the actors involved; the
waiter bookkeeping then becomes local to a single actor and `$wait_lock`
disappears. That await redesign is tracked separately and builds on this split;
nothing in the current layout presupposes a particular protocol.

See also: [Actors](actors.md), [Scheduler](scheduler.md), and
[Memory and GC](memory.md).
