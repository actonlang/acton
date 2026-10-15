#pragma once

#define MPMC 2

#include "rts.h"


// Workers take the locks of these queues all the time. Each queue gets a cache
// line of its own, so that a thread working on one queue does not slow down
// threads working on its neighbours. 128 bytes is the line size of Apple's
// cores and twice that of x86 cores.
//
// Threads read head and count of these queues without taking the queue's
// lock. Every access to these fields, also under the lock, is a relaxed
// atomic load or store (__atomic_load_n, __atomic_store_n). A change under
// the lock is a relaxed load followed by a relaxed store of the new value
// (rq_count_add): the lock orders the threads that write, so they need no
// atomic read-modify-write. The other fields are accessed only under the
// lock (tail), and those accesses are plain.
#if defined MPMC && MPMC == 3
// TODO: do atomics!
struct mpmcq {
    $Actor  head;
    $Actor tail;
    uint64_t count;
    $Lock lock;
} __attribute__((aligned(128)));
#else
struct mpmcq {
    $Actor head;
    $Actor tail;
    uint64_t count;
    $Lock lock;
} __attribute__((aligned(128)));
#endif

extern struct mpmcq rqs[NUM_RQS];

// With threads, rqs[i] holds only the actors pinned to worker i (for
// example actors whose libuv handles live on its loop) and rqs[SHARED_RQ]
// is the injection queue for the unpinned actors made ready on the main
// thread (worker 0) and on threads that are not workers. An unpinned actor
// made ready by worker i > 0 goes to the end of worker i's local queue,
// lrqs[i], which has no limit: worker i takes from its head, and other
// workers take from it to balance the load. Only worker i adds to lrqs[i],
// and lrqs[0] stays empty.
extern struct mpmcq lrqs[MAX_WTHREADS];

// A worker looks at the injection queue before its local queue when it has not
// done so for this long. Actors made ready on the main thread, by timers,
// input on stdin and the actors pinned to it, and on threads that are not
// workers wait there, and a worker whose local queue is never empty, for
// example because it holds an actor that always has another message, would
// otherwise leave them waiting.
#define INJECT_NS (50 * 1000)

static inline bool rq_empty(struct mpmcq *q) {
    return __atomic_load_n(&q->head, __ATOMIC_RELAXED) == NULL;
}
