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
    unsigned long long count;
    $Lock lock;
} __attribute__((aligned(128)));
#else
struct mpmcq {
    $Actor head;
    $Actor tail;
    unsigned long long count;
    $Lock lock;
} __attribute__((aligned(128)));
#endif

extern struct mpmcq rqs[NUM_RQS];
