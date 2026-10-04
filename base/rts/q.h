#pragma once

#define MPMC 2

#include "rts.h"


// Workers take the locks of these queues all the time. Each queue gets a cache
// line of its own, so that a thread working on one queue does not slow down
// threads working on its neighbours. 128 bytes is the line size of Apple's
// cores and twice that of x86 cores.
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
