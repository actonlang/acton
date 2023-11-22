#pragma once

#define MPMC 2

#include "rts.h"


#if defined MPMC && MPMC == 3
// TODO: do atomics!
struct mpmcq {
    $Actor  head;
    $Actor tail;
    unsigned long long count;
    $Lock lock;
};
#else
struct mpmcq {
    $Actor head;
    $Actor tail;
    unsigned long long count;
    $Lock lock;
};
#endif

extern struct mpmcq rqs[NUM_RQS];
