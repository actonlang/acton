#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdatomic.h>
#include <math.h>
#include <time.h>

_Atomic(int) atomic_var = 0;

volatile int volatile_var = 0;

#define N 100000000

double timestamp() {
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    time_t s = t.tv_sec;
    long µs = round(t.tv_nsec / 1.0e3);   // ns -> µs
    if (µs > 999999) {
        ++s;
        µs = 0;
    }
    return (double)s + µs/1e6;
}

int main(int argc, char **argv) {
    double t0 = timestamp();
//    for (int i = 0; i<N; i++) {
    for (int i = 0; i<2*N; i++) {
        int old = 0;
        int new = 1;
        atomic_compare_exchange_strong(&atomic_var, &old, new);
        //atomic_compare_exchange_weak(&atomic_var, &new, old);
    }
    printf("%d successful atomic ops: %.6f s\n", 2*N, timestamp() - t0);

    atomic_var = 0;
    t0 = timestamp();
    for (int i = 0; i<N; i++) {
        int old = 1;
        int new = 12345;
        atomic_compare_exchange_strong(&atomic_var, &old, new);
        atomic_compare_exchange_strong(&atomic_var, &old, new);
    }
    printf("%d failed atomic ops: %.6f s\n", 2*N, timestamp() - t0);
    
    t0 = timestamp();
    for (int i = 0; i<N; i++) {
        int old = 0;
        int new = 1;
        if (volatile_var==old)
            volatile_var = new;
        else
            old = volatile_var;
        if (volatile_var==new)
            volatile_var = old;
        else
            new = volatile_var;
    }
    printf("%d non-atomic ops: %.6f s\n", 2*N, timestamp() - t0);    

    volatile_var = 0;
    t0 = timestamp();
//    for (int i = 0; i<N; i++) {
    for (int i = 0; i<2*N; i++) {
        int old = 1;
        int new = 12345;
        if (volatile_var==old)
            volatile_var = new;
        else
            old = volatile_var;
        //if (volatile_var==old)
        //    volatile_var = new;
        //else
        //    old = volatile_var;
    }
    printf("%d failed non-atomic ops: %.6f s\n", 2*N, timestamp() - t0);    
}