#include <assert.h>
#if defined(USE_JEMALLOC)
#include <jemalloc/jemalloc.h>
#endif

#include "kernelops.h"

void spinlock_lock(volatile atomic_flag *f) {
    while (atomic_flag_test_and_set(f) == true) {
        // wait for it...
    }
}
void spinlock_unlock(volatile atomic_flag *f) {
    atomic_flag_clear(f);
}


_Atomic uint32_t clos_created = 0;
_Atomic uint64_t clos_create_time = 0;

Clos CLOS(R (*code)(Clos, WORD), int n) {
    atomic_fetch_add(&clos_created, 1);
    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);

    const size_t size = sizeof(struct Clos) + n * sizeof(WORD);
    Clos c = aligned_alloc(64, size);
    //Clos c = malloc(size);
    assert(c != NULL);
    c->code = code;
    c->nvar = n;
    for(int x = 0; x < n; ++x) {
        c->var[x] = (WORD)0xbadf00d; // "bad food", i.e. uninitialized variable
    }

    clock_gettime(CLOCK_MONOTONIC, &t1);
    atomic_fetch_add(&clos_create_time, (t1.tv_sec - t0.tv_sec)*1000000000 + (t1.tv_nsec - t0.tv_nsec));

    return c;
}

_Atomic uint32_t msg_created = 0;
_Atomic uint64_t msg_create_time = 0;

Msg MSG(Clos clos) {
    atomic_fetch_add(&msg_created, 1);
    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);

    const size_t size = sizeof(struct Msg);
    Msg m = aligned_alloc(64, size);
    //Msg m = malloc(size);
    assert(m != NULL);
    m->next = NULL;
    m->waiting = NULL;
    m->clos = clos;
#if defined(MUTEX_OPS)
    m->mut = (pthread_mutex_t)PTHREAD_MUTEX_INITIALIZER;
#endif

    clock_gettime(CLOCK_MONOTONIC, &t1);
    atomic_fetch_add(&msg_create_time, (t1.tv_sec - t0.tv_sec)*1000000000 + (t1.tv_nsec - t0.tv_nsec));

    return m;
}

Actor ACTOR(int n) {
    Actor a = malloc(sizeof(struct Actor) + n * sizeof(WORD));
    a->next = NULL;
    a->msg = NULL;
#if defined(MUTEX_OPS)
    a->mut = (pthread_mutex_t)PTHREAD_MUTEX_INITIALIZER;
#endif
    return a;
}

// ready queue head
Actor readyQ = NULL;

#if defined(MUTEX_OPS)
pthread_mutex_t ready_mut = PTHREAD_MUTEX_INITIALIZER;
#endif

void kernelops_INIT() {
}

void kernelops_CLOSE() {
#if defined(USE_JEMALLOC)
    //malloc_stats_print(NULL, NULL, NULL);
#endif
}

_Atomic uint32_t readyQ_max = 0;
#if defined(BASIC_OPS) || defined(MUTEX_OPS)
void ready_PUSH(Actor a) {
    assert(a->msg != NULL);
#if defined(MUTEX_OPS)
    pthread_mutex_lock(&ready_mut);
#endif
    if (readyQ) {
        Actor x = readyQ;
        uint32_t count = 0;
        while (x->next) {
            ++count;
            x = x->next;
        }
        if(readyQ_max < count) {  // no, this isn't atomic...
            readyQ_max = count;
        }
        x->next = a;
    } else {
        readyQ = a;
    }
    a->next = NULL;
#if defined(MUTEX_OPS)
    pthread_mutex_unlock(&ready_mut);
#endif
}

Actor ready_POP() {
    Actor res = NULL;
#if defined(MUTEX_OPS)
    pthread_mutex_lock(&ready_mut);
#endif
    if (readyQ) {
        Actor x = readyQ;
        readyQ = x->next;
        x->next = NULL;
        res = x;
    }
#if defined(MUTEX_OPS)
    pthread_mutex_unlock(&ready_mut);
#endif
    return res;
}

_Atomic uint32_t msg_enq_count = 0;
_Atomic uint32_t msgQ_max = 0;

bool msg_ENQ(Msg m, Actor a) {
    bool did_enq = true;
#if defined(MUTEX_OPS)
    pthread_mutex_lock(&a->mut);
#endif
    atomic_fetch_add(&msg_enq_count, 1);
    m->next = NULL;
    if (a->msg) {
        Msg x = a->msg;
        uint32_t count = 0;
        while (x->next) {
            ++count;
            x = x->next;
        }
        if(msgQ_max < count) {  // no, this isn't atomic...
            msgQ_max = count;
        }
        x->next = m;
        did_enq = false;
    } else {
        a->msg = m;
    }
#if defined(MUTEX_OPS)
    pthread_mutex_unlock(&a->mut);
#endif
    return did_enq;
}

bool msg_DEQ(Actor a) {
    bool has_more = false;
#if defined(MUTEX_OPS)
    pthread_mutex_lock(&a->mut);
#endif
    if (a->msg) {
        Msg x = a->msg;
        a->msg = x->next;
        x->next = NULL;
        has_more = a->msg != NULL;
    }
#if defined(MUTEX_OPS)
    pthread_mutex_unlock(&a->mut);
#endif
    return has_more;
}

bool waiting_ADD(Actor a, Msg m) {
    bool did_add = false;
#if defined(MUTEX_OPS)
    pthread_mutex_lock(&m->mut);
#endif
    if (m->clos) {
        a->next = m->waiting; // no lock needed; this actor can not be in the ready Q at this time
        m->waiting = a;
        did_add = true;
    }
#if defined(MUTEX_OPS)
    pthread_mutex_unlock(&m->mut);
#endif
    return did_add;
}

_Atomic uint32_t wait_freeze_count = 0;

Actor waiting_FREEZE(Msg m) {
#if defined(MUTEX_OPS)
    pthread_mutex_lock(&m->mut);
#endif
    atomic_fetch_add(&wait_freeze_count, 1);
    m->clos = NULL;
#if defined(MUTEX_OPS)
    pthread_mutex_unlock(&m->mut);
#endif
    Actor waiting = m->waiting;
    m->waiting = NULL;
    return waiting;
}
#endif
