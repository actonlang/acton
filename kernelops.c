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
    assert(m != NULL);
    m->next = NULL;
    m->waiting = NULL;
    m->clos = clos;
#if defined(WAITQ_MUTEX)
    m->wait_lock = (pthread_mutex_t)PTHREAD_MUTEX_INITIALIZER;
#endif

    clock_gettime(CLOCK_MONOTONIC, &t1);
    atomic_fetch_add(&msg_create_time, (t1.tv_sec - t0.tv_sec)*1000000000 + (t1.tv_nsec - t0.tv_nsec));

    return m;
}

Actor ACTOR(int n) {
    const size_t size = sizeof(struct Actor) + n * sizeof(WORD);
    Actor a = aligned_alloc(64, size);
    assert(a != NULL);
    a->next = NULL;
    a->msg = NULL;
#if defined(MSGQ_MUTEX)
    a->msg_lock = (pthread_mutex_t)PTHREAD_MUTEX_INITIALIZER;
#endif

    return a;
}

// global ready queue
#if defined(READYQ_LF)
#else
Actor readyQ;
#endif

#if defined(READYQ_MUTEX)
pthread_mutex_t ready_lock = PTHREAD_MUTEX_INITIALIZER;
#endif

void kernelops_INIT() {
    printf("\x1b[34;1m|\x1b[m \x1b[34mReady Q    :\x1b[m  ");
#if defined(READYQ_LF)
    printf("\x1b[32;1mlock-free\x1b[m\n");
#elif defined(READYQ_MUTEX)
    printf("\x1b[31mmutex\x1b[m\n");
#endif

    printf("\x1b[34;1m|\x1b[m \x1b[34mMessage Q  :\x1b[m  ");
#if defined(MSGQ_LF)
    printf("\x1b[32;1mlock-free\x1b[m\n");
#elif defined(MSGQ_SPIN)
    printf("\x1b[33mspinlock\x1b[m\n");
#elif defined(MSGQ_MUTEX)
    printf("\x1b[31;mmutex\x1b[m\n");
#endif

    printf("\x1b[34;1m|\x1b[m \x1b[34mWaiting Q  :\x1b[m  ");
#if defined(WAITQ_LF)
    printf("\x1b[32;1mlock-free\x1b[m\n");
#elif defined(WAITQ_MUTEX)
    printf("\x1b[31mmutex\x1b[m\n");
#endif

    printf("\x1b[34;1m|\x1b[m \x1b[34mAllocator  :\x1b[m  ");
#if defined(USE_JEMALLOC)
    printf("\x1b[32;1mjemalloc\x1b[m\n");
#else
    printf("\x1b[33mglibc\x1b[m\n");
#endif

#if defined(READYQ_LF)
#else
    readyQ = NULL;
#endif

#if defined(MSGQ_LF)

#endif
}

void kernelops_CLOSE() {
#if defined(USE_JEMALLOC)
    //malloc_stats_print(NULL, NULL, NULL);
#endif

#if defined(READYQ_LF)
#else
    readyQ = NULL;
#endif
}

_Atomic uint32_t readyQ_pushes = 0;
_Atomic uint32_t readyQ_max = 0;

void ready_PUSH(Actor a) {
    atomic_fetch_add(&readyQ_pushes, 1);
#if defined(READYQ_MUTEX)
    pthread_mutex_lock(&ready_lock);
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
#if defined(READYQ_MUTEX)
    pthread_mutex_unlock(&ready_lock);
#endif
#endif
}

Actor ready_POP() {
    Actor actor = NULL;
#if defined(READYQ_LF)
#else
#if defined(READQ_MUTEX)
    pthread_mutex_lock(&ready_lock);
#endif
    if (readyQ) {
        Actor x = readyQ;
        readyQ = x->next;
        x->next = NULL;
        actor = x;
    }
#if defined(READQ_MUTEX)
    pthread_mutex_unlock(&ready_lock);
#endif
#endif
    return actor;
}


#if defined(MSGQ_LF)
#error Message Q lock-free is not implemented!
#endif

_Atomic uint32_t msg_enq_count = 0;
_Atomic uint32_t msgQ_max = 0;

bool msg_ENQ(Msg m, Actor a) {
    atomic_fetch_add(&msg_enq_count, 1);
    bool was_first = true;
#if defined(MSGQ_MUTEX)
    pthread_mutex_lock(&a->msg_lock);
#endif
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
        was_first = false;
    } else {
        a->msg = m;
    }
#if defined(MSGQ_MUTEX)
    pthread_mutex_unlock(&a->msg_lock);
#endif
    return was_first;
}

bool msg_DEQ(Actor a) {
    bool has_more = false;
#if defined(MSGQ_MUTEX)
    pthread_mutex_lock(&a->msg_lock);
#endif
    if (a->msg) {
        Msg x = a->msg;
        a->msg = x->next;
        x->next = NULL;
        has_more = a->msg != NULL;
    }
#if defined(MSGQ_MUTEX)
    pthread_mutex_unlock(&a->msg_lock);
#endif
    return has_more;
}

#if defined(WAITQ_LF)
#error Wait Q lock-free is not implemented!
#endif

bool waiting_ADD(Actor a, Msg m) {
    bool did_add = false;
#if defined(WAITQ_MUTEX)
    pthread_mutex_lock(&m->wait_lock);
#endif
    if (m->clos) {
        a->next = m->waiting; // the actor can't be in the ready Q at this time
        m->waiting = a;
        did_add = true;
    }
#if defined(WAITQ_MUTEX)
    pthread_mutex_unlock(&m->wait_lock);
#endif
    return did_add;
}

_Atomic uint32_t wait_freeze_count = 0;

Actor waiting_FREEZE(Msg m) {
    atomic_fetch_add(&wait_freeze_count, 1);
#if defined(WAITQ_MUTEX)
    pthread_mutex_lock(&m->wait_lock);
#endif
    m->clos = NULL;
#if defined(WAITQ_MUTEX)
    pthread_mutex_unlock(&m->wait_lock);
#endif
    Actor waiting = m->waiting;
    m->waiting = NULL;
    return waiting;
}
