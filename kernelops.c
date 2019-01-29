#include <assert.h>
#if defined(USE_JEMALLOC)
#include <jemalloc/jemalloc.h>
#endif

#include "kernelops.h"

extern double timestamp();  // in kernel.c

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

    const double t0 = timestamp();

    const size_t size = sizeof(struct Clos) + n * sizeof(WORD);
    Clos c = aligned_alloc(64, size);
    assert(c != NULL);
    c->code = code;
    c->nvar = n;
    for(int x = 0; x < n; ++x) {
        c->var[x] = (WORD)0xbadf00d; // "bad food", i.e. uninitialized variable
    }

    atomic_fetch_add(&clos_create_time, (uint64_t)(timestamp()*1e9 - t0*1e9));

    return c;
}

_Atomic uint32_t msg_created = 0;
_Atomic uint64_t msg_create_time = 0;

Msg MSG(Clos clos) {
    atomic_fetch_add(&msg_created, 1);

    const double t0 = timestamp();

    const size_t size = sizeof(struct Msg);
    Msg m = aligned_alloc(64, size);
    assert(m != NULL);
    m->next = NULL;
    m->waiting = NULL;
    m->clos = clos;
#if defined(WAITQ_MUTEX)
    m->wait_lock = (pthread_mutex_t)PTHREAD_MUTEX_INITIALIZER;
#endif

    atomic_fetch_add(&msg_create_time, (uint64_t)(timestamp()*1e9 - t0*1e9));

    return m;
}

Actor ACTOR(int n) {
    const size_t size = sizeof(struct Actor) + n * sizeof(WORD);
    Actor a = aligned_alloc(64, size);
    assert(a != NULL);
    a->next = NULL;
    a->msgQ = NULL;
    a->msgTail = NULL;
#if defined(MSGQ_MUTEX)
    a->msg_lock = (pthread_mutex_t)PTHREAD_MUTEX_INITIALIZER;
#elif defined(MSGQ_SPIN)
    atomic_flag_clear(&a->msg_lock);
#endif

    return a;
}

// global ready queue
#if defined(READYQ_LF)
struct lfds711_ringbuffer_state readyQ;
struct lfds711_ringbuffer_element *readyQ_elems;
#else
Actor readyQ;
Actor readyTail;
#endif

#if defined(READYQ_MUTEX)
pthread_mutex_t readyQ_lock = PTHREAD_MUTEX_INITIALIZER;
#elif defined(READYQ_SPIN)
volatile atomic_flag readyQ_lock;
#endif

void kernelops_INIT() {
    printf("\x1b[34;1m|\x1b[m \x1b[34mReady Q    :\x1b[m  ");
#if defined(READYQ_LF)
    printf("\x1b[32;1mlock-free\x1b[m\n");
#elif defined(READYQ_SPIN)
    printf("\x1b[33mspinlock\x1b[m\n");
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
    const int N = 10000;
    printf("\x1b[34mReady Q LF:\x1b[m \x1b[1m%d\x1b[m \x1b[34melements,\x1b[m \x1b[1m%ld\x1b[m \x1b[34mbytes each\x1b[m\n", N, sizeof(struct lfds711_ringbuffer_element));

    readyQ_elems = aligned_alloc(64, sizeof(struct lfds711_ringbuffer_element)*(N + 1));
    assert(readyQ_elems != NULL);
    lfds711_ringbuffer_init_valid_on_current_logical_core(&readyQ, readyQ_elems, N + 1, NULL);
#elif defined(READYQ_SPIN)
    atomic_flag_clear(&readyQ_lock);
#else
    readyQ = NULL;
    readyTail = NULL;
#endif
}

void kernelops_CLOSE() {
#if defined(USE_JEMALLOC)
    //malloc_stats_print(NULL, NULL, NULL);
#endif

#if defined(READYQ_LF)
    free((void *)readyQ_elems);
#else
    readyQ = NULL;
#endif
}

_Atomic uint32_t readyQ_pushes = 0;
_Atomic uint32_t readyQ_pops = 0;

_Atomic uint64_t readyQ_push_time = 0;
_Atomic uint64_t readyQ_pop_time = 0;

void ready_PUSH(Actor a) {
    atomic_fetch_add(&readyQ_pushes, 1);

    const double t0 = timestamp();

#if defined(READYQ_LF)
    enum lfds711_misc_flag overwrote;
    void *overwritten;
    lfds711_ringbuffer_write(&readyQ, (void *)a, NULL, &overwrote, &overwritten, NULL);
    if(overwrote == LFDS711_MISC_FLAG_RAISED) {
        printf("overwrote!\n");
        // TODO: mutex lock to allocate another buffer - must lock ALL threads!
        // TODO: write 'overwritten' to new buffer
    }
#else
#if defined(READYQ_SPIN)
    spinlock_lock(&readyQ_lock);
#elif defined(READYQ_MUTEX)
    pthread_mutex_lock(&readyQ_lock);
#endif
    if (readyTail) {
      readyTail->next = a;
      readyTail = a;
    } else {
      readyTail = a;
      readyQ = a;
    }
    a->next = NULL;
#if defined(READYQ_SPIN)
    spinlock_unlock(&readyQ_lock);
#elif defined(READYQ_MUTEX)
    pthread_mutex_unlock(&readyQ_lock);
#endif
#endif

    atomic_fetch_add(&readyQ_push_time, (uint64_t)(timestamp()*1e9 - t0*1e9));
}

Actor ready_POP() {
    atomic_fetch_add(&readyQ_pops, 1);

    const double t0 = timestamp();

    Actor actor = NULL;
#if defined(READYQ_LF)
    // TODO: read from all existing buffers
    lfds711_ringbuffer_read(&readyQ, (void **)&actor, NULL);
#else
#if defined(READQ_MUTEX)
    pthread_mutex_lock(&readyQ_lock);
#endif
    if (readyQ) {
        actor = readyQ;
        readyQ = actor->next;
        if (!readyQ)
            readyTail = NULL;
        actor->next = NULL;
    }
#if defined(READQ_MUTEX)
    pthread_mutex_unlock(&readyQ_lock);
#endif
#endif

    atomic_fetch_add(&readyQ_pop_time, (uint64_t)(timestamp()*1e9 - t0*1e9));

    return actor;
}


#if defined(MSGQ_LF)
#error Message Q lock-free is not implemented!
#endif

_Atomic uint32_t msg_enq_count = 0;

bool msg_ENQ(Msg m, Actor a) {
    atomic_fetch_add(&msg_enq_count, 1);
    bool was_first = true;
#if defined(MSGQ_MUTEX)
    pthread_mutex_lock(&a->msg_lock);
#elif defined(MSGQ_SPIN)
    spinlock_lock(&a->msg_lock);
#endif
    m->next = NULL;
    m->next = NULL;
    if (a->msgTail) {
        a->msgTail->next = m;
        a->msgTail = m;
        was_first = false;
    } else {
        a->msgTail = m;
        a->msgQ = m;
    }
#if defined(MSGQ_MUTEX)
    pthread_mutex_unlock(&a->msg_lock);
#elif defined(MSGQ_SPIN)
    spinlock_unlock(&a->msg_lock);
#endif
    return was_first;
}

bool msg_DEQ(Actor a) {
    bool has_more = false;
#if defined(MSGQ_MUTEX)
    pthread_mutex_lock(&a->msg_lock);
#elif defined(MSGQ_SPIN)
    spinlock_lock(&a->msg_lock);
#endif
    if (a->msgQ) {
        Msg x = a->msgQ;
        a->msgQ = x->next;
        if (!a->msgQ)
            a->msgTail = NULL;
        x->next = NULL;
        has_more = a->msgQ != NULL;
    }
#if defined(MSGQ_MUTEX)
    pthread_mutex_unlock(&a->msg_lock);
#elif defined(MSGQ_SPIN)
    spinlock_unlock(&a->msg_lock);
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

