#include <assert.h>
#include <stdio.h>  // pqueue.h needs FILE
#include <pqueue.h>

#include "kernelops.h"

#if defined(USE_JEMALLOC)
#include <jemalloc/jemalloc.h>
#endif



static const size_t MEM_ALIGN = 64;

atomic_uint_least32_t clos_create_count = 0;
atomic_uint_least64_t clos_create_time = 0;

Clos CLOS(R (*code)(Clos, WORD), int n) {
    atomic_fetch_add(&clos_create_count, 1);

    const tsc_t t0 = timestamp_tsc();

    const size_t size = sizeof(struct Clos) + n * sizeof(WORD);
    //Clos c = aligned_alloc(MEM_ALIGN, size);
    Clos c = malloc(size);
    assert(c != NULL);
    c->code = code;
    c->nvar = n;
    for(int x = 0; x < n; ++x) {
        c->var[x] = (WORD)0xbadf00d; // "bad food", i.e. uninitialized variable
    }

    atomic_fetch_add(&clos_create_time, (uint64_t)(timestamp_tsc() - t0));

    return c;
}

atomic_uint_least32_t msg_create_count = 0;
atomic_uint_least64_t msg_create_time = 0;

Msg MSG(Actor to, Clos clos) {
    atomic_fetch_add(&msg_create_count, 1);

    const double t0 = timestamp_tsc();

    const size_t size = sizeof(struct Msg);
    //Msg m = aligned_alloc(MEM_ALIGN, size);
    Msg m = malloc(size);
    assert(m != NULL);
    m->to = to;
    m->next = NULL;
    m->waiting = NULL;
    m->clos = clos;
    m->time_baseline = 0;
#if WAITQ == MUTEX
    m->wait_lock = (pthread_mutex_t)PTHREAD_MUTEX_INITIALIZER;
#elif WAITQ == SPIN
    atomic_flag_clear(&m->wait_lock);
#endif

    atomic_fetch_add(&msg_create_time,  (uint64_t)(timestamp_tsc() - t0));

    return m;
}

Actor ACTOR(int n) {
    const size_t size = sizeof(struct Actor) + n * sizeof(WORD);
    //Actor a = aligned_alloc(MEM_ALIGN, size);
    Actor a = malloc(size);
    assert(a != NULL);
    a->next = NULL;
    a->msgQ = NULL;
    a->msgTail = NULL;
#if MSGQ == MUTEX
    a->msg_lock = (pthread_mutex_t)PTHREAD_MUTEX_INITIALIZER;
#elif MSGQ == SPIN
    atomic_flag_clear(&a->msg_lock);
#endif

    return a;
}

// global ready queue
Actor readyQ;
Actor readyTail;

// global timer queue
pqueue_t timerQ;


int timerQ_cmppri(pqueue_pri_t next_prio, pqueue_pri_t curr_prio) {
    // This callback should return 0 for 'lower' and non-zero
    //for 'higher', or vice versa if reverse priority is desired

    return curr_prio < next_prio ? 1 : 0;  // "less" is "higher prio" in the timer Q
}
pqueue_pri_t timerQ_getpri(void *item) {
    return ((TimedMsg)item)->trigger_time;
}
void timerQ_setpri(void *item, pqueue_pri_t prio) {
    ((TimedMsg)item)->trigger_time = prio;
}
size_t timerQ_getpos(void *item) {
    return ((TimedMsg)item)->pqueue_pos;
}
void timerQ_setpos(void *item, size_t pos) {
    ((TimedMsg)item)->pqueue_pos = pos;
}
void *timerQ_realloc(void *oldbuf, size_t newsize) {
    if (oldbuf)
        printf("\x1b[34;1mre-allocating\x1b[m timer Q buffer -> %ld\n", newsize);
    return realloc(oldbuf, newsize);
}


#if READYQ == MUTEX
pthread_mutex_t readyQ_lock = PTHREAD_MUTEX_INITIALIZER;
#elif READYQ == SPIN
volatile atomic_flag readyQ_lock;
#endif

#if TIMERQ == MUTEX
pthread_mutex_t timerQ_lock = PTHREAD_MUTEX_INITIALIZER;
#elif TIMERQ == SPIN
volatile atomic_flag timerQ_lock;
#endif

void print_impl(char *title, int impl) {
    printf("\x1b[34;1m|\x1b[m \x1b[34m%-10s:\x1b[m  ", title);

    if (impl == LFREE)
        printf("\x1b[32;1mlock-free\x1b[m\n");
    else if (impl == SPIN)
        printf("\x1b[33mspinlock\x1b[m\n");
    else if (impl == MUTEX)
        printf("\x1b[31mmutex\x1b[m\n");
}

double _tsc2ns = 1.0/2.35; // TODO: should be calibrated at runtime

void kernelops_INIT() {
    //print_impl("Ready Q", READYQ);
    //print_impl("Message Q", MSGQ);
    //print_impl("Waiting Q", WAITQ);

    //printf("\x1b[34;1m|\x1b[m \x1b[34mAllocator :\x1b[m  ");
#if defined(USE_JEMALLOC)
    //printf("\x1b[32;1mjemalloc\x1b[m\n");
#else
    //printf("\x1b[33mglibc\x1b[m\n");
#endif

#if READYQ == SPIN
    atomic_flag_clear(&readyQ_lock);
#else
    readyQ = NULL;
    readyTail = NULL;
#endif

    const size_t timerQ_initsize = 5000;
    //printf("Timer Q initial size: %ld\n", timerQ_initsize);
    pqueue_init(
            &timerQ,
            timerQ_initsize,
            timerQ_realloc,
            timerQ_cmppri,
            timerQ_getpri,
            timerQ_setpri,
            timerQ_getpos,
            timerQ_setpos);
}

double tsc2ns(tsc_t tsc) {
    return tsc*_tsc2ns;
}

void kernelops_CLOSE() {
#if defined(USE_JEMALLOC)
    //malloc_stats_print(NULL, NULL, NULL);
#endif

    readyQ = NULL;
    readyTail = NULL;

    pqueue_free(&timerQ);
}


// ################========   Ready Q  ========################

#if READYQ == LFREE
#error Lock-free Ready Q is not implemented!
#endif

atomic_uint_least32_t readyQ_ins_count = 0;
atomic_uint_least64_t readyQ_ins_time = 0;

atomic_uint_least32_t readyQ_poll_count = 0;
atomic_uint_least64_t readyQ_poll_time = 0;

void ready_INSERT(Actor a) {
    atomic_fetch_add(&readyQ_ins_count, 1);

    const double t0 = timestamp_tsc();

#if READYQ == MUTEX
    pthread_mutex_lock(&readyQ_lock);
#elif READYQ == SPIN
    spinlock_lock(&readyQ_lock);
#endif

    if (readyTail) {
      readyTail->next = a;
      readyTail = a;
    } else {
      readyTail = a;
      readyQ = a;
    }
    a->next = NULL;

#if READYQ == MUTEX
    pthread_mutex_unlock(&readyQ_lock);
#elif READYQ == SPIN
    spinlock_unlock(&readyQ_lock);
#endif

    atomic_fetch_add(&readyQ_ins_time,  (uint64_t)(timestamp_tsc() - t0));
}

Actor ready_POLL() {
    atomic_fetch_add(&readyQ_poll_count, 1);

    const double t0 = timestamp_tsc();

    Actor actor = NULL;
#if READYQ == MUTEX
    pthread_mutex_lock(&readyQ_lock);
#elif READYQ == SPIN
    spinlock_lock(&readyQ_lock);
#endif

    if (readyQ) {
        actor = readyQ;
        readyQ = actor->next;
        if (! readyQ)
            readyTail = NULL;
        actor->next = NULL;
    }

#if READYQ == MUTEX
    pthread_mutex_unlock(&readyQ_lock);
#elif READYQ == SPIN
    spinlock_unlock(&readyQ_lock);
#endif

    atomic_fetch_add(&readyQ_poll_time, (uint64_t)(timestamp_tsc() - t0));

    return actor;
}


// ################========   Message Q  ========################

#if MSGQ == LFREE
#error Lock-free Message Q is not implemented!
#endif

atomic_uint_least32_t msg_enq_count = 0;
atomic_uint_least64_t msg_enq_time = 0;

bool msg_ENQ(Msg m, Actor a) {
    atomic_fetch_add(&msg_enq_count, 1);
    const double t0 = timestamp_tsc();

    bool was_first = true;

#if MSGQ == MUTEX
    pthread_mutex_lock(&a->msg_lock);
#elif MSGQ == SPIN
    spinlock_lock(&a->msg_lock);
#endif

    m->next = NULL;
    if (a->msgTail) {
        a->msgTail->next = m;
        a->msgTail = m;
        was_first = false;
    } else {
        a->msgTail = m;
        a->msgQ = m;
    }

#if MSGQ == MUTEX
    pthread_mutex_unlock(&a->msg_lock);
#elif MSGQ == SPIN
    spinlock_unlock(&a->msg_lock);
#endif

    atomic_fetch_add(&msg_enq_time, (uint64_t)(timestamp_tsc() - t0));

    return was_first;
}

Msg msg_PEEK(Actor a) {
	return a->msgQ;
}


atomic_uint_least32_t msg_deq_count = 0;
atomic_uint_least64_t msg_deq_time = 0;



bool msg_DEQ(Actor a) {
    atomic_fetch_add(&msg_deq_count, 1);
    const double t0 = timestamp_tsc();

    bool has_more = false;
#if MSGQ == MUTEX
    pthread_mutex_lock(&a->msg_lock);
#elif MSGQ == SPIN
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

#if MSGQ == MUTEX
    pthread_mutex_unlock(&a->msg_lock);
#elif MSGQ == SPIN
    spinlock_unlock(&a->msg_lock);
#endif

    atomic_fetch_add(&msg_deq_time, (uint64_t)(timestamp_tsc() - t0));

    return has_more;
}


// ################========   Waiting Q  ========################

#if WAITQ == LFREE
#error Lock-free Waiting Q is not implemented!
#endif

bool waiting_INSERT(Actor a, Msg m) {
    bool did_add = false;
#if WAITQ == MUTEX
    pthread_mutex_lock(&m->wait_lock);
#endif
    if (m->clos) {
        a->next = m->waiting; // the actor can't be in the ready Q at this time
        m->waiting = a;
        did_add = true;
    }
#if WAITQ == MUTEX
    pthread_mutex_unlock(&m->wait_lock);
#endif
    return did_add;
}

atomic_uint_least32_t wait_freeze_count = 0;

Actor waiting_FREEZE(Msg m) {
    atomic_fetch_add(&wait_freeze_count, 1);
#if WAITQ == MUTEX
    pthread_mutex_lock(&m->wait_lock);
#elif WAITQ == SPIN
    spinlock_lock(&m->wait_lock);
#endif

    m->clos = NULL;

#if WAITQ == MUTEX
    pthread_mutex_unlock(&m->wait_lock);
#elif WAITQ == SPIN
    spinlock_unlock(&m->wait_lock);
#endif

    Actor waiting = m->waiting;
    m->waiting = NULL;

    return waiting;
}


// ################========   Timer Q  ========################

atomic_uint_least32_t timer_ins_count = 0;
atomic_uint_least64_t timer_ins_time = 0;
atomic_uint_least32_t timer_poll_count = 0;
atomic_uint_least64_t timer_poll_time = 0;

TimedMsg timer_INSERT(monotonic_time trigger_time, Msg m) {
    atomic_fetch_add(&timer_ins_count, 1);
    const double t0 = timestamp_tsc();

    TimedMsg tm = aligned_alloc(MEM_ALIGN, sizeof(struct TimedMsg));
    if(tm != NULL) {
        tm->m = m;
        tm->trigger_time = trigger_time;

#if TIMERQ == MUTEX
        pthread_mutex_lock(&timerQ_lock);
#elif TIMERQ == SPIN
        spinlock_lock(&timerQ_lock);
#endif

        pqueue_insert(&timerQ, tm);

#if TIMERQ == MUTEX
        pthread_mutex_unlock(&timerQ_lock);
#elif TIMERQ == SPIN
        spinlock_unlock(&timerQ_lock);
#endif
    }

    atomic_fetch_add(&timer_ins_time, (uint64_t)(timestamp_tsc() - t0));

    return tm;
}

TimedMsg timer_POLL(monotonic_time now) {
    atomic_fetch_add(&timer_poll_count, 1);
    const double t0 = timestamp_tsc();

    TimedMsg tm;

#if TIMERQ == MUTEX
    pthread_mutex_lock(&timerQ_lock);
#elif TIMERQ == SPIN
    spinlock_lock(&timerQ_lock);
#endif

    tm = (TimedMsg)pqueue_peek(&timerQ);
    if (tm != NULL && now >= tm->trigger_time) {
        pqueue_remove(&timerQ, tm);
    } else {
        tm = NULL;
    }

#if TIMERQ == MUTEX
    pthread_mutex_unlock(&timerQ_lock);
#elif TIMERQ == SPIN
    spinlock_unlock(&timerQ_lock);
#endif

    atomic_fetch_add(&timer_poll_time, (uint64_t)(timestamp_tsc() - t0));

    return tm;
}
