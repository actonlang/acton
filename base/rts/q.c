#include "rts.h"
#include "q.h"

// Spin-wait hint, used between reads of a lock that is taken. It does not
// give the thread up to the OS; it only holds this hardware thread back for
// a moment.
//
// x86: pause tells the core that this thread is in a spin-wait loop. The
// core stops issuing the thread's instructions for a short time (from about
// ten to over a hundred cycles, depending on the CPU). With hyper-threading
// (SMT), the other hardware thread on the same core gets the core's shared
// execution units meanwhile, and without it the core saves power. pause
// also keeps the core from speculating far ahead through the loop's reads,
// so leaving the loop when the lock changes does not cost a pipeline flush.
//
// arm64: the corresponding hint, yield, does nothing on most cores, which
// have no SMT (Apple's among them), so a loop of yields spins at full speed.
// isb flushes the core's pipeline, so the instructions after it are fetched
// anew; that takes a short, roughly fixed time, which makes it a delay.
static inline void cpu_relax(void) {
#if defined(__x86_64__) || defined(__i386__)
    __builtin_ia32_pause();
#elif defined(__aarch64__)
    __asm__ __volatile__("isb" ::: "memory");
#endif
}

// Test-and-test-and-set. While the lock is taken, waiters only read it, so
// each keeps a shared copy of its cache line instead of taking the line
// from the holder and from each other, and a waiter tries to take the lock
// only when it looks free. The pause between reads doubles up to a limit,
// so the waiters do not all retry the moment the lock is released. With
// many workers waiting on one ready queue, a lower limit makes more of them
// retry at once, and each handoff of the lock gets slower. Taking the lock
// is an acquire and releasing it a release. Neither is a full barrier on
// Arm: a load after spinlock_unlock() can complete before the stores made
// under the lock are visible to other threads, so code that needs that
// order, like wake_wt(), issues a fence.
static inline void spinlock_lock($Lock *f) {
    unsigned int backoff = 1;
    while (atomic_exchange(f, 1)) {
        do {
            for (unsigned int i = 0; i < backoff; i++)
                cpu_relax();
            if (backoff < 256)
                backoff <<= 1;
        } while (atomic_load_explicit(f, memory_order_relaxed));
    }
}
static inline void spinlock_unlock($Lock *f) {
    atomic_store(f, 0);
}

#if defined MPMC && MPMC == 3
int ENQ_ready($Actor a) {
    // TODO: atomics!
}
#elif defined MPMC && MPMC == 2
// Add d to the count of q, which is locked (see struct mpmcq)
static inline void rq_count_add(struct mpmcq *q, long d) {
    unsigned long long n = __atomic_load_n(&q->count, __ATOMIC_RELAXED);
    __atomic_store_n(&q->count, n + (unsigned long long)d, __ATOMIC_RELAXED);
}

int ENQ_ready($Actor a) {
    int i = a->$affinity;
    assert(a != NULL && a->$waitsfor == NULL);
    spinlock_lock(&rqs[i].lock);
    if (rqs[i].tail) {
        rqs[i].tail->$next = a;
        rqs[i].tail = a;
    } else {
        __atomic_store_n(&rqs[i].head, a, __ATOMIC_RELAXED);
        rqs[i].tail = a;
    }
    a->$next = NULL;
    rq_count_add(&rqs[i], 1);
    spinlock_unlock(&rqs[i].lock);
    // If we enqueue to someone who is not us, immediately wake them up...
    WorkerCtx wctx = GET_WCTX();
    if (wctx != NULL) {
        long our_wtid = wctx->id;
        if (our_wtid != i)
            wake_wt(i);
    }
    return i;
}
#else
int ENQ_ready($Actor a) {
    int i = a->$affinity;
    spinlock_lock(&rqs[i].lock);
    $Actor x = __atomic_load_n(&rqs[i].head, __ATOMIC_RELAXED);
    if (x) {
        while (x->$next)
            x = x->$next;
        x->$next = a;
    } else {
        __atomic_store_n(&rqs[i].head, a, __ATOMIC_RELAXED);
    }
    a->$next = NULL;
    spinlock_unlock(&rqs[i].lock);
    // If we enqueue to someone who is not us, immediately wake them up...
    WorkerCtx wctx = GET_WCTX();
    if (wctx != NULL) {
        long our_wtid = wctx->id;
        if (our_wtid != i)
            wake_wt(i);
    }
    return i;
}
#endif

// Atomically enqueue actor "a" onto the right ready-queue, either a thread
// local one or the "default" shared one.

// Atomically dequeue and return the first actor from a ready-queue, first
// dequeueing from the thread specific queue and second from the global shared
// readyQ or return NULL if no work is found.
#if defined MPMC && MPMC == 3
$Actor _DEQ_ready(int idx) {
    // TODO: atomics!
}
#elif defined MPMC && MPMC == 2
$Actor _DEQ_ready(int idx) {
    $Actor res = NULL;
    if (__atomic_load_n(&rqs[idx].head, __ATOMIC_RELAXED) == NULL) {
        return res;
    }

    spinlock_lock(&rqs[idx].lock);
    res = __atomic_load_n(&rqs[idx].head, __ATOMIC_RELAXED);
    if (res) {
        $Actor next = res->$next;
        __atomic_store_n(&rqs[idx].head, next, __ATOMIC_RELAXED);
        res->$next = NULL;
        if (next == NULL) {
            rqs[idx].tail = NULL;
        }
        assert(res->$waitsfor == NULL);
        rq_count_add(&rqs[idx], -1);
    } else {
        rqs[idx].tail = NULL;
    }
    spinlock_unlock(&rqs[idx].lock);
    return res;
}
#else
// First version
$Actor _DEQ_ready(int idx) {
    $Actor res = NULL;
    if (__atomic_load_n(&rqs[idx].head, __ATOMIC_RELAXED) == NULL)
        return res;

    spinlock_lock(&rqs[idx].lock);
    res = __atomic_load_n(&rqs[idx].head, __ATOMIC_RELAXED);
    if (res) {
        __atomic_store_n(&rqs[idx].head, res->$next, __ATOMIC_RELAXED);
        res->$next = NULL;
    }
    spinlock_unlock(&rqs[idx].lock);
    return res;
}
#endif

$Actor DEQ_ready(int idx) {
    assert(idx >= 0 && idx < 256);
    $Actor res = _DEQ_ready(idx);
    if (res)
        return res;

    // Unless we are running without threads, worker thread 0 (our main thread)
    // is special and does not pick up work from the shared queue. It only
    // serves special actors scheduled on it.
    if (idx == 0)
        return NULL;

    res = _DEQ_ready(SHARED_RQ);
    return res;
}


#if MSGQ == 2
// Atomically enqueue message "m" onto the queue of actor "a",
// return true if the queue was previously empty.
bool ENQ_msg(B_Msg m, $Actor a) {
    bool did_enq = true;
    spinlock_lock(&a->B_Msg_lock);
    m->$next = NULL;
    if (a->B_Msg_tail) {
        a->B_Msg_tail->$next = m;
        a->B_Msg_tail = m;
        did_enq = false;
    } else {
        a->B_Msg = m;
        a->B_Msg_tail = m;
    }
    spinlock_unlock(&a->B_Msg_lock);
    return did_enq;
}

// Atomically dequeue the first message from the queue of actor "a",
// return true if the queue still holds messages.
bool DEQ_msg($Actor a) {
    bool has_more = false;
    spinlock_lock(&a->B_Msg_lock);
    B_Msg x = a->B_Msg;
    if (x) {
        a->B_Msg = x->$next;
        x->$next = NULL;
        if (a->B_Msg == NULL) {
            a->B_Msg_tail = NULL;
        }
        has_more = a->B_Msg != NULL;
    } else {
        a->B_Msg_tail = NULL;
    }
    spinlock_unlock(&a->B_Msg_lock);
    return has_more;
}
#else // MSGQ == 1
// Atomically enqueue message "m" onto the queue of actor "a",
// return true if the queue was previously empty.
bool ENQ_msg(B_Msg m, $Actor a) {
    bool did_enq = true;
    spinlock_lock(&a->B_Msg_lock);
    m->$next = NULL;
    if (a->B_Msg) {
        B_Msg x = a->B_Msg;
        while (x->$next)
            x = x->$next;
        x->$next = m;
        did_enq = false;
    } else {
        a->B_Msg = m;
    }
    spinlock_unlock(&a->B_Msg_lock);
    return did_enq;
}

// Atomically dequeue the first message from the queue of actor "a",
// return true if the queue still holds messages.
bool DEQ_msg($Actor a) {
    bool has_more = false;
    spinlock_lock(&a->B_Msg_lock);
    if (a->B_Msg) {
        B_Msg x = a->B_Msg;
        a->B_Msg = x->$next;
        x->$next = NULL;
        has_more = a->B_Msg != NULL;
    }
    spinlock_unlock(&a->B_Msg_lock);
    return has_more;
}
#endif // MSGQ
