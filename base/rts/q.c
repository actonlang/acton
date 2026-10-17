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
int32_t ENQ_ready($Actor a, uint32_t ready_us) {
    // TODO: atomics!
}
#elif defined MPMC && MPMC == 2
// Add d to the count of q, which is locked (see struct mpmcq)
static inline void rq_count_add(struct mpmcq *q, int64_t d) {
    uint64_t n = __atomic_load_n(&q->count, __ATOMIC_RELAXED);
    __atomic_store_n(&q->count, n + (uint64_t)d, __ATOMIC_RELAXED);
}

// The ready stamp of actor "a", which is in a ready queue
static inline uint32_t ready_stamp($Actor a) {
    return a->B_Msg->$ready_us;
}

// Store the ready stamp of h, the new head of q, in head_us. q is locked
// (see struct mpmcq).
static inline void rq_head_changed(struct mpmcq *q, $Actor h) {
    __atomic_store_n(&q->head_us, ready_stamp(h), __ATOMIC_RELAXED);
}

// Add the chain first..last of n actors, whose last->$next is NULL, to the
// end of q, in order
static inline void rq_append(struct mpmcq *q, $Actor first, $Actor last, int64_t n) {
    spinlock_lock(&q->lock);
    if (q->tail) {
        q->tail->$next = first;
    } else {
        __atomic_store_n(&q->head, first, __ATOMIC_RELAXED);
        rq_head_changed(q, first);
    }
    q->tail = last;
    rq_count_add(q, n);
    spinlock_unlock(&q->lock);
}

// Take the actor at the head of q, or return NULL if it is empty
static inline $Actor rq_pop(struct mpmcq *q) {
    if (rq_empty(q))
        return NULL;
    spinlock_lock(&q->lock);
    $Actor a = __atomic_load_n(&q->head, __ATOMIC_RELAXED);
    if (a) {
        $Actor next = a->$next;
        __atomic_store_n(&q->head, next, __ATOMIC_RELAXED);
        if (next == NULL)
            q->tail = NULL;
        else
            rq_head_changed(q, next);
        a->$next = NULL;
        assert(a->$waitsfor == NULL);
        rq_count_add(q, -1);
    }
    spinlock_unlock(&q->lock);
    return a;
}

#ifdef ACTON_THREADS
// A worker takes at most this many actors from another queue at once
#define STEAL_MAX 32

// Cut actors from the head of q: half of the actors in q, rounded up, and
// at most STEAL_MAX. Unless "all" is set, stop at the first actor that did
// not become ready before "before", and cut none if that is the head.
// Return the first and set *last and *got, or return NULL if it cuts none.
static $Actor lrq_cut(struct mpmcq *q, bool all, uint32_t before, $Actor *last, int64_t *got) {
    spinlock_lock(&q->lock);
    $Actor first = __atomic_load_n(&q->head, __ATOMIC_RELAXED);
    if (first == NULL || (!all && stamp_diff(ready_stamp(first), before) >= 0)) {
        spinlock_unlock(&q->lock);
        return NULL;
    }
    uint64_t len = __atomic_load_n(&q->count, __ATOMIC_RELAXED);
    int64_t take = (int64_t)((len + 1) / 2);
    if (take > STEAL_MAX)
        take = STEAL_MAX;
    $Actor l = first;
    int64_t n = 1;
    while (n < take && l->$next && (all || stamp_diff(ready_stamp(l->$next), before) < 0)) {
        l = l->$next;
        n++;
    }
    $Actor next = l->$next;
    __atomic_store_n(&q->head, next, __ATOMIC_RELAXED);
    if (next == NULL)
        q->tail = NULL;
    else
        rq_head_changed(q, next);
    rq_count_add(q, -n);
    l->$next = NULL;
    spinlock_unlock(&q->lock);
    *last = l;
    *got = n;
    return first;
}

// Add the chain first..last of n actors to the head of q, in order, ahead of
// the actors already there
static inline void rq_prepend(struct mpmcq *q, $Actor first, $Actor last, int64_t n) {
    spinlock_lock(&q->lock);
    $Actor head = __atomic_load_n(&q->head, __ATOMIC_RELAXED);
    last->$next = head;
    if (head == NULL)
        q->tail = last;
    __atomic_store_n(&q->head, first, __ATOMIC_RELAXED);
    rq_head_changed(q, first);
    rq_count_add(q, n);
    spinlock_unlock(&q->lock);
}

// A worker with no work steals from the first local queue that holds an
// actor, looking at the workers in turn from a random one. It runs the
// first actor it took and queues the rest at the end of its own queue.
static $Actor lrq_steal(WorkerCtx wctx) {
    int64_t self = wctx->id;
    wctx->steal_seed ^= wctx->steal_seed << 13;
    wctx->steal_seed ^= wctx->steal_seed >> 17;
    wctx->steal_seed ^= wctx->steal_seed << 5;
    int64_t n = num_wthreads;
    for (int64_t k = 0; k < n; k++) {
        int64_t v = 1 + (int64_t)((wctx->steal_seed + k) % n);
        if (v == self || rq_empty(&lrqs[v]))
            continue;
        $Actor last;
        int64_t got;
        $Actor first = lrq_cut(&lrqs[v], true, 0, &last, &got);
        if (first) {
            WT_STAT_ADD(wt_stats[self].steal_ops, 1);
            WT_STAT_ADD(wt_stats[self].steal_actors, got);
            if (got > 1)
                rq_append(&lrqs[self], first->$next, last, got - 1);
            first->$next = NULL;
            return first;
        }
    }
    return NULL;
}

// Worker wctx looks at one other worker's local queue, going round all of
// them. DEQ_ready calls this each time the worker looks at the injection queue
// first, whether or not it has work. If, by head_us, the actor at the head of
// that queue became ready at least BALANCE_NS before the actor at the head of
// our queue, or before now when our queue is empty, we cut from its head,
// under that queue's lock, the actors that became ready before that, within
// the limits of lrq_cut, and put them at the head of our queue, ahead of the
// actors already there. So actors that wait behind a long continuation, a long
// queue or a thread the OS does not run move to other workers.
static void lrq_balance(WorkerCtx wctx, int64_t now) {
    int64_t n = num_wthreads;
    if (n < 2)
        return;
    int64_t self = wctx->id;
    struct mpmcq *own = &lrqs[self];
    int64_t k = 1 + (int64_t)(wctx->balance_next++ % (uint64_t)(n - 1));
    int64_t v = 1 + (self - 1 + k) % n;
    struct mpmcq *q = &lrqs[v];
    WT_STAT_ADD(wt_stats[self].balance_looks, 1);
    if (rq_empty(q)) {
        WT_STAT_ADD(wt_stats[self].balance_young, 1);
        return;
    }
    uint32_t theirs = __atomic_load_n(&q->head_us, __ATOMIC_RELAXED);
    uint32_t ours = rq_empty(own) ? stamp_us(now) : __atomic_load_n(&own->head_us, __ATOMIC_RELAXED);
    if (stamp_diff(ours, theirs) < BALANCE_NS / 1000) {
        WT_STAT_ADD(wt_stats[self].balance_young, 1);
        return;
    }
    $Actor last;
    int64_t got;
    $Actor first = lrq_cut(q, false, ours, &last, &got);
    if (first) {
        WT_STAT_ADD(wt_stats[self].balance_ops, 1);
        WT_STAT_ADD(wt_stats[self].balance_actors, got);
        rq_prepend(own, first, last, got);
    } else {
        WT_STAT_ADD(wt_stats[self].balance_stale, 1);
    }
}

// Take a batch from the injection queue: its length divided by the number
// of workers other than the main thread (num_wthreads), rounded down, plus
// one, and at most STEAL_MAX, so that a burst of injected actors spreads
// over the workers that take from it. Run the first and queue the rest at
// the end of our queue.
static $Actor deq_injected(int64_t self) {
    struct mpmcq *q = &rqs[SHARED_RQ];
    if (rq_empty(q))
        return NULL;
    spinlock_lock(&q->lock);
    $Actor first = __atomic_load_n(&q->head, __ATOMIC_RELAXED);
    if (first == NULL) {
        spinlock_unlock(&q->lock);
        return NULL;
    }
    uint64_t len = __atomic_load_n(&q->count, __ATOMIC_RELAXED);
    int64_t take = (int64_t)(len / (uint64_t)num_wthreads) + 1;
    if (take > STEAL_MAX)
        take = STEAL_MAX;
    $Actor l = first;
    int64_t n = 1;
    while (n < take && l->$next) {
        l = l->$next;
        n++;
    }
    $Actor next = l->$next;
    __atomic_store_n(&q->head, next, __ATOMIC_RELAXED);
    if (next == NULL)
        q->tail = NULL;
    else
        rq_head_changed(q, next);
    rq_count_add(q, -n);
    l->$next = NULL;
    spinlock_unlock(&q->lock);
    if (n > 1)
        rq_append(&lrqs[self], first->$next, l, n - 1);
    first->$next = NULL;
    return first;
}
#endif

// Make actor "a" ready, with ready_us as its ready stamp, and return the
// queue it went to: SHARED_RQ, the id of the worker it is pinned to, or
// with threads the id of the worker whose local queue took it. It always
// goes to the end of that queue.
int32_t ENQ_ready($Actor a, uint32_t ready_us) {
    int i = a->$affinity;
    assert(a != NULL && a->$waitsfor == NULL);
    WorkerCtx wctx = GET_WCTX();
    a->B_Msg->$ready_us = ready_us;
    a->$next = NULL;
#ifdef ACTON_THREADS
    // An unpinned actor made ready by a worker stays with that worker. We
    // wake no other worker for it here: the worker wakes a searcher for the
    // actors in its local queue when it starts a continuation (offer_local).
    if (i == SHARED_RQ && wctx != NULL && wctx->id > 0) {
        rq_append(&lrqs[wctx->id], a, a, 1);
        return (int32_t)wctx->id;
    }
#endif
    rq_append(&rqs[i], a, a, 1);
    // If we enqueue to someone who is not us, immediately wake them up...
    if (wctx == NULL || wctx->id != i)
        wake_wt(i);
    return i;
}
#else
int32_t ENQ_ready($Actor a, uint32_t ready_us) {
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
        int64_t our_wtid = wctx->id;
        if (our_wtid != i)
            wake_wt(i);
    }
    return i;
}
#endif

// Take the actor at the head of rqs[idx], or return NULL if it is empty
#if defined MPMC && MPMC == 3
$Actor _DEQ_ready(int64_t idx) {
    // TODO: atomics!
}
#elif defined MPMC && MPMC == 2
$Actor _DEQ_ready(int64_t idx) {
    return rq_pop(&rqs[idx]);
}
#else
// First version
$Actor _DEQ_ready(int64_t idx) {
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

// Whether worker wctx, at time now, is due to look at the injection queue
// before its local queue, and at another worker's queue: INJECT_NS or more
// after it last did (inject_due).
// The main thread does not take from the injection queue and is never due.
static inline bool look_due(WorkerCtx wctx, int64_t now) {
    return wctx->id > 0 && now >= wctx->inject_due;
}

// Take the next actor for worker wctx. now is when it last finished a
// continuation, or when its work callback started if it has finished none
// since.
$Actor DEQ_ready(WorkerCtx wctx, int64_t now) {
    int64_t idx = wctx->id;
    assert(idx >= 0 && idx < 256);
    $Actor res = _DEQ_ready(idx);
    if (res)
        return res;

    // Unless we are running without threads, worker thread 0 (our main thread)
    // runs only the actors pinned to it and takes from no other queue.
    if (idx == 0)
        return NULL;

#if defined ACTON_THREADS && defined MPMC && MPMC == 2
    // Local work first, then the injection queue, then steal from another
    // worker. But the injection queue goes first when the worker is due to
    // look there first (look_due), and then it also looks at another
    // worker's queue (lrq_balance).
    bool look = look_due(wctx, now);
    if (look) {
        wctx->inject_due = now + INJECT_NS;
        res = deq_injected(idx);
        lrq_balance(wctx, now);
    }
    if (!res)
        res = rq_pop(&lrqs[idx]);
    if (!res && !look)
        res = deq_injected(idx);
    if (!res)
        res = lrq_steal(wctx);
    return res;
#else
    res = _DEQ_ready(SHARED_RQ);
    return res;
#endif
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
