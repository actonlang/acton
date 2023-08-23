#include "rts.h"
#include "q.h"

static inline void spinlock_lock($Lock *f) {
    while (atomic_flag_test_and_set(f) == true) {
        // spin until we could set the flag
    }
}
static inline void spinlock_unlock($Lock *f) {
    atomic_flag_clear(f);
}

#if defined MPMC && MPMC == 3
int ENQ_ready($Actor a) {
    // TODO: atomics!
}
#elif defined MPMC && MPMC == 2
int ENQ_ready($Actor a) {
    int i = a->$affinity;
    spinlock_lock(&rqs[i].lock);
    if (rqs[i].tail) {
        rqs[i].tail->$next = a;
        rqs[i].tail = a;
    } else {
        rqs[i].head = a;
        rqs[i].tail = a;
    }
    a->$next = NULL;
    spinlock_unlock(&rqs[i].lock);
    // If we enqueue to someone who is not us, immediately wake them up...
    int our_wtid = (int)pthread_getspecific(pkey_wtid);
    if (our_wtid != i)
        wake_wt(i);
    return i;
}
#else
int ENQ_ready($Actor a) {
    int i = a->$affinity;
    spinlock_lock(&rqs[i].lock);
    if (rqs[i].head) {
        $Actor x = rqs[i].head;
        while (x->$next)
            x = x->$next;
        x->$next = a;
    } else {
        rqs[i].head = a;
    }
    a->$next = NULL;
    spinlock_unlock(&rqs[i].lock);
    // If we enqueue to someone who is not us, immediately wake them up...
    int our_wtid = (int)pthread_getspecific(pkey_wtid);
    if (our_wtid != i)
        wake_wt(i);
    return i;
}
#endif

// Atomically enqueue actor "a" onto the right ready-queue, either a thread
// local one or the "default" of 0 which is a shared queue among all worker
// threads. The index is offset by 1 so worker thread 0 is at index 1.

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
    if (rqs[idx].head == NULL) {
        return res;
    }

    spinlock_lock(&rqs[idx].lock);
    res = rqs[idx].head;
    if (res) {
        rqs[idx].head = res->$next;
        res->$next = NULL;
        if (rqs[idx].head == NULL) {
            rqs[idx].tail = NULL;
        }
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
    if (rqs[idx].head == NULL)
        return res;

    spinlock_lock(&rqs[idx].lock);
    res = rqs[idx].head;
    if (res) {
        rqs[idx].head = res->$next;
        res->$next = NULL;
    }
    spinlock_unlock(&rqs[idx].lock);
    return res;
}
#endif

$Actor DEQ_ready(int idx) {
    $Actor res = _DEQ_ready(idx);
    if (res)
        return res;

    res = _DEQ_ready(0);
    return res;
}



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
