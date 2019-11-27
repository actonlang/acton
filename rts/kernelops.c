#include "kernelops.h"

Clos CLOS(R (*code)(Clos, WORD), int n) {
    Clos c = malloc(sizeof(struct Clos) + n * sizeof(WORD));
    c->code = code;
    c->nvar = n;
    for(int x = 0; x < n; ++x) {
        c->var[x] = (WORD)0xbadf00d; // "bad food"
    }
    return c;
}

Msg MSG(Clos clos) {
    Msg m = malloc(sizeof(struct Msg));
    m->next = NULL;
    m->waiting = NULL;
    m->clos = clos;
    atomic_flag_clear(&m->wait_lock);
    return m;
}

Actor ACTOR(int n) {
    Actor a = malloc(sizeof(struct Actor) + n * sizeof(WORD));
    a->next = NULL;
    a->msg = NULL;
    atomic_flag_clear(&a->msg_lock);
    return a;
}


Actor readyQ = NULL;
volatile atomic_flag readyQ_lock;


void ENQ_ready(Actor a) {
    spinlock_lock(&readyQ_lock);
    if (readyQ) {
        Actor x = readyQ;
        while (x->next)
            x = x->next;
        x->next = a;
    } else {
        readyQ = a;
    }
    a->next = NULL;
    spinlock_unlock(&readyQ_lock);
}

Actor DEQ_ready() {
    Actor res = NULL;
    spinlock_lock(&readyQ_lock);
    if (readyQ) {
        Actor x = readyQ;
        readyQ = x->next;
        x->next = NULL;
        res = x;
    }
    spinlock_unlock(&readyQ_lock);
    return res;
}

bool ENQ_msg(Msg m, Actor a) {
    bool did_enq = true;
    spinlock_lock(&a->msg_lock);
    m->next = NULL;
    if (a->msg) {
        Msg x = a->msg;
        while (x->next)
            x = x->next;
        x->next = m;
        did_enq = false;
    } else {
        a->msg = m;
    }
    spinlock_unlock(&a->msg_lock);
    return did_enq;
}

bool DEQ_msg(Actor a) {
    bool has_more = false;
    spinlock_lock(&a->msg_lock);
    if (a->msg) {
        Msg x = a->msg;
        a->msg = x->next;
        x->next = NULL;
        has_more = a->msg != NULL;
    }
    spinlock_unlock(&a->msg_lock);
    return has_more;
}

bool ADD_waiting(Actor a, Msg m) {
    bool did_add = false;
    spinlock_lock(&m->wait_lock);
    if (m->clos) {
        a->next = m->waiting;
        m->waiting = a;
        did_add = true;
    }
    spinlock_unlock(&m->wait_lock);
    return did_add;
}

Actor FREEZE_waiting(Msg m) {
    spinlock_lock(&m->wait_lock);
    m->clos = NULL;
    spinlock_unlock(&m->wait_lock);
    Actor res = m->waiting;
    m->waiting = NULL;
    return res;
}
