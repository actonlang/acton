#include <assert.h>

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
#if defined(MUTEX_OPS)
    pthread_mutex_init(&m->mut, NULL);
#endif
    return m;
}

Actor ACTOR(int n) {
    Actor a = malloc(sizeof(struct Actor) + n * sizeof(WORD));
    a->next = NULL;
    a->msg = NULL;
#if defined(MUTEX_OPS)
    pthread_mutex_init(&a->mut, NULL);
#endif
    return a;
}


Actor readyQ = NULL;

#if defined(MUTEX_OPS)
pthread_mutex_t ready_mut = PTHREAD_MUTEX_INITIALIZER;
#endif

#if defined(BASIC_OPS) || defined(MUTEX_OPS)
void ready_PUSH(Actor a) {
    assert(a->msg != NULL);
#if defined(MUTEX_OPS)
    pthread_mutex_lock(&ready_mut);
#endif
    if (readyQ) {
        Actor x = readyQ;
        while (x->next)
            x = x->next;
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

bool msg_ENQ(Msg m, Actor a) {
    bool did_enq = true;
#if defined(MUTEX_OPS)
    pthread_mutex_lock(&a->mut);
#endif
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
        a->next = m->waiting;
        m->waiting = a;
        did_add = true;
    }
#if defined(MUTEX_OPS)
    pthread_mutex_unlock(&m->mut);
#endif
    return did_add;
}

Actor waiting_FREEZE(Msg m) {
#if defined(MUTEX_OPS)
    pthread_mutex_lock(&m->mut);
#endif
    m->clos = NULL;
#if defined(MUTEX_OPS)
    pthread_mutex_unlock(&m->mut);
#endif
    Actor res = m->waiting;
    m->waiting = NULL;
    return res;
}
#endif
