#include "kernelops.h"

Clos CLOS(R (*code)(Clos, WORD), int n) {
    Clos c = malloc(sizeof(struct Clos) + n * sizeof(WORD));
    c->code = code;
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
void ENQ_ready(Actor n) {
#if defined(MUTEX_OPS)
    pthread_mutex_lock(&ready_mut);
#endif
    if (readyQ) {
        Actor a = readyQ;
        while (a->next)
            a = a->next;
        a->next = n;
    } else {
        readyQ = n;
    }
    n->next = NULL;
#if defined(MUTEX_OPS)
    pthread_mutex_unlock(&ready_mut);
#endif
}

Actor DEQ_ready() {
    Actor res = NULL;
#if defined(MUTEX_OPS)
    pthread_mutex_lock(&ready_mut);
#endif
    if (readyQ) {
        Actor n = readyQ;
        readyQ = n->next;
        n->next = NULL;
        res = n;
    }
#if defined(MUTEX_OPS)
    pthread_mutex_unlock(&ready_mut);
#endif
    return res;
}

bool ENQ_msg(Msg m, Actor to) {
    bool res = true;
#if defined(MUTEX_OPS)
    pthread_mutex_lock(&to->mut);
#endif
    m->next = NULL;
    if (to->msg) {
        Msg a = to->msg;
        while (a->next)
            a = a->next;
        a->next = m;
        res = false;
    } else {
        to->msg = m;
    }
#if defined(MUTEX_OPS)
    pthread_mutex_unlock(&to->mut);
#endif
    return res;
}

bool DEQ_msg(Actor to) {
    bool res = false;
#if defined(MUTEX_OPS)
    pthread_mutex_lock(&to->mut);
#endif
    if (to->msg) {
        Msg m = to->msg;
        to->msg = m->next;
        m->next = NULL;
        res = true;
    }
#if defined(MUTEX_OPS)
    pthread_mutex_unlock(&to->mut);
#endif
    return res;
}

bool ADD_waiting(Actor a, Msg m) {
    bool res = false;
#if defined(MUTEX_OPS)
    pthread_mutex_lock(&m->mut);
#endif
    if (m->clos) {
        a->next = m->waiting;
        m->waiting = a;
        res = true;
    }
#if defined(MUTEX_OPS)
    pthread_mutex_unlock(&m->mut);
#endif
    return res;
}

Actor FREEZE_waiting(Msg m) {
#if defined(MUTEX_OPS)
    pthread_mutex_lock(&m->mut);
#endif
    m->clos = NULL;
#if defined(MUTEX_OPS)
    pthread_mutex_unlock(&m->mut);
#endif
    return m->waiting;
}
#endif
