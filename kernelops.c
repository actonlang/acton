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

Actor readyQ = NULL;
Actor readyTail = NULL;

#if defined(BASIC_OPS)

Msg MSG(Clos clos) {
    Msg m = malloc(sizeof(struct Msg));
    m->next = NULL;
    m->waiting = NULL;
    m->clos = clos;
    return m;
}

Actor ACTOR(int n) {
    Actor a = malloc(sizeof(struct Actor) + n * sizeof(WORD));
    a->next = NULL;
    a->msgQ = NULL;
    a->msgTail = NULL;
    return a;
}

void ENQ_ready(Actor a) {
    a->next = NULL;
    if (readyTail) {
        readyTail->next = a;
        readyTail = a;
    } else {
        readyTail = a;
        readyQ = a;
    }
}

Actor DEQ_ready() {
    Actor res = NULL;
    if (readyQ) {
        Actor x = readyQ;
        readyQ = x->next;
        if (!readyQ)
            readyTail = NULL;
        x->next = NULL;
        res = x;
    }
    return res;
}

bool ENQ_msg(Msg m, Actor a) {
    bool was_empty = true;
    m->next = NULL;
    if (a->msgTail) {
        a->msgTail->next = m;
        a->msgTail = m;
        was_empty = false;
    } else {
        a->msgTail = m;
        a->msgQ = m;
    }
    return was_empty;
}

bool DEQ_msg(Actor a) {
    bool has_more = false;
    if (a->msgQ) {
        Msg x = a->msgQ;
        a->msgQ = x->next;
        if (!a->msgQ)
            a->msgTail = NULL;
        x->next = NULL;
        has_more = a->msgQ != NULL;
    }
    return has_more;
}

bool ADD_waiting(Actor a, Msg m) {
    bool did_add = false;
    if (m->clos) {
        a->next = m->waiting;
        m->waiting = a;
        did_add = true;
    }
    return did_add;
}

Actor FREEZE_waiting(Msg m) {
    m->clos = NULL;
    Actor res = m->waiting;
    m->waiting = NULL;
    return res;
}

#endif



#if defined(MUTEX_OPS)

Msg MSG(Clos clos) {
    Msg m = malloc(sizeof(struct Msg));
    m->next = NULL;
    m->waiting = NULL;
    m->clos = clos;
    pthread_mutex_init(&m->mut, NULL);
    return m;
}

Actor ACTOR(int n) {
    Actor a = malloc(sizeof(struct Actor) + n * sizeof(WORD));
    a->next = NULL;
    a->msgQ = NULL;
    a->msgTail = NULL;
    pthread_mutex_init(&a->mut, NULL);
    return a;
}

pthread_mutex_t ready_mut = PTHREAD_MUTEX_INITIALIZER;

void ENQ_ready(Actor a) {
	assert(a->msgQ != NULL);
    a->next = NULL;
    pthread_mutex_lock(&ready_mut);
    if (readyTail) {
        readyTail->next = a;
        readyTail = a;
    } else {
        readyTail = a;
        readyQ = a;
    }
    pthread_mutex_unlock(&ready_mut);
}

Actor DEQ_ready() {
    Actor res = NULL;
    pthread_mutex_lock(&ready_mut);
    if (readyQ) {
        Actor x = readyQ;
        readyQ = x->next;
        if (!readyQ)
            readyTail = NULL;
        x->next = NULL;
        res = x;
    }
    pthread_mutex_unlock(&ready_mut);
    return res;
}

bool ENQ_msg(Msg m, Actor a) {
    bool was_empty = true;
    pthread_mutex_lock(&a->mut);
    m->next = NULL;
    if (a->msgTail) {
        a->msgTail->next = m;
        a->msgTail = m;
        was_empty = false;
    } else {
        a->msgTail = m;
        a->msgQ = m;
    }
    pthread_mutex_unlock(&a->mut);
    return was_empty;
}

bool DEQ_msg(Actor a) {
    bool has_more = false;
    pthread_mutex_lock(&a->mut);
    if (a->msgQ) {
        Msg x = a->msgQ;
        a->msgQ = x->next;
        if (!a->msgQ)
            a->msgTail = NULL;
        x->next = NULL;
        has_more = a->msgQ != NULL;
    }
    pthread_mutex_unlock(&a->mut);
    return has_more;
}

bool ADD_waiting(Actor a, Msg m) {
    bool did_add = false;
    pthread_mutex_lock(&m->mut);
    if (m->clos) {
        a->next = m->waiting;
        m->waiting = a;
        did_add = true;
    }
    pthread_mutex_unlock(&m->mut);
    return did_add;
}

Actor FREEZE_waiting(Msg m) {
    pthread_mutex_lock(&m->mut);
    m->clos = NULL;
    pthread_mutex_unlock(&m->mut);
    Actor res = m->waiting;
    m->waiting = NULL;
    return res;
}
#endif


#if defined(LFREE_OPS)

Msg MSG(Clos clos) {
    Msg m = malloc(sizeof(struct Msg));
    m->next = NULL;
    m->waiting = NULL;
    m->clos = clos;
    return m;
}

Actor ACTOR(int n) {
    Actor a = malloc(sizeof(struct Actor) + n * sizeof(WORD));
    a->next = NULL;
    a->msgQ = NULL;
    a->msgTail = NULL;
    return a;
}

void ENQ_ready(Actor a) {
    a->next = NULL;
    if (readyTail) {
        readyTail->next = a;
        readyTail = a;
    } else {
        readyTail = a;
        readyQ = a;
    }
}

Actor DEQ_ready() {
    Actor res = NULL;
    if (readyQ) {
        Actor x = readyQ;
        readyQ = x->next;
        if (!readyQ)
            readyTail = NULL;
        x->next = NULL;
        res = x;
    }
    return res;
}

bool ENQ_msg(Msg m, Actor a) {
    bool was_empty = true;
    m->next = NULL;
    if (a->msgTail) {
        a->msgTail->next = m;
        a->msgTail = m;
        was_empty = false;
    } else {
        a->msgTail = m;
        a->msgQ = m;
    }
    return was_empty;
}

bool DEQ_msg(Actor a) {
    bool has_more = false;
    if (a->msgQ) {
        Msg x = a->msgQ;
        a->msgQ = x->next;
        if (!a->msgQ)
            a->msgTail = NULL;
        x->next = NULL;
        has_more = a->msgQ != NULL;
    }
    return has_more;
}

bool ADD_waiting(Actor a, Msg m) {
    bool did_add = false;
    if (m->clos) {
        a->next = m->waiting;
        m->waiting = a;
        did_add = true;
    }
    return did_add;
}

Actor FREEZE_waiting(Msg m) {
    m->clos = NULL;
    Actor res = m->waiting;
    m->waiting = NULL;
    return res;
}

#endif
