#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <pthread.h>
#include <stdint.h>

#define None (WORD)0

typedef void *WORD;

struct Clos;
struct Msg;
struct Actor;
struct Thread;

typedef struct R R;
typedef struct Clos *Clos;
typedef struct Msg *Msg;
typedef struct Actor *Actor;
typedef struct Thread *Thread;

enum RTAG { RDONE, RCONT, RWAIT };

struct R {
    int tag;
    Clos cont;
    WORD value;
};

struct Clos {
    R (*code)(Clos, WORD);
    WORD var[];
};

struct Msg {
    Msg next;
    Actor waiting;
    Clos clos;
    WORD value;
};

struct Actor {
    Actor next;
    Msg msg;
    WORD state[];
};

struct Thread {
    Thread next;
    Actor current;
    pthread_t id;
};

Clos CLOS(R (*code)(Clos, WORD), int n) {
    Clos c = malloc(sizeof(struct Clos) + n * sizeof(WORD));
    c->code = code;
    return c;
}

Clos CLOS1(R (*code)(Clos,WORD), WORD v0) {
    Clos c = CLOS(code,1);
    c->var[0] = v0;
    return c;
}

Clos CLOS2(R (*code)(Clos,WORD), WORD v0, WORD v1) {
    Clos c = CLOS(code,2);
    c->var[0] = v0;
    c->var[1] = v1;
    return c;
}
    
Clos CLOS3(R (*code)(Clos,WORD), WORD v0, WORD v1, WORD v2) {
    Clos c = CLOS(code,3);
    c->var[0] = v0;
    c->var[1] = v1;
    c->var[2] = v2;
    return c;
}

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
    a->msg = NULL;
    return a;
}

struct QNode {
    struct QNode *next;
};

typedef struct QNode *QNode;

void enqueue(QNode n, QNode *anchor) {
    if (*anchor) {
        QNode a = *anchor;
        while (a->next)
            a = a->next;
        a->next = n;
    } else {
        *anchor = n;
    }
    n->next = NULL;
}

void *dequeue(QNode *anchor) {
    if (*anchor) {
        QNode n = *anchor;
        *anchor = n->next;
        n->next = NULL;
        return n;
    } else {
        return NULL;
    }
}

#define ENQUEUE(n,q)    enqueue((QNode)n, (QNode*)&q)
#define DEQUEUE(q)      dequeue((QNode*)&q)
#define EMPTY(q)        ((q)==NULL)

Actor readyQ = NULL;

R DONE(Clos this, WORD val) {
    return (R){RDONE, NULL, val};
}

struct Clos doneC = { DONE };

Msg ASYNC(Actor to, Clos c) {
    Msg m = MSG(c);
    m->value = &doneC;
    if (EMPTY(to->msg)) {           // MUTEX A(to)
        ENQUEUE(m, to->msg);        //
        ENQUEUE(to, readyQ);        //
    } else {                        //
        ENQUEUE(m, to->msg);        //
    }                               //
    return m;
}

R AWAIT(Msg m, Clos th) {
    return (R){RWAIT, th, m};
}

void loop() {
    while (1) {
        Actor a = DEQUEUE(readyQ);
        if (a) {
            Msg m = a->msg;
            R r = m->clos->code(m->clos, m->value);
            switch (r.tag) {
                case RDONE:
                    m->value = r.value;
                    m->clos = NULL;                             // MUTEX M(m)
                    while (1) {
                        Actor b = DEQUEUE(m->waiting);
                        if (!b)
                            break;
                        b->msg->value = r.value;
                        ENQUEUE(b, readyQ);
                    }
                    DEQUEUE(a->msg);                            // MUTEX A(a)
                    if (!EMPTY(a->msg))
                        ENQUEUE(a, readyQ);
                    break;
                case RCONT:
                    m->clos = r.cont;
                    m->value = r.value;
                    ENQUEUE(a, readyQ);
                    break;
                case RWAIT:
                    if (((Msg)r.value)->clos) {                 // MUTEX M(r.value)
                        m->clos = r.cont;                       //
                        ENQUEUE(a, ((Msg)r.value)->waiting);    // (Order is not important here)
                    } else {                                    //
                        m->clos = r.cont;                       //
                        m->value = ((Msg)r.value)->value;       //
                        ENQUEUE(a, readyQ);                     //
                    }                                           //
                    break;
            }
        } else {
            printf("OUT OF WORK!\n");
            getchar();
        }
    }
}

WORD bootstrap(Clos c) {
    WORD v = &doneC;
    while (1) {
        R r = c->code(c, v);
        if (r.tag == RDONE)
            return r.value;
        c = r.cont;
        v = r.value;
    }
}

///////////////////////////////////////////////////////////////////////

R pong(Actor self, WORD n, WORD q, Clos then);

R lam1(Clos this, WORD _) {
    return pong(this->var[0], ((Actor)this->var[0])->state[0], this->var[1], this->var[2]);
}

R ping(Actor self, WORD q, Clos then) {
    self->state[0] = (WORD)((int64_t)self->state[0] + 1);
    int64_t j = (int64_t)self->state[0]*(int64_t)q;
    if (j % 100000 == 0)
        printf("Ping %ld\n", j);
    return (R){RCONT, CLOS3(lam1,self,q,then), None};
}

R ping1(Clos this, WORD th) {
    return ping(this->var[0], this->var[1], th);
}

R pong(Actor self, WORD n, WORD q, Clos then) {
    int64_t j = (int64_t)n*(int64_t)q;
    if (j % 100000 == 0)
        printf("     %ld Pong\n", j);
    ASYNC(self, CLOS2(ping1,self,q));
    return (R){RCONT, then, None};
}

R ping2(Clos this, WORD th) {
    return ping(this->var[0], this->var[1], th);
}

R Pingpong(Clos this, WORD then) {
    Actor self = ACTOR(1);
    self->state[0] = 0;
    ASYNC(self, CLOS2(ping2,self,this->var[0]));
    return (R){RCONT, then, self};
}

int main(int argc, char **argv) {
    Actor root = bootstrap(CLOS1(Pingpong, (WORD)1));
    (void)root;  // unused

    loop();
}

/*
-------------------------------- original

actor Pingpong(i):
    var count = 0
    async def ping(q):
        count += 1
        print('Ping %d', count*q)
        _ = pong(count, q)
        return None
    def pong(n,q):
        print('     %d Pong', n*q)
        ping(q)
        return None
    ping(i)

-------------------------------- explicit ASYNC

actor Pingpong(i):
    var count = 0
    def ping(q):
        count += 1
        print('Ping %d', count*q)
        _ = pong(count, q)
        return None
    def pong(n,q):
        print('     %d Pong', n*q)
        ASYNC_(self, lambda: ping(q))
        return None
    ASYNC_(self, lambda: ping(i))

-------------------------------- explicit ACTOR

def Pingpong(i):
    self = ACTOR(1)
    self.count = 0
    def ping(q):
        self.count += 1
        print('Ping %d', self.count*q)
        _ = pong(self.count, q)
        return None
    def pong(n,q):
        print('     %d Pong', n*q)
        ASYNC_(self, lambda: ping(q))
        return None
    ASYNC_(self, lambda: ping(i))
    return self

-------------------------------- CPS

def Pingpong(i, then):
    self = ACTOR(1)
    self.count = 0
    def ping(q, then):
        self.count +=1
        print('Ping %d', self.count*q)
        return RCONT(lambda _: pong(self.count, q, then), None)
    def pong(n, q, then):
        print('     %d Pong', n*q)
        ASYNC(self, lambda th: ping(q,th))
        return RCONT(then, None)
    ASYNC(self, lambda th: ping(i,th))
    return RCONT(then, self)

-------------------------------- explicit lambdas

def Pingpong(i, then):
    self = ACTOR(1)
    self.count = 0
    def ping(q, then):
        self.count +=1
        print('Ping %d', self.count*q)
        def lam1(_):
            return pong(self.count, q, then)
        return RCONT(lam1, None)
    def pong(n, q, then):
        print('     %d Pong', n*q)
        def ping1(th):
            return ping(q, th)
        ASYNC(self, ping1)
        return RCONT(then, None)
    def ping2(th):
        return ping(i, th)
    ASYNC(self, ping2)
    return RCONT(then, self)

-------------------------------- closure-conversion

def Pingpong(i, then):
    self = ACTOR(1)
    self.count = 0
    def ping(self, q, then):
        self.count +=1
        print('Ping %d', self.count*q)
        def lam1(self, q, then, _):
            return pong(self, self.count, q, then)
        return RCONT(CLOS3(lam1,self,q,then), None)
    def pong(self, n, q, then):
        print('     %d Pong', n*q)
        def ping1(self, q, th):
            return ping(self, q, th)
        ASYNC(self, CLOS2(ping1,self,q))
        return RCONT(then, None)
    def ping2(self, i, th):
        return ping(self, i, th)
    ASYNC(self, CLOS2(ping2,self,i))
    return RCONT(then, self)

-------------------------------- lambda-lifting

def lam1(self, q, then, _):
    return pong(self, self.count, q, then)

def ping(self, q, then):
    self.count +=1
    print('Ping %d', self.count*q)
    return RCONT(CLOS3(lam1,self,q,then), None)

def ping1(self, q, th):
    return ping(self, q, th)

def pong(self, n, q, then):
    print('     %d Pong', n*q)
    ASYNC(self, CLOS2(ping1,self,q))
    return RCONT(then, None)

def ping2(self, i, th):
    return ping(self, i, th)

def Pingpong(i, then):
    self = ACTOR(1)
    self.count = 0
    ASYNC(self, CLOS2(ping2,self,i))
    return RCONT(then, self)

-------------------------------- explicit this

def lam1(this, _):
    return pong(this.self, this.self.count, this.q, this.then)

def ping(self, q, then):
    self.count +=1
    print('Ping %d', self.count*q)
    return RCONT(CLOS3(lam1,self,q,then), None)

def ping1(this, th):
    return ping(this.self, this.q, th)

def pong(self, n, q, then):
    print('     %d Pong', n*q)
    ASYNC(self, CLOS2(ping1,self,q))
    return RCONT(then, None)

def ping2(this, th):
    return ping(this.self, this.i, th)

def Pingpong(this, then):
    self = ACTOR(1)
    self.count = 0
    ASYNC(self, CLOS2(ping2,self,this.i))
    return RCONT(then, self)

-------------------------------- anonymous variable arrays

def lam1(this, _):
    return pong(this[0], this[0][0], this[1], this[2])

def ping(self, q, then):
    self[0] +=1
    print('Ping %d', self[0]*q)
    return RCONT(CLOS3(lam1,self,q,then), None)

def ping1(this, th):
    return ping(this[0], this[1], th)

def pong(self, n, q, then):
    print('     %d Pong', n*q)
    ASYNC(self, CLOS2(ping1,self,q))
    return RCONT(then, None)

def ping2(this, th):
    return ping(this[0], this[1], th)

def Pingpong(this, then):
    self = ACTOR(1)
    self[0] = 0
    ASYNC(self, CLOS2(ping2,self,this[0]))
    return RCONT(then, self)

-------------------------------- explicit var and state offsets

def lam1(this, _):
    return pong(this.var[0], this.var[0].state[0], this.var[1], this.var[2])

def ping(self, q, then):
    self.state[0] +=1
    print('Ping %d', self.state[0]*q)
    return RCONT(CLOS3(lam1,self,q,then), None)

def ping1(this, th):
    return ping(this.var[0], this.var[1], th)

def pong(self, n, q, then):
    print('     %d Pong', n*q)
    ASYNC(self, CLOS2(ping1,self,q))
    return RCONT(then, None)

def ping2(this, th):
    return ping(this.var[0], this.var[1], th)

def Pingpong(this, then):
    self = ACTOR(1)
    self.state[0] = 0
    ASYNC(self, CLOS2(ping2,self,this.var[0]))
    return RCONT(then, self)

*/
