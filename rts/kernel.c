//#define _POSIX_C_SOURCE 200112L  // clock_nanosleep()//
#include <time.h>
#include <math.h> // round()
#include <stdatomic.h>
#include <unistd.h>  // sysconf()
#include <pthread.h>

#include "kernelops.h"

char *RTAG_name(RTAG tag) {
    switch (tag) {
        case RDONE: return "RDONE"; break;
        case RCONT: return "RCONT"; break;
        case RWAIT: return "RWAIT"; break;
        case REXIT: return "REXIT"; break;
    }
}

void dump_clos(Clos c);

Clos CLOS1(R (*code)(Clos,WORD), WORD v0) {
    Clos c = CLOS(code, 1);
    c->var[0] = v0;
    return c;
}

Clos CLOS2(R (*code)(Clos,WORD), WORD v0, WORD v1) {
    Clos c = CLOS(code, 2);
    c->var[0] = v0;
    c->var[1] = v1;
    return c;
}
    
Clos CLOS3(R (*code)(Clos,WORD), WORD v0, WORD v1, WORD v2) {
    Clos c = CLOS(code, 3);
    c->var[0] = v0;
    c->var[1] = v1;
    c->var[2] = v2;
    return c;
}

R DONE(Clos this, WORD val) {
    return _DONE(NULL, val);
}

struct Clos doneC = { DONE };

Msg ASYNC(Actor to, Clos c) {
    //printf("+ ASYNC to:%p\n", (void *)to);
    Msg m = MSG(c);
    m->value = &doneC;
    //printf(">>> ENQ_msg -> %p\n", (void *)to);
    if (ENQ_msg(m, to)) {
        //printf(">>> ENQ_ready %p\n", (void *)to);
        ENQ_ready(to);
    }
    return m;
}

R AWAIT(Msg m, Clos th) {
    //printf("+ AWAIT\n");
    return _WAIT(th, m);
}

void *main_loop(void *arg) {
    int idx = (int)arg;
    printf("Hello, I'm %d\n", idx);
    while (1) {
        Actor current = DEQ_ready();
        if (current) {
            //printf("<<< DEQ_ready %d %p\n", idx, (void *)current);

            Msg m = current->msg;

            R r = m->clos->code(m->clos, m->value);

            switch (r.tag) {
                case RDONE: {
                    //printf("RDONE %d %d\n", idx, (int)r.value);
                    m->value = r.value;
                    Actor b = FREEZE_waiting(m);
                    while (b) {
                        b->msg->value = r.value;
                        ENQ_ready(b);
                        b = b->next;
                    }
                    //printf("<<< DEQ_msg %p\n", (void *)current);
                    if (DEQ_msg(current)) {
                        //printf(">>> ENQ_ready %p\n", (void *)current);
                        ENQ_ready(current);
                    }
                    break;
                }
                case RCONT: {
                    //printf("RCONT %d %d\n", idx, (int)r.value);
                    m->clos = r.cont;
                    m->value = r.value;
                    //printf(">>> ENQ_ready %p\n", (void *)current);
                    ENQ_ready(current);
                    break;
                }
                case RWAIT: {
                    //printf("RWAIT %d %lx\n", idx, (long)r.value);
                    m->clos = r.cont;
                    Msg x = (Msg)r.value;
                    if (!ADD_waiting(current, x)) {
                        m->value = x->value;
                        //printf(">>> ENQ_ready %p\n", (void *)current);
                        ENQ_ready(current);
                    }
                    break;
                case REXIT:
                    //fprintf(stderr, "[thread exit; %ld]\n", (long)pthread_self());
                    exit((int)r.value);
                }
            }
        } else {
            printf("OUT OF WORK!   (%d)\n", idx);
            //getchar();
            static struct timespec idle_wait = { 0, 50000000 };  // 500ms
            nanosleep(&idle_wait, NULL);
       }
    }
}

WORD bootstrap(Clos c) {
    printf("> bootstrap\n");
    WORD v = &doneC;
    while (1) {
        R r = c->code(c, v);
        if (r.tag == RDONE)
            return r.value;
        c = r.cont;
        v = r.value;
    }
    printf("< bootstrap\n");
}



///////////////////////////////////////////////////////////////////////


#include "pingpong.c"

#define ROOT Pingpong

int main(int argc, char **argv) {
    long num_threads = sysconf(_SC_NPROCESSORS_ONLN);
    if (argc > 1) {
        int n = atoi(argv[1]);
        if (n > 0) {
            num_threads = n;
        }
    }

    Actor roots[num_threads];
    for (int i = 0; i<num_threads; i++)
        roots[i] = bootstrap(CLOS1(ROOT, (WORD)(i+1)));

    printf("%ld worker threads\n", num_threads);

    // start worker threads, one per CPU
    pthread_t threads[num_threads];
    for(int idx = 0; idx < num_threads; ++idx) {
        pthread_create(&threads[idx], NULL, main_loop, (void*)idx);
    }
    
    // TODO: run I/O polling thread

    for(int idx = 0; idx < num_threads; ++idx) {
        pthread_join(threads[idx], NULL);
    }
}


void dump_clos(Clos c) {
    if (c == NULL) {
        printf("<NULL cont>");
    } else {
        printf("[");
        for (int idx = 0; idx < c->nvar; ++idx) {
            if (idx > 0) printf(", ");
            printf("%p", c->var[idx]);
        }
        printf("]");
    }
    printf("\n");
}
