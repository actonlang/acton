//#define _POSIX_C_SOURCE 200112L  // clock_nanosleep()//
#include <time.h>
#include <math.h> // round()
#include <stdatomic.h>
#include <unistd.h>  // sysconf()
#include <pthread.h>
#include <assert.h>

#include "kernelops.h"

#define None (WORD)0

#define _DONE(cont, value) (R){RDONE, (cont), (value)}
#define _CONT(cont, value) (R){RCONT, (cont), (value)}
#define _WAIT(cont, value) (R){RWAIT, (cont), (value)}
#define _EXIT(cont, value) (R){REXIT, (cont), (value)}

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

_Atomic int msg_count = 0;

void loop(void *arg) {
    int idx = (int)arg;
    printf("Hello, I'm %d\n", idx);
    while (1) {
        Actor current = DEQ_ready();
        if (current) {
            //printf("<<< DEQ_ready %d %p\n", idx, (void *)current);

            Msg m = current->msg;
			atomic_fetch_add(&msg_count, 1);

            assert(m != NULL);

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

const int PRINT_INTERVAL = 500000;
int PING_LIMIT = PRINT_INTERVAL * 6;  // must be multiple of PRINT_INTERVAL

#include "pingpong2.c"
void *thread_main(void *arg) {
    loop(arg);

    return NULL;
}

double timestamp() {
    struct timespec t;

    clock_gettime(CLOCK_MONOTONIC, &t);

    time_t s = t.tv_sec;
    long µs = round(t.tv_nsec / 1.0e3);   // ns -> µs
    if (µs > 999999) {
        ++s;
        µs = 0;
    }
    return (double)s + µs/1e6;
}

void print_timestamp(double t) {
    printf("time stamp: %.6f\n", t);
}

static double t0 = 0.0;

void cleanup() {
    printf("END\n");

    double t = timestamp();
    printf("total duration: %.6f\n", t - t0);
	printf("message count: %d   (%.2f Mmsg/s)\n", msg_count, (msg_count/1e6)/(t - t0));
}


///////////////////////////////////////////////////////////////////////



int main(int argc, char **argv) {
    atexit(cleanup);

    long num_threads = sysconf(_SC_NPROCESSORS_ONLN);
    if (argc > 1) {
        int n = atoi(argv[1]);
        if (n > 0) {
            num_threads = n;
        }
    }

    Actor roots[num_threads];
    for (int i = 0; i<num_threads; i++)
        roots[i] = bootstrap(BOOSTRAP_CLOSURE);

    printf("%ld worker threads\n", num_threads);

    t0 = timestamp();

    // start worker threads, one per CPU
    pthread_t threads[num_threads];
    for(int idx = 0; idx < num_threads; ++idx) {
        pthread_create(&threads[idx], NULL, thread_main, (void*)idx);
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
