#define _GNU_SOURCE  // pthread_setaffinity_np(), CPU_SET, etc

#include <time.h>
#include <math.h> // round()
#include <stdatomic.h>
#include <unistd.h>  // sysconf(), getopt()
#include <pthread.h>
#include <assert.h>
#include <locale.h> // setlocale()

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
    Msg m = MSG(c);
    m->value = &doneC;
    if (msg_ENQ(m, to)) {
        // as we were first, add the actor to the ready Q
        ready_PUSH(to);
    }
    return m;
}

R AWAIT(Msg m, Clos th) {
    return _WAIT(th, m);
}

_Atomic int loop_count = 0;
_Atomic int idle_count = 0;

atomic_bool thread_stop_flag = false;

void loop(int thread_id) {
    //printf("[%d] message loop\n", thread_id);

    while (atomic_load(&thread_stop_flag) == false) {
        Actor current = ready_POP();
        if (current) {
            Msg m = current->msg;
            atomic_fetch_add(&loop_count, 1);

            R r = m->clos->code(m->clos, m->value);

            switch (r.tag) {
                case RDONE: {
                    m->value = r.value;
                    Actor b = waiting_FREEZE(m);
                    while (b) {
                        b->msg->value = r.value;
                        Actor next = b->next;  // need to copy b->next; ready_PUSH will reset it
                        ready_PUSH(b);

                        b = next;
                    }

                    if (msg_DEQ(current)) {
                        ready_PUSH(current);
                    }
                    break;
                }
                case RCONT: {
                    m->clos = r.cont;
                    m->value = r.value;

                    ready_PUSH(current);
                    break;
                }
                case RWAIT: {
                    m->clos = r.cont;
                    Msg x = (Msg)r.value;

                    if (! waiting_ADD(current, x)) {
                        m->value = x->value;
                        ready_PUSH(current);
                    }
                    break;
                case REXIT:
                    // flag all threads to stop
                    atomic_store(&thread_stop_flag, true);
                    break;
                }
            }
        } else {
            printf("[%d] idle!\n", thread_id);
            atomic_fetch_add(&idle_count, 1);
            static struct timespec idle_wait = { 0, 1000000 };
            nanosleep(&idle_wait, NULL);
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

int PRINT_INTERVAL;
int PING_LIMIT;  // must be multiple of PRINT_INTERVAL

#include "pingpong2.c"

void *thread_main(void *arg) {
    const int thread_id = (int)arg;

    loop(thread_id);

    return NULL;
}

double timestamp() {
    struct timespec t;

    clock_gettime(CLOCK_MONOTONIC, &t);

    double s = (double)t.tv_sec;
    double frac = t.tv_nsec / 1e9;   // ns -> s
    return s + frac;
}

static double t0 = 0.0;

void cleanup() {
    printf("======================================================================\n");

    double t = timestamp();
    printf("total duration:      \x1b[1m%.3f\x1b[m seconds\n", t - t0);
    printf("total loops:         \x1b[1m%'d\x1b[m   \x1b[33;1m%.3f\x1b[m Mloops/s\n", loop_count, (loop_count/1e6)/(t - t0));
    printf("waiting_FREEZEs:     \x1b[1m%'d\x1b[m   \x1b[33;1m%.3f\x1b[m Mfreeze/s\n", wait_freeze_count, (wait_freeze_count/1e6)/(t - t0));
    printf("msg_ENQs:            \x1b[1m%'d\x1b[m   \x1b[33;1m%.3f\x1b[m Mmsg/s\n", msg_enq_count, (msg_enq_count/1e6)/(t - t0));
    printf("ready Q max size:    \x1b[1m%'d\x1b[m\n", readyQ_max);
    printf("msg Q max size:      \x1b[1m%'d\x1b[m\n", msgQ_max);
    printf("messages created:  \x1b[1m%'17d\x1b[m\n", msg_created);
    printf("CLOS created:      \x1b[1m%'17d\x1b[m\n", clos_created);
    printf("idle thread count: \x1b[1m%'17d\x1b[m\n", idle_count);

    printf("CLOS create time:  \x1b[1m%.3f\x1b[m ms  \x1b[33;1m%.1f ns/create\x1b[m\n", atomic_load(&clos_create_time)/1e6, ((double)atomic_load(&clos_create_time))/clos_created);
    printf("MSG create time:   \x1b[1m%.3f\x1b[m ms  \x1b[33;1m%.1f ns/create\x1b[m\n", atomic_load(&msg_create_time)/1e6, ((double)atomic_load(&clos_create_time))/msg_created);

}


void print_usage(char *name) {
    fprintf(stderr, "Usage: %s [-t num-threads] [-l ping-limit] [-i print-interval]\n", name);
}

///////////////////////////////////////////////////////////////////////


int main(int argc, char *argv[]) {
    setlocale(LC_NUMERIC, "");  // to get thousand separators

    const long num_cpu = sysconf(_SC_NPROCESSORS_ONLN);
    long num_threads = num_cpu;

	// initial interval and limit
    PRINT_INTERVAL = 1000000/(int)pow(num_threads, 1.3);
    PING_LIMIT = 10*PRINT_INTERVAL;
    if (PING_LIMIT > 10000000) {
        PING_LIMIT = 10000000;
    }

    int opt;
    while ((opt = getopt(argc, argv, "t:l:p:")) != -1) {
        switch (opt) {
            case 't': {
                int n = atoi(optarg);
                if (n > 0 && n <= num_cpu) {
                    num_threads = n;
                    PRINT_INTERVAL = 1000000/(int)pow(num_threads, 1.3);
                    PING_LIMIT = 10*PRINT_INTERVAL;
                }
            }
            break;
            case 'l': {
                int n = atoi(optarg);
                if (n > 0) {
                    PING_LIMIT = n;
                    PRINT_INTERVAL = PING_LIMIT / 10;
                }
            }
            break;
            case 'p': {
                int n = atoi(optarg);
                if (n > 0) {
                    PRINT_INTERVAL = n;
                    PING_LIMIT = 10*PRINT_INTERVAL;
                }
            }
            break;
            default:
                print_usage(argv[0]);
                exit(EXIT_FAILURE);
        }
    }

    if (optind < argc) {  // unepxected extra args
        print_usage(argv[0]);
        exit(EXIT_FAILURE);
    }

    atexit(cleanup);

    printf("\x1b[34mPrint interval:\x1b[m \x1b[1m%'d\x1b[m  \x1b[34m~~  Ping limit:\x1b[m \x1b[1m%'d\x1b[m\n", PRINT_INTERVAL, PING_LIMIT);

    printf("\x1b[34mWorker threads:\x1b[m \x1b[1m%'ld\x1b[m  \x1b[34m~~  CPU cores:\x1b[m \x1b[1m%ld\x1b[m\n", num_threads, num_cpu);

    // initialize the global ready queue
    kernelops_INIT();

    Actor roots[num_threads];
    for (int i = 0; i<num_threads; i++)
        roots[i] = bootstrap(BOOSTRAP_CLOSURE);

    t0 = timestamp();

    printf("======================================================================\n");

    // start worker threads, one per CPU
    pthread_t threads[num_threads];
    pthread_attr_t attrs;
    cpu_set_t cpu_set;
    for(int th_id = 0; th_id < num_threads; ++th_id) {
        pthread_attr_init(&attrs);
        CPU_ZERO(&cpu_set);
        // assume we're hyperthreaded; use every other core
        int core_id = ((th_id * 2) % num_cpu) + (th_id*2/num_cpu);
        CPU_SET(core_id, &cpu_set);
#if defined(__linux__)
        pthread_attr_setaffinity_np(&attrs, sizeof(cpu_set_t), &cpu_set);
#else
        printf("\x1b[41;1mSetting thread affinity is not implemented for your OS\x1b[m\n");
        // __APPLE__
        // __FreeBSD__
        // __unix__
#endif

        pthread_create(&threads[th_id], &attrs, thread_main, (void *)th_id);
    }
    
    // TODO: run I/O thread

    for(int th_id = 0; th_id < num_threads; ++th_id) {
        pthread_join(threads[th_id], NULL);
        printf("[%d] exited\n", th_id);
    }
    printf("\x1b[33;1mall threads stopped\x1b[m\n");
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
