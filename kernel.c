#include "platform.h"

#define _GNU_SOURCE  // pthread_setaffinity_np(), CPU_SET, etc

#include <time.h>
#include <math.h> // round()
#include <stdatomic.h>
#include <unistd.h>  // sysconf(), getopt()
#include <pthread.h>
#include <assert.h>
#include <locale.h> // setlocale()

#include "kernelops.h"

#if defined(IS_MACOS)
int pthread_setaffinity_np(pthread_t thread, size_t cpu_size, cpu_set_t *cpu_set);
#endif

char *RTAG_name(RTAG tag);
void dump_clos(Clos c);

#define None ((WORD)0)

#define _DONE(cont, value) (R){RDONE, (cont), (value)}
#define _CONT(cont, value) (R){RCONT, (cont), (value)}
#define _WAIT(cont, value) (R){RWAIT, (cont), (value)}
#define _EXIT(cont, value) (R){REXIT, (cont), (value)}


Clos CLOS1(R (*code)(Clos, WORD), WORD v0) {
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

static struct Clos doneC = { DONE };

Msg ASYNC(Msg m) {
    m->value = &doneC;
    if (msg_ENQ(m, m->to)) {
        // as we were first, add the actor to the ready Q
        ready_INSERT(m->to);
    }
    return m;
}

R AWAIT(Msg m, Clos th) {
    return _WAIT(th, m);
}

TimedMsg POSTPONE(monotonic_time trigger_time, Msg m) {
    return timer_INSERT(trigger_time, m);
}

_Bool postpone_CANCEL(TimedMsg tm) {
    // spin until someone cleared the message pointer
    while (1) {
        Msg m = tm->m;
        if (! m)
            return false;
        if (atomic_compare_exchange_weak(&tm->m, &m, NULL)) {
            return true;
        }
    }
}

_Atomic int loop_count = 0;
_Atomic int idle_count = 0;
_Atomic uint64_t clos_exec_time = 0;

atomic_bool thread_stop_flag = false;

size_t SLOWDOWN = 500;

void loop(int thread_id) {

    monotonic_time timer_last_poll = 0;
    const monotonic_duration timer_poll_interval = 10*MT_MICROS;

    while (atomic_load(&thread_stop_flag) == false) {

        atomic_fetch_add(&loop_count, 1);




        // 1 : poll the timer Q

        const monotonic_time now = monotonic_now();

        if (now - timer_last_poll > timer_poll_interval) { // don't poll _every_ time
            timer_last_poll = now;
            TimedMsg tm;
            while ((tm = timer_POLL(now)) != NULL) {
                if (tm->m == NULL) {  // postpone was cancelled
                    printf("[%d] postpone message was cancelled\n", thread_id);
                } else {
                    printf("[%d] posting timed message, t.time: %lu lag: %.2f µs\n", thread_id, tm->trigger_time, (now - tm->trigger_time)/1e3);
                    Msg m = tm->m;
                    tm->m = NULL;  // this timed message can no longer be cancelled
                    m->time_baseline = tm->trigger_time;
                    ASYNC(m);
                }
            }
        }


        // 2 : poll the ready Q

        Actor current = ready_POLL();

        if (current) {
            Msg m = current->msgQ;
            assert(m != NULL);

            if (m->time_baseline == 0)
                m->time_baseline = now;

            assert(m->clos != NULL);
            assert(m->clos->code != NULL);

            double t0 = timestamp_tsc();

            // "look busy"
            for(volatile size_t idx = 0; idx < SLOWDOWN; idx += 2) {
                --idx;
            }

            R r = m->clos->code(m->clos, m->value);

            atomic_fetch_add(&clos_exec_time, (uint64_t)(timestamp_tsc() - t0));

            switch (r.tag) {
                case RDONE: {
                    m->value = r.value;
                    Actor b = waiting_FREEZE(m);
                    while (b) {
                        b->msgQ->value = r.value;
                        Actor next = b->next;  // ready_INSERT() clears b->next
                        ready_INSERT(b);

                        b = next;
                    }

                    if (msg_DEQ(current)) {
                        ready_INSERT(current);
                    }
                    break;
                }
                case RCONT: {
                    m->clos = r.cont;
                    m->value = r.value;

                    ready_INSERT(current);
                    break;
                }
                case RWAIT: {
                    m->clos = r.cont;
                    Msg x = (Msg)r.value;

                    if (! waiting_INSERT(current, x)) {
                        m->value = x->value;
                        ready_INSERT(current);
                    }
                    break;
                }
                case REXIT:
                    // flag all threads to stop
                    atomic_store(&thread_stop_flag, true);
                    break;
            }
        } else {

            // 3 : squander our precious clock cycles!

            printf("[%d] idle!\n", thread_id);
            atomic_fetch_add(&idle_count, 1);
            static const struct timespec idle_wait = {
                .tv_sec = 0,
                .tv_nsec = 1000000,
            };
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

static uint64_t t0 = 0;

void cleanup() {
    printf("======================================================================\n");

    const tsc_t t = timestamp_tsc();
    const tsc_t dur = (t - t0);
    const double dur_s = tsc2ns(dur)/1e9;

    printf("total duration:        \x1b[1m%.4f\x1b[m seconds\n", dur_s);
    printf("total loops:           \x1b[1m%17d\x1b[m\n", loop_count);
    printf("loop time:             \x1b[33;1m%.2f\x1b[m ns/loop\n", 1e9*(dur_s/loop_count));
    printf("idle thread count:     \x1b[1m%17d\x1b[m\n", idle_count);
    printf("messages created:      \x1b[1m%17d\x1b[m\n", msg_create_count);
    printf("CLOS created:          \x1b[1m%17d\x1b[m\n", clos_create_count);
    printf("msg_ENQs:              \x1b[1m%17d\x1b[m\n", msg_enq_count);
    printf("msg_DEQs:              \x1b[1m%17d\x1b[m\n", msg_deq_count);
    printf("waiting_FREEZEs:       \x1b[1m%17d\x1b[m\n", wait_freeze_count);
    printf("ready Q inserts:       \x1b[1m%17d\x1b[m\n", readyQ_ins_count);
    printf("ready Q polls:         \x1b[1m%17d\x1b[m\n", readyQ_poll_count);
    printf("ready Q add time:      \x1b[33;1m%.1f\x1b[m ns/add\n", tsc2ns(readyQ_ins_time)/readyQ_ins_count);
    printf("ready Q poll time:     \x1b[33;1m%.1f\x1b[m ns/poll\n", tsc2ns(readyQ_poll_time)/readyQ_poll_count);
    printf("msg enqueue time:      \x1b[33;1m%.1f\x1b[m ns/enq\n", tsc2ns(msg_enq_time)/msg_enq_count);
    printf("msg dequeue time:      \x1b[33;1m%.1f\x1b[m ns/deq\n", tsc2ns(msg_deq_time)/msg_deq_count);
    printf("timer Q inserts:       \x1b[1m%17d\x1b[m\n", timer_ins_count);
    printf("timer Q polls:         \x1b[1m%17d\x1b[m\n", timer_poll_count);
    printf("timer Q ins time:      \x1b[33;1m%.1f\x1b[m ns/insert\n", tsc2ns(timer_ins_time)/timer_ins_count);
    printf("timer Q poll time:     \x1b[33;1m%.1f\x1b[m ns/poll\n", tsc2ns(timer_poll_time)/timer_poll_count);
    printf("MSG create time:       \x1b[33;1m%.1f\x1b[m ns/create\n", tsc2ns(msg_create_time)/msg_create_count);
    printf("CLOS create time:      \x1b[33;1m%.1f\x1b[m ns/create\n", tsc2ns(clos_create_time)/clos_create_count);
    printf("CLOS exec time:        \x1b[33;1m%.1f\x1b[m ns/exec\n", tsc2ns(clos_exec_time)/loop_count);

    kernelops_CLOSE();
}


void print_usage(char *name) {
    fprintf(stderr, "Usage: %s [-t num-threads] [-l ping-limit] [-p print-interval] [-S idle-spin-count] [-T TSC frequency in MHz]\n", name);
    fprintf(stderr, "    default TSC frequency: %.1f MHz\n", 1000/_tsc2ns);
}

///////////////////////////////////////////////////////////////////////


int main(int argc, char *argv[]) {
    setlocale(LC_NUMERIC, "");  // to get thousand separators

    //printf("1 ns:   %'lu\n", MT_NANOS);
    //printf("1 µs:   %'lu\n", MT_MICROS);
    //printf("1 ms:   %'lu\n", MT_MILLIS);
    //printf("1 sec:  %'lu\n", MT_SECOND);
    //printf("1 min:  %'lu\n", MT_MINUTE);
    //printf("1 hour: %'lu\n", MT_HOUR);

    const long num_cpu = sysconf(_SC_NPROCESSORS_ONLN);
    long num_threads = num_cpu;

    // initial interval and limit
    PRINT_INTERVAL = 1000000/(int)pow(num_threads, 1.3);
    PING_LIMIT = 10*PRINT_INTERVAL;
    if (PING_LIMIT > 10000000) {
        PING_LIMIT = 10000000;
    }

    int opt;
    while ((opt = getopt(argc, argv, "ht:l:p:S:T:")) != -1) {
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
            case 'S': {
                int n = atoi(optarg);
                if (n > 0) {
                    SLOWDOWN = n;
                }
            }
            break;
            case 'T': {
                double n = atof(optarg);
                if (n > 0) {
                    _tsc2ns = 1000.0/n;
                }
            }
            break;
            case 'h':
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

    printf("\x1b[34mPrint interval:\x1b[m \x1b[1m%d\x1b[m  \x1b[34m~~  Ping limit:\x1b[m \x1b[1m%d\x1b[m\n", PRINT_INTERVAL, PING_LIMIT);

    printf("\x1b[34mWorker threads:\x1b[m \x1b[1m%ld\x1b[m  \x1b[34m~~  CPU cores:\x1b[m \x1b[1m%ld\x1b[m\n", num_threads, num_cpu);

    // initialize the global ready queue
    kernelops_INIT();

    Actor roots[num_threads];
    for (int i = 0; i<num_threads; i++) {
        roots[i] = bootstrap(BOOSTRAP_CLOSURE);
    }
    (void)roots;

    t0 = timestamp_tsc();

    printf("======================================================================\n");

    // start worker threads, one per CPU
    pthread_t threads[num_threads];
    pthread_attr_t attrs;
#if defined(IS_GNU_LINUX) || defined(IS_MACOS)
    cpu_set_t cpu_set;
#elif defined(IS_FREEBSD)
    cpuset_t cpu_set;
#endif
    for(int th_id = 0; th_id < num_threads; ++th_id) {
        pthread_attr_init(&attrs);
#if defined(IS_GNU_LINUX) || defined(IS_FREEBSD) || defined(IS_MACOS)
        CPU_ZERO(&cpu_set);
        // assume we're hyperthreaded; use every other core
        int core_id = ((th_id * 2) % num_cpu) + (th_id*2/num_cpu);
        CPU_SET(core_id, &cpu_set);
        pthread_attr_setaffinity_np(&attrs, sizeof(cpu_set), &cpu_set);
#else
        printf("\x1b[41;1mSetting thread affinity is not implemented for your OS\x1b[m\n");
        // __unix__
#endif

        pthread_create(&threads[th_id], &attrs, thread_main, (void *)th_id);
    }
    
    // TODO: run I/O thread

    for(int th_id = 0; th_id < num_threads; ++th_id) {
        pthread_join(threads[th_id], NULL);
        printf("[%d] exited\n", th_id);
    }
    printf("\x1b[34;1mall threads stopped\x1b[m\n");
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

char *RTAG_name(RTAG tag) {
    switch (tag) {
        case RDONE: return "RDONE"; break;
        case RCONT: return "RCONT"; break;
        case RWAIT: return "RWAIT"; break;
        case REXIT: return "REXIT"; break;
    }
	return "<BAD tag!>";
}

#if defined(IS_MACOS)
// NOTE: this supports only a single core in the specified 'cpu_set'
int pthread_setaffinity_np(pthread_t thread, size_t cpu_size, cpu_set_t *cpu_set) {
    int core = 0;
    for (core = 0; core < 8 * cpu_size; core++) {
        if (CPU_ISSET(core, cpu_set))
            break;
    }
    thread_affinity_policy_data_t policy = { core };

    thread_port_t mach_thread = pthread_mach_thread_np(thread);
    thread_policy_set(mach_thread, THREAD_AFFINITY_POLICY, (thread_policy_t)&policy, 1);

    return 0;
}
#endif
