#include "rts/common.h"
#include <uv.h>

#if defined(__linux__) || defined(__APPLE__)
#include <pthread.h>
#include <signal.h>
#include <stdatomic.h>
#include <unistd.h>

static _Atomic int collecting;
static _Atomic unsigned collections;
static unsigned forks;
static int checking_spawn;

static void check_fork_mask(void) {
    if (!checking_spawn)
        return;

    sigset_t mask;
    if (pthread_sigmask(SIG_SETMASK, NULL, &mask) != 0)
        _exit(1);
    int suspend_signal = GC_get_suspend_signal();
    int restart_signal = GC_get_thr_restart_signal();
    if ((suspend_signal > 0 && sigismember(&mask, suspend_signal) != 0) ||
        (restart_signal > 0 && sigismember(&mask, restart_signal) != 0)) {
        static const char message[] = "libuv blocked GC signals before fork\n";
        (void)write(STDERR_FILENO, message, sizeof(message) - 1);
        _exit(1);
    }
    forks++;
}

static void *collect(void *unused) {
    (void)unused;
    while (atomic_load(&collecting)) {
        GC_gcollect();
        atomic_fetch_add(&collections, 1);
        usleep(1000);
    }
    return NULL;
}

static void spawn_exit_cb(uv_process_t *process, int64_t status, int signal) {
    int *success = process->data;
    *success = status == 0 && signal == 0;
    uv_close((uv_handle_t *)process, NULL);
}
#endif

void spawn_gcQ___ext_init__() {
}

bool spawn_gcQ_check_spawn_gc() {
#if defined(__linux__) || defined(__APPLE__)
    // An OS alarm can still end the test if collection deadlocks all workers.
    alarm(30);
    if (pthread_atfork(check_fork_mask, NULL, NULL) != 0)
        return false;

    uv_loop_t loop;
    if (uv_loop_init(&loop) != 0)
        return false;

    atomic_store(&collecting, 1);
    pthread_t collector;
    if (pthread_create(&collector, NULL, collect, NULL) != 0)
        return false;

    bool success = true;
    for (unsigned i = 0; i < 100; i++) {
        int exited = 0;
        uv_process_t process = {0};
        process.data = &exited;
        char *args[] = {"/bin/sh", "-c", "exit 0", NULL};
        uv_process_options_t options = {0};
        options.file = args[0];
        options.args = args;
        options.exit_cb = spawn_exit_cb;
        // Setting our existing UID forces libuv's fork fallback without
        // requiring elevated privileges or changing the child's identity.
        options.flags = UV_PROCESS_SETUID;
        options.uid = geteuid();
        checking_spawn = 1;
        int result = uv_spawn(&loop, &process, &options);
        checking_spawn = 0;
        if (result != 0) {
            success = false;
            break;
        }
        uv_run(&loop, UV_RUN_DEFAULT);
        if (!exited) {
            success = false;
            break;
        }
    }

    atomic_store(&collecting, 0);
    if (pthread_join(collector, NULL) != 0)
        success = false;
    if (uv_loop_close(&loop) != 0)
        success = false;
    alarm(0);
    return success && forks == 100 && atomic_load(&collections) > 0;
#else
    // The signal-mask exception applies to libuv's Unix fork path.
    return true;
#endif
}
