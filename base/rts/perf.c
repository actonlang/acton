#include "perf.h"

#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <uv.h>

#if defined(__linux__)
#include <linux/perf_event.h>
#include <sys/syscall.h>
#include <unistd.h>
#elif defined(__APPLE__)
#include <libproc.h>
#include <sys/resource.h>
#include <unistd.h>
#endif

static const char *perf_status = "not enabled at process start";
static const char *perf_scope = "process:user+kernel";
static bool perf_enabled;

#if defined(__linux__) && defined(PERF_ATTR_SIZE_VER7)
static int perf_cycles_fd = -1;
static int perf_instructions_fd = -1;

static int perf_open(uint64_t event, int group, bool user_only) {
    struct perf_event_attr attr = {0};
    attr.type = PERF_TYPE_HARDWARE;
    attr.size = sizeof(attr);
    attr.config = event;
    attr.inherit = 1;
    // Include future threads, but not processes spawned by the benchmark.
    attr.inherit_thread = 1;
    attr.exclude_kernel = user_only;
    attr.exclude_hv = 1;
    attr.read_format = PERF_FORMAT_TOTAL_TIME_ENABLED | PERF_FORMAT_TOTAL_TIME_RUNNING;
    return (int)syscall(SYS_perf_event_open, &attr, 0, -1, group, PERF_FLAG_FD_CLOEXEC);
}

static bool perf_open_group(bool user_only) {
    perf_cycles_fd = perf_open(PERF_COUNT_HW_CPU_CYCLES, -1, user_only);
    if (perf_cycles_fd < 0)
        return false;
    perf_instructions_fd = perf_open(PERF_COUNT_HW_INSTRUCTIONS, perf_cycles_fd, user_only);
    if (perf_instructions_fd < 0) {
        int saved_errno = errno;
        close(perf_cycles_fd);
        perf_cycles_fd = -1;
        errno = saved_errno;
        return false;
    }
    return true;
}

static bool perf_read_event(int fd, uint64_t *count, uint64_t *enabled, uint64_t *running) {
    uint64_t values[3];
    ssize_t size;
    do {
        size = read(fd, values, sizeof(values));
    } while (size < 0 && errno == EINTR);
    if (size != (ssize_t)sizeof(values) || values[2] > values[1])
        return false;
    *count = values[0];
    *enabled = values[1];
    *running = values[2];
    return true;
}
#endif

void rts_perf_init(void) {
    const char *enabled = getenv("ACTON_TEST_PERF");
    perf_enabled = enabled != NULL && strcmp(enabled, "1") == 0;
    // Do not change the behaviour of Acton subprocesses launched by a test.
    uv_os_unsetenv("ACTON_TEST_PERF");
    if (!perf_enabled)
        return;
#if defined(__linux__) && defined(PERF_ATTR_SIZE_VER7)
    // Run before GC_INIT: both collector and RTS threads must inherit events.
    if (perf_open_group(false)) {
        perf_status = "available";
    } else if ((errno == EACCES || errno == EPERM) && perf_open_group(true)) {
        perf_scope = "process:user";
        perf_status = "available";
    } else {
        perf_status = errno == EACCES || errno == EPERM
            ? "permission denied" : "thread counters not supported";
    }
#elif defined(__APPLE__) && defined(RUSAGE_INFO_V4)
    struct rusage_info_v4 usage = {0};
    if (proc_pid_rusage(getpid(), RUSAGE_INFO_V4, (rusage_info_t *)&usage) == 0
            && usage.ri_instructions > 0 && usage.ri_cycles > 0)
        perf_status = "available";
    else
        perf_status = "not supported by this system";
#else
    perf_status = "not supported by this system";
#endif
}

void rts_perf_read(struct rts_perf_sample *sample) {
    memset(sample, 0, sizeof(*sample));
    uv_rusage_t usage;
    if (uv_getrusage(&usage) == 0) {
        sample->cpu_available = true;
        sample->user_ns = (uint64_t)usage.ru_utime.tv_sec * 1000000000 + (uint64_t)usage.ru_utime.tv_usec * 1000;
        sample->system_ns = (uint64_t)usage.ru_stime.tv_sec * 1000000000 + (uint64_t)usage.ru_stime.tv_usec * 1000;
    }
    if (!perf_enabled || strcmp(perf_status, "available") != 0)
        return;
#if defined(__linux__) && defined(PERF_ATTR_SIZE_VER7)
    // Inherited group reads are unsupported. Individual reads include all
    // live and exited inherited threads. Keep coverage to detect multiplexing.
    sample->hardware_available = perf_read_event(perf_cycles_fd, &sample->cycles,
            &sample->cycles_enabled, &sample->cycles_running)
        && perf_read_event(perf_instructions_fd, &sample->instructions,
            &sample->instructions_enabled, &sample->instructions_running);
#elif defined(__APPLE__) && defined(RUSAGE_INFO_V4)
    struct rusage_info_v4 counters = {0};
    if (proc_pid_rusage(getpid(), RUSAGE_INFO_V4, (rusage_info_t *)&counters) == 0
            && counters.ri_instructions > 0 && counters.ri_cycles > 0) {
        sample->hardware_available = true;
        sample->instructions = counters.ri_instructions;
        sample->cycles = counters.ri_cycles;
        // These are maintained by the kernel without a multiplexed event set.
        uint64_t now = uv_hrtime();
        sample->instructions_enabled = sample->instructions_running = now;
        sample->cycles_enabled = sample->cycles_running = now;
    }
#endif
}

const char *rts_perf_backend(void) {
#if defined(__linux__)
    return "perf_event_open";
#elif defined(__APPLE__)
    return "proc_pid_rusage";
#else
    return "none";
#endif
}

const char *rts_perf_scope(void) { return perf_scope; }
const char *rts_perf_status(void) { return perf_status; }
