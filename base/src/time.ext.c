#define GC_THREADS 1
#include <gc.h>

#ifdef _WIN32
#else
#include <sys/timex.h>
#endif
#include <uv.h>

#include "../rts/io.h"
#include "../rts/log.h"

unsigned long time__incarnation = 0;
// Clock data
int time__realclock_status = 0;
#ifdef _WIN32
#else
struct timex time__realclock_tx = {0};
#endif

// Reduce UUID to 64 bits. Good enough for our purposes.
unsigned long time__uuid_to_ulong(const char* uuid) {
    unsigned long result = 0;
    unsigned long value;
    int i;

    for (i = 0; i < 16; i++) {
        if (i == 8 || i == 13 || i == 18 || i == 23) {
            if (uuid[i] != '-') {
                return 0;
            }
        } else {
            value = isdigit(uuid[i]) ? uuid[i] - '0' : toupper(uuid[i]) - 'A' + 10;
            result = (result << 4) | (value & 0xF);
        }
    }

    return result;
}


void time__get_clock_data_cb(uv_timer_t *ev) {
#ifdef __linux__
    time__realclock_status = adjtimex(&time__realclock_tx);
#elif defined(_WIN32) // Windows
    // TODO: implement Windows support?
    time__realclock_status = 0;
#else
    time__realclock_status = ntp_adjtime(&time__realclock_tx);
#endif
}

void timeQ___ext_init__() {
    // Monotonic clocks have an arbitrary starting point, so we need to keep
    // track of which "incarnation" of the clock we're using, thus
    // time__incarnation. We really want to track a boot-id, which is unique to
    // a particular boot of a particular machine. Linux provides this in the
    // /proc/sys/kernel/random/boot_id file, but that's not available on all
    // platforms. Our fallback method is to track the "boot-id" of an Acton
    // node, which we effectively accomplish by just setting time__incarnation
    // to a random value at startup. As we do this once on startup of an Acton
    // node, it means we will be able to compare the measurements within the
    // same Acton node and during that node's lifetime. If the node is
    // restarted, we will get a new ID and will thus not be able to compare. Nor
    // are we able to compare between Acton nodes, as it should.

    // Using Linux boot-id as incarnation
    FILE* boot_id = fopen("/proc/sys/kernel/random/boot_id", "r");
    if (boot_id != NULL) {
        // Read the contents of the file.
        char buf[64];
        size_t nread = fread(buf, 1, sizeof(buf), boot_id);
        if (nread > 0) {
            // Convert the contents to an integer.
            time__incarnation = time__uuid_to_ulong(buf);
            log_debug("Using Linux boot-id for clock incarnation id: %lu", time__incarnation);
        }
        fclose(boot_id);
    }

    // Fallback to random incarnation ID.
    if (time__incarnation == 0) {
        // We failed to read the boot ID or don't have one on this platform, so
        // we just use a random value.
        time__incarnation = rand();
        log_debug("Using random value for clock incarnation id: %lu", time__incarnation);
    }

    // Schedule background work to get clock data. Fetching this data using
    // ntp_adjtime is considerably slower than a simple clock_gettime call, so
    // we do it in the background. Once every 10 seconds should be enough.
    // To get an idea, benching on my machine ntp_adjtime is about 50x slower:
    //   ntp_adjtime time per call: 586.459146 ns
    //   ntp_adjtime operations per second: 1705148.614052
    //   clock_gettime time per call: 17.545191 ns
    //   clock_gettime operations per second: 56995674.769229
    // We don't want to slow down our programs like that, getting the time
    // should be cheap, which is why we do this somewhat elaborate dance.
    uv_timer_t *time__get_clock_data_ev = acton_malloc(sizeof(uv_timer_t));
    uv_timer_init(aux_uv_loop, time__get_clock_data_ev);
    uv_timer_start(time__get_clock_data_ev, time__get_clock_data_cb, 10000, 10000);
    time__get_clock_data_cb(NULL);
}

int64_t timeQ_U__get_incarnation () {
    return time__incarnation;
}


B_tuple timeQ_get_monotonic () {
    uv_timespec64_t ts;
    if (uv_clock_gettime(UV_CLOCK_MONOTONIC, &ts) == -1) {
        char errmsg[1024] = "Error getting time: ";
        uv_strerror_r(errno, errmsg + strlen(errmsg), sizeof(errmsg) - strlen(errmsg));
        log_warn("%s", errmsg);
        $RAISE(((B_BaseException)B_RuntimeErrorG_new(to$str(errmsg))));
    }
    return $NEWTUPLE(2,
                     toB_int(ts.tv_sec),
                     toB_int(ts.tv_nsec));
}

B_tuple timeQ_get_realtime () {
    uv_timespec64_t ts;
    if (uv_clock_gettime(UV_CLOCK_REALTIME, &ts) == -1) {
        char errmsg[1024] = "Error getting time: ";
        uv_strerror_r(errno, errmsg + strlen(errmsg), sizeof(errmsg) - strlen(errmsg));
        log_warn("%s", errmsg);
        $RAISE(((B_BaseException)B_RuntimeErrorG_new(to$str(errmsg))));
    }
    return $NEWTUPLE(2,
                     toB_int(ts.tv_sec),
                     toB_int(ts.tv_nsec));
}


B_tuple timeQ_get_clock_data () {
#ifdef _WIN32
    return $NEWTUPLE(3,
                     toB_int(time__realclock_status),
                     toB_int(0),
                     toB_int(0));
#else
    return $NEWTUPLE(3,
                     toB_int(time__realclock_status),
                     toB_int(time__realclock_tx.offset),
                     toB_int(time__realclock_tx.esterror));
#endif
}

B_tuple timeQ_U_1localtime (int64_t seconds) {
    time_t t = seconds;
    struct tm tm;
#ifdef _WIN32
    errno_t result = localtime_s(&tm, &t);
    if (result != 0) {
        char errmsg[1024] = "Error getting time: ";
        uv_strerror_r(errno, errmsg + strlen(errmsg), sizeof(errmsg) - strlen(errmsg));
        log_warn("%s", errmsg);
        $RAISE(((B_BaseException)B_RuntimeErrorG_new(to$str(errmsg))));
    }
    return $NEWTUPLE(11,
                     toB_int(tm.tm_year + 1900),
                     toB_int(tm.tm_mon + 1),
                     toB_int(tm.tm_mday),
                     toB_int(tm.tm_hour),
                     toB_int(tm.tm_min),
                     toB_int(tm.tm_sec),
                     toB_int(tm.tm_wday),
                     toB_int(tm.tm_yday),
                     toB_int(tm.tm_isdst),
                     to$str(""),
                     toB_int(0));
#else
    localtime_r(&t, &tm);
    return $NEWTUPLE(11,
                     toB_int(tm.tm_year + 1900),
                     toB_int(tm.tm_mon + 1),
                     toB_int(tm.tm_mday),
                     toB_int(tm.tm_hour),
                     toB_int(tm.tm_min),
                     toB_int(tm.tm_sec),
                     toB_int(tm.tm_wday),
                     toB_int(tm.tm_yday),
                     toB_int(tm.tm_isdst),
                     to$str(tm.tm_zone),
                     toB_int(tm.tm_gmtoff));
#endif
}

B_tuple timeQ_U_3gmtime (int64_t seconds) {
    time_t t = seconds;
    struct tm tm;
#ifdef _WIN32
    // TODO: implement this!
    return $NEWTUPLE(9,
                     toB_int(0),
                     toB_int(0),
                     toB_int(0),
                     toB_int(0),
                     toB_int(0),
                     toB_int(0),
                     toB_int(0),
                     toB_int(0),
                     toB_int(0));
#else
    gmtime_r(&t, &tm);
    return $NEWTUPLE(9,
                     toB_int(tm.tm_year + 1900),
                     toB_int(tm.tm_mon + 1),
                     toB_int(tm.tm_mday),
                     toB_int(tm.tm_hour),
                     toB_int(tm.tm_min),
                     toB_int(tm.tm_sec),
                     toB_int(tm.tm_wday),
                     toB_int(tm.tm_yday),
                     toB_int(tm.tm_isdst));
#endif
}
