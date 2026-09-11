#include <uv.h>

#include "rts/perf.h"

enum {
    TESTING_LOOP_CLAIMED = 1,
    TESTING_LOOP_STARTED = 2,
    TESTING_LOOP_EXHAUSTED = 4,
    TESTING_LOOP_CLOSED = 8,
    TESTING_LOOP_INVALID = 16,
    TESTING_LOOP_BUSY = 32
};

void testingQ___ext_init__() {}

static void testing_loop_invalid(testingQ_PerfLoop self, const char *message) {
    uint64_t state = __atomic_load_n(&self->_state, __ATOMIC_ACQUIRE);
    while (!(state & TESTING_LOOP_CLOSED) &&
           !__atomic_compare_exchange_n(&self->_state, &state,
                                        state | TESTING_LOOP_INVALID, false,
                                        __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE)) {}
    $RAISE((B_BaseException)B_ValueErrorG_new(to$str((char *)message)));
}

testingQ_PerfLoop testingQ_PerfLoopD_claim(testingQ_PerfLoop self) {
    uint64_t state = 0;
    if (!__atomic_compare_exchange_n(&self->_state, &state,
                                     TESTING_LOOP_CLAIMED, false,
                                     __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE))
        testing_loop_invalid(self, "Use t.loop() once per test invocation");
    return self;
}

static void testing_loop_start(testingQ_PerfLoop self) {
    if (self->_phase == 0)
        return;
    if (self->_phase != 3) {
        self->_start_wall_ns = uv_hrtime();
        return;
    }
    struct rts_perf_sample sample;
    rts_perf_read(&sample);
    self->_start_cpu_valid = sample.cpu_available;
    self->_start_hardware_valid = sample.hardware_available;
    self->_start_user_ns = sample.user_ns;
    self->_start_system_ns = sample.system_ns;
    self->_start_instructions = sample.instructions;
    self->_start_cycles = sample.cycles;
    self->_start_instructions_enabled = sample.instructions_enabled;
    self->_start_instructions_running = sample.instructions_running;
    self->_start_cycles_enabled = sample.cycles_enabled;
    self->_start_cycles_running = sample.cycles_running;
    self->_start_gc_ms = GC_get_full_gc_total_time();
    self->_start_alloc = GC_get_total_bytes();
    self->_start_wall_ns = uv_hrtime();
}

static bool testing_loop_covered(uint64_t start_enabled, uint64_t start_running,
                                 uint64_t enabled, uint64_t running) {
    return enabled >= start_enabled && running > start_running &&
           enabled - start_enabled == running - start_running;
}

static bool testing_loop_end(testingQ_PerfLoop self) {
    if (self->_phase == 0)
        return true;
    uint64_t wall_ns = uv_hrtime() - self->_start_wall_ns;
    self->_last_wall_ns = wall_ns;
    if (self->_phase != 3)
        return true;
    uint64_t allocated = GC_get_total_bytes();
    uint64_t gc_ms = GC_get_full_gc_total_time();
    struct rts_perf_sample sample;
    rts_perf_read(&sample);
    if (allocated < self->_start_alloc || gc_ms < self->_start_gc_ms)
        return false;
    self->_wall_ns += wall_ns;
    self->_gc_ns += (gc_ms - self->_start_gc_ms) * 1000000;
    self->_allocated_bytes += allocated - self->_start_alloc;
    if (self->_start_cpu_valid && sample.cpu_available &&
        sample.user_ns >= self->_start_user_ns &&
        sample.system_ns >= self->_start_system_ns) {
        self->_user_ns += sample.user_ns - self->_start_user_ns;
        self->_system_ns += sample.system_ns - self->_start_system_ns;
    } else {
        self->_cpu_valid = false;
    }
    if (self->_start_hardware_valid && sample.hardware_available &&
        sample.instructions >= self->_start_instructions &&
        sample.cycles >= self->_start_cycles &&
        testing_loop_covered(self->_start_instructions_enabled,
                             self->_start_instructions_running,
                             sample.instructions_enabled, sample.instructions_running) &&
        testing_loop_covered(self->_start_cycles_enabled, self->_start_cycles_running,
                             sample.cycles_enabled, sample.cycles_running)) {
        self->_instructions += sample.instructions - self->_start_instructions;
        self->_cycles += sample.cycles - self->_start_cycles;
    } else {
        self->_hardware_valid = false;
    }
    return true;
}

static void testing_loop_batch(testingQ_PerfLoop self, uint64_t completed) {
    if (self->_phase < 2 || self->_once) {
        self->_batch_size = 1;
        return;
    }
    uint64_t duration = self->_last_wall_ns ? self->_last_wall_ns : 1;
    uint64_t count = completed * 1000000 / duration;
    if (count < 1)
        count = 1;
    if (count > completed * 4)
        count = completed * 4;
    if (count > 1048576)
        count = 1048576;
    self->_batch_size = count;
}

B_int testingQ_PerfLoopD___next__(testingQ_PerfLoop self) {
    uint64_t state = __atomic_load_n(&self->_state, __ATOMIC_ACQUIRE);
    for (;;) {
        if (!(state & TESTING_LOOP_CLAIMED) ||
            (state & (TESTING_LOOP_CLOSED | TESTING_LOOP_INVALID | TESTING_LOOP_BUSY))) {
            testing_loop_invalid(self, "Cannot advance a closed or concurrent t.loop()");
            return NULL;
        }
        if (state & TESTING_LOOP_EXHAUSTED) {
            $RAISE((B_BaseException)B_StopIterationG_new(to$str("t.loop() exhausted")));
            return NULL;
        }
        if (__atomic_compare_exchange_n(&self->_state, &state,
                                         state | TESTING_LOOP_BUSY, false,
                                         __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE))
            break;
    }
    // Fast iterations reuse the scale box and do no timing or policy work.
    if ((state & TESTING_LOOP_STARTED) && self->_batch_left > 0) {
        self->_batch_left--;
        B_int scale = self->_yield_scale;
        __atomic_fetch_and(&self->_state, ~((uint64_t)TESTING_LOOP_BUSY), __ATOMIC_RELEASE);
        return scale;
    }

    uint64_t completed = (state & TESTING_LOOP_STARTED) ? self->_batch_size : 0;
    // $PUSH allocates its exception frame, so stop measurement before it.
    bool counters_valid = completed ? testing_loop_end(self) : true;
    B_int scale = NULL;
    if ($PUSH()) {
        if (!counters_valid)
            testing_loop_invalid(self, "Runtime counters decreased during t.loop()");
        if (completed)
            testing_loop_batch(self, completed);
        scale = testingQ_PerfLoopD__advance(self, (int64_t)completed);
        uint64_t current = __atomic_load_n(&self->_state, __ATOMIC_ACQUIRE);
        if (current & (TESTING_LOOP_CLOSED | TESTING_LOOP_INVALID))
            testing_loop_invalid(self, "t.loop() completed while being advanced");
        if (scale) {
            self->_yield_scale = scale;
            self->_batch_left = self->_batch_size - 1;
            testing_loop_start(self);
            __atomic_fetch_or(&self->_state, TESTING_LOOP_STARTED, __ATOMIC_RELEASE);
        } else {
            __atomic_fetch_or(&self->_state, TESTING_LOOP_EXHAUSTED, __ATOMIC_RELEASE);
        }
        $DROP();
    } else {
        B_BaseException exception = $POP();
        __atomic_fetch_or(&self->_state, TESTING_LOOP_INVALID, __ATOMIC_RELAXED);
        __atomic_fetch_and(&self->_state, ~((uint64_t)TESTING_LOOP_BUSY), __ATOMIC_RELEASE);
        $RAISE(exception);
        return NULL;
    }
    __atomic_fetch_and(&self->_state, ~((uint64_t)TESTING_LOOP_BUSY), __ATOMIC_RELEASE);
    if (!scale)
        $RAISE((B_BaseException)B_StopIterationG_new(to$str("t.loop() exhausted")));
    return scale;
}

B_NoneType testingQ_PerfLoopD_close(testingQ_PerfLoop self) {
    uint64_t state = __atomic_load_n(&self->_state, __ATOMIC_ACQUIRE);
    for (;;) {
        uint64_t closed = state | TESTING_LOOP_CLOSED;
        if (state & TESTING_LOOP_BUSY)
            closed |= TESTING_LOOP_INVALID;
        if (__atomic_compare_exchange_n(&self->_state, &state, closed, false,
                                         __ATOMIC_ACQ_REL, __ATOMIC_ACQUIRE))
            break;
    }
    // Only boundary work holds BUSY. Workload bodies never hold this gate.
    while (__atomic_load_n(&self->_state, __ATOMIC_ACQUIRE) & TESTING_LOOP_BUSY)
        uv_sleep(0);
    return B_None;
}

B_Msg testingQ_PerfLoopD__report(testingQ_PerfLoop self, B_bool success,
                                B_Exception exception, B_str output) {
    testingQ_PerfLoopD_close(self);
    $action report = self->_report_result;
    if (!report) {
        testing_loop_invalid(self, "No test result callback for this loop");
        return NULL;
    }
    return ((B_Msg (*)($action, B_bool, B_Exception, B_str))report->$class->__asyn__)
        (report, success, exception, output);
}

int64_t testingQ_PerfLoopD_status(testingQ_PerfLoop self) {
    uint64_t state = __atomic_load_n(&self->_state, __ATOMIC_ACQUIRE);
    if (state & TESTING_LOOP_INVALID)
        return 3;
    if (state & TESTING_LOOP_EXHAUSTED)
        return 2;
    if (state & TESTING_LOOP_CLAIMED)
        return 1;
    return 0;
}
