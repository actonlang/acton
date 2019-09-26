#include "unity.h"

#include "kernelops.h"

void setUp() {
    kernelops_INIT(false);
}

void tearDown() {
    kernelops_CLOSE();
}

void test_PollEmpty() {
    TimedMsg ptm = timer_POLL(10);   // any time, really
    TEST_ASSERT_EQUAL(ptm, 0);
}

void test_InsertAndPollOne() {
    monotonic_time t0 = 0;
    Msg m = (void *)1;   // we're not using it, just need a "unique" value
    TimedMsg itm = timer_INSERT(t0, m);
    
    TEST_ASSERT_EQUAL(itm->trigger_time, t0);
    TEST_ASSERT_EQUAL(itm->m, m);

    TimedMsg ptm = timer_POLL(t0 + 1);
    TEST_ASSERT_EQUAL(ptm, itm);
    TEST_ASSERT_EQUAL(ptm->trigger_time, t0);
    TEST_ASSERT_EQUAL(ptm->m, m);
}

void test_InsertAndPollFail() {
    monotonic_time t0 = 0;
    Msg m = (void *)1;
    timer_INSERT(t0 + 1, m);

    // poll with an earlier time
    TimedMsg ptm = timer_POLL(t0);
    TEST_ASSERT_EQUAL(ptm, 0);
}

void test_InsertAndPollMultiple() {
    Msg m[3];
    TimedMsg itm[3];
    for(int idx = 0; idx < 3; idx++) {
        m[idx] = (void *)idx;
        monotonic_time tt = idx;
        itm[idx] = timer_INSERT(tt, m[idx]);
        TEST_ASSERT_EQUAL(itm[idx]->trigger_time, tt);
        TEST_ASSERT_EQUAL(itm[idx]->m, m[idx]);
    }

    // expect them in the "same" order (by increasing time)
    for(int idx = 0; idx < 3; idx++) {
        TimedMsg ptm = timer_POLL(10);
        TEST_ASSERT_EQUAL(ptm, itm[idx]);
        TEST_ASSERT_EQUAL(ptm->trigger_time, idx);  // increasing time
        TEST_ASSERT_EQUAL(ptm->m, m[idx]);
    }
}

void test_InsertReversedAndPollMultiple() {
    Msg m[3];
    TimedMsg itm[3];
    for(int idx = 0; idx < 3; idx++) {
        m[idx] = (void *)idx;
        monotonic_time tt = 2 - idx;
        itm[idx] = timer_INSERT(tt, m[idx]);
        TEST_ASSERT_EQUAL(itm[idx]->trigger_time, tt);
        TEST_ASSERT_EQUAL(itm[idx]->m, m[idx]);
    }

    // expect them in "reverse" order (by increasing time)
    for(int idx = 0; idx < 3; idx++) {
        TimedMsg ptm = timer_POLL(10);
        TEST_ASSERT_EQUAL(ptm, itm[2 - idx]);
        TEST_ASSERT_EQUAL(ptm->trigger_time, idx);   // increasing time
        TEST_ASSERT_EQUAL(ptm->m, m[2 - idx]);
    }
}

void test_InsertOneAndPollEarly() {
    monotonic_time tt = 100;  // a later time
    Msg m = (void *)1;
    timer_INSERT(tt, m);

    TimedMsg tm = timer_POLL(5);   // earlier than 100
    TEST_ASSERT_EQUAL(tm, 0);
}
