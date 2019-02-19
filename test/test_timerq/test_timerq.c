#include "unity.h"

#include "kernelops.h"

void setUp() {
    kernelops_INIT(false);
}

void tearDown() {
    kernelops_CLOSE();
}

void test_InsertAndPollOne() {
    monotonic_time t0 = 0;
    Msg m = (void *)1;   // we're not using it
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
    Msg m = (void *)1;   // we're not using it
    timer_INSERT(t0 + 1, m);

    // poll with an earlier time
    TimedMsg ptm = timer_POLL(t0);
    TEST_ASSERT_EQUAL(ptm, 0);
}

void test_InsertAndPollMultiple() {
    monotonic_time t0 = 0;
    Msg m0 = (void *)1;   // we're not using it
    TimedMsg itm0 = timer_INSERT(t0, m0);
    Msg m1 = (void *)2;   // we're not using it
    TimedMsg itm1 = timer_INSERT(t0 + 1, m1);
    Msg m2 = (void *)3;   // we're not using it
    TimedMsg itm2 = timer_INSERT(t0 + 2, m2);
    
    TEST_ASSERT_EQUAL(itm0->trigger_time, t0);
    TEST_ASSERT_EQUAL(itm0->m, m0);
    TEST_ASSERT_EQUAL(itm1->trigger_time, t0 + 1);
    TEST_ASSERT_EQUAL(itm1->m, m1);
    TEST_ASSERT_EQUAL(itm2->trigger_time, t0 + 2);
    TEST_ASSERT_EQUAL(itm2->m, m2);


    TimedMsg ptm0 = timer_POLL(t0 + 3);
    TEST_ASSERT_EQUAL(ptm0, itm0);
    TEST_ASSERT_EQUAL(ptm0->trigger_time, t0);
    TEST_ASSERT_EQUAL(ptm0->m, m0);

    TimedMsg ptm1 = timer_POLL(t0 + 3);
    TEST_ASSERT_EQUAL(ptm1, itm1);
    TEST_ASSERT_EQUAL(ptm1->trigger_time, t0 + 1);
    TEST_ASSERT_EQUAL(ptm1->m, m1);

    TimedMsg ptm2 = timer_POLL(t0 + 3);
    TEST_ASSERT_EQUAL(ptm2, itm2);
    TEST_ASSERT_EQUAL(ptm2->trigger_time, t0 + 2);
    TEST_ASSERT_EQUAL(ptm2->m, m2);
}

void test_InsertReversedAndPollMultiple() {
    monotonic_time t0 = 0;
    Msg m2 = (void *)3;   // we're not using it
    TimedMsg itm2 = timer_INSERT(t0 + 2, m2);
    Msg m1 = (void *)2;   // we're not using it
    TimedMsg itm1 = timer_INSERT(t0 + 1, m1);
    Msg m0 = (void *)1;   // we're not using it
    TimedMsg itm0 = timer_INSERT(t0, m0);
    
    TEST_ASSERT_EQUAL(itm0->trigger_time, t0);
    TEST_ASSERT_EQUAL(itm0->m, m0);
    TEST_ASSERT_EQUAL(itm1->trigger_time, t0 + 1);
    TEST_ASSERT_EQUAL(itm1->m, m1);
    TEST_ASSERT_EQUAL(itm2->trigger_time, t0 + 2);
    TEST_ASSERT_EQUAL(itm2->m, m2);

    TimedMsg ptm0 = timer_POLL(t0 + 3);
    TEST_ASSERT_EQUAL(ptm0, itm0);
    TEST_ASSERT_EQUAL(ptm0->trigger_time, t0);
    TEST_ASSERT_EQUAL(ptm0->m, m0);

    TimedMsg ptm1 = timer_POLL(t0 + 3);
    TEST_ASSERT_EQUAL(ptm1, itm1);
    TEST_ASSERT_EQUAL(ptm1->trigger_time, t0 + 1);
    TEST_ASSERT_EQUAL(ptm1->m, m1);

    TimedMsg ptm2 = timer_POLL(t0 + 3);
    TEST_ASSERT_EQUAL(ptm2, itm2);
    TEST_ASSERT_EQUAL(ptm2->trigger_time, t0 + 2);
    TEST_ASSERT_EQUAL(ptm2->m, m2);
}
