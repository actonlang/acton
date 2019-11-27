#include "unity.h"

#include "kernelops.h"


void setUp() {
    kernelops_INIT();
}

void tearDown() {
    kernelops_CLOSE();
}

void test_ACTOR() {
    Actor a = ACTOR(2);
    TEST_ASSERT_EQUAL(a->next, NULL);
    TEST_ASSERT_NOT_EQUAL(a->state, NULL);
    TEST_ASSERT_EQUAL(a->msgQ, NULL);
    TEST_ASSERT_EQUAL(a->msgTail, NULL);
}

void test_CLOS() {
    Clos c = CLOS((code_t)1, 2);
    TEST_ASSERT_EQUAL(c->code, 1);
    TEST_ASSERT_NOT_EQUAL(c->var, NULL);
}

void test_MSG() {
    Msg m = MSG((Actor)1, (Clos)2);
    TEST_ASSERT_EQUAL(m->to, 1);
    TEST_ASSERT_EQUAL(m->next, NULL);
    TEST_ASSERT_EQUAL(m->waiting, NULL);
    TEST_ASSERT_EQUAL(m->clos, 2);
    TEST_ASSERT_EQUAL(m->time_baseline, 0);
}
