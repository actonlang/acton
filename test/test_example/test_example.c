#include "unity.h"


void setUp() {
    printf(">>>  setUp()\n");
}

void tearDown() {
    printf("<<<  tearDown()\n");
}

void test_Test1() {
    TEST_ASSERT_TRUE(1);
}

void test_WillFail() {
    TEST_ASSERT_TRUE(0);
}
