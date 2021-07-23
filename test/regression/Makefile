# The general assumption for test cases are that they implement an actor called
# 'main' - this makes it possible to compile all of the test cases in the same
# way.
ACTONC=../../actonc

# Expand to all of the tests in this directory
TEST_SOURCES=$(shell ls *.act)
# rewrite to the binaries they will produce
TESTS=$(TEST_SOURCES:.act=)

# Here we mark tests as failing, if we expect them to fail. They can fail either
# during compilation using actonc or at run time.
FAILING_COMPILATION=if-return ifisnone 11-int-float 12-for-actor 20-for-break quotes-in-triquotes rtail triquotes-across-lines
FAILING_RUNNING=segfault subtract_off_by_one
ALL_FAILING=$(FAILING_COMPILATION) $(FAILING_RUNNING)

SUCCESS_TESTS=$(filter-out $(ALL_FAILING),$(TESTS))

test: $(TESTS)

$(SUCCESS_TESTS):
	$(ACTONC) $@.act --root main
	./$@

# We run the failing test cases too, to ensure that they do indeed fail. If a
# test case that is marked as failing actually succeeds, that is an error! It is
# easily fixed by simply removing it from the FAILING_COMPILATION or
# FAILING_RUNNING variable.
$(FAILING_COMPILATION):
	! $(ACTONC) $@.act --root main

$(FAILING_RUNNING):
	$(ACTONC) $@.act --root main
	! ./$@

13-print-actor-method:
	$(ACTONC) $@.act --root main
	@echo "We really should see 2 'pong' messages, but will see 1 due to bug #13"
	@echo "Since this is a negative test, we check for 1, this can later be changed to 2 when working"
	./$@
	./$@ | grep -c pong | grep 1

.PHONY: test test-examples $(TESTS)
