# The general assumption for test cases are that they implement an actor called
# 'main' - this makes it possible to compile all of the test cases in the same
# way.
ACTONC=../../dist/actonc

# Expand to all of the tests in this directory
TEST_SOURCES=$(shell ls *.act)
# rewrite to the binaries they will produce
TESTS=$(TEST_SOURCES:.act=)

# Here we mark tests as failing, if we expect them to fail. They can fail either
# during compilation using actonc or at run time.
FAILING_COMPILATION= \
	11-int-float \
	12-for-actor \
	20-for-break \
	174-variable-not-in-scope \
	176-cannot-unify-atom-and-int \
	if-return \
	quotes-in-triquotes \
	triquotes-across-lines
FAILING_RUNNING= \
	125-async-actor-method-call \
	segfault \
	subtract_off_by_one
ALL_FAILING=$(FAILING_COMPILATION) $(FAILING_RUNNING)

SUCCESS_TESTS=$(filter-out $(ALL_FAILING),$(TESTS))

test: $(TESTS)

$(SUCCESS_TESTS):
	@echo ""
	@echo "== Running test: $@"
	$(ACTONC) $@.act --root main
	./$@

# We run the failing test cases too, to ensure that they do indeed fail. If a
# test case that is marked as failing actually succeeds, that is an error! It is
# easily fixed by simply removing it from the FAILING_COMPILATION or
# FAILING_RUNNING variable.
$(FAILING_COMPILATION):
	@echo ""
	@echo "== Running negative test (expecting compilation failure): $@"
	! $(ACTONC) $@.act --root main >/dev/null 2>&1

$(FAILING_RUNNING):
	@echo ""
	@echo "== Running negative test (expecting run time failure): $@"
	$(ACTONC) $@.act --root main >/dev/null 2>&1
	! ./$@ >/dev/null 2>&1

105-type-inference-order:
	@echo "Test '105-type-inference-order' is temporarily disabled"

13-print-actor-method:
	$(ACTONC) $@.act --root main
	@echo "Now we really see 2 'pong' messages"
	./$@
	./$@ | grep -c pong | grep 2

.PHONY: test test-examples $(TESTS)
