ACTONC=../actonc
EXAMPLES=abs average counter files sieve sumto overlap
TESTS=fail1 for-break if-return segfault quotes-in-triquotes triquotes-across-lines string-concat

DISABLED_TESTS=fail1 for-break if-return ifisnone quotes-in-triquotes triquotes-across-lines string-concat segfault

ALL_TESTS=$(EXAMPLES) $(TESTS)

RUN_TESTS=$(filter-out $(DISABLED_TESTS),$(ALL_TESTS))

test: $(RUN_TESTS)

test-examples: $(EXAMPLES)

$(EXAMPLES):
	$(ACTONC) ../examples/$@.act --root main
	../examples/$@

$(TESTS):
	$(ACTONC) $@.act --root main
	./$@

ifisnone:
	$(ACTONC) ifisnone.act

.PHONY: test test-examples $(TESTS) $(EXAMPLES)
