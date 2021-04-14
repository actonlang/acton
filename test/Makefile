ACTONC=../actonc
EXAMPLES=abs average counter files sieve sumto worker
TESTS=fail1 segfault quotes-in-triquotes triquotes-across-lines string-concat

test:
	$(MAKE) abs
	$(MAKE) average
	$(MAKE) counter
	$(MAKE) sieve
	$(MAKE) sumto
	@echo TODO: fix below entries
	#$(MAKE) fail1
	#$(MAKE) files  # some compile time error
	#$(MAKE) quotes-in-triquotes
	#$(MAKE) triquotes-across-lines
	#$(MAKE) string-concat
	#$(MAKE) worker # this never exits, shouldn't acton exit when the root actor exits?
	#$(MAKE) ifisnone
	#$(MAKE) segfault

test-examples: $(EXAMPLES)

$(EXAMPLES):
	$(ACTONC) ../examples/$@.act --root main
	../examples/$@

$(TESTS):
	$(ACTONC) $@.act --root main
	./$@

ifisnone:
	$(ACTONC) ifisnone.act

.PHONY: test test-examples $(TESTS)
