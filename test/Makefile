ACTONC=../actonc
EXAMPLES=abs average counter files sieve sumto worker

test:
	$(MAKE) abs
	$(MAKE) average
	$(MAKE) counter
	$(MAKE) sieve
	$(MAKE) sumto
	@echo TODO: fix below entries
	#$(MAKE) files  # some compile time error
	#$(MAKE) worker # this never exist, shouldn't acton exit when the root actor exits?
	#$(MAKE) ifisnone

test-examples: $(EXAMPLES)

$(EXAMPLES):
	$(ACTONC) ../examples/$@.act --root main
	../examples/$@

ifisnone:
	$(ACTONC) ifisnone.act

.PHONY: test test-examples
