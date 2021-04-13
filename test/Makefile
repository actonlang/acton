ACTONC=../actonc
EXAMPLES=abs average files

test:
	$(MAKE) abs
	$(MAKE) average
	@echo TODO: fix below entries
	#$(MAKE) files
	#$(MAKE) ifisnone

test-examples: $(EXAMPLES)

$(EXAMPLES):
	$(ACTONC) ../examples/$@.act --root main
	../examples/$@

ifisnone:
	$(ACTONC) ifisnone.act

.PHONY: test test-examples
