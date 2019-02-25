# find all top-level directories prefixed with 'test_'  (exclude the example test)
TESTDIRS := $(shell find * -prune -type d -name 'test_*' ! -name 'test_example' )

.PHONY: $(TESTDIRS)
$(TESTDIRS):
	@for td in $(TESTDIRS); do \
	$(MAKE) -C $$td | ./testfilter "$(PWD)"; \
	done


.PHONY: list
list:
ifeq ($(words $(TESTDIRS)),1)
	@echo "Found 1 test:"
else
	@echo "Found $(words $(TESTDIRS)) tests:"
endif
	@for td in $(TESTDIRS); do \
	echo "    $$td"; \
	done

.PHONY: clean
clean:
	@for td in $(TESTDIRS); do \
	$(MAKE) -C $$td clean | ./testfilter "$(PWD)"; \
	done
