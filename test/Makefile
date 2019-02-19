# find all top-level directories prefixed with 'test_'
TESTDIRS := $(shell find * -maxdepth 1 -type d -name 'test_*')

.PHONY: $(TESTDIRS)
$(TESTDIRS):
	@for td in $(TESTDIRS); do \
	$(MAKE) -C $$td | sed -r "s#(Entering|Leaving) directory '$(PWD)/#\1 directory '#g"; \
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
	$(MAKE) -C $$td clean | sed -r "s#(Entering|Leaving) directory '$(PWD)/#\1 directory '#g"; \
	done
