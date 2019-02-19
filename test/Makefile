# find all top-level directories prefixed with 'test_'
TESTDIRS := $(shell find * -maxdepth 1 -type d -name 'test_*')

.PHONY: $(TESTDIRS)
$(TESTDIRS):
	@$(MAKE) -C $@ | sed -r "s#(Entering|Leaving) directory '$(PWD)/#\1 directory '#g"

.PHONY: list
list:
ifeq ($(words $(TESTDIRS)),1)
	@echo "Found 1 test:"
else
	@echo "Found $(words $(TESTDIRS)) tests:"
endif
	$(foreach td, $(TESTDIRS), @echo "    $(td)")

.PHONY: clean
clean:
	$(foreach td, $(TESTDIRS), @make -C $(td) clean | sed -r "s#(Entering|Leaving) directory '$(PWD)/#\1 directory '#g")
