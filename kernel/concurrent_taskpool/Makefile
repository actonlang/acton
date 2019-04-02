
SUBDIRS := src tests_and_benchmarks 

all: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

.PHONY: all $(SUBDIRS)

clean:
	$(MAKE) -C src clean
	$(MAKE) -C tests_and_benchmarks clean
