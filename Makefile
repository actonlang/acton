DEBUG ?= 0
# jemalloc is faster so let's use it by default (if it's found)
USE_JEM ?= 1

CC := cc
CXX := c++

CFLAGS := -std=c11 -Wall -Werror -pedantic -pedantic-errors
CXXFLAGS := -std=c++11 -Wall -Werror -pedantic -pedantic-errors
LDFLAGS :=

ifeq ($(DEBUG), 1)
	CFLAGS += -g -DDEBUG
	CXXFLAGS += -g -DDEBUG
else
	CFLAGS += -O3 -DNDEBUG
	CXXFLAGS += -O3 -DNDEBUG
endif

ifeq ($(shell $(CC) -v 2>&1 | grep -c "clang version"), 1)
LDFLAGS += -fuse-ld=bfd
endif


# there's no pkg-config ??
JEM_LIB?=$(wildcard /usr/lib/x86_64-linux-gnu/libjemalloc.a)

ifeq ($(USE_JEM), 1)
ifeq ($(JEM_LIB),)
$(error jemalloc library not in assumed location, use JEM_LIB=<file location>)
endif
$(info Using jemalloc: $(JEM_LIB))
CFLAGS += -DUSE_JEMALLOC
LDFLAGS += $(JEM_LIB)
endif

# for e.g. clock_gettime
CFLAGS += -D_XOPEN_SOURCE=600

PQUEUE_LIB := deps/libpqueue/libpqueue.a

all: kernel test_io

kernel: Makefile deps/libpqueue/libpqueue.a kernelops.c kernelops.h kernel.c pingpong.c pingpong2.c
	cc -std=c11 \
		-Wall -Werror -pedantic -pedantic-errors \
		-Wno-int-to-void-pointer-cast \
		-Wno-int-to-pointer-cast \
		-Wno-pointer-to-int-cast \
		-Wno-error=int-to-pointer-cast \
		-Wno-unknown-pragmas \
		-I deps/libpqueue/src \
		$(CFLAGS) \
		kernelops.c \
		kernel.c \
		$(PQUEUE_LIB) \
		$(LDFLAGS) \
		-pthread \
		-lpthread \
		-lm \
		-o kernel


$(PQUEUE_LIB):
	@make -C deps/libpqueue

hashtable_impl.o: hashtable_impl.cc
	$(CXX) $(CXXFLAGS) -Wno-pessimizing-move -I deps/libcuckoo -c $^

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $^

test_io: test_io.o hashtable_impl.o
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -pthread -o $@ $^

.PHONY: test
test:
	@make -C test

clean:
	rm -f *.a *.o
	rm -f test_io
	rm -f kernel


.PHONY: help
help:
	@echo "Targets:"
	@echo "  all         Same as `kernel`"
	@echo "  kernel      Build the kernel (test program)"
	@echo "  test_io     Testing program of IO polling"
	@echo "  clean       Do some cleaning"
	@echo "Options:"
	@echo "  USE_JEM=1   Use jemalloc (default: on)"
	@echo "              requires a jemalloc .so or .a file (use JEM_LIB=...)"
