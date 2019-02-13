DEBUG ?= 0

CFLAGS := -std=c11 -Wall -Werror -pedantic -pedantic-errors
CXXFLAGS := -std=c++11 -Wall -Werror -pedantic -pedantic-errors
LIBS :=

ifeq ($(DEBUG), 1)
	CFLAGS += -g -DDEBUG
	CXXFLAGS += -g -DDEBUG
else
	CFLAGS += -DNDEBUG
	CXXFLAGS += -DNDEBUG
endif

# jemalloc is faster so let's use it by default
# there's no pkg-config ??
JEM_LIB?=$(wildcard /usr/lib/x86_64-linux-gnu/libjemalloc.a)
ifeq ($(JEM_LIB),)
$(info ** Not using jemalloc)
else
$(info Using jemalloc: $(JEM_LIB))
CFLAGS := -DUSE_JEMALLOC
LIBS += $(JEM_LIB)
endif

CFLAGS += $(LDFS_FLAGS)
LIBS += $(LDFS_LIBS)

all: kernel test_io

kernel: kernelops.c kernelops.h kernel.c pingpong.c pingpong2.c
	$(CC) $(CFLAGS) \
		-O3 \
		-Wno-int-to-void-pointer-cast \
		-Wno-unknown-pragmas \
		-Iliblfds7.1.1/liblfds711/inc \
		kernelops.c \
		kernel.c \
		liblfds7.1.1/liblfds711/bin/liblfds711.a \
		$(LIBS) \
		-pthread \
		-lpthread \
		-lm \
		-o kernel

hashtable.o: hashtable.cc hashtable.h
	$(CXX) $(CXXFLAGS) -I deps/libcuckoo -c hashtable.cc

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $^

test_io: test_io.o hashtable.o
	$(CXX) $(CXXFLAGS) -pthread -o $@ $^

clean:
	rm -f *.a *.o
	rm -f test_io
	rm -f kernel
