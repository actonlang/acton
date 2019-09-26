# jemalloc is faster so let's use it by default
JEM_LIBS:=
JEM_FLAGS:=

# there's no pkg-config ??
JEM_LIB?=$(wildcard /usr/lib/x86_64-linux-gnu/libjemalloc.a)

ifeq ($(JEM_LIB),)
$(info ** Not using jemalloc)
else
$(info Using jemalloc: $(JEM_LIB))
JEM_FLAGS:=-DUSE_JEMALLOC
JEM_LIBS:=$(JEM_LIB)
endif

LDFS_FLAGS:=-I ~/liblfds7.1.1/liblfds711/inc -L ~/liblfds7.1.1/liblfds711/bin
LDFS_LIBS:=-llfds711

CFLAGS:=$(JEM_FLAGS) $(LDFS_FLAGS)
LIBS:=$(JEM_LIBS) $(LDFS_LIBS)

all: concurrent_task_pool_test_multithreaded concurrent_task_pool_test_multithreaded_mixed

concurrent_task_pool_test_multithreaded: concurrent_task_pool_test_multithreaded.c
	cc -std=c11 concurrent_task_pool_test_multithreaded.c -O3 \
		$(CFLAGS) -I ../src -L ../lib \
		-ltaskpool -latomic -pthread -lm \
		$(LIBS) \
		-Wno-int-to-pointer-cast -Wno-pointer-to-int-cast \
		-o concurrent_task_pool_test_multithreaded

concurrent_task_pool_test_multithreaded_mixed: concurrent_task_pool_test_multithreaded_mixed.c
	cc -std=c11 concurrent_task_pool_test_multithreaded_mixed.c -O3 \
		$(CFLAGS) -I ../src -L ../lib \
		-ltaskpool -latomic -pthread -lm \
		$(LIBS) \
		-Wno-int-to-pointer-cast -Wno-pointer-to-int-cast \
		-o concurrent_task_pool_test_multithreaded_mixed
clean:
	rm -f concurrent_task_pool_test_multithreaded concurrent_task_pool_test_multithreaded_mixed
