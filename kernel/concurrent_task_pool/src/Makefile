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

CFLAGS:=$(JEM_FLAGS)
LIBS:=$(JEM_LIBS)

all: libtaskpool

libtaskpool: concurrent_task_pool_list.o concurrent_task_pool_tree.o
	ar rcs ../lib/libtaskpool.a concurrent_task_pool_list.o concurrent_task_pool_tree.o
	ranlib ../lib/libtaskpool.a

concurrent_task_pool_list.o:
	cc concurrent_task_pool_list.c -c -I src -latomic -lm -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast

concurrent_task_pool_tree.o:
	cc concurrent_task_pool_tree.c -c -I src -latomic -lm -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast

clean:
	rm -f *.o *.a
