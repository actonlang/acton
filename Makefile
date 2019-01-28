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

CFLAGS:=$(JEM_FLAGS) $(LDFS_FLAGS)
LIBS:=$(JEM_LIBS) $(LDFS_LIBS)


all: kernel

kernel: kernelops.c kernelops.h kernel.c pingpong.c pingpong2.c
	cc -std=c11 -O3 \
		-Wall -Werror -pedantic -pedantic-errors \
		-Wno-int-to-void-pointer-cast \
		-Wno-unknown-pragmas \
		-Iliblfds7.1.1/liblfds711/inc \
		$(CFLAGS) \
		kernelops.c \
		kernel.c \
		liblfds7.1.1/liblfds711/bin/liblfds711.a \
		$(LIBS) \
		-pthread \
		-lpthread \
		-lm \
		-o kernel

clean:
	rm -f kernel
