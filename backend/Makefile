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

CFLAGS:=$(JEM_FLAGS) -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast -g -O0
LIBS:=$(JEM_LIBS)

LFLAGS=#-latomic -lm

LIBDIR=../lib

all: libdb db_unit_tests

libdb: db.o skiplist.o $(LIBDIR)
	ar rcs $(LIBDIR)/libdb.a db.o skiplist.o
	ranlib $(LIBDIR)/libdb.a

db.o:
	cc db.c -c -I src $(LFLAGS) $(CFLAGS)

skiplist.o:
	cc skiplist.c -c -I src $(LFLAGS) $(CFLAGS)

db_unit_tests: db_unit_tests.c
	cc -std=c11 db_unit_tests.c \
		$(CFLAGS) -I ../src -L ../lib \
		-ldb $(LFLAGS) -pthread \
		$(LIBS) \
		$(CFLAGS) \
		-o db_unit_tests

$(LIBDIR):
	mkdir -p $(LIBDIR)

clean:
	rm -rf *.o *.a $(LIBDIR)

