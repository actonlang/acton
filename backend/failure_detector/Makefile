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

LFLAGS=-luuid#-latomic -lm

LIBDIR=../lib

all: skiplist_test libdb db_unit_tests queue_unit_tests txn_unit_tests

libdb: txns.o txn_state.o db.o queue.o vector_clock.o skiplist.o $(LIBDIR)
	ar rcs $(LIBDIR)/libdb.a db.o queue.o skiplist.o vector_clock.o txn_state.o txns.o
	ranlib $(LIBDIR)/libdb.a

db.o:
	cc db.c -c -I src $(LFLAGS) $(CFLAGS)

queue.o:
	cc queue.c -c -I src $(LFLAGS) $(CFLAGS)

skiplist.o:
	cc skiplist.c -c -I src $(LFLAGS) $(CFLAGS)
	
vector_clock.o:
	cc failure_detector/vector_clock.c -c -I src $(LFLAGS) $(CFLAGS)

txn_state.o:
	cc txn_state.c -c -I src $(LFLAGS) $(CFLAGS)

txns.o:
	cc txns.c -c -I src $(LFLAGS) $(CFLAGS)


db_unit_tests: db_unit_tests.c
	cc -std=c11 db_unit_tests.c \
		$(CFLAGS) -I ../src -L ../lib \
		-ldb $(LFLAGS) -pthread \
		$(LIBS) \
		$(CFLAGS) \
		-o db_unit_tests

queue_unit_tests: queue_unit_tests.c
	cc queue_unit_tests.c \
		$(CFLAGS) -I ../src -L ../lib \
		-ldb $(LFLAGS) -pthread \
		$(LIBS) \
		$(CFLAGS) \
		-o queue_unit_tests

txn_unit_tests: txn_unit_tests.c
	cc txn_unit_tests.c \
		$(CFLAGS) -I ../src -L ../lib \
		-ldb $(LFLAGS) -pthread \
		$(LIBS) \
		$(CFLAGS) \
		-o txn_unit_tests
		
skiplist_test: skiplist_test.c skiplist.c
	cc skiplist_test.c skiplist.c \
		$(CFLAGS) -I ../src \
		$(LFLAGS) \
		$(CFLAGS) \
		-o skiplist_test

$(LIBDIR):
	mkdir -p $(LIBDIR)

clean:
	rm -rf *.o $(LIBDIR) skiplist_test db_unit_tests queue_unit_tests txn_unit_tests
