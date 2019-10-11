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

LIBDIR=../../lib

all: skiplist_test libdb db_unit_tests queue_unit_tests txn_unit_tests



vector_clock.o:
	cc vector_clock.c -c -I src $(LFLAGS) $(CFLAGS)

db_messages.pb-c.c: db_messages.proto
	protoc-c --c_out=. db_messages.proto

db_messages.pb-c.o: db_messages.pb-c.c
	cc $(LFLAGS) $(CFLAGS) -I/usr/local/include -Wno-unused-value db_messages.pb-c.cå

libvc: vector_clock.o db_messages.pb-c.o
	ar rcs $(LIBDIR)/libvc.a vector_clock.o db_messages.pb-c.o
	ranlib $(LIBDIR)/libvc.a

db_messages_test: db_messages_test.c
	cc db_messages_test.c \
		$(CFLAGS) -I ../src -I/usr/local/include -L ../lib \
		-lvc $(LFLAGS) -L/usr/local/lib -pthread -lprotobuf-c \
		$(LIBS) \
		$(CFLAGS) \
		-o queue_unit_tests

	cc -pthread $(LFLAGS) $(CFLAGS) -I/usr/local/include -Wno-unused-value vector_clock. fd.c cells.c db_queries.c db_messages_test.c db_messages.pb-c.c -L/usr/local/lib -lprotobuf-c -o db_messages_test

$(LIBDIR):
	mkdir -p $(LIBDIR)

clean:
	rm -rf *.o $(LIBDIR)/libvc.a db_messages_test
