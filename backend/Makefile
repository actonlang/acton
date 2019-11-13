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

SUBDIRS = failure_detector

.PHONY: subdirs $(SUBDIRS)

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

all: skiplist_test libdb libcomm db_unit_tests queue_unit_tests txn_unit_tests failure_detector remote_txn_unit_tests client server

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

comm.o:
	cc comm.c -c -I src $(LFLAGS) $(CFLAGS)
	
client_api.o:
	cc client_api.c -c -I src $(LFLAGS) $(CFLAGS)

libdb: txns.o txn_state.o db.o queue.o vector_clock.o skiplist.o $(LIBDIR)
	ar rcs $(LIBDIR)/libdb.a db.o queue.o skiplist.o vector_clock.o txn_state.o txns.o
	ranlib $(LIBDIR)/libdb.a

libcomm: comm.o $(LIBDIR)
	ar rcs $(LIBDIR)/libcomm.a comm.o
	ranlib $(LIBDIR)/libcomm.a

libdbclient: client_api.o failure_detector $(LIBDIR)
	ar rcs $(LIBDIR)/libdbclient.a client_api.o
	ranlib $(LIBDIR)/libdbclient.a

db_unit_tests: db_unit_tests.c libdb
	cc -std=c11 db_unit_tests.c \
		$(CFLAGS) -I ../src -L $(LIBDIR) \
		-ldb $(LFLAGS) -pthread \
		$(LIBS) \
		-o db_unit_tests

queue_unit_tests: queue_unit_tests.c libdb
	cc queue_unit_tests.c \
		$(CFLAGS) -I ../src -L $(LIBDIR) \
		-ldb $(LFLAGS) -pthread \
		$(LIBS) \
		-o queue_unit_tests

txn_unit_tests: txn_unit_tests.c libdb
	cc txn_unit_tests.c \
		$(CFLAGS) -I ../src -L $(LIBDIR) \
		-ldb $(LFLAGS) -pthread \
		$(LIBS) \
		-o txn_unit_tests
		
skiplist_test: skiplist_test.c skiplist.c
	cc skiplist_test.c skiplist.c \
		$(CFLAGS) -I ../src \
		$(LFLAGS) \
		-o skiplist_test
		
client: client.c libdb libdbclient
	cc client.c \
	 $(CFLAGS) \
	 -L $(LIBDIR) -ldbclient -lremote -lcomm -ldb -lprotobuf-c -pthread -luuid \
	 -o client

server: server.c libdb failure_detector
	cc server.c \
	 $(CFLAGS) \
	 -L $(LIBDIR) -lremote -lcomm -ldb -lprotobuf-c -pthread -luuid \
	 -o server

remote_txn_unit_tests: remote_txn_unit_tests.c libdb libdbclient
	cc remote_txn_unit_tests.c \
	 $(CFLAGS) \
	 -L $(LIBDIR) -ldbclient -lremote -lcomm -ldb -lprotobuf-c -pthread -luuid \
	 -o remote_txn_unit_tests

$(LIBDIR):
	mkdir -p $(LIBDIR)

clean:
	rm -rf *.o $(LIBDIR) skiplist_test db_unit_tests queue_unit_tests txn_unit_tests remote_txn_unit_tests server client
