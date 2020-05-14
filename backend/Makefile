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

all: failure_detector skiplist_test libdb libcomm db_unit_tests queue_unit_tests actor_ring_tests_local actor_ring_tests_remote client server

db.o:
	cc db.c -c -I src $(LFLAGS) $(CFLAGS)

queue.o:
	cc queue.c -c -I src $(LFLAGS) $(CFLAGS)

skiplist.o:
	cc skiplist.c -c -I src $(LFLAGS) $(CFLAGS)
	
# vector_clock.o:
# 	cc failure_detector/vector_clock.c -c -I src $(LFLAGS) $(CFLAGS)

txn_state.o:
	cc txn_state.c -c -I src $(LFLAGS) $(CFLAGS)

txns.o:
	cc txns.c -c -I src $(LFLAGS) $(CFLAGS)

comm.o:
	cc comm.c -c -I src $(LFLAGS) $(CFLAGS)
	
client_api.o:
	cc client_api.c -c -I src $(LFLAGS) $(CFLAGS)

libdb: txns.o txn_state.o db.o queue.o skiplist.o $(LIBDIR)
	ar rcs $(LIBDIR)/libdb.a db.o queue.o skiplist.o txn_state.o txns.o
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
		-ldb -lvc $(LFLAGS) -pthread \
		$(LIBS) \
		-o db_unit_tests

queue_unit_tests: queue_unit_tests.c libdb
	cc queue_unit_tests.c \
		$(CFLAGS) -I ../src -L $(LIBDIR) \
		-ldb -lvc -lremote -lprotobuf-c $(LFLAGS) -pthread \
		$(LIBS) \
		-o queue_unit_tests

actor_ring_tests_local: actor_ring_tests_local.c libdb
	cc actor_ring_tests_local.c \
		$(CFLAGS) -I ../src -L $(LIBDIR) \
		-ldb -lvc -lremote -lprotobuf-c $(LFLAGS) -pthread \
		$(LIBS) \
		-o actor_ring_tests_local
		
skiplist_test: skiplist_test.c skiplist.c
	cc skiplist_test.c skiplist.c \
		$(CFLAGS) -I ../src \
		$(LFLAGS) \
		-o skiplist_test
		
client: client.c libdb libdbclient
	cc client.c \
	 $(CFLAGS) \
	 -L $(LIBDIR) -ldbclient -lremote -lcomm -ldb -lvc -lprotobuf-c -pthread -luuid \
	 -o client

server: server.c libdb failure_detector
	cc server.c \
	 $(CFLAGS) \
	 -L $(LIBDIR) -lremote -lcomm -ldb -lvc -lprotobuf-c -pthread -luuid \
	 -o server

actor_ring_tests_remote: actor_ring_tests_remote.c libdb libdbclient failure_detector
	cc actor_ring_tests_remote.c \
	 $(CFLAGS) \
	 -L $(LIBDIR) -ldbclient -lremote -lcomm -ldb -lvc -lprotobuf-c -pthread -luuid \
	 -o actor_ring_tests_remote

$(LIBDIR):
	mkdir -p $(LIBDIR)

clean:
	rm -rf *.o failure_detector/*.o $(LIBDIR) skiplist_test db_unit_tests queue_unit_tests actor_ring_tests_local actor_ring_tests_remote server client failure_detector/db_messages_test
