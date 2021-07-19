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

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
	LARGP=-largp
else
	LARGP=
endif

SUBDIRS = failure_detector

all: failure_detector skiplist_test $(LIBDIR)/libcomm.a $(LIBDIR)/libdb.a db_unit_tests queue_unit_tests actor_ring_tests_local actor_ring_tests_remote client server

.PHONY: subdirs $(SUBDIRS)

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@


db.o: db.c
	cc db.c -c -I src $(LFLAGS) $(CFLAGS)

queue.o: queue.c
	cc queue.c -c -I src $(LFLAGS) $(CFLAGS)

skiplist.o: skiplist.c
	cc skiplist.c -c -I src $(LFLAGS) $(CFLAGS)

# vector_clock.o:
# 	cc failure_detector/vector_clock.c -c -I src $(LFLAGS) $(CFLAGS)

txn_state.o: txn_state.c
	cc txn_state.c -c -I src $(LFLAGS) $(CFLAGS)

txns.o: txns.c
	cc txns.c -c -I src $(LFLAGS) $(CFLAGS)

comm.o: comm.c
	cc comm.c -c -I src $(LFLAGS) $(CFLAGS)

client_api.o:
	cc client_api.c -c -I src $(LFLAGS) $(CFLAGS)

$(LIBDIR)/libcomm.a:
	$(MAKE) -C .. lib/libcomm.a

$(LIBDIR)/libdb.a:
	$(MAKE) -C .. lib/libdb.a

$(LIBDIR)/libdbclient.a:
	$(MAKE) -C .. lib/libdbclient.a

.PHONY: $(LIBDIR)/libcomm.a $(LIBDIR)/libdb.a $(LIBDIR)/libdbclient.a

db_unit_tests: db_unit_tests.c $(LIBDIR)/libdb.a
	cc -std=c11 db_unit_tests.c \
		$(CFLAGS) -I ../src -L $(LIBDIR) -L/usr/local/opt/util-linux/lib \
		-ldb -lvc $(LFLAGS) -pthread \
		$(LIBS) \
		-o db_unit_tests

queue_unit_tests: queue_unit_tests.c $(LIBDIR)/libdb.a
	cc queue_unit_tests.c \
		$(CFLAGS) -I ../src -L $(LIBDIR) -L/usr/local/opt/util-linux/lib \
		-ldb -lvc -lremote -lprotobuf-c $(LFLAGS) -pthread \
		$(LIBS) \
		-o queue_unit_tests

actor_ring_tests_local: actor_ring_tests_local.c $(LIBDIR)/libdb.a
	cc actor_ring_tests_local.c \
		$(CFLAGS) -I ../src -L $(LIBDIR) -L/usr/local/opt/util-linux/lib \
		-ldb -lvc -lremote -lprotobuf-c $(LFLAGS) -pthread \
		$(LIBS) \
		-o actor_ring_tests_local

skiplist_test: skiplist_test.c skiplist.c
	cc skiplist_test.c skiplist.c \
		$(CFLAGS) -I ../src \
		$(LFLAGS) -L/usr/local/opt/util-linux/lib \
		-o skiplist_test

client: client.c $(LIBDIR)/libdb.a $(LIBDIR)/libdbclient.a
	cc client.c \
	 $(CFLAGS) $(LARGP) \
	 -L $(LIBDIR) -L/usr/local/opt/util-linux/lib -ldbclient -lremote -lcomm -ldb -lvc -lprotobuf-c -pthread -luuid \
	 -o client

server: server.c $(LIBDIR)/libdb.a failure_detector
	cc server.c \
	 $(CFLAGS) $(LARGP) \
	 -L $(LIBDIR) -L/usr/local/opt/util-linux/lib -lremote -lcomm -ldb -lvc -lprotobuf-c -pthread -luuid \
	 -o server

actor_ring_tests_remote: actor_ring_tests_remote.c $(LIBDIR)/libdb.a $(LIBDIR)/libdbclient.a failure_detector
	cc actor_ring_tests_remote.c \
	 $(CFLAGS) \
	 -L $(LIBDIR) -L/usr/local/opt/util-linux/lib -ldbclient -lremote -lcomm -ldb -lvc -lprotobuf-c -pthread -luuid \
	 -o actor_ring_tests_remote

clean:
	rm -rf *.o failure_detector/*.o $(LIBDIR)/libdb.a $(LIBDIR)/libcomm.a $(LIBDIR)/libdbclient.a skiplist_test db_unit_tests queue_unit_tests actor_ring_tests_local actor_ring_tests_remote server client failure_detector/db_messages_test
