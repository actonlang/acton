# look for jemalloc
JEM_LIB?=$(wildcard /usr/lib/x86_64-linux-gnu/libjemalloc.a)

ifeq ($(JEM_LIB),)
$(info ** Not using jemalloc)
JEM_LIBS:=
JEM_FLAGS:=
else
$(info Using jemalloc: $(JEM_LIB))
JEM_FLAGS:=-DUSE_JEMALLOC
JEM_LIBS:=$(JEM_LIB)
endif

CFLAGS:=$(JEM_FLAGS) -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast -g -O0
LIBS:=$(JEM_LIBS)

LFLAGS=-luuid -lm

LIBDIR=../lib

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
	LARGP=-largp
else
	LARGP=
endif

all: $(LIBDIR)/libcomm.a $(LIBDIR)/libdb.a $(LIBDIR)/libdbclient.a $(LIBDIR)/libremote.a $(LIBDIR)/libvc.a client server

%.o: %.c
	cc $^ -c $(LFLAGS) $(CFLAGS) -o $@

failure_detector/%.o: failure_detector/%.c
	cc $^ -c $(LFLAGS) $(CFLAGS) -o $@

failure_detector/db_messages.pb-c.c: failure_detector/db_messages.proto
	protoc-c --c_out=$@ $^

$(LIBDIR)/libcomm.a:
	$(MAKE) -C .. lib/libcomm.a

$(LIBDIR)/libdb.a:
	$(MAKE) -C .. lib/libdb.a

$(LIBDIR)/libdbclient.a:
	$(MAKE) -C .. lib/libdbclient.a

$(LIBDIR)/libremote.a:
	$(MAKE) -C .. lib/libremote.a

$(LIBDIR)/libvc.a:
	$(MAKE) -C .. lib/libvc.a

.PHONY: $(LIBDIR)/libcomm.a $(LIBDIR)/libdb.a $(LIBDIR)/libdbclient.a

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

# tests
TESTS=failure_detector/db_messages_test actor_ring_tests_local actor_ring_tests_remote db_unit_tests queue_unit_tests skiplist_test
build-tests: $(TESTS)

test: $(TESTS)
	@echo DISABLED TEST: failure_detector/db_messages_test
	./actor_ring_tests_local
	./actor_ring_tests_remote
	./db_unit_tests
	./queue_unit_tests
	./skiplist_test

failure_detector/db_messages_test: failure_detector/db_messages_test.c $(LIBDIR)/libremote.a $(LIBDIR)/libvc.a
	cc $^ \
		$(CFLAGS) -I/usr/local/include -I/usr/include \
		-L $(LIBDIR) -L/usr/local/lib -L/usr/local/opt/util-linux/lib -lvc -lremote -pthread -lprotobuf-c $(LFLAGS) \
		$(LIBS) \
		-o $@

actor_ring_tests_local: actor_ring_tests_local.c $(LIBDIR)/libdb.a
	cc actor_ring_tests_local.c \
		$(CFLAGS) -I ../src -L $(LIBDIR) -L/usr/local/opt/util-linux/lib \
		-ldb -lvc -lremote -lprotobuf-c $(LFLAGS) -pthread \
		$(LIBS) \
		-o actor_ring_tests_local

actor_ring_tests_remote: actor_ring_tests_remote.c $(LIBDIR)/libdb.a $(LIBDIR)/libdbclient.a failure_detector
	cc actor_ring_tests_remote.c \
	 $(CFLAGS) \
	 -L $(LIBDIR) -L/usr/local/opt/util-linux/lib -ldbclient -lremote -lcomm -ldb -lvc -lprotobuf-c -pthread -luuid \
	 -o actor_ring_tests_remote

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

skiplist_test: skiplist_test.c skiplist.c
	cc skiplist_test.c skiplist.c \
		$(CFLAGS) -I ../src \
		$(LFLAGS) -L/usr/local/opt/util-linux/lib \
		-o skiplist_test

clean:
	rm -rf *.o failure_detector/*.o failure_detector/*.a $(LIBDIR)/libcomm.a $(LIBDIR)/libdb.a $(LIBDIR)/libdbclient.a $(LIBDIR)/libremote.a $(LIBDIR)/libvc.a $(TESTS) server client failure_detector/db_messages_test
