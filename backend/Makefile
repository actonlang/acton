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
	$(CC) -o$@ $< -c $(CFLAGS) $(LFLAGS)

failure_detector/%.o: failure_detector/%.c
	$(CC) -o$@ $< -c $(CFLAGS) $(LFLAGS)

failure_detector/db_messages.pb-c.c: failure_detector/db_messages.proto
	protoc-c --c_out=$@ $<

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

client: client.c $(LIBDIR)/libdb.a $(LIBDIR)/libdbclient.a $(LIBDIR)/libremote.a $(LIBDIR)/libcomm.a $(LIBDIR)/libvc.a
	$(CC) -o$@ $< $(CFLAGS) $(LARGP) \
		-L $(LIBDIR) -L/usr/local/opt/util-linux/lib \
		-ldbclient -lremote -lcomm -ldb -lvc -lprotobuf-c -pthread -luuid

server: server.c failure_detector $(LIBDIR)/libremote.a $(LIBDIR)/libcomm.a $(LIBDIR)/libdb.a $(LIBDIR)/libvc.a
	$(CC) -o$@ $< $(CFLAGS) $(LARGP) \
		-L $(LIBDIR) -L/usr/local/opt/util-linux/lib \
		-lremote -lcomm -ldb -lvc -lprotobuf-c -pthread -luuid

# tests
TESTS=failure_detector/db_messages_test actor_ring_tests_local actor_ring_tests_remote db_unit_tests queue_unit_tests skiplist_test
build-tests: $(TESTS)

test: $(TESTS)
	@echo DISABLED TEST: failure_detector/db_messages_test
	./actor_ring_tests_local
	./actor_ring_tests_remote
	./db_unit_tests
	@echo DISABLED test: ./queue_unit_tests
	./skiplist_test

failure_detector/db_messages_test: failure_detector/db_messages_test.c $(LIBDIR)/libremote.a $(LIBDIR)/libvc.a
	$(CC) -o$@ $< $(CFLAGS) \
		-I/usr/local/include -I/usr/include \
		-L $(LIBDIR) -L/usr/local/lib -L/usr/local/opt/util-linux/lib \
		-lvc -lremote -pthread -lprotobuf-c $(LFLAGS) \
		$(LIBS)

actor_ring_tests_local: actor_ring_tests_local.c $(LIBDIR)/libdb.a
	$(CC) -o$@ $< $(CFLAGS) \
		-I ../src -L $(LIBDIR) -L/usr/local/opt/util-linux/lib \
		-ldb -lvc -lremote -lprotobuf-c $(LFLAGS) -pthread \
		$(LIBS)

actor_ring_tests_remote: actor_ring_tests_remote.c $(LIBDIR)/libdb.a $(LIBDIR)/libdbclient.a failure_detector
	$(CC) -o$@ $< $(CFLAGS) \
		-L $(LIBDIR) -L/usr/local/opt/util-linux/lib \
		-ldbclient -lremote -lcomm -ldb -lvc -lprotobuf-c -pthread -luuid

db_unit_tests: db_unit_tests.c $(LIBDIR)/libdb.a
	$(CC) -o$@ $< -std=c11 $(CFLAGS) \
		-I ../src -L $(LIBDIR) -L/usr/local/opt/util-linux/lib \
		-ldb -lvc $(LFLAGS) -pthread \
		$(LIBS)

queue_unit_tests: queue_unit_tests.c $(LIBDIR)/libdb.a
	$(CC) -o$@ $< $(CFLAGS) \
		-I ../src -L $(LIBDIR) -L/usr/local/opt/util-linux/lib \
		-ldb -lvc -lremote -lprotobuf-c $(LFLAGS) -pthread

skiplist_test: skiplist_test.c skiplist.c
	$(CC) -o$@ $^ $(CFLAGS) \
		-I ../src \
		-L/usr/local/opt/util-linux/lib $(LFLAGS)

clean:
	rm -rf *.o failure_detector/*.o failure_detector/*.a $(LIBDIR)/libcomm.a $(LIBDIR)/libdb.a $(LIBDIR)/libdbclient.a $(LIBDIR)/libremote.a $(LIBDIR)/libvc.a $(TESTS) server client failure_detector/db_messages_test
