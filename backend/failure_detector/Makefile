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

all: db_messages_test

vector_clock.o:
	cc vector_clock.c -c -I src $(LFLAGS) $(CFLAGS)

db_messages.pb-c.c: db_messages.proto
	protoc-c --c_out=. db_messages.proto

db_messages.pb-c.o: db_messages.pb-c.c
	cc db_messages.pb-c.c -c $(LFLAGS) $(CFLAGS)

cells.o: cells.c
	cc cells.c -c $(LFLAGS) $(CFLAGS)

db_queries.o: db_queries.c
	cc db_queries.c -c $(LFLAGS) $(CFLAGS)

fd.o: fd.c
	cc fd.c -c $(LFLAGS) $(CFLAGS)


libvc: vector_clock.o db_messages.pb-c.o $(LIBDIR)
	ar rcs $(LIBDIR)/libvc.a vector_clock.o
	ranlib $(LIBDIR)/libvc.a

libremote: db_messages.pb-c.o cells.o db_queries.o fd.o $(LIBDIR)
	ar rcs $(LIBDIR)/libremote.a db_messages.pb-c.o cells.o db_queries.o fd.o
	ranlib $(LIBDIR)/libremote.a

db_messages_test: db_messages_test.c libremote libvc
	cc db_messages_test.c \
		$(CFLAGS) -I ../src -I/usr/local/include -I/usr/include \
		-L $(LIBDIR) -L/usr/local/lib -L/usr/local/opt/util-linux/lib -lvc -lremote -pthread -lprotobuf-c $(LFLAGS) \
		$(LIBS) \
		-o db_messages_test

$(LIBDIR):
	mkdir -p $(LIBDIR)

clean:
	rm -rf *.o $(LIBDIR)/libvc.a $(LIBDIR)/libremote.a db_messages_test
