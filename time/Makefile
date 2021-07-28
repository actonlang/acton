ifeq ($(shell uname -s),Linux)
CFLAGS += -I/usr/include/kqueue
endif

all: time.o

time.o: time.h time.c
	cc $(CFLAGS) -I.. -c time.c
	ar rc ../lib/libActon.a time.o
	cp time.h ../modules

clean:
	rm -f time.o
