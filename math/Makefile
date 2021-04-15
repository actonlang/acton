ifeq ($(shell uname -s),Linux)
CFLAGS += -I/usr/include/kqueue
endif

all: math.o

math.o: math.h math.c
	cc $(CFLAGS) -I.. -c math.c
	ar rc ../lib/libActon.a math.o
	cp math.h ../modules

clean:
	rm -f math.o
