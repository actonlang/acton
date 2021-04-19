ifeq ($(shell uname -s),Linux)
CFLAGS += -I/usr/include/kqueue
endif

all: numpy.o

numpy.o: numpy.h numpy.c init.h init.c ndarray.h ndarray.c ndselect.h ndselect.c \
         primitive.h primitive.c protocol_impls.h protocol_impls.c quickselect.h quickselect.c
	cc $(CFLAGS) -I.. -O3 -c numpy.c
	ar rc ../lib/libActon.a numpy.o
	cp numpy.h ../modules

clean:
	rm -f numpy.o
