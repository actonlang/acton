all: numpy.o

numpy.o: numpy.h numpy.c init.h init.c ndarray.h ndarray.c ndselect.h ndselect.c \
         primitive.h primitive.c protocol_impls.h protocol_impls.c quickselect.h quickselect.c
	cc -I.. -c numpy.c
	ar rc ../lib/libActon.a numpy.o
	cp numpy.h ../modules

clean:
	rm numpy.o
