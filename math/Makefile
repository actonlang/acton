all: math.o

math.o: math.h math.c
	cc -I.. -c math.c
	ar rc ../lib/libActon.a math.o
	cp math.h ../modules

clean:
	rm math.o
