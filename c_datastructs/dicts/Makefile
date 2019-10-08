all:
	gcc -O3 -I../include -c dict.c
	gcc -O3 -I../include ../hash/hash.o ../iterator/iterator.o dict.o dict_test.c

clean:
	rm *.o a.out
