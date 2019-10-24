all:
	gcc -O3 -I../include -c set.c
	gcc -O3 -I../include ../hash/hash.o ../iterator/iterator.o set.o set_test.c

clean:
	rm *.o a.out
