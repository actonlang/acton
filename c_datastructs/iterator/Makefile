all:
	gcc -O3 -I../include -c iterator.c
	gcc -O3 -I../include ../lists/list.o iterator.o ../slices/slice.o iterator_test.c

clean:
	rm *.o a.out
