all:
	gcc -c slice.c
	gcc -c list.c
	gcc -c protocols.c
	gcc -c str.c
	gcc slice.o list.o protocols.o slice_test.c -o slice_test
	gcc slice.o list.o protocols.o sieve.c -o sieve
	gcc slice.o list.o protocols.o protocol_test.c -o protocol_test
	gcc slice.o list.o protocols.o str.o str_test2.c -o str_test2 -lutf8proc

clean:
	rm *.o slice_test sieve protocol_test str_test2
