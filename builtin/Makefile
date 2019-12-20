all:
	gcc slice.c list.c protocols.c slice_test.c -o slice_test
	gcc slice.o list.o protocols.o sieve.c -o sieve
	gcc slice.o list.o protocols.o protocol_test.c -o protocol_test

clean:
	rm *.o slice_test sieve protocol_test
