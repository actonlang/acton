all:
	gcc ../builtin.o slice_test.c -o slice_test -lutf8proc
	gcc ../builtin.o sieve.c -o sieve -lutf8proc
	gcc ../builtin.o protocol_test.c -o protocol_test  -lutf8proc
	gcc ../builtin.o str_test2.c -o str_test2 -lutf8proc
	gcc ../builtin.o hash_test.c -o hash_test -lutf8proc
	gcc ../builtin.o container_test.c -o container_test -lutf8proc
	gcc ../builtin.o dict_test2.c -o dict_test2 -lutf8proc
	gcc ../builtin.o set_test.c -o set_test -lutf8proc
	gcc ../builtin.o int_test.c -o int_test -lutf8proc
clean:
	rm slice_test sieve protocol_test str_test2 hash_test container_test dict_test2 set_test

tests:
	./slice_test
	./sieve
	./protocol_test
	./str_test2
	./hash_test
	./container_test
	./dict_test2
	./set_test
	./int_test


