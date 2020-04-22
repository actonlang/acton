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
	gcc ../builtin.o float_test.c -o float_test -lutf8proc

Pingpong: Pingpong.c Pingpong.h
	cc 	-Wall -Werror -Wno-int-to-void-pointer-cast \
		-lutf8proc \
		../../rts/rts.o \
		../builtin.o \
		Pingpong.c \
		-o Pingpong

clean:
	rm -f *.o
	rm -f slice_test sieve protocol_test str_test2 hash_test container_test dict_test2 set_test int_test float_test Pingpong

runtests:
	./slice_test
	./sieve
	./protocol_test
	./str_test2
	./hash_test
	./container_test
	./dict_test2
	./set_test
	./int_test
	./float_test


