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
	gcc ../builtin.o complex_test.c -o complex_test -lutf8proc
	gcc ../builtin.o iterator_test.c -o iterator_test -lutf8proc
	gcc ../builtin.o range_test.c -o range_test -lutf8proc
	gcc ../builtin.o tuple_test.c -o tuple_test -lutf8proc
	gcc ../builtin.o builtin_functions_test.c -o builtin_functions_test -lutf8proc
	gcc ../builtin.o bytearray_test.c -o bytearray_test -lutf8proc

Pingpong: Pingpong.c Pingpong.h
	cc 	-Wall -Werror -Wno-int-to-void-pointer-cast \
		-I../../ \
		-lutf8proc \
		../../rts/rts.c \
		../builtin.o \
		../minienv.o \
		Pingpong.c \
		-o Pingpong

Pingpong2: ../../rts/rts.c Pingpong.c Pingpong.h
	cc 	-Wall -Werror -Wno-int-to-void-pointer-cast \
		-I../../ \
		-lutf8proc \
		-D EXPERIMENT=1 \
		-pthread \
		../../rts/rts.c \
		../builtin.o \
		../minienv.o \
		Pingpong.c \
		-o Pingpong2

clean:
	rm -f *.o
	rm -f slice_test sieve protocol_test str_test2 hash_test container_test dict_test2 set_test int_test float_test complex_test iterator_test range_test tuple_test builtin_functions_test bytearray_test Pingpong Pingpong2

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
	./complex_test
	./iterator_test
	./range_test
	./tuple_test
	./builtin_functions_test
	./bytearray_test


