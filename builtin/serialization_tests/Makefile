all:
	gcc ../builtin.o list_serialize_test.c -o list_serialize_test -lutf8proc
	gcc ../builtin.o dict_serialize_test.c -o dict_serialize_test -lutf8proc
	gcc ../builtin.o str_serialize_test.c -o str_serialize_test -lutf8proc
	gcc ../builtin.o set_serialize_test.c -o set_serialize_test -lutf8proc

clean:
	rm *.o
	rm  list_serialize_test dict_serialize_test str_serialize_test set_serialize_test

runtests:
	./list_serialize_test
	./dict_serialize_test
	./str_serialize_test
	./set_serialize_test
