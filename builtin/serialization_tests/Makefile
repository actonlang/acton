all:
	gcc ../builtin.o list_serialize_test.c -o list_serialize_test -lutf8proc
	gcc ../builtin.o dict_serialize_test.c -o dict_serialize_test -lutf8proc
	gcc ../builtin.o str_serialize_test.c -o str_serialize_test -lutf8proc
	gcc ../builtin.o set_serialize_test.c -o set_serialize_test -lutf8proc
	gcc ../builtin.o multi_serialization_test.c -o multi_serialization_test -lutf8proc
	gcc -c graph.c
	gcc ../builtin.o graph.o graph_serialize_test.c -o graph_serialize_test -lutf8proc

ActorRing: ActorRing.c ActorRing.h
	cc 	-Wall -Werror -Wno-int-to-void-pointer-cast \
		-I../../ \
		-lutf8proc \
		../../rts/rts.o \
		../builtin.o \
		../env.o \
		ActorRing.c \
		-o ActorRing

clean:
	rm -f *.o
	rm -f list_serialize_test dict_serialize_test str_serialize_test set_serialize_test multi_serialization_test 
	rm -f ActorRing

runtests:
	./list_serialize_test
	./dict_serialize_test
	./str_serialize_test
	./set_serialize_test
	./multi_serialization_test
	./graph_serialize_test
