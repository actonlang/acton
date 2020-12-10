all: builtin.o minienv.o

builtin.o: builtin.c builtin.h Iterator.h Iterator.c __builtin__.h __builtin__.c builtin_functions.h builtin_functions.c \
           class_hierarchy.h class_hierarchy.c common.h common.c complx.h complex.c csiphash.c dict.h dict.c dict_impl.h dict_impl.c \
           exceptions.h exceptions.c float.h float.c hash.h hash.c int.h int.c list.h list.c list_impl.h list_impl.c none.h none.c \
           range.h range.c registration.h registration.c serialize.h serialize.c set.h set.c set_impl.h set_impl.c \
           slice.h slice.c str.h str.c tuple.h tuple.c
	cc -c -O3 builtin.c
	ar cr ../lib/libActon.a builtin.o

minienv.o: minienv.c minienv.h builtin.o
	cc -c -O3 minienv.c
	ar cr ../lib/libActon.a minienv.o

clean:
	rm -f builtin.o minienv.o


