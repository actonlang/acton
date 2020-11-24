all:
	gcc -c -g builtin.c
	gcc -c -g minienv.c
	ar cr ../lib/libActon.a builtin.o minienv.o
	cd tests && $(MAKE)
	cd serialization_tests && $(MAKE)
clean:
	rm builtin.o
	cd tests && $(MAKE) clean
	cd serialization_tests && $(MAKE) clean
runtests:
	cd tests && $(MAKE) runtests
	cd serialization_tests && $(MAKE) runtests

