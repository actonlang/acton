all:
	gcc -c builtin.c
	cd tests && $(MAKE)
	cd serialization_tests && $(MAKE)
clean:
	rm builtin.o
	cd tests && $(MAKE) clean
	cd serialization_tests && $(MAKE)
runtests:
	cd tests && $(MAKE) runtests
	cd serialization_tests && $(MAKE) runtests

