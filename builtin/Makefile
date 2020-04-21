all:
	gcc -c builtin.c
	cd tests && $(MAKE)
clean:
	rm builtin.o
	cd tests && $(MAKE) clean
runtests:
	cd tests && $(MAKE) runtests

