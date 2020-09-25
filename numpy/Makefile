all:
	gcc -c -O3 numpy.c
	cd tests && $(MAKE)
clean:
	rm numpy.o
	cd tests && $(MAKE) clean
runtests:
	cd tests && $(MAKE) runtests
