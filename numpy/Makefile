all:
	gcc -c -O3 ndarray.c
	cd numpytests && $(MAKE)
clean:
	rm ndarray.o
	cd numpytests && $(MAKE) clean
runtests:
	cd numpytests && $(MAKE) runtests
