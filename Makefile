all:
	cc -std=c11 -Wall -Werror -pedantic -pedantic-errors kernel.c -o kernel

clean:
	rm -f kernel
