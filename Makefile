all:
	cc -std=c11 -O3 -Wall -Werror -pedantic -pedantic-errors \
		kernel.c \
		-lpthread \
		-lm \
		-o kernel

clean:
	rm -f kernel
