all:
	cc -std=c11 \
		-Wall -Werror -pedantic -pedantic-errors \
		-Wno-int-to-void-pointer-cast \
		kernelops.c \
		kernel.c \
		-pthread \
		-lpthread \
		-lm \
		-o kernel

clean:
	rm -f kernel
