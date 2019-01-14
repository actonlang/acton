all:
	cc -std=c11 -Wall -pedantic -Wno-int-to-void-pointer-cast kernel.c -o kernel

clean:
	rm -f kernel
