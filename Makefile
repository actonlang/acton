all:
	cc -std=c11 -Wall -pedantic -Wno-int-to-void-pointer-cast kernelops.c kernel.c -o kernel

clean:
	rm -f kernel
