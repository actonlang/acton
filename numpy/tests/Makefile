all:
	gcc ../../builtin/builtin.o ../numpy.o  -g indextest.c -o indextest -lutf8proc
	gcc ../../builtin/builtin.o ../numpy.o  -g reshapetest.c -o reshapetest -lutf8proc
	gcc ../../builtin/builtin.o ../numpy.o  -g functiontest.c -o functiontest -lutf8proc
	gcc ../../builtin/builtin.o ../numpy.o  -g test1.c -o test1 -lutf8proc
	gcc ../../builtin/builtin.o ../numpy.o  -g sumtest.c -o sumtest -lutf8proc
	gcc ../../builtin/builtin.o ../numpy.o  -g opertest.c -o opertest -lutf8proc
	gcc ../../builtin/builtin.o ../numpy.o  -g strtest.c -o strtest -lutf8proc
	gcc ../../builtin/builtin.o ../numpy.o  -g loesstest.c -o loesstest -lutf8proc

clean:	rm indextest reshapetest functiontest test1 sumtest opertest loesstest

runtests:
	./indextest
	./reshapetest
	./functiontest
	./test1 1000 5
	./sumtest 10
	./opertest
	./strtest
	./loesstest 10

