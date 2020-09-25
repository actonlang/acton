all:
	gcc ../../builtin/builtin.o ../numpy.o  -O3 indextest.c -o indextest -lutf8proc
	gcc ../../builtin/builtin.o ../numpy.o  -O3 reshapetest.c -o reshapetest -lutf8proc
	gcc ../../builtin/builtin.o ../numpy.o  -O3 test1.c -o test1 -lutf8proc
	gcc ../../builtin/builtin.o ../numpy.o  -O3 sumtest.c -o sumtest -lutf8proc
	gcc ../../builtin/builtin.o ../numpy.o  -O3 opertest.c -o opertest -lutf8proc
	gcc ../../builtin/builtin.o ../numpy.o  -O3 strtest.c -o strtest -lutf8proc
	gcc ../../builtin/builtin.o ../numpy.o  -O3 loesstest.c -o loesstest -lutf8proc

clean:	rm indextest reshapetest test1 sumtest opertest loesstest

runtests:
	./indextest
	./reshapetest
	./test1 1000 5
	./sumtest 10
	./opertest
	./strtest
	./loesstest 10

