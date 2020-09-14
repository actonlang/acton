all:
	gcc ../../builtin/builtin.o ../ndarray.o -O3 indextest.c -o indextest -lutf8proc
	gcc ../../builtin/builtin.o ../ndarray.o -O3 reshapetest.c -o reshapetest -lutf8proc
	gcc ../../builtin/builtin.o ../ndarray.o -g test1.c -o test1 -lutf8proc
	gcc ../../builtin/builtin.o ../ndarray.o -O3 sumftest.c -o sumftest -lutf8proc
	gcc ../../builtin/builtin.o ../ndarray.o -O3 opertest.c -o opertest -lutf8proc

clean:	rm indextest reshapetest test1 sumftest opertest

runtests:
	./indextest
	./reshapetest
	./test1 1000 5
	./sumftest 10
	./opertest
