libpqueue.a: src/pqueue.o
	@ar -cr $@ $^

src/pqueue.o: src/pqueue.c
	@cc -c -O3 -std=c11 $^ -o $@

clean:
	@rm -f libpqueue.a
