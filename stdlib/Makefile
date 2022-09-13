
CFLAGS+= -fno-common -I.. -I../dist -I../dist/include -L../lib -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast -Wformat -Werror=format-security
CFLAGS_REL= -O3 -DREL
CFLAGS_DEV= -g -DDEV

out/dev/lib/acton.rts.o: src/acton/rts.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CFLAGS_DEV) -I. -Iout/ -c $< -o'$@'


out/rel/lib/acton.rts.o: src/acton/rts.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CFLAGS_REL) -I. -Iout/ -c $< -o'$@'

out/dev/lib/math.o: src/math.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CFLAGS_DEV) -Iout/ -c $< -o$@

out/rel/lib/math.o: src/math.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CFLAGS_REL) -Iout/ -c $< -o$@


NUMPY_CFILES=$(wildcard c_src/numpy/*.h)
out/dev/lib/numpy.o: src/numpy.c src/numpy.h out/types/math.h $(NUMPY_CFILES)
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CFLAGS_DEV) -Wno-unused-result -Iout/ -c $< -o$@

out/rel/lib/numpy.o: src/numpy.c src/numpy.h out/types/math.h $(NUMPY_CFILES)
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CFLAGS_REL) -Wno-unused-result -Iout/ -c $< -o$@

out/dev/lib/random.o: src/random.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CFLAGS_DEV) -c $< -o$@

out/rel/lib/random.o: src/random.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CFLAGS_REL) -c $< -o$@

out/dev/lib/_time.o: src/_time.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CFLAGS_DEV) -I -Iout/ -c $< -o$@

out/rel/lib/_time.o: src/_time.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CFLAGS_REL) -I -Iout/ -c $< -o$@
