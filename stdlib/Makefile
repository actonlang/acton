
CFLAGS+= -I.. -Ideps -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast -Wformat -Werror=format-security
CFLAGS_REL= -O3 -DREL
CFLAGS_DEV= -g -DDEV

out/lib/acton$$rts_dev.o: src/acton/rts.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CFLAGS_DEV) -I. -Iout/ -c $< -o'$@'


out/lib/acton$$rts_rel.o: src/acton/rts.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CFLAGS_REL) -I. -Iout/ -c $< -o'$@'

out/lib/math_dev.o: src/math.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CFLAGS_DEV) -Iout/ -c $< -o$@

out/lib/math_rel.o: src/math.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CFLAGS_REL) -Iout/ -c $< -o$@


NUMPY_CFILES=$(wildcard c_src/numpy/*.h)
ifeq ($(shell uname -s),Linux)
NUMPY_CFLAGS+=-lbsd -ldl -lmd
endif
out/lib/numpy_dev.o: src/numpy.c src/numpy.h out/types/math.h $(NUMPY_CFILES) out/lib/math_dev.o
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CFLAGS_DEV) -Wno-unused-result -r -Iout/ $< -o$@ $(NUMPY_CFLAGS) out/lib/math_dev.o

out/lib/numpy_rel.o: src/numpy.c src/numpy.h out/types/math.h $(NUMPY_CFILES) out/lib/math_rel.o
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CFLAGS_REL) -Wno-unused-result -r -Iout/ $< -o$@ $(NUMPY_CFLAGS) out/lib/math_rel.o

out/lib/random_dev.o: src/random.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CFLAGS_DEV) -c $< -o$@

out/lib/random_rel.o: src/random.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CFLAGS_REL) -c $< -o$@

out/lib/_time_dev.o: src/_time.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CFLAGS_DEV) -I -Iout/ -c $< -o$@

out/lib/_time_rel.o: src/_time.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CFLAGS_REL) -I -Iout/ -c $< -o$@
