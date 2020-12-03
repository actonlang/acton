all: rts.o pingpong # rts2.o pingpong2

rts.o: rts.c rts.h
	cc 	-Wall -Werror -Wno-int-to-void-pointer-cast \
		-pthread \
		-c -O3 rts.c
	ar cr ../lib/libActon.a rts.o

pingpong: rts.o pingpong.c pingpong.h
	cc 	-Wall -Werror -Wno-int-to-void-pointer-cast \
		-lutf8proc \
		rts.o \
		../builtin/builtin.o \
		../builtin/minienv.o \
		pingpong.c \
		-o pingpong

pingpong2: rts.c pingpong.c pingpong.h
	cc 	-Wall -Werror -Wno-int-to-void-pointer-cast \
		-lutf8proc \
		-D EXPERIMENT=1 \
		-pthread \
		rts.c \
		../builtin/builtin.o \
		../builtin/minienv.o \
		pingpong.c \
		-o pingpong2

clean:
	rm -f rts.o pingpong rts2.o pingpong2

