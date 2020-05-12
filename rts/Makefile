all: rts.o pingpong rts2.o pingpong2

rts.o: rts.c rts.h
	cc 	-Wall -Werror -Wno-int-to-void-pointer-cast \
		-pthread \
		-c rts.c

pingpong: rts.o pingpong.c pingpong.h
	cc 	-Wall -Werror -Wno-int-to-void-pointer-cast \
		-lutf8proc \
		rts.o \
		../builtin/builtin.o \
		pingpong.c \
		-o pingpong

rts2.o: rts2.c rts.h
	cc 	-Wall -Werror -Wno-int-to-void-pointer-cast \
		-pthread \
		-c rts2.c

pingpong2: rts2.o pingpong2.c pingpong.h
	cc 	-Wall -Werror -Wno-int-to-void-pointer-cast \
		-lutf8proc \
		rts2.o \
		../builtin/builtin.o \
		pingpong2.c \
		-o pingpong2

clean:
	rm -f rts.o pingpong rts2.o pingpong2

