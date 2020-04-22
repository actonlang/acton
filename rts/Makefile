all: rts.o pingpong

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

clean:
	rm -f rts.o pingpong
