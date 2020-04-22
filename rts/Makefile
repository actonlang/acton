all: rts.o Pingpong

rts.o: rts.c rts.h
	cc 	-Wall -Werror -Wno-int-to-void-pointer-cast \
		-pthread \
		-c rts.c
	
Pingpong: rts.o pingpong.c pingpong.h
	cc 	-Wall -Werror -Wno-int-to-void-pointer-cast \
		-lutf8proc \
		rts.o \
		../builtin/builtin.o \
		pingpong.c \
		-o Pingpong

clean:
	rm -f rts.o Pingpong
