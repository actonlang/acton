all: rts.o Pingpong

rts.o: rts.c rts.h
	cc 	-Wall -Werror -Wno-int-to-void-pointer-cast \
		-pthread \
		-c rts.c
	
Pingpong: rts.o pingpong.c
	cc 	-Wall -Werror -Wno-int-to-void-pointer-cast \
		rts.o \
		pingpong.c \
		-DROOT=NEWPingpong \
		main.c \
		-o Pingpong

clean:
	rm -f rts.o Pingpong
