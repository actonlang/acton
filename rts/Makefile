all: pingpong

pingpong: rts.c rts.h pingpong.c
	cc -std=c11 \
		-Wall -Werror -Wno-int-to-void-pointer-cast \
		-pthread \
		pingpong.c \
		-DROOT=Pingpong \
		rts.c \
		-o Pingpong

clean:
	rm -f Pingpong
