all: pingpong

pingpong:
	cc -std=c11 \
		-Wall -Werror -pedantic -pedantic-errors \
		-Wno-int-to-void-pointer-cast \
		-pthread \
		pingpong.c \
		-DROOT=Pingpong \
		rts.c \
		-o Pingpong

clean:
	rm -f Pingpong
