all: rts.o

rts.o: rts.c rts.h
	cc  -Wno-int-to-void-pointer-cast \
		-pthread \
		-c -O3 rts.c
	ar cr ../lib/libActon.a rts.o

pingpong: rts.o pingpong.c pingpong.h
	cc 	-Wno-int-to-void-pointer-cast \
		-L ../lib \
		-lutf8proc -ldbclient -lremote -lcomm -ldb -lvc -lprotobuf-c \
		rts.o \
		../builtin/builtin.o \
		../builtin/minienv.o \
		pingpong.c \
		-o pingpong

pingpong2: rts.c pingpong.c pingpong.h
	cc 	-Wno-int-to-void-pointer-cast \
		-L ../lib \
		-lutf8proc -ldbclient -lremote -lcomm -ldb -lvc -lprotobuf-c \
		-D EXPERIMENT=1 \
		-pthread \
		rts.c \
		../builtin/builtin.o \
		../builtin/minienv.o \
		pingpong.c \
		-o pingpong2

clean:
	rm -f rts.o pingpong rts2.o pingpong2

