all: rts.o

rts.o: rts.c rts.h
	cc  -Wno-int-to-void-pointer-cast \
		-pthread \
		-c -O3 rts.c
	ar cr ../lib/libActon.a rts.o
	cc -c empty.c
	ar cr ../lib/libcomm.a empty.o
	ar cr ../lib/libdb.a empty.o
	ar cr ../lib/libdbclient.a empty.o

pingpong: rts.o pingpong.c pingpong.h
	cc 	-Wno-int-to-void-pointer-cast \
		-L ../lib \
		-lutf8proc -ldbclient -lremote -lcomm -ldb -lvc -lprotobuf-c \
		rts.o \
		../builtin/builtin.o \
		../builtin/minienv.o \
		pingpong.c \
		-o pingpong

clean:
	rm -f rts.o empty.o pingpong

