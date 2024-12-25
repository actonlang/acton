#!/bin/sh
cd ..
/home/kll/terastream/acton/dist/zig/zig cc  -target x86_64-linux-gnu.2.28  -Werror=return-type  -O3  -c  -isystem /home/kll/terastream/acton/dist/inc -Isimple -Isimple/out -I/home/kll/terastream/acton/dist -I/home/kll/terastream/acton/dist/../deps/instdir/include -o/home/kll/terastream/acton/compiler/test/actonc/project/simple/out/rel/lib/a.o simple/out/types/a.c
/home/kll/terastream/acton/dist/zig/zig ar  rcs /home/kll/terastream/acton/compiler/test/actonc/project/simple/out/rel/lib/libActonProject.a /home/kll/terastream/acton/compiler/test/actonc/project/simple/out/rel/lib/a.o
