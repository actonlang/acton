#!/bin/sh
cc -Werror -c -I/home/kll/terastream/acton/test/actonc/project/simple/out -I/home/kll/terastream/acton/dist -o/home/kll/terastream/acton/test/actonc/project/simple/out/rel/lib/'a'.o /home/kll/terastream/acton/test/actonc/project/simple/out/types/a.c
ar rcs /home/kll/terastream/acton/test/actonc/project/simple/out/rel/lib/libActonProject.a /home/kll/terastream/acton/test/actonc/project/simple/out/rel/lib/'a'.o
