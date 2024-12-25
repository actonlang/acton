#!/bin/sh
cc -Werror -c -I/home/kll/terastream/acton/test/actonc/project/auto_root/out -I/home/kll/terastream/acton/dist -o/home/kll/terastream/acton/test/actonc/project/auto_root/out/rel/lib/'test'.o /home/kll/terastream/acton/test/actonc/project/auto_root/out/types/test.c
ar rcs /home/kll/terastream/acton/test/actonc/project/auto_root/out/rel/lib/libActonProject.a /home/kll/terastream/acton/test/actonc/project/auto_root/out/rel/lib/'test'.o
