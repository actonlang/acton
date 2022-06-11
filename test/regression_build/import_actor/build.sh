#!/bin/sh
cc  -c -I/home/kll/terastream/acton/test/regression_build/import_actor/out -I/home/kll/terastream/acton/dist -o/home/kll/terastream/acton/test/regression_build/import_actor/out/rel/lib/'foo'.o /home/kll/terastream/acton/test/regression_build/import_actor/out/types/foo.c
ar rcs /home/kll/terastream/acton/test/regression_build/import_actor/out/rel/lib/libActonProject.a /home/kll/terastream/acton/test/regression_build/import_actor/out/rel/lib/'foo'.o
