include common.mk

VERSION_INFO=$(subst acton ,,$(shell ./dist/actonc --version DUMMY))

ifeq ($(shell uname -s),Linux)
CFLAGS += -I/usr/include/kqueue
endif
MODULES=

all: dist

help:
	@echo "Available make targets"
	@echo "  all     - build everything"
	@echo "  dist    - build complete distribution"
	@echo "  actonc  - build the Acton compiler"
	@echo "  backend - build the database backend"
	@echo "  rts     - build the Run Time System"
	@echo ""
	@echo "  test    - run the test suite"



# /backend ----------------------------------------------
BACKEND_MODULES = backend/comm.o backend/db.o backend/queue.o backend/skiplist.o backend/txn_state.o backend/txns.o
.PHONY: $(BACKEND_MODULES)
MODULES += $(BACKEND_MODULES)
# pass through for now.. should probably move the actual definitions from
# backend/Makefile to here instead for proper dependency tracking
$(BACKEND_MODULES):
	$(MAKE) -C backend $(notdir $@)


# /builtin ----------------------------------------------
MODULES += builtin/builtin.o builtin/minienv.o
builtin/builtin.o: builtin/builtin.c builtin/builtin.h builtin/Iterator.h builtin/Iterator.c builtin/__builtin__.h builtin/__builtin__.c builtin/builtin_functions.h builtin/builtin_functions.c \
           builtin/class_hierarchy.h builtin/class_hierarchy.c builtin/common.h builtin/common.c builtin/complx.h builtin/complex.c builtin/csiphash.c builtin/dict.h builtin/dict.c builtin/dict_impl.h builtin/dict_impl.c \
           builtin/exceptions.h builtin/exceptions.c builtin/float.h builtin/float.c builtin/hash.h builtin/hash.c builtin/int.h builtin/int.c builtin/list.h builtin/list.c builtin/list_impl.h builtin/list_impl.c builtin/none.h builtin/none.c \
           builtin/range.h builtin/range.c builtin/registration.h builtin/registration.c builtin/serialize.h builtin/serialize.c builtin/set.h builtin/set.c builtin/set_impl.h builtin/set_impl.c \
           builtin/slice.h builtin/slice.c builtin/str.h builtin/str.c builtin/tuple.h builtin/tuple.c
	cc $(CFLAGS) -g -c -O3 $< -o$@

builtin/minienv.o: builtin/minienv.c builtin/minienv.h builtin/builtin.o
	cc $(CFLAGS) -g -c -O3 $< -o$@


ACTONC=dist/actonc --syspath .
TYMODULES=modules/__builtin__.ty modules/math.ty modules/numpy.ty modules/time.ty

modules/numpy.h: numpy/numpy.h
	cp $< $@

modules/__builtin__.ty: modules/__builtin__.act actonc
	$(ACTONC) $< --stub

MODULES += modules/math.o
modules/math.ty: modules/math.act modules/math.h modules/__builtin__.ty actonc
	$(ACTONC) $< --stub

modules/math.o: modules/math.c modules/math.h
	cc $(CFLAGS) -I. -c $< -o$(subst $,\$,$@)

modules/numpy.ty: modules/numpy.act modules/math.ty actonc
	$(ACTONC) $< --stub

MODULES += modules/random.o
modules/random.ty: modules/random.act modules/random.h actonc
	$(ACTONC) $< --stub

modules/random.o: modules/random.c modules/random.h
	cc $(CFLAGS) -I. -c $< -o$(subst $,\$,$@)

MODULES += modules/time.o
modules/time.ty: modules/time.act modules/time.h actonc
	$(ACTONC) $< --stub

modules/time.o: modules/time.c modules/time.h
	cc $(CFLAGS) -I. -c $< -o$(subst $,\$,$@)

MODULES += modules/acton/acton$$rts.o
modules/acton/rts.ty: modules/acton/rts.act modules/time.h actonc
	$(ACTONC) $< --stub

modules/acton/acton$$rts.o: modules/acton/rts.c modules/acton/rts.h actonc
	cc $(CFLAGS) -I. -c $< -o$(subst $,\$,$@)


# /numpy ------------------------------------------------
MODULES += numpy/numpy.o
numpy/numpy.o: numpy/numpy.c numpy/numpy.h numpy/init.h numpy/init.c \
		numpy/ndarray.h numpy/ndarray.c numpy/ndselect.h \
		numpy/ndselect.c numpy/primitive.h numpy/primitive.c \
		numpy/protocol_impls.h numpy/protocol_impls.c numpy/quickselect.h \
		numpy/quickselect.c
	cc $(CFLAGS) -I. -O3 -c $< -o$@


# /lib --------------------------------------------------
LIBS=lib/libActon.a lib/libcomm.a lib/libdb.a lib/libdbclient.a lib/libremote.a lib/libvc.a

lib/libActon.a: builtin/builtin.o builtin/minienv.o modules/math.o numpy/numpy.o rts/empty.o rts/rts.o modules/time.o modules/acton/acton$$rts.o $(MODULES)
	ar rcs $@ $(subst $,\$,$^)

lib/libcomm.a: backend/comm.o rts/empty.o
	ar rcs $@ $^

lib/libdb.a: backend/db.o backend/queue.o backend/skiplist.o backend/txn_state.o backend/txns.o rts/empty.o
	ar rcs $@ $^

lib/libdbclient.a: backend/client_api.o rts/empty.o
	ar rcs $@ $^

lib/libremote.a: backend/failure_detector/db_messages.pb-c.o backend/failure_detector/cells.o backend/failure_detector/db_queries.o backend/failure_detector/fd.o
	ar rcs $@ $^

lib/libvc.a: backend/failure_detector/vector_clock.o
	ar rcs $@ $^


# /rts --------------------------------------------------
MODULES += rts/rts.o rts/empty.o
rts/rts.o: rts/rts.c rts/rts.h
	cc $(CFLAGS) -g -Wno-int-to-void-pointer-cast \
		-pthread \
		-c -O3 $< -o $@

rts/empty.o: rts/empty.c
	cc $(CFLAGS) -c $< -o $@

rts/pingpong: rts/pingpong.c rts/pingpong.h rts/rts.o
	cc $(CFLAGS) -Wno-int-to-void-pointer-cast \
		-L lib \
		-lutf8proc -ldbclient -lremote -lcomm -ldb -lvc -lprotobuf-c \
		rts/rts.o \
		builtin/builtin.o \
		builtin/minienv.o \
		$< \
		-o $@



# top level targets
actonc:
	$(MAKE) -C compiler install

backend:
	$(MAKE) -C backend

rts: $(MODULES) $(LIBS) $(TYMODULES)

test:
	$(MAKE) -C backend test
	$(MAKE) -C test

clean: clean-compiler clean-backend clean-rts clean-dist

clean-compiler:
	rm -f actonc
	$(MAKE) -C compiler clean

clean-backend:
	$(MAKE) -C backend clean

clean-rts:
	rm -f $(MODULES) $(LIBS) $(TYMODULES) modules/math.h modules/numpy.h

clean-dist:
	rm -rf dist/lib dist/modules dist/builtin dist/rts

ARCH=$(shell uname -s -m | sed -e 's/ /-/' | tr '[A-Z]' '[a-z]')
GNU_TAR := $(shell ls --version 2>&1 | grep GNU >/dev/null 2>&1; echo $$?)
ifeq ($(GNU_TAR),0)
TAR_TRANSFORM_OPT=--transform 's,^dist,acton,'
else
TAR_TRANSFORM_OPT=-s ,^dist,acton,
endif
acton-$(ARCH)-$(VERSION_INFO).tar.bz2:
	tar jcvf $@ $(TAR_TRANSFORM_OPT) --exclude .gitignore dist

release: dist
	$(MAKE) acton-$(ARCH)-$(VERSION_INFO).tar.bz2

dist: actonc backend rts
	mkdir -p dist/lib dist/modules dist/builtin dist/rts
	cp backend/server backend/actondb
	mv backend/actondb dist/
	cp lib/*.a dist/lib/
	cp builtin/*.h dist/builtin/
	cp rts/rts.h dist/rts/rts.h
	find modules -name "*.h" -or -name "*.ty" | xargs -n 1 dirname | xargs -I {} mkdir -p dist/{}
	find modules -name "*.h" -or -name "*.ty" | xargs -I {} cp {} dist/{}

.PHONY: all backend compiler dist rts clean clean-compiler clean-backend clean-rts test release acton-$(ARCH)-$(VERSION).tar.bz2
