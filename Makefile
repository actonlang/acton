include common.mk

VERSION_INFO=$(subst acton ,,$(shell ./actonc --version DUMMY))

ifeq ($(shell uname -s),Linux)
CFLAGS += -I/usr/include/kqueue
endif
MODULES=

all: actonc rts

help:
	@echo "Available make targets"
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


# /math -------------------------------------------------
MODULES += math/math.o
math/math.o: math/math.c math/math.h
	cc $(CFLAGS) -I. -c $< -o$@


# /modules ----------------------------------------------
ACTONC=./actonc
TYMODULES=modules/__builtin__.ty modules/math.ty modules/numpy.ty modules/time.ty
modules/math.h: math/math.h
	cp $< $@

modules/numpy.h: numpy/numpy.h
	cp $< $@

modules/time.h: time/time.h
	cp $< $@

modules/__builtin__.ty: modules/__builtin__.act actonc
	$(ACTONC) $< --stub

modules/math.ty: modules/math.act modules/math.h modules/__builtin__.ty actonc
	$(ACTONC) $< --stub

modules/numpy.ty: modules/numpy.act modules/math.ty actonc
	$(ACTONC) $< --stub

modules/time.ty: modules/time.act modules/time.h actonc
	$(ACTONC) $< --stub


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

lib/libActon.a: builtin/builtin.o builtin/minienv.o math/math.o numpy/numpy.o rts/empty.o rts/rts.o time/time.o
	ar rcs $@ $^

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


# /time -------------------------------------------------
MODULES += time/time.o
time/time.o: time/time.c time/time.h
	cc $(CFLAGS) -I. -c $< -o$@

# top level targets
actonc:
	$(MAKE) -C compiler install

backend:
	$(MAKE) -C backend

rts: $(MODULES) $(LIBS) $(TYMODULES)

test:
	$(MAKE) -C backend test
	$(MAKE) -C test

clean: clean-compiler clean-backend clean-rts

clean-compiler:
	rm -f actonc
	$(MAKE) -C compiler clean

clean-backend:
	$(MAKE) -C backend clean

clean-rts:
	rm -f $(MODULES) $(LIBS) $(TYMODULES) modules/math.h modules/numpy.h

ARCH=$(shell uname -s -m | sed -e 's/ /-/' | tr '[A-Z]' '[a-z]')
RELEASE_MANIFEST=actonc backend builtin lib math modules numpy rts
GNU_TAR := $(shell ls --version 2>&1 | grep GNU >/dev/null 2>&1; echo $$?)
ifeq ($(GNU_TAR),0)
TAR_TRANSFORM_OPT=--transform 's,^,acton/,'
else
TAR_TRANSFORM_OPT=-s ,^,acton/,
endif
acton-$(ARCH)-$(VERSION_INFO).tar.bz2:
	tar jcvf $@ $(TAR_TRANSFORM_OPT) $(RELEASE_MANIFEST)

release: acton-$(ARCH)-$(VERSION_INFO).tar.bz2

.PHONY: all compiler backend rts clean clean-compiler clean-backend clean-rts test release acton-$(ARCH)-$(VERSION).tar.bz2
