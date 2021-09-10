include common.mk
CHANGELOG_VERSION=$(shell grep '^\#\# \[[0-9]' CHANGELOG.md | sed 's/\#\# \[\([^]]\{1,\}\)].*/\1/' | head -n1)

ifeq ($(shell ls dist/bin/actonc >/dev/null 2>&1; echo $$?),0)
VERSION_INFO:=$(shell dist/bin/actonc --version | head -n1 | cut -d' ' -f2)
else
VERSION_INFO:=unknown
endif

ifeq ($(shell uname -s),Linux)
CFLAGS += -Werror -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast -I/usr/include/kqueue
endif

ACTONC=dist/actonc

all: version-check distribution

help:
	@echo "Available make targets"
	@echo "  all     - build everything"
	@echo "  dist    - build complete distribution"
	@echo "  actonc  - build the Acton compiler"
	@echo "  backend - build the database backend"
	@echo "  rts     - build the Run Time System"
	@echo ""
	@echo "  test    - run the test suite"


version-check:
ifneq ($(VERSION), $(CHANGELOG_VERSION))
	$(error Version in common.mk ($(VERSION)) differs from last version in CHANGELOG.md ($(CHANGELOG_VERSION)))
endif
.PHONY: version-check


# /backend ----------------------------------------------
BACKEND_OFILES = backend/comm.o backend/db.o backend/queue.o backend/skiplist.o backend/txn_state.o backend/txns.o
.PHONY: $(BACKEND_OFILES)
# pass through for now.. should probably move the actual definitions from
# backend/Makefile to here instead for proper dependency tracking
$(BACKEND_OFILES):
	$(MAKE) -C backend $(notdir $@)

backend/server: lib/libcomm.a lib/libremote.a lib/libvc.a
	$(MAKE) -C backend $(notdir $@)


# /builtin ----------------------------------------------
ENV_FILES=$(wildcard builtin/minienv.*)
BUILTIN_HFILES=$(filter-out $(ENV_FILES),$(wildcard builtin/*.h))
BUILTIN_CFILES=$(filter-out $(ENV_FILES),$(wildcard builtin/*.c))
builtin/builtin.o: builtin/builtin.c $(BUILTIN_HFILES) $(BUILTIN_CFILES)
	cc $(CFLAGS) -Wno-unused-result -g -c -O3 $< -o$@

builtin/minienv.o: builtin/minienv.c builtin/minienv.h builtin/builtin.o
	cc $(CFLAGS) -g -c -O3 $< -o$@

# Building the builtin, rts and stdlib is a little tricky as we have to be
# careful about order. First comes the __builtin__.act file,
STDLIB_ACTFILES=$(wildcard stdlib/src/*.act)
STDLIB_CFILES=$(wildcard stdlib/src/*.c)
STDLIB_TYFILES=$(subst src,out/types,$(STDLIB_CFILES:.c=.ty))
STDLIB_HFILES=$(subst src,out/types,$(STDLIB_CFILES:.c=.h))
STDLIB_OFILES=$(subst src,out/release,$(STDLIB_CFILES:.c=.o))
STDLIB_ACTS=$(not-in $(STDLIB_ACTFILES),$(STDLIB_CFILES))

# __builtin__.ty is special, it even has special handling in actonc. Essentially
# all other modules depend on it, so it must be compiled first. While we use
# wildcard patterns for all other files, we have explicit targets for
# __builtin__.ty to make things work. Other .ty file targets etc depend on this,
# so we get the order right.
dist/types/__builtin__.ty: stdlib/out/types/__builtin__.ty
	@mkdir -p $(dir $@)
	cp $< $@

stdlib/out/types/__builtin__.ty: stdlib/src/__builtin__.act
	@mkdir -p $(dir $@)
	$(ACTONC) $< --stub

stdlib/out/types/%.ty: stdlib/src/%.act dist/types/__builtin__.ty
	@mkdir -p $(dir $@)
	$(ACTONC) $< --stub

stdlib/out/types/%.h: stdlib/src/%.h
	@mkdir -p $(dir $@)
	cp $< $@

stdlib/out/release/%.o: stdlib/src/%.c
	@mkdir -p $(dir $@)
	cc $(CFLAGS) -I. -Istdlib/out/ -c $< -o$(subst $,\$,$@)

NUMPY_HFILES=$(wildcard stdlib/c_src/numpy/*.h)
NUMPY_CFILES=$(wildcard stdlib/c_src/numpy/*.h)
stdlib/out/release/numpy.o: stdlib/src/numpy.c stdlib/src/numpy.h stdlib/out/types/math.h $(NUMPY_HFILES) $(NUMPY_CFILES)
	@mkdir -p $(dir $@)
	cc $(CFLAGS) -Wno-unused-result -I. -Istdlib/out/ -c $< -o$(subst $,\$,$@)

clean-stdlib:
	rm -f $(STDLIB_HFILES) $(STDLIB_OFILES) $(STDLIB_TYFILES)

# /lib --------------------------------------------------
LIBS=lib/libActon.a lib/libActonRTSdebug.a lib/libcomm.a lib/libdb.a lib/libdbclient.a lib/libremote.a lib/libvc.a
#
# If we later let actonc build things, it would produce a libActonProject.a file
# in the stdlib directory, which we would need to join together with rts.o etc
# to form the final libActon (or maybe produce a libActonStdlib and link with?)
lib/libActon.a: builtin/builtin.o builtin/minienv.o $(STDLIB_OFILES) stdlib/out/release/numpy.o rts/empty.o rts/rts.o
	ar rcs $@ $(subst $,\$,$^)

lib/libActonRTSdebug.a: rts/rts-debug.o
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
rts/rts.o: rts/rts.c rts/rts.h
	cc $(CFLAGS) -g -Wno-int-to-void-pointer-cast \
		-Wno-unused-result \
		-pthread \
		-c -O3 $< -o $@

rts/rts-debug.o: rts/rts.c rts/rts.h
	cc $(CFLAGS) -DRTS_DEBUG -g -Wno-int-to-void-pointer-cast \
		-Wno-unused-result \
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
actonc: dist/actonc

compiler/actonc:
	$(MAKE) -C compiler install
	mkdir -p dist/bin
	cp compiler/actonc dist/bin/actonc

backend:
	$(MAKE) -C backend

rts: $(LIBS)

test:
	$(MAKE) -C backend test
	$(MAKE) -C test

clean: clean-compiler clean-distribution clean-backend clean-rts clean-stdlib

clean-compiler:
	rm -f actonc
	$(MAKE) -C compiler clean

clean-backend:
	$(MAKE) -C backend clean

clean-rts:
	rm -f $(LIBS) $(TYFILES)

# == DIST ==
#

dist/actonc: compiler/actonc
	@mkdir -p $(dir $@)
	cp $< $@

# This does a little hack, first copying and then moving the file in place. This
# is to avoid an error if actondb is currently running. cp tries to open the
# file and modify it, which the Linux kernel (and perhaps others?) will prevent
# if the file to be modified is an executable program that is currently running.
# We work around it by moving / renaming the file in place instead!
dist/actondb: backend/server dist/actonc
	@mkdir -p $(dir $@)
	cp $< backend/actondb
	mv $< $@

dist/builtin/%: builtin/%
	@mkdir -p $(dir $@)
	cp $< $@

dist/rts/%: rts/%
	@mkdir -p $(dir $@)
	cp $< $@

dist/types/%: stdlib/out/types/%
	@mkdir -p $(dir $@)
	cp $< $@

dist/lib/%: lib/%
	@mkdir -p $(dir $@)
	cp $< $@

dist/lib/libActon.a: lib/libActon.a
	@mkdir -p $(dir $@)
	cp $< $@

DIST_ACTONC=dist/actonc
DIST_ACTONDB=dist/actondb
DIST_BUILTIN=$(addprefix dist/,$(BUILTIN_HFILES)) dist/builtin/minienv.h dist/builtin/builtin.o
DIST_RTS=dist/rts/empty.o dist/rts/rts.h dist/rts/rts.o
DIST_HFILES=$(subst stdlib/out/types,dist/types,$(STDLIB_HFILES))
DIST_TYFILES=$(subst stdlib/out/types,dist/types,$(STDLIB_TYFILES))
DIST_LIBS=$(addprefix dist/,$(LIBS))

.PHONY: distribution clean-distribution
distribution: $(DIST_ACTONC) $(DIST_ACTONDB) $(DIST_BUILTIN) $(DIST_RTS) $(DIST_HFILES) $(DIST_TYFILES) $(DIST_LIBS)

clean-distribution:
	rm -f $(DIST_ACTONC) $(DIST_ACTONDB) $(DIST_BUILTIN) $(DIST_RTS) $(DIST_HFILES) $(DIST_TYFILES) $(DIST_LIBS)

# == release ==
# This is where we take our distribution and turn it into a release tar ball
ARCH=$(shell uname -s -m | sed -e 's/ /-/' | tr '[A-Z]' '[a-z]')
GNU_TAR := $(shell ls --version 2>&1 | grep GNU >/dev/null 2>&1; echo $$?)
ifeq ($(GNU_TAR),0)
TAR_TRANSFORM_OPT=--transform 's,^dist,acton,'
else
TAR_TRANSFORM_OPT=-s ,^dist,acton,
endif

acton-$(ARCH)-$(VERSION_INFO).tar.bz2:
	tar jcvf $@ $(TAR_TRANSFORM_OPT) --exclude .gitignore dist

release: distribution
	$(MAKE) acton-$(ARCH)-$(VERSION_INFO).tar.bz2

.PHONY: all backend compiler distribution rts clean clean-compiler clean-backend clean-rts test release acton-$(ARCH)-$(VERSION).tar.bz2
