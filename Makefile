include common.mk
CHANGELOG_VERSION=$(shell grep '^\#\# \[[0-9]' CHANGELOG.md | sed 's/\#\# \[\([^]]\{1,\}\)].*/\1/' | head -n1)

ACTONC=dist/bin/actonc

ifeq ($(shell ls $(ACTONC) >/dev/null 2>&1; echo $$?),0)
VERSION_INFO:=$(shell $(ACTONC) --version | head -n1 | cut -d' ' -f2)
else
VERSION_INFO:=unknown
endif

ifeq ($(shell uname -s),Linux)
CFLAGS += -Werror -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast
endif


.PHONY: all
all: version-check distribution

.PHONY: help
help:
	@echo "Available make targets"
	@echo "  all     - build everything"
	@echo "  dist    - build complete distribution"
	@echo "  actonc  - build the Acton compiler"
	@echo "  backend - build the database backend"
	@echo "  rts     - build the Run Time System"
	@echo ""
	@echo "  test    - run the test suite"


.PHONY: version-check
version-check:
ifneq ($(VERSION), $(CHANGELOG_VERSION))
	$(error Version in common.mk ($(VERSION)) differs from last version in CHANGELOG.md ($(CHANGELOG_VERSION)))
endif


# /backend ----------------------------------------------
.PHONY: $(BACKEND_OFILES)
BACKEND_OFILES = backend/comm.o backend/db.o backend/queue.o backend/skiplist.o backend/txn_state.o backend/txns.o
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
STDLIB_ACTFILES=$(wildcard stdlib/src/*.act stdlib/src/**/*.act)
STDLIB_CFILES=$(wildcard stdlib/src/*.c stdlib/src/**/*.c)
STDLIB_TYFILES=$(subst src,out/types,$(STDLIB_CFILES:.c=.ty))
STDLIB_HFILES=$(subst src,out/types,$(STDLIB_CFILES:.c=.h))
STDLIB_OFILES=$(subst src,out/release,$(STDLIB_CFILES:.c=.o))
STDLIB_ACTS=$(not-in $(STDLIB_ACTFILES),$(STDLIB_CFILES))

# __builtin__.ty is special, it even has special handling in actonc. Essentially
# all other modules depend on it, so it must be compiled first, which is why we
# specify it as a dependency. This rule also builds __builtin__.ty but make is
# clever enough to cancel out a dependency on itself.
stdlib/out/types/%.ty: stdlib/src/%.act dist/types/__builtin__.ty $(ACTONC)
	@mkdir -p $(dir $@)
	$(ACTONC) $< --stub

stdlib/out/types/%.h: stdlib/src/%.h
	@mkdir -p $(dir $@)
	cp $< $@

stdlib/out/release/%.o: stdlib/src/%.c
	@mkdir -p $(dir $@)
	cc $(CFLAGS) -I. -Istdlib/ -Istdlib/out/ -c $< -o$(subst $,\$,$@)

NUMPY_HFILES=$(wildcard stdlib/c_src/numpy/*.h)
NUMPY_CFILES=$(wildcard stdlib/c_src/numpy/*.h)
stdlib/out/release/numpy.o: stdlib/src/numpy.c stdlib/src/numpy.h stdlib/out/types/math.h $(NUMPY_HFILES) $(NUMPY_CFILES)
	@mkdir -p $(dir $@)
	cc $(CFLAGS) -Wno-unused-result -I. -Istdlib/out/ -c $< -o$(subst $,\$,$@)

# /lib --------------------------------------------------
ARCHIVES=lib/libActon.a lib/libActonRTSdebug.a lib/libcomm.a lib/libdb.a lib/libdbclient.a lib/libremote.a lib/libvc.a

# If we later let actonc build things, it would produce a libActonProject.a file
# in the stdlib directory, which we would need to join together with rts.o etc
# to form the final libActon (or maybe produce a libActonStdlib and link with?)
OFILES += builtin/builtin.o builtin/minienv.o $(STDLIB_OFILES) stdlib/out/release/numpy.o rts/empty.o rts/rts.o
lib/libActon.a: builtin/builtin.o builtin/minienv.o $(STDLIB_OFILES) stdlib/out/release/numpy.o rts/empty.o rts/rts.o
	ar rcs $@ $(subst $,\$,$^)

OFILES += rts/rts-debug.o
lib/libActonRTSdebug.a: rts/rts-debug.o
	ar rcs $@ $(subst $,\$,$^)

OFILES += backend/comm.o rts/empty.o
lib/libcomm.a: backend/comm.o rts/empty.o
	ar rcs $@ $^

OFILES += backend/db.o backend/queue.o backend/skiplist.o backend/txn_state.o backend/txns.o rts/empty.o
lib/libdb.a: backend/db.o backend/queue.o backend/skiplist.o backend/txn_state.o backend/txns.o rts/empty.o
	ar rcs $@ $^

OFILES += backend/client_api.o rts/empty.o
lib/libdbclient.a: backend/client_api.o rts/empty.o
	ar rcs $@ $^

OFILES += backend/failure_detector/db_messages.pb-c.o backend/failure_detector/cells.o backend/failure_detector/db_queries.o backend/failure_detector/fd.o
lib/libremote.a: backend/failure_detector/db_messages.pb-c.o backend/failure_detector/cells.o backend/failure_detector/db_queries.o backend/failure_detector/fd.o
	ar rcs $@ $^

OFILES += backend/failure_detector/vector_clock.o
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
# NOTE: we don't do proper dependency declaration for actonc and let stack
# handle it, thus this target is declared a PHONY so stack can always run
.PHONY: compiler/actonc
compiler/actonc:
	$(MAKE) -C compiler install
	mkdir -p dist/bin

.PHONY: backend
backend:
	$(MAKE) -C backend

.PHONY: rts
rts: $(ARCHIVES)

.PHONY: test
test:
	$(MAKE) -C backend test
	$(MAKE) -C test

.PHONY: clean
clean: clean-compiler clean-distribution clean-backend clean-rts

.PHONY: clean-compiler
clean-compiler:
	$(MAKE) -C compiler clean

.PHONY: clean-backend
clean-backend:
	$(MAKE) -C backend clean

.PHONY: clean-rts
clean-rts:
	rm -f $(ARCHIVES) $(OFILES) $(STDLIB_HFILES) $(STDLIB_OFILES) $(STDLIB_TYFILES)

# == DIST ==
#

$(ACTONC): compiler/actonc
	@mkdir -p $(dir $@)
	cp $< $@

# This does a little hack, first copying and then moving the file in place. This
# is to avoid an error if actondb is currently running. cp tries to open the
# file and modify it, which the Linux kernel (and perhaps others?) will prevent
# if the file to be modified is an executable program that is currently running.
# We work around it by moving / renaming the file in place instead!
dist/bin/actondb: backend/server
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

DIST_BINS=$(ACTONC) dist/bin/actondb
DIST_HFILES=dist/rts/rts.h \
	dist/builtin/minienv.h \
	$(addprefix dist/,$(BUILTIN_HFILES)) \
	$(subst stdlib/out/types,dist/types,$(STDLIB_HFILES))
DIST_TYFILES=$(subst stdlib/out/types,dist/types,$(STDLIB_TYFILES))
DIST_ARCHIVES=$(addprefix dist/,$(ARCHIVES))

.PHONY: distribution clean-distribution
distribution: $(DIST_BINS) $(DIST_HFILES) $(DIST_TYFILES) $(DIST_ARCHIVES)

clean-distribution:
	rm -rf dist

# == release ==
# This is where we take our distribution and turn it into a release tar ball
ARCH=$(shell uname -s -m | sed -e 's/ /-/' | tr '[A-Z]' '[a-z]')
GNU_TAR := $(shell ls --version 2>&1 | grep GNU >/dev/null 2>&1; echo $$?)
ifeq ($(GNU_TAR),0)
TAR_TRANSFORM_OPT=--transform 's,^dist,acton,'
else
TAR_TRANSFORM_OPT=-s ,^dist,acton,
endif

.PHONY: acton-$(ARCH)-$(VERSION_INFO).tar.bz2
acton-$(ARCH)-$(VERSION_INFO).tar.bz2:
	tar jcvf $@ $(TAR_TRANSFORM_OPT) --exclude .gitignore dist

.PHONY: release
release: distribution
	$(MAKE) acton-$(ARCH)-$(VERSION_INFO).tar.bz2

.PHONY: install
install:
	mkdir -p $(DESTDIR)/usr/bin $(DESTDIR)/usr/lib/acton
	cp -a dist/. $(DESTDIR)/usr/lib/acton/
	cd $(DESTDIR)/usr/bin && ln -s ../lib/acton/bin/actonc
	cd $(DESTDIR)/usr/bin && ln -s ../lib/acton/bin/actondb

debian/changelog: debian/changelog.in CHANGELOG.md
	cat $< | sed 's/VERSION/$(VERSION)/' > $@

.PHONY: debs
debs: debian/changelog
	debuild -i -us -uc -b
