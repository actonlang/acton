include common.mk
CHANGELOG_VERSION=$(shell grep '^\#\# \[[0-9]' CHANGELOG.md | sed 's/\#\# \[\([^]]\{1,\}\)].*/\1/' | head -n1)
GIT_VERSION_TAG=$(shell git tag --points-at HEAD 2>/dev/null | grep "v[0-9]" | sed -e 's/^v//')

PKGCONFIG=$(shell which pkg-config)
ifeq ($(PKGCONFIG),)
$(error "pkg-config must be installed")
endif

ACTONC=dist/bin/actonc
ACTC=dist/bin/actonc

# Determine which xargs we have. BSD xargs does not have --no-run-if-empty,
# rather, it is the default behavior so the argument is superfluous. We check if
# we are using GNU xargs by trying to run xargs --version and grep for 'GNU', if
# that returns 0 we are on GNU and will use 'xargs --no-run-if-empty', otherwise
# we are on BSD and will use 'xargs' straight up.
XARGS_CHECK := $(shell xargs --version 2>&1 | grep GNU >/dev/null 2>&1; echo $$?)
ifeq ($(XARGS_CHECK),0)
	XARGS := xargs --no-run-if-empty
else
	XARGS := xargs
endif

# This is the version we will stamp into actonc
BUILD_TIME=$(shell date "+%Y%m%d.%-H.%-M.%-S")
ifdef BUILD_RELEASE
export VERSION_INFO?=$(VERSION)
else
export VERSION_INFO?=$(VERSION).$(BUILD_TIME)
endif

CFLAGS+= -I. -Ideps -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast -Wformat -Werror=format-security
CFLAGS_REL= -O3 -DREL
CFLAGS_DEV= -g -DDEV
LDFLAGS+=-Llib
LDLIBS+=-lprotobuf-c_a -lm -lpthread

# look for jemalloc
JEM_LIB?=$(wildcard /usr/lib/x86_64-linux-gnu/libjemalloc.a)
ifneq ($(JEM_LIB),)
$(info Using jemalloc: $(JEM_LIB))
CFLAGS+=-DUSE_JEMALLOC
LDFLAGS+=-L$(dir $(JEM_LIB))
LDLIBS+=-ljemalloc
endif

# -- Apple Mac OS X ------------------------------------------------------------
ifeq ($(shell uname -s),Darwin)
LDLIBS+=-largp

# -- M1
ifeq ($(shell uname -m),arm64)
CFLAGS += -I/opt/homebrew/include
LDFLAGS += -L/opt/homebrew/lib
endif

# -- Intel CPU
ifeq ($(shell uname -m),x86_64)
endif

endif # -- END: Apple Mac OS X -------------------------------------------------


# -- Linux ---------------------------------------------------------------------
ifeq ($(shell uname -s),Linux)
CFLAGS += -Werror
LDLIBS+=-luuid
endif # -- END: Linux ----------------------------------------------------------

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
ifneq ($(GIT_VERSION_TAG),) # if we are on a git tag..
ifneq ($(VERSION),$(GIT_VERSION_TAG)) # ..ensure the git tag is same as version in common.mk
	$(error Current git tag ($(GIT_VERSION_TAG)) differs from version in common.mk ($(VERSION)))
endif
endif

ENV_FILES=$(wildcard builtin/env.*)
BUILTIN_HFILES=$(filter-out $(ENV_FILES),$(wildcard builtin/*.h))
BUILTIN_CFILES=$(filter-out $(ENV_FILES),$(wildcard builtin/*.c))

DBARCHIVE=lib/libActonDB.a
ARCHIVES=lib/dev/libActon.a lib/rel/libActon.a lib/libprotobuf-c_a.a lib/libutf8proc_a.a lib/libuv_a.a
ifeq ($(shell uname -s),Linux)
ARCHIVES+=lib/libbsd_a.a lib/libmd_a.a
endif

DIST_BINS=$(ACTONC) dist/bin/actondb dist/bin/runacton
DIST_HFILES=\
	dist/rts/io.h \
	dist/rts/rts.h \
	dist/builtin/env.h \
	$(addprefix dist/,$(BUILTIN_HFILES))
DIST_DBARCHIVE=$(addprefix dist/,$(DBARCHIVE))
DIST_ARCHIVES=$(addprefix dist/,$(ARCHIVES))


# /backend ----------------------------------------------
backend/actondb: backend/actondb.c lib/libActonDB.a
	$(CC) -o$@ $< $(CFLAGS) \
		$(LDFLAGS) \
		-lActonDB \
		$(LDLIBS)

backend/comm.o: backend/comm.c backend/comm.h backend/failure_detector/db_queries.h
	$(CC) -DLOG_USE_COLOR -g -o$@ $< -c $(CFLAGS)

backend/db.o: backend/db.c backend/db.h backend/skiplist.h
	$(CC) -DLOG_USE_COLOR -g -o$@ $< -c $(CFLAGS)

backend/log.o: backend/log.c
	$(CC) -DLOG_USE_COLOR -g -o$@ $< -c $(CFLAGS)

backend/queue.o: backend/queue.c backend/queue.h backend/log.h backend/failure_detector/cells.h backend/failure_detector/db_queries.h
	$(CC) -DLOG_USE_COLOR -g -o$@ $< -c $(CFLAGS)

backend/skiplist.o: backend/skiplist.c backend/skiplist.h backend/log.h backend/fastrand.h
	$(CC) -DLOG_USE_COLOR -g -o$@ $< -c $(CFLAGS)

backend/txn_state.o: backend/txn_state.c backend/txn_state.h
	$(CC) -DLOG_USE_COLOR -g -o$@ $< -c $(CFLAGS)

backend/txns.o: backend/txns.c backend/txns.h
	$(CC) -DLOG_USE_COLOR -g -o$@ $< -c $(CFLAGS)

backend/client_api.o: backend/client_api.c backend/client_api.h backend/log.h backend/hashes.h
	$(CC) -DLOG_USE_COLOR -g -o$@ $< -c $(CFLAGS)

backend/failure_detector/db_messages.pb-c.o: backend/failure_detector/db_messages.pb-c.c backend/failure_detector/db_messages.pb-c.h
	$(CC) -DLOG_USE_COLOR -g -o$@ $< -c $(CFLAGS)

backend/failure_detector/cells.o: backend/failure_detector/cells.c backend/failure_detector/cells.h
	$(CC) -DLOG_USE_COLOR -g -o$@ $< -c $(CFLAGS)

backend/failure_detector/db_queries.o: backend/failure_detector/db_queries.c backend/failure_detector/db_queries.h backend/log.h backend/failure_detector/db_messages.pb-c.h
	$(CC) -DLOG_USE_COLOR -g -o$@ $< -c $(CFLAGS)

backend/failure_detector/fd.o: backend/failure_detector/fd.c backend/failure_detector/fd.h backend/failure_detector/db_messages.pb-c.h
	$(CC) -DLOG_USE_COLOR -g -o$@ $< -c $(CFLAGS)

backend/failure_detector/vector_clock.o: backend/failure_detector/vector_clock.c backend/failure_detector/vector_clock.h backend/failure_detector/db_messages.pb-c.h
	$(CC) -DLOG_USE_COLOR -g -o$@ $< -c $(CFLAGS)

# backend tests
BACKEND_TESTS=backend/failure_detector/db_messages_test \
	backend/test/actor_ring_tests_local \
	backend/test/actor_ring_tests_remote \
	backend/test/db_unit_tests \
	backend/test/queue_unit_tests \
	backend/test/skiplist_test \
	backend/test/test_client

.PHONY: test-backend
test-backend: $(BACKEND_TESTS)
	@echo DISABLED TEST: backend/failure_detector/db_messages_test
	./backend/test/actor_ring_tests_local
	./backend/test/actor_ring_tests_remote
	./backend/test/db_unit_tests
	@echo DISABLED test: ./backend/test/queue_unit_tests
	./backend/test/skiplist_test

backend/failure_detector/db_messages_test: backend/failure_detector/db_messages_test.c lib/libActonDB.a
	$(CC) -o$@ $< $(CFLAGS) \
		$(LDFLAGS) \
		-lActonDB $(LDLIBS)

backend/test/%: backend/test/%.c lib/libActonDB.a
	$(CC) -o$@ $< $(CFLAGS) -Ibackend \
		$(LDFLAGS) -lActonDB $(LDLIBS)

backend/test/skiplist_test: backend/test/skiplist_test.c backend/skiplist.c
	$(CC) -o$@ $^ $(CFLAGS) -Ibackend \
		$(LDLIBS)

# /builtin ----------------------------------------------
builtin/builtin_dev.o: builtin/builtin.c $(BUILTIN_HFILES) $(BUILTIN_CFILES)
	$(CC) $(CFLAGS) $(CFLAGS_DEV) -Wno-unused-result -c $< -o$@

builtin/builtin_rel.o: builtin/builtin.c $(BUILTIN_HFILES) $(BUILTIN_CFILES)
	$(CC) $(CFLAGS) $(CFLAGS_REL) -Wno-unused-result -c $< -o$@

builtin/env_dev.o: builtin/env.c builtin/env.h builtin/builtin_dev.o
	$(CC) $(CFLAGS) $(CFLAGS_DEV) -c $< -o$@

builtin/env_rel.o: builtin/env.c builtin/env.h builtin/builtin_rel.o
	$(CC) $(CFLAGS) $(CFLAGS_REL) -c $< -o$@


# /compiler ----------------------------------------------
ACTONC_ALL_HS=$(wildcard compiler/*.hs compiler/**/*.hs)
ACTONC_TEST_HS=$(wildcard compiler/tests/*.hs)
ACTONC_HS=$(filter-out $(ACTONC_TEST_HS),$(ACTONC_ALL_HS))
compiler/actonc: compiler/package.yaml.in compiler/stack.yaml $(ACTONC_HS)
	cd compiler && stack build --dry-run 2>&1 | grep "Nothing to build" || \
		(sed 's,^version:.*,version:      "$(VERSION_INFO)",' < package.yaml.in > package.yaml \
		&& stack build --ghc-options -j4 \
		&& stack --local-bin-path=. install 2>/dev/null)

.PHONY: clean-compiler
clean-compiler:
	cd compiler && stack clean >/dev/null 2>&1 || true
	rm -f compiler/actonc compiler/package.yaml compiler/acton.cabal

OFILES += deps/netstring_dev.o deps/netstring_rel.o deps/yyjson_dev.o deps/yyjson_rel.o
deps/netstring_dev.o: deps/netstring.c
	$(CC) $(CFLAGS) $(CFLAGS_DEV) -c $< -o$@

deps/netstring_rel.o: deps/netstring.c
	$(CC) $(CFLAGS) $(CFLAGS_REL) -c $< -o$@

deps/yyjson_dev.o: deps/yyjson.c
	$(CC) $(CFLAGS) $(CFLAGS_DEV) -c $< -o$@

deps/yyjson_rel.o: deps/yyjson.c
	$(CC) $(CFLAGS) $(CFLAGS_REL) -c $< -o$@

# Building the builtin, rts and stdlib is a little tricky as we have to be
# careful about order. First comes the __builtin__.act file,
STDLIB_ACTFILES=$(wildcard stdlib/src/*.act stdlib/src/**/*.act)
STDLIB_ACTFILES_NS=$(filter-out stdlib/src/__builtin__.act,$(STDLIB_ACTFILES))
STDLIB_CFILES=$(wildcard stdlib/src/*.c stdlib/src/**/*.c)
STDLIB_SRCFILES=$(wildcard stdlib/src/* stdlib/src/**/* stdlib/c_src/* stdlib/c_src/**/*)
STDLIB_ACTON_MODULES=$(filter-out $(STDLIB_CFILES:.c=.act),$(STDLIB_ACTFILES_NS))
STDLIB_TYFILES=$(subst src,out/types,$(STDLIB_ACTFILES:.act=.ty))
STDLIB_TYFILES_C=$(subst src,out/types,$(STDLIB_CFILES:.c=.ty))
STDLIB_HFILES=$(subst src,out/types,$(STDLIB_ACTFILES_NS:.act=.h))
STDLIB_HFILES_C=$(subst src,out/types,$(STDLIB_CFILES:.c=.h))
STDLIB_DEV_OFILES_ACT=$(subst src,out/lib,$(STDLIB_ACTS:.act=_dev.o))
STDLIB_REL_OFILES_ACT=$(subst src,out/lib,$(STDLIB_ACTS:.act=_rel.o))
STDLIB_DEV_OFILES=$(STDLIB_DEV_OFILES_ACT)
STDLIB_REL_OFILES=$(STDLIB_REL_OFILES_ACT)
STDLIB_OFILES=$(STDLIB_DEV_OFILES) $(STDLIB_REL_OFILES)


# __builtin__.ty is special, it even has special handling in actonc. Essentially
# all other modules depend on it, so it must be compiled first.
dist/types/__builtin__.ty: builtin/ty/out/types/__builtin__.ty
	@mkdir -p $(dir $@)
	cp $< $@

builtin/ty/out/types/__builtin__.ty: builtin/ty/src/__builtin__.act $(ACTONC)
	@mkdir -p $(dir $@)
	$(ACTC) $<

# Build our standard library
stdlib/out/dev/lib/libActonProject.a: $(STDLIB_SRCFILES) dist/types/__builtin__.ty $(DIST_HFILES) $(ACTONC)
	cd stdlib && ../$(ACTC) build --dev

stdlib/out/rel/lib/libActonProject.a: $(STDLIB_SRCFILES) dist/types/__builtin__.ty $(DIST_HFILES) $(ACTONC)
	cd stdlib && ../$(ACTC) build
	cp -a stdlib/out/types/. dist/types/


# /lib --------------------------------------------------

LIBACTON_DEV_OFILES=builtin/builtin_dev.o builtin/env_dev.o rts/empty.o rts/io_dev.o rts/log.o rts/rts_dev.o deps/netstring_dev.o deps/yyjson_dev.o
OFILES += $(LIBACTON_DEV_OFILES)
lib/dev/libActon.a: stdlib/out/dev/lib/libActonProject.a $(LIBACTON_DEV_OFILES)
	@mkdir -p $(dir $@)
	cp -a $< $@
	ar rcs $@ $(filter-out stdlib/out/dev/lib/libActonProject.a,$^)

LIBACTON_REL_OFILES=$(LIBACTON_DEV_OFILES:_dev.o=_rel.o)
OFILES += $(LIBACTON_REL_OFILES)
lib/rel/libActon.a: stdlib/out/rel/lib/libActonProject.a $(LIBACTON_REL_OFILES)
	@mkdir -p $(dir $@)
	cp -a $< $@
	ar rcs $@ $(filter-out stdlib/out/rel/lib/libActonProject.a,$^)

# Include static libs
LIBBSD_LIBDIR:=$(shell pkg-config --variable=libdir libbsd 2>/dev/null)
LIBBSD_A:=$(shell ls $(LIBBSD_LIBDIR)/libbsd_a.a $(LIBBSD_LIBDIR)/libbsd.a 2>/dev/null | head -n1)
lib/libbsd_a.a: $(LIBBSD_A)
	cp $< $@

LIBMD_LIBDIR:=$(shell pkg-config --variable=libdir libmd 2>/dev/null)
LIBMD_A:=$(shell ls $(LIBMD_LIBDIR)/libmd_a.a $(LIBMD_LIBDIR)/libmd.a 2>/dev/null | head -n1)
lib/libmd_a.a: $(LIBMD_A)
	cp $< $@

LIBPROTOBUFC_LIBDIR:=$(shell pkg-config --variable=libdir libprotobuf-c 2>/dev/null)
LIBPROTOBUFC_A:=$(shell ls $(LIBPROTOBUFC_LIBDIR)/libprotobuf-c_a.a $(LIBPROTOBUFC_LIBDIR)/libprotobuf-c.a 2>/dev/null | head -n1)
lib/libprotobuf-c_a.a: $(LIBPROTOBUFC_A)
	cp $< $@

LIBUTF8PROC_LIBDIR:=$(shell pkg-config --variable=libdir libutf8proc 2>/dev/null)
LIBUTF8PROC_A:=$(shell ls $(LIBUTF8PROC_LIBDIR)/libutf8proc_a.a $(LIBUTF8PROC_LIBDIR)/libutf8proc.a 2>/dev/null | head -n1)
lib/libutf8proc_a.a: $(LIBUTF8PROC_A)
	cp $< $@

LIBUV_LIBDIR:=$(shell pkg-config --variable=libdir libuv 2>/dev/null)
LIBUV_A:=$(shell ls $(LIBUV_LIBDIR)/libuv_a.a $(LIBUV_LIBDIR)/libuv.a 2>/dev/null | head -n1)
lib/libuv_a.a: $(LIBUV_A)
	cp $< $@

COMM_OFILES += backend/comm.o rts/empty.o
DB_OFILES += backend/db.o backend/queue.o backend/skiplist.o backend/txn_state.o backend/txns.o rts/empty.o
DBCLIENT_OFILES += backend/client_api.o rts/empty.o
REMOTE_OFILES += backend/failure_detector/db_messages.pb-c.o backend/failure_detector/cells.o backend/failure_detector/db_queries.o backend/failure_detector/fd.o
VC_OFILES += backend/failure_detector/vector_clock.o
BACKEND_OFILES=$(COMM_OFILES) $(DB_OFILES) $(DBCLIENT_OFILES) $(REMOTE_OFILES) $(VC_OFILES) backend/log.o deps/netstring_rel.o deps/yyjson_rel.o
OFILES += $(BACKEND_OFILES)
lib/libActonDB.a: $(BACKEND_OFILES)
	rm -f $@
	ar rcs $@ $^


# /rts --------------------------------------------------
OFILES += rts/io_dev.o rts/io_rel.o rts/log.o rts/rts_dev.o rts/rts_rel.o rts/empty.o
rts/io_dev.o: rts/io.c rts/io.h
	$(CC) $(CFLAGS) $(CFLAGS_DEV) $(LDFLAGS) \
		-c $< -o $@

rts/io_rel.o: rts/io.c rts/io.h
	$(CC) $(CFLAGS) $(CFLAGS_REL) $(LDFLAGS) \
		-c $< -o $@

rts/log.o: rts/log.c rts/log.h
	$(CC) $(CFLAGS) $(CFLAGS_DEV) -DLOG_USE_COLOR -c $< -o$@

rts/rts_dev.o: rts/rts.c rts/rts.h
	$(CC) $(CFLAGS) $(CFLAGS_DEV) \
		-Wno-int-to-void-pointer-cast -Wno-unused-result \
		-c $< -o $@

rts/rts_rel.o: rts/rts.c rts/rts.h
	$(CC) $(CFLAGS) $(CFLAGS_REL) \
		-Wno-int-to-void-pointer-cast -Wno-unused-result \
		-c $< -o $@

rts/empty.o: rts/empty.c
	$(CC) $(CFLAGS) -g -c $< -o $@

rts/pingpong: rts/pingpong.c rts/pingpong.h rts/rts.o
	$(CC) $(CFLAGS) -Wno-int-to-void-pointer-cast \
		-lutf8proc -lActonDB \
		$(LDLIBS)
		rts/rts.o \
		builtin/builtin.o \
		builtin/env.o \
		$< \
		-o $@


# top level targets

.PHONY: backend
backend:
	$(MAKE) -C backend

.PHONY: rts
rts: $(ARCHIVES)


.PHONY: test test-builtins test-compiler test-db test-examples test-lang test-regressions test-rts test-stdlib
test:
	cd compiler && stack test
	$(MAKE) -C backend test
	$(MAKE) test-rts-db

test-builtins:
	cd compiler && stack test --ta '-p "Builtins"'

test-compiler:
	cd compiler && stack test --ta '-p "compiler"'

test-db:
	cd compiler && stack test --ta '-p "DB"'

test-examples:
	cd compiler && stack test --ta '-p "Examples"'

test-lang:
	cd compiler && stack test --ta '-p "Core language"'

test-regressions:
	cd compiler && stack test --ta '-p "Regression"'

test-rts:
	cd compiler && stack test --ta '-p "RTS"'

test-rts-db:
	$(MAKE) -C test

test-stdlib:
	cd compiler && stack test --ta '-p "stdlib"'


.PHONY: clean
clean: clean-compiler clean-distribution clean-backend clean-rts

.PHONY: clean-backend
clean-backend:
	rm -f $(DBARCHIVE) $(BACKEND_OFILES) backend/actondb

.PHONY: clean-rts
clean-rts:
	rm -rf $(ARCHIVES) $(DBARCHIVE) $(OFILES) $(STDLIB_HFILES) $(STDLIB_OFILES) $(STDLIB_TYFILES) stdlib/out/

# == DIST ==
#

# This does a little hack, first copying and then moving the file in place. This
# is to avoid an error if the executable is currently running. cp tries to open
# the file and modify it, which the Linux kernel (and perhaps others?) will
# prevent if the file to be modified is an executable program that is currently
# running.  We work around it by moving / renaming the file in place instead!
dist/bin/actonc: compiler/actonc
	@mkdir -p $(dir $@)
	cp $< $@.tmp
	mv $@.tmp $@

dist/bin/actondb: backend/actondb
	@mkdir -p $(dir $@)
	cp $< $@.tmp
	mv $@.tmp $@

dist/bin/runacton: bin/runacton
	@mkdir -p $(dir $@)
	cp $< $@.tmp
	mv $@.tmp $@

dist/builtin/%: builtin/%
	@mkdir -p $(dir $@)
	cp $< $@

dist/rts/%: rts/%
	@mkdir -p $(dir $@)
	cp $< $@

dist/types/%: stdlib/out/types/% stdlib
	@mkdir -p $(dir $@)
	cp $< $@

dist/lib/%: lib/%
	@mkdir -p $(dir $@)
	cp $< $@

dist/completion/acton.bash-completion: completion/acton.bash-completion
	mkdir -p $(dir $@)
	cp $< $@

.PHONY: distribution clean-distribution
distribution: $(DIST_ARCHIVES) $(DIST_BINS) $(DIST_HFILES) $(DIST_TYFILES) $(DIST_DBARCHIVE)

clean-distribution:
	rm -rf dist

# == release ==
# This is where we take our distribution and turn it into a release tar ball
ARCH=$(shell uname -s -m | sed -e 's/ /-/' | tr '[A-Z]' '[a-z]')
GNU_TAR := $(shell sed --version 2>&1 | grep GNU >/dev/null 2>&1; echo $$?)
ifeq ($(GNU_TAR),0)
TAR_TRANSFORM_OPT=--transform 's,^dist,acton,'
else
TAR_TRANSFORM_OPT=-s ,^dist,acton,
endif

ACTONC_VERSION=$(shell $(ACTONC) --numeric-version 2>/dev/null)
.PHONY: acton-$(ARCH)-$(ACTONC_VERSION).tar.bz2
acton-$(ARCH)-$(ACTONC_VERSION).tar.bz2:
	tar jcvf $@ $(TAR_TRANSFORM_OPT) --exclude .gitignore dist

.PHONY: release
release: distribution
	$(MAKE) acton-$(ARCH)-$(ACTONC_VERSION).tar.bz2

.PHONY: install
install:
	mkdir -p $(DESTDIR)/usr/bin $(DESTDIR)/usr/lib/acton
	cp -a dist/. $(DESTDIR)/usr/lib/acton/
	cd $(DESTDIR)/usr/bin && ln -s ../lib/acton/bin/actonc
	cd $(DESTDIR)/usr/bin && ln -s ../lib/acton/bin/actondb
	cd $(DESTDIR)/usr/bin && ln -s ../lib/acton/bin/runacton

.PHONY: debian/changelog
debian/changelog: debian/changelog.in CHANGELOG.md
	cat $< | sed 's/VERSION/$(VERSION_INFO)/' > $@

.PHONY: debs
debs: debian/changelog
	debuild --preserve-envvar VERSION_INFO -i -us -uc -b
