include common.mk
TD:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
CHANGELOG_VERSION=$(shell grep '^\#\# \[[0-9]' CHANGELOG.md | sed 's/\#\# \[\([^]]\{1,\}\)].*/\1/' | head -n1)
GIT_VERSION_TAG=$(shell git tag --points-at HEAD 2>/dev/null | grep "v[0-9]" | sed -e 's/^v//')

PKGCONFIG=$(shell which pkg-config)
ifeq ($(PKGCONFIG),)
$(error "pkg-config must be installed")
endif

ACTONC=dist/bin/actonc
ACTC=dist/bin/actonc
ZIG_VERSION:=0.11.0-dev.2985+3f3b1a680
CC=$(TD)/deps/zig/zig cc
CXX=$(TD)/deps/zig/zig c++
ZIG=deps/zig
LIBGC=lib/libactongc-$(PLATFORM).a
export CC
export CXX

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

# TODO: remove -fno-sanitize=undefined, which is zig default as to help catch UB
CFLAGS+= -fno-sanitize=undefined -I. -I$(TD)/deps/instdir/include -Ideps -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast -Wformat -Werror=format-security
CFLAGS_REL= -O3 -DREL
CFLAGS_DEV= -g -DDEV
LDFLAGS+=-L$(TD)/lib
LDLIBS += -lActonDeps-$(PLATFORM) -lm -lpthread

# rewrite arm64 to aarch64
ifeq ($(shell uname -m),arm64)
ARCH:=aarch64
else
ARCH:=$(shell uname -m)
endif

# -- Apple Mac OS X ------------------------------------------------------------
ifeq ($(shell uname -s),Darwin)
OS:=macos
endif

PLATFORM=$(ARCH)-$(OS)


CFLAGS += -Werror
# -- Linux ---------------------------------------------------------------------
ifeq ($(shell uname -s),Linux)
OS:=linux
ifeq ($(shell uname -m),x86_64)
CFLAGS_TARGET := -target x86_64-linux-gnu.2.28
else
$(error "Unsupported architecture for Linux?")
endif
endif # -- END: Linux ----------------------------------------------------------
# NOTE: we allow UB in deps since it is not really our job to clean up... but in
# a better world?
CFLAGS_DEPS=-fno-sanitize=undefined $(CFLAGS_TARGET)
CFLAGS+=$(CFLAGS_TARGET)
export CFLAGS
export LDFLAGS

.PHONY: all
all: version-check
	$(MAKE) $(DIST_ZIG)
	$(MAKE) distribution

.PHONY: help
help:
	@echo "Available make targets:"
	@echo "  all     - build everything"
	@echo "  dist    - build complete distribution"
	@echo "  actonc  - build the Acton compiler"
	@echo "  backend - build the database backend"
	@echo "  rts     - build the Run Time System"
	@echo ""
	@echo "  test    - run the test suite"
	@echo ""
	@echo "  clean   - /normal/ clean repo"
	@echo "  clean-all - thorough cleaning"
	@echo "  clean-deps-rm - remove downloaded deps, normally never needed?"


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

DBARCHIVE=lib/libActonDB.a
ARCHIVES=lib/libActonDeps-$(PLATFORM).a lib/libactongc-$(PLATFORM).a

BUILTIN_HFILES=$(wildcard base/builtin/*.h)

DIST_BINS=$(ACTONC) dist/bin/actondb dist/bin/runacton
DIST_HFILES=\
	dist/rts/io.h \
	dist/rts/rts.h \
	dist/builtin/env.h \
	$(patsubst base/%,dist/%,$(BUILTIN_HFILES))
DIST_DBARCHIVE=$(addprefix dist/,$(DBARCHIVE))
DIST_ARCHIVES=$(addprefix dist/,$(ARCHIVES))
DIST_ZIG=dist/zig

# Listing DEPSA as prerequisites in a target means it is dependent upon at least
# one of  the external libraries that we place in libActonDeps
DEPSA:=lib/libActonDeps-$(PLATFORM).a

CFLAGS_DB = -I. -Ideps -I$(TD)/deps/instdir/include -DLOG_USE_COLOR -g
CFLAGS_DB+= $(CFLAGS_TARGET)
# TODO: enable sanitization of undefined behavior!
CFLAGS_DB+= -fno-sanitize=undefined -Werror
# TODO: clean up casts and remove this!
CFLAGS_DB+= -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast
# /backend ----------------------------------------------
backend/actondb: backend/actondb.c lib/libActonDB.a $(DEPSA)
	$(CC) -o$@ $< $(CFLAGS_DB) \
		$(LDFLAGS) \
		-lActonDB \
		$(LDLIBS)

.PHONY: base/out/rel/lib/libActonProject.a base/out/dev/lib/libActonProject.a
base/out/rel/lib/libActonProject.a: $(ACTONC) $(DEPSA)
	cd base && ../dist/bin/actonc build --auto-stub

base/out/dev/lib/libActonProject.a: $(ACTONC) $(DEPSA)
	cd base && ../dist/bin/actonc build --auto-stub --dev

base/out/types/__builtin__.ty: $(ACTONC)
	cd base && ../dist/bin/actonc src/__builtin__.act

backend/comm.o: backend/comm.c backend/comm.h backend/failure_detector/db_queries.h $(DEPSA)
	$(CC) -o$@ $< -c $(CFLAGS_DB)

backend/hash_ring.o: backend/hash_ring.c backend/hash_ring.h
	$(CC) -o$@ $< -c $(CFLAGS_DB)

backend/queue_callback.o: backend/queue_callback.c backend/queue_callback.h backend/common.h
	$(CC) -o$@ $< -c $(CFLAGS_DB)

backend/db.o: backend/db.c backend/db.h backend/skiplist.h backend/hash_ring.h backend/common.h $(DEPSA)
	$(CC) -o$@ $< -c $(CFLAGS_DB)

backend/queue.o: backend/queue.c backend/queue.h backend/queue_callback.h backend/log.h backend/failure_detector/cells.h backend/failure_detector/db_queries.h backend/common.h $(DEPSA)
	$(CC) -o$@ $< -c $(CFLAGS_DB)

backend/queue_groups.o: backend/queue_groups.c backend/queue_groups.h backend/queue_callback.h backend/skiplist.h backend/log.h backend/common.h $(DEPSA)
	$(CC) -o$@ $< -c $(CFLAGS_DB)

backend/log.o: backend/log.c
	$(CC) -o$@ $< -c $(CFLAGS_DB)

backend/skiplist.o: backend/skiplist.c backend/skiplist.h backend/log.h backend/fastrand.h $(DEPSA)
	$(CC) -o$@ $< -c $(CFLAGS_DB)

backend/txn_state.o: backend/txn_state.c backend/txn_state.h $(DEPSA)
	$(CC) -o$@ $< -c $(CFLAGS_DB)

backend/txns.o: backend/txns.c backend/txns.h $(DEPSA)
	$(CC) -o$@ $< -c $(CFLAGS_DB)

backend/client_api.o: backend/client_api.c backend/client_api.h backend/log.h backend/hashes.h $(DEPSA) $(LIBGC)
	$(CC) -o$@ $< -c $(CFLAGS_DB)

backend/failure_detector/db_messages.pb-c.o: backend/failure_detector/db_messages.pb-c.c backend/failure_detector/db_messages.pb-c.h $(DEPSA)
	$(CC) -o$@ $< -c $(CFLAGS_DB)

backend/failure_detector/cells.o: backend/failure_detector/cells.c backend/failure_detector/cells.h $(DEPSA)
	$(CC) -o$@ $< -c $(CFLAGS_DB)

backend/failure_detector/db_queries.o: backend/failure_detector/db_queries.c backend/failure_detector/db_queries.h backend/log.h backend/failure_detector/db_messages.pb-c.h $(DEPSA)
	$(CC) -o$@ $< -c $(CFLAGS_DB)

backend/failure_detector/fd.o: backend/failure_detector/fd.c backend/failure_detector/fd.h backend/failure_detector/db_messages.pb-c.h $(DEPSA)
	$(CC) -o$@ $< -c $(CFLAGS_DB)

backend/failure_detector/vector_clock.o: backend/failure_detector/vector_clock.c backend/failure_detector/vector_clock.h backend/failure_detector/db_messages.pb-c.h $(DEPSA)
	$(CC) -o$@ $< -c $(CFLAGS_DB)

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
	$(CC) -o$@ $< $(CFLAGS_DB) \
		$(LDFLAGS) \
		-lActonDB $(LDLIBS)

backend/test/%: backend/test/%.c lib/libActonDB.a
	$(CC) -o$@ $< $(CFLAGS_DB) -Ibackend \
		$(LDFLAGS) -lActonDB $(LDLIBS)

backend/test/skiplist_test: backend/test/skiplist_test.c backend/skiplist.c
	$(CC) -o$@ $^ $(CFLAGS_DB) -Ibackend \
		$(LDLIBS)

ifeq ($(ARCH),x86_64)
ZIG_ARCH_ARG=-mcpu=x86_64
endif
builder/builder: builder/build.zig $(ZIG)
	rm -rf builder/zig-cache builder/zig-out
	(echo 'const root = @import("build.zig");'; tail -n +2 deps/zig/lib/build_runner.zig) > builder/build_runner.zig
	cd builder && ../$(ZIG)/zig build-exe build_runner.zig -femit-bin=builder $(ZIG_ARCH_ARG)

# /compiler ----------------------------------------------
ACTONC_ALL_HS=$(wildcard compiler/*.hs compiler/**/*.hs)
ACTONC_TEST_HS=$(wildcard compiler/tests/*.hs)
ACTONC_HS=$(filter-out $(ACTONC_TEST_HS),$(ACTONC_ALL_HS))
# Set STATIC_ACTONC=true to build a static actonc binary. This works on Debian
# 11 and distributions of similar age while it seems to fail on newer versions.
ifeq ($(STATIC_ACTONC),true)
ACTC_GHC_OPTS=-optl-static
endif
# NOTE: we're unsetting CC to avoid using zig cc for stack / ghc, which doesn't
# seem to work properly
compiler/actonc: compiler/package.yaml.in compiler/stack.yaml dist/builder $(ACTONC_HS)
	cd compiler && unset CC && unset CFLAGS && stack build --dry-run 2>&1 | grep "Nothing to build" || \
		(sed 's,^version:.*,version:      "$(VERSION_INFO)",' < package.yaml.in > package.yaml \
		&& stack build --ghc-options='-j4 $(ACTC_GHC_OPTS)' \
		&& stack --local-bin-path=. install 2>/dev/null)

.PHONY: clean-compiler
clean-compiler:
	cd compiler && stack clean >/dev/null 2>&1 || true
	rm -f compiler/actonc compiler/package.yaml compiler/acton.cabal

# /deps --------------------------------------------------
DEPS_DIRS=deps/bsdnt deps/libbsd deps/libmd deps/libprotobuf_c deps/libutf8proc deps/libuv deps/libxml2 deps/util-linux

# libActonDeps.a
# This is an archive of all external libraries that we depend on. Each library
# comes in its own .a archive but in order to hide this and free the compiler
# from having to juggle a bunch of -l options, we bake the contents of all these
# .a archives into one big archive and link with that. We have to extract into
# subdirectories to prevent overwriting if there are name collisions between
# different library archives.

DEP_LIBS+=deps/instdir/lib/libargp.a
ifeq ($(shell uname -s),Linux)
DEP_LIBS+=deps/instdir/lib/libbsd.a
DEP_LIBS+=deps/instdir/lib/libmd.a
endif
DEP_LIBS+=deps/instdir/lib/libbsdnt.a
DEP_LIBS+=deps/instdir/lib/libpcre2-8.a
DEP_LIBS+=deps/instdir/lib/libpcre2-posix.a
DEP_LIBS+=deps/instdir/lib/libprotobuf-c.a
DEP_LIBS+=deps/instdir/lib/libutf8proc.a
DEP_LIBS+=deps/instdir/lib/libuuid.a
DEP_LIBS+=deps/instdir/lib/libuv.a
DEP_LIBS+=deps/instdir/lib/libxml2.a

DEPS_REFS=\
	$(LIBARGP_REF) \
	$(LIBBSD_REF) \
	$(LIBMD_REF) \
	$(LIBBSDNT_REF) \
	$(LIBPCRE2_REF) \
	$(LIBPROTOBUF_C_REF) \
	$(LIBUTF8PROC_REF) \
	$(LIBUUID_REF) \
	$(LIBUV_REF) \
	$(LIBXML2_REF)

DEPS_SUM:=$(shell echo $(DEPS_REFS) | shasum | cut -d' ' -f1)

.PHONY: build-deps show-deps-sum
show-deps-sum:
	@echo $(DEPS_SUM)

build-deps: deps-$(DEPS_SUM)-$(PLATFORM).tar.bz2

deps-$(DEPS_SUM)-$(PLATFORM).tar.bz2: deps-$(DEPS_SUM)-$(PLATFORM).tar
	bzip2 -9 $<

deps-$(DEPS_SUM)-$(PLATFORM).tar: export ALWAYS_BUILD=true
deps-$(DEPS_SUM)-$(PLATFORM).tar: $(DEPSA) lib/libactongc-$(PLATFORM).a
	tar cvf $@ $(DEPSA) lib/libactongc-$(PLATFORM).a
	cd deps/instdir && tar rvf ../../$@ include

deps-download/$(DEPS_SUM):
	ls deps-download/$(DEPS_SUM) >/dev/null 2>&1 || \
		(mkdir -p deps-download/$(DEPS_SUM) \
		&& curl -f -L https://github.com/actonlang/libactondeps/releases/download/deps-$(DEPS_SUM)/deps-$(DEPS_SUM)-$(PLATFORM).tar.bz2 -o deps-download/deps-$(DEPS_SUM)-$(PLATFORM).tar.bz2 \
		&& cd deps-download/$(DEPS_SUM) && tar jxf ../deps-$(DEPS_SUM)-$(PLATFORM).tar.bz2)

# Attempt downloading the deps archive or if it fails, build it locally.
# Also copy in headers from downloaded archive into deps/instdir/include, so the
# headers are available, just as if we had built the deps locally.
lib/libActonDeps-$(PLATFORM).a:
	($(MAKE) -j1 check-download-allowed deps-download/$(DEPS_SUM) \
		&& mkdir -p deps/instdir/include && cp -r deps-download/$(DEPS_SUM)/include/* deps/instdir/include/ \
		&& cp -r deps-download/$(DEPS_SUM)/lib/libActonDeps-$(PLATFORM).a $@) \
	|| ($(MAKE) lib_deps/libActonDeps-$(PLATFORM).a && cp lib_deps/libActonDeps-$(PLATFORM).a $@)

lib_deps/libActonDeps-$(PLATFORM).a: dist/zig $(DEP_LIBS)
	mkdir -p lib_deps
	for LIB in $(DEP_LIBS); do \
		LIBNAME=$$(basename $${LIB} .a); \
		mkdir -p lib_deps/$${LIBNAME}; \
		$$(cd lib_deps/$${LIBNAME} && ar x $(TD)/$${LIB}); \
	done
	cd lib_deps && ar -qc libActonDeps-$(PLATFORM).a */*.o ../deps/*.o

lib/libactongc-$(PLATFORM).a:
	($(MAKE) -j1 check-download-allowed deps-download/$(DEPS_SUM) \
		&& cp deps-download/$(DEPS_SUM)/lib/libactongc-$(PLATFORM).a $@) \
	|| ($(MAKE) deps/instdir/lib/libgc.a dist/zig && cp deps/instdir/lib/libgc.a $@)

# Check if ALWAYS_BUILD=true and if so, fail rule. Used before running a
# download target, so if this fails, we do not download and can fail over to the
# local build case.
.PHONY: check-download-allowed
check-download-allowed:
ifeq ($(ALWAYS_BUILD),true)
	false
endif

.PHONY: clean-deps
clean-deps:
	-for I in $(DEPS_DIRS); do ls $${I} >/dev/null 2>&1 && echo Cleaning $${I} && make -C $(TD)/$${I} clean; done
	-$(MAKE) -C deps/libgc/ -f Makefile.direct clean
	rm -rf deps/instdir lib/libActonDeps-$(PLATFORM).a

clean-deps-rm:
	rm -rf $(DEPS_DIRS) deps/libgc deps/zig deps/zig-*.tar* deps-download

# /deps/libargp --------------------------------------------
LIBARGP_REF=1.5.0
deps/libargp:
	ls $@ >/dev/null 2>&1 || git clone https://github.com/argp-standalone/argp-standalone.git $@

# NOTE: autoconf incantataion taken from (now removed) CI config of
# argp-standalone repo:
# https://github.com/argp-standalone/argp-standalone/commit/0297fd805e760499cdca605466851729e377169a
deps/instdir/lib/libargp.a: deps/libargp $(ZIG)
	mkdir -p $(dir $@) $(shell dirname $(dir $@))/include
	cd $< \
	&& git checkout $(LIBARGP_REF) \
	&& aclocal \
	&& autoheader \
	&& autoconf \
	&& automake --add-missing \
	&& autoreconf \
	&& ./configure --prefix=$(TD)/deps/instdir --enable-static --disable-shared CFLAGS="-I../incbsd -I$(TD)/deps/instdir/include -L$(TD)/deps/instdir/lib $(CFLAGS_DEPS)" \
	&& make -j \
	&& cp argp.h ../instdir/include/ \
	&& cp libargp.a ../instdir/lib/

# /deps/libbsdnt --------------------------------------------
LIBBSDNT_REF=97053f366618b0e987a76bc6d6992165c8ea843e
deps/libbsdnt:
	ls $@ >/dev/null 2>&1 || git clone https://github.com/wbhart/bsdnt.git $@

# NOTE: we don't pass in CFLAGS_DEPS which includes setting -target to zig,
# because the autoconf stuff in bsdnt is so old it doesn't accept this style of
# argument passing it seems? -target=foo works whereas -target foo does not. It
# seems fine for now since this is likely a pure library, not interacting
# anything with libc, but maybe we should fix it?
deps/instdir/lib/libbsdnt.a: deps/libbsdnt $(ZIG)
	mkdir -p $(dir $@) $(shell dirname $(dir $@))/include
	cd $< \
	&& git checkout $(LIBBSDNT_REF) \
	&& ./configure --prefix=$(TD)/deps/instdir --enable-static --disable-shared \
	&& make -j && make install

# /deps/libbsd --------------------------------------------
LIBBSD_REF=0.11.7
deps/libbsd:
	ls $@ >/dev/null 2>&1 || git clone https://gitlab.freedesktop.org/libbsd/libbsd.git $@

# NOTE: there's a silly copy going on in here to work around an issue with
# include paths. Files in the libbsd/src directory include things like
# <unistd.h> which are available in libbsd/include/bsd and some part of automake
# adds a -I../include/bsd argument but zig already has other paths, like
# /usr/include ahead of this path so when it's looking for unistd.h it's going
# to find the system wide one and not the one in include/bsd. While specifying
# an include directory with -I in CFLAGS adds it at the start of the search
# list, we can't do this for ../include/bsd as it considers it a duplicate arg
# and ignores it - it only gets added late in the path list. It doesn't appear
# possible to influence and add something earlier in the search path and I don't
# understand automake well enough to determine if we could somehow add it
# earlier. Thus, the only workaround I found is to use a different name, which
# we achieve simply by copying libbsd/include/bsd to libbsd/incbsd and adding
# that with -I../incbsd
deps/instdir/lib/libbsd.a: deps/libbsd deps/instdir/lib/libmd.a $(ZIG)
	mkdir -p $(dir $@) $(shell dirname $(dir $@))/include
	cd $< \
	&& git checkout $(LIBBSD_REF) \
	&& rm -rf incbsd \
	&& cp -av include/bsd incbsd \
	&& ./autogen \
	&& ./configure --prefix=$(TD)/deps/instdir --enable-static --disable-shared CFLAGS="-I../incbsd -I$(TD)/deps/instdir/include -L$(TD)/deps/instdir/lib $(CFLAGS_DEPS)" \
	&& make -j && make install

# /deps/libgc --------------------------------------------
LIBGC_REF=daea2f19089c32f38de916b8949fde42d73daf6f
deps/libgc:
	ls $@ >/dev/null 2>&1 || git clone https://github.com/ivmai/bdwgc.git $@

deps/instdir/lib/libgc.a: deps/libgc $(ZIG)
	mkdir -p $(dir $@) $(shell dirname $(dir $@))/include
	cd $< \
	&& git checkout $(LIBGC_REF) \
	&& unset CFLAGS \
	&& export CFLAGS_EXTRA="-DLARGE_CONFIG -DGC_BUILTIN_ATOMIC -DGC_THREADS -DNO_PROC_FOR_LIBRARIES -DREDIRECT_MALLOC=GC_malloc -DIGNORE_FREE -DPARALLEL_MARK $(CFLAGS_DEPS)" \
	&& make -f Makefile.direct -j base_lib \
	&& cp $(TD)/deps/libgc/libgc.a $(TD)/$@ \
	&& cp -r $(TD)/deps/libgc/include/gc* $(TD)/deps/instdir/include/

# /deps/libmd --------------------------------------------
LIBMD_REF=1.0.4
deps/libmd:
	ls $@ >/dev/null 2>&1 || git clone https://gitlab.freedesktop.org/libbsd/libmd.git $@

deps/instdir/lib/libmd.a: deps/libmd $(ZIG)
	mkdir -p $(dir $@)
	cd $< \
	&& git checkout $(LIBMD_REF) \
	&& ./autogen \
	&& ./configure --prefix=$(TD)/deps/instdir --enable-static --disable-shared CFLAGS="$(CFLAGS_DEPS)" \
	&& make -j && make install

# /deps/libprotobuf_c --------------------------------------------
LIBPROTOBUF_C_REF=abc67a11c6db271bedbb9f58be85d6f4e2ea8389
deps/libprotobuf_c:
	ls $@ >/dev/null 2>&1 || git clone https://github.com/protobuf-c/protobuf-c.git $@

deps/instdir/lib/libprotobuf-c.a: deps/libprotobuf_c $(ZIG)
	mkdir -p $(dir $@)
	cd $< \
	&& git checkout $(LIBPROTOBUF_C_REF) \
	&& ./autogen.sh \
	&& ./configure --prefix=$(TD)/deps/instdir --enable-static --disable-shared --disable-protoc CFLAGS="--verbose $(CFLAGS_DEPS)" CXXFLAGS="--verbose $(CFLAGS_TARGET)" \
	&& make -j && make install

# /deps/libutf8proc --------------------------------------
LIBUTF8PROC_REF=63f31c908ef7656415f73d6c178f08181239f74c
deps/libutf8proc:
	ls $@ >/dev/null 2>&1 || git clone https://github.com/JuliaStrings/utf8proc.git $@

deps/instdir/lib/libutf8proc.a: deps/libutf8proc $(ZIG)
	mkdir -p $(dir $@)
	cd $< \
	&& git checkout $(LIBUTF8PROC_REF) \
	&& make CFLAGS="$(CFLAGS_DEPS)" \
	&& make install prefix=$(TD)/deps/instdir

# /deps/libuuid ------------------------------------------
LIBUUID_REF=v2.38.1
deps/util-linux:
	ls $@ >/dev/null 2>&1 || git clone https://github.com/util-linux/util-linux.git $@

deps/instdir/lib/libuuid.a: deps/util-linux $(ZIG)
	mkdir -p $(dir $@)
	cd $< \
	&& git checkout $(LIBUUID_REF) \
	&& ./autogen.sh \
	&& ./configure --prefix=$(TD)/deps/instdir --disable-nls --disable-poman --disable-all-programs --enable-libuuid --enable-static --disable-shared CFLAGS="$(CFLAGS_DEPS)" \
	&& make -j && make install

# /deps/libuv --------------------------------------------
LIBUV_REF=3e7d2a649275cce3c2d43c67205e627931bda55e
deps/libuv:
	ls $@ >/dev/null 2>&1 || git clone https://github.com/libuv/libuv.git $@

deps/instdir/lib/libuv.a: deps/libuv $(ZIG)
	mkdir -p $(dir $@)
	cd $< \
	&& git checkout $(LIBUV_REF) \
	&& ./autogen.sh \
	&& ./configure --prefix=$(TD)/deps/instdir --enable-static --disable-shared CFLAGS="$(CFLAGS_DEPS)" \
	&& make -j && make install

# /deps/libxml2 ------------------------------------------
LIBXML2_REF=644a89e080bced793295f61f18aac8cfad6bece2
deps/libxml2:
	ls $@ >/dev/null 2>&1 || git clone https://github.com/GNOME/libxml2.git $@

deps/instdir/lib/libxml2.a: deps/libxml2 $(ZIG)
	mkdir -p $(dir $@)
	cd $< \
	&& git checkout $(LIBXML2_REF) \
	&& sed -i -e 's/1.16.3/1.16.1/' configure.ac \
	&& ./autogen.sh --without-python --without-iconv --without-zlib --without-lzma --prefix=$(TD)/deps/instdir --enable-static --disable-shared CFLAGS="$(CFLAGS_DEPS)" \
	&& make -j && make install \
	&& mv $(TD)/deps/instdir/include/libxml2/libxml $(TD)/deps/instdir/include/libxml

# /deps/pcre2 --------------------------------------------
LIBPCRE2_REF=pcre2-10.42
deps/pcre2:
	ls $@ >/dev/null 2>&1 || git clone https://github.com/PCRE2Project/pcre2.git $@

deps/instdir/lib/libpcre2-8.a: deps/pcre2 $(ZIG)
	mkdir -p $(dir $@)
	cd $< \
	&& git checkout $(LIBPCRE2_REF) \
	&& ./autogen.sh \
	&& ./configure --prefix=$(TD)/deps/instdir --enable-static --disable-shared CFLAGS="$(CFLAGS_DEPS)" \
	&& make -j && make install

# Dummy rule that depends on above target since it produces both libpcre2-8.a
# and libpcre2-posix.a, this is just so that the libActonDeps.a target can rely
# on libpcre2-posix.a (since it copies it into the archive) but if we don't have
# a matching target it would fail.
deps/instdir/lib/libpcre2-posix.a: deps/instdir/lib/libpcre2-8.a

# --

OFILES += deps/netstring_dev.o deps/netstring_rel.o deps/yyjson_dev.o deps/yyjson_rel.o
deps/netstring_dev.o: deps/netstring.c
	$(CC) $(CFLAGS) $(CFLAGS_DEV) -c $< -o$@

deps/netstring_rel.o: deps/netstring.c
	$(CC) $(CFLAGS) $(CFLAGS_REL) -c $< -o$@

deps/yyjson_dev.o: deps/yyjson.c
	$(CC) $(CFLAGS) $(CFLAGS_DEV) -c $< -o$@

deps/yyjson_rel.o: deps/yyjson.c
	$(CC) $(CFLAGS) $(CFLAGS_REL) -c $< -o$@

# /lib --------------------------------------------------

COMM_OFILES += backend/comm.o
DB_OFILES += backend/db.o backend/queue.o backend/skiplist.o backend/txn_state.o backend/txns.o backend/queue_callback.o backend/hash_ring.o backend/queue_groups.o
DBCLIENT_OFILES += backend/client_api.o backend/queue_callback.o backend/hash_ring.o
REMOTE_OFILES += backend/failure_detector/db_messages.pb-c.o backend/failure_detector/cells.o backend/failure_detector/db_queries.o backend/failure_detector/fd.o
VC_OFILES += backend/failure_detector/vector_clock.o
BACKEND_OFILES=$(COMM_OFILES) $(DB_OFILES) $(DBCLIENT_OFILES) $(REMOTE_OFILES) $(VC_OFILES) backend/log.o deps/netstring_rel.o deps/yyjson_rel.o
OFILES += $(BACKEND_OFILES)
lib/libActonDB.a: $(BACKEND_OFILES)
	rm -f $@
	ar rcs $@ $^

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


.PHONY: clean clean-all clean-backend clean-base
clean: clean-distribution clean-backend clean-base

clean-all: clean clean-compiler clean-deps
	rm -rf lib_deps lib/*

clean-backend:
	rm -f $(DBARCHIVE) $(BACKEND_OFILES) backend/actondb

clean-base:
	rm -rf base/build-cache base/out builder/build_runner* builder/builder* builder/zig-cache builder/zig-out
	rm -rf $(ARCHIVES) $(DBARCHIVE) $(OFILES) builtin/__builtin__.h builtin/__builtin__.c builtin/ty/out stdlib/out/

# == DIST ==
#

# We depend on libActon.a because the base/out directory will be populated as a
# result of building it, and we want to copy those files!
dist/base: base dist/lib/dev/libActon.a
	@mkdir -p $@
	cp -r base/Acton.toml base/builtin base/out base/rts base/src base/stdlib dist/base/

# This does a little hack, first copying and then moving the file in place. This
# is to avoid an error if the executable is currently running. cp tries to open
# the file and modify it, which the Linux kernel (and perhaps others?) will
# prevent if the file to be modified is an executable program that is currently
# running.  We work around it by moving / renaming the file in place instead!
dist/bin/actonc: compiler/actonc $(ZIG)
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

dist/builder: builder/builder
	@mkdir -p $(dir $@)
	cp $< $@

dist/builtin/%: base/builtin/%
	@mkdir -p $(dir $@)
	cp $< $@

dist/inc: $(DEPSA)
	@mkdir -p $(dir $@)
	cp -a deps/instdir/include $@

dist/rts/%: base/rts/%
	@mkdir -p $(dir $@)
	cp $< $@

dist/lib/%: lib/%
	@mkdir -p $(dir $@)
	cp $< $@

dist/lib/dev/libActon.a: base/out/dev/lib/libActonProject.a
	@mkdir -p $(dir $@)
	cp $< $@

dist/lib/rel/libActon.a: base/out/rel/lib/libActonProject.a
	@mkdir -p $(dir $@)
	cp $< $@

dist/lib/libActonDB.a: lib/libActonDB.a
	@mkdir -p $(dir $@)
	cp $< $@

dist/completion/acton.bash-completion: completion/acton.bash-completion
	mkdir -p $(dir $@)
	cp $< $@

dist/zig: deps/zig
	mkdir -p $(dir $@)
	ls $@ > /dev/null 2>&1 || cp -a $< $@

deps/zig: deps/zig-$(OS)-$(ARCH)-$(ZIG_VERSION).tar.xz
	mkdir -p $@
	cd $@ && tar Jx --strip-components=1 -f ../../$^
	cp -a deps/zig-extras/* $@

# Check if ZIG_VERSION contains -dev, in which case we pull down a nightly,
# otherwise its a release
deps/zig-$(OS)-$(ARCH)-$(ZIG_VERSION).tar.xz:
ifeq ($(findstring -dev,$(ZIG_VERSION)),-dev)
	curl -o $@ https://ziglang.org/builds/zig-$(OS)-$(ARCH)-$(ZIG_VERSION).tar.xz
else
	curl -o $@ https://ziglang.org/download/$(ZIG_VERSION)/zig-$(OS)-$(ARCH)-$(ZIG_VERSION).tar.xz
endif

.PHONY: distribution clean-distribution
distribution: dist/base $(DIST_ARCHIVES) dist/lib/dev/libActon.a dist/lib/rel/libActon.a dist/builder dist/inc $(DIST_BINS) $(DIST_HFILES) $(DIST_DBARCHIVE) $(DIST_ZIG)

clean-distribution:
	rm -rf dist

# == release ==
# This is where we take our distribution and turn it into a release tar ball
PLATARCH=$(shell uname -s -m | sed -e 's/ /-/' | tr '[A-Z]' '[a-z]')
GNU_TAR := $(shell sed --version 2>&1 | grep GNU >/dev/null 2>&1; echo $$?)
ifeq ($(GNU_TAR),0)
TAR_TRANSFORM_OPT=--transform 's,^dist,acton,'
else
TAR_TRANSFORM_OPT=-s ,^dist,acton,
endif

# Do grep to only get a version number. If there's an error, we get an empty
# string which is better than getting the error message itself.
ACTONC_VERSION=$(shell $(ACTONC) --numeric-version 2>/dev/null | grep -E "^[0-9.]+$$")
.PHONY: acton-$(PLATARCH)-$(ACTONC_VERSION).tar.bz2
acton-$(PLATARCH)-$(ACTONC_VERSION).tar.bz2:
	tar jcvf $@ $(TAR_TRANSFORM_OPT) --exclude .gitignore dist

.PHONY: release
release: distribution
	$(MAKE) acton-$(PLATARCH)-$(ACTONC_VERSION).tar.bz2

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
	debuild --preserve-envvar VERSION_INFO --preserve-envvar STATIC_ACTONC -i -us -uc -b
