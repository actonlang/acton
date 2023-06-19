include common.mk
TD:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
CHANGELOG_VERSION=$(shell grep '^\#\# \[[0-9]' CHANGELOG.md | sed 's/\#\# \[\([^]]\{1,\}\)].*/\1/' | head -n1)
GIT_VERSION_TAG=$(shell git tag --points-at HEAD 2>/dev/null | grep "v[0-9]" | sed -e 's/^v//')

ZIG_LOCAL_CACHE_DIR ?= $(TD)/zig-cache
export ZIG_LOCAL_CACHE_DIR

ACTONC=dist/bin/actonc
ACTC=dist/bin/actonc
ZIG_VERSION:=0.11.0-dev.3384+00ff65357
ZIG=$(TD)/dist/zig/zig
AR=$(ZIG) ar
CC=$(ZIG) cc
CXX=$(ZIG) c++
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
CFLAGS_TARGET := -target x86_64-linux-gnu.2.27
ZIG_TARGET := -Dtarget=x86_64-linux-gnu.2.27
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
	@echo "  clean-downloads - remove downloaded deps, normally never needed?"


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

BUILTIN_HFILES=$(wildcard base/builtin/*.h)

DIST_BINS=$(ACTONC) dist/bin/actondb dist/bin/runacton
DIST_HFILES=\
	dist/rts/io.h \
	dist/rts/rts.h
DIST_ZIG=dist/zig

CFLAGS_DB = -I. -Ideps -I$(TD)/dist/depsout/include -DLOG_USE_COLOR -g
CFLAGS_DB+= $(CFLAGS_TARGET)
# TODO: enable sanitization of undefined behavior!
CFLAGS_DB+= -fno-sanitize=undefined -Werror
# TODO: clean up casts and remove this!
CFLAGS_DB+= -Wno-int-to-pointer-cast -Wno-pointer-to-int-cast

.PHONY: test-backend
test-backend: $(BACKEND_TESTS)
	@echo DISABLED TEST: backend/failure_detector/db_messages_test
	./backend/test/actor_ring_tests_local
	./backend/test/actor_ring_tests_remote
	./backend/test/db_unit_tests
	@echo DISABLED test: ./backend/test/queue_unit_tests
	./backend/test/skiplist_test

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
DEPS_DIRS += dist/deps/libargp
DEPS_DIRS += dist/deps/libbsdnt
DEPS_DIRS += dist/deps/libgc
DEPS_DIRS += dist/deps/libnetstring
DEPS_DIRS += dist/deps/pcre2
DEPS_DIRS += dist/deps/libprotobuf_c
DEPS_DIRS += dist/deps/libutf8proc
DEPS_DIRS += dist/deps/libuuid
DEPS_DIRS += dist/deps/libuv
DEPS_DIRS += dist/deps/libxml2
DEPS_DIRS += dist/deps/libyyjson

DEPS += dist/depsout/lib/libactongc.a
DEPS += dist/depsout/lib/libargp.a
DEPS += dist/depsout/lib/libbsdnt.a
DEPS += dist/depsout/lib/libpcre2.a
DEPS += dist/depsout/lib/libprotobuf-c.a
DEPS += dist/depsout/lib/libutf8proc.a
DEPS += dist/depsout/lib/libuuid.a
DEPS += dist/depsout/lib/libuv.a
DEPS += dist/depsout/lib/libxml2.a
DEPS += dist/depsout/lib/libnetstring.a
DEPS += dist/depsout/lib/libyyjson.a

.PHONE: clean-downloads
clean-downloads:
	rm -rf deps-download

# /deps/libargp --------------------------------------------
LIBARGP_REF=a2b750b4439099c223fd0fb70c902df396791fb1
deps-download/$(LIBARGP_REF).tar.gz:
	mkdir -p deps-download
	curl -f -L -o $@ https://github.com/actonlang/argp-standalone/archive/$(LIBARGP_REF).tar.gz

dist/deps/libargp: deps-download/$(LIBARGP_REF).tar.gz
	mkdir -p $@
	cd $@ && tar zx --strip-components=1 -f $(TD)/$<
	rm -rf $@/testsuite
	touch $(TD)/$@

dist/depsout/lib/libargp.a: dist/deps/libargp $(DIST_ZIG)
	cd $< && $(ZIG) build $(ZIG_TARGET) --prefix $(TD)/dist/depsout

# /deps/libbsdnt --------------------------------------------
LIBBSDNT_REF=20c727a5f390d1d4d2c22a3c5bfabb5276d34757
deps-download/$(LIBBSDNT_REF).tar.gz:
	mkdir -p deps-download
	curl -f -L -o $@ https://github.com/actonlang/bsdnt/archive/$(LIBBSDNT_REF).tar.gz

dist/deps/libbsdnt: deps-download/$(LIBBSDNT_REF).tar.gz
	mkdir -p $@
	cd $@ && tar zx --strip-components=1 -f $(TD)/$<
	touch $(TD)/$@

dist/depsout/lib/libbsdnt.a: dist/deps/libbsdnt $(DIST_ZIG)
	cd $< && $(ZIG) build $(ZIG_TARGET) --prefix $(TD)/dist/depsout

# /deps/libgc --------------------------------------------
LIBGC_REF=8ee0dc25da0a4572dc3ba706b3d26983f1928d21
deps-download/$(LIBGC_REF).tar.gz:
	mkdir -p deps-download
	curl -f -L -o $@ https://github.com/actonlang/bdwgc/archive/$(LIBGC_REF).tar.gz

dist/deps/libgc: deps-download/$(LIBGC_REF).tar.gz
	mkdir -p $@
	cd $@ && tar zx --strip-components=1 -f $(TD)/$<
	rm -rf $@/cord $@/extra $@/tests $@/tools
	touch $(TD)/$@

dist/depsout/lib/libactongc.a: dist/deps/libgc $(DIST_ZIG)
	cd $< && $(ZIG) build $(ZIG_TARGET) --prefix $(TD)/dist/depsout
	mv dist/depsout/lib/libgc.a $@

# /deps/libprotobuf_c --------------------------------------------
LIBPROTOBUF_C_REF=5499f774396953c2ef63e725e2f03a5c0bdeff73
deps-download/$(LIBPROTOBUF_C_REF).tar.gz:
	mkdir -p deps-download
	curl -f -L -o $@ https://github.com/actonlang/protobuf-c/archive/$(LIBPROTOBUF_C_REF).tar.gz

dist/deps/libprotobuf_c: deps-download/$(LIBPROTOBUF_C_REF).tar.gz
	mkdir -p $@
	cd $@ && tar zx --strip-components=1 -f $(TD)/$<
	touch $(TD)/$@

dist/depsout/lib/libprotobuf-c.a: dist/deps/libprotobuf_c $(DIST_ZIG)
	cd $< && $(ZIG) build $(ZIG_TARGET) --prefix $(TD)/dist/depsout

# /deps/libutf8proc --------------------------------------
LIBUTF8PROC_REF=3c489aea1a497b98f6cc28ea5b218181b84769e6
deps-download/$(LIBUTF8PROC_REF).tar.gz:
	mkdir -p deps-download
	curl -f -L -o $@ https://github.com/actonlang/utf8proc/archive/$(LIBUTF8PROC_REF).tar.gz

dist/deps/libutf8proc: deps-download/$(LIBUTF8PROC_REF).tar.gz
	mkdir -p $@
	cd $@ && tar zx --strip-components=1 -f $(TD)/$<
	touch $(TD)/$@

dist/depsout/lib/libutf8proc.a: dist/deps/libutf8proc $(DIST_ZIG)
	cd $< && $(ZIG) build $(ZIG_TARGET) --prefix $(TD)/dist/depsout

# /deps/libuuid ------------------------------------------
dist/deps/libuuid: deps/libuuid
	mkdir -p $(TD)/$@
	cp -a $</* $(TD)/$@

dist/depsout/lib/libuuid.a: dist/deps/libuuid $(DIST_ZIG)
	cd $< && $(ZIG) build $(ZIG_TARGET) --prefix $(TD)/dist/depsout

# /deps/libuv --------------------------------------------
LIBUV_REF=53b7649fc83f8cee6f0170b335222a759c0a26f0
deps-download/$(LIBUV_REF).tar.gz:
	mkdir -p deps-download
	curl -f -L -o $@ https://github.com/actonlang/libuv/archive/$(LIBUV_REF).tar.gz

dist/deps/libuv: deps-download/$(LIBUV_REF).tar.gz
	mkdir -p $@
	cd $@ && tar zx --strip-components=1 -f $(TD)/$< libuv-$(LIBUV_REF)/build.zig libuv-$(LIBUV_REF)/include libuv-$(LIBUV_REF)/src
	touch $(TD)/$@

dist/depsout/lib/libuv.a: dist/deps/libuv $(DIST_ZIG)
	cd $< && $(ZIG) build $(ZIG_TARGET) --prefix $(TD)/dist/depsout

# /deps/libxml2 ------------------------------------------
LIBXML2_REF=8459e725c3294d8d637317036f9d8b10860195dc
deps-download/$(LIBXML2_REF).tar.gz:
	mkdir -p deps-download
	curl -f -L -o $@ https://github.com/actonlang/libxml2/archive/$(LIBXML2_REF).tar.gz

dist/deps/libxml2: deps-download/$(LIBXML2_REF).tar.gz
	mkdir -p $@
	cd $@ && tar zx --strip-components=1 -f $(TD)/$<
	rm -rf $@/doc $@/example $@/fuzz $@/os400 $@/python $@/test*
	touch $(TD)/$@

dist/depsout/lib/libxml2.a: dist/deps/libxml2 $(DIST_ZIG)
	cd $< && $(ZIG) build $(ZIG_TARGET) --prefix $(TD)/dist/depsout

# /deps/pcre2 --------------------------------------------
LIBPCRE2_REF=ece17affd4f1d57eb148af9a39c64c1bb19b0e51
deps-download/$(LIBPCRE2_REF).tar.gz:
	mkdir -p deps-download
	curl -f -L -o $@ https://github.com/actonlang/pcre2/archive/$(LIBPCRE2_REF).tar.gz

dist/deps/pcre2: deps-download/$(LIBPCRE2_REF).tar.gz
	mkdir -p $@
	cd $@ && tar zx --strip-components=1 -f $(TD)/$<
	touch $(TD)/$@

dist/depsout/lib/libpcre2.a: dist/deps/pcre2 $(DIST_ZIG)
	cd $< && $(ZIG) build $(ZIG_TARGET) --prefix $(TD)/dist/depsout

# --
dist/deps/libnetstring: deps/libnetstring $(DIST_ZIG)
	mkdir -p $(TD)/$@
	cp -a $</* $(TD)/$@

dist/depsout/lib/libnetstring.a: dist/deps/libnetstring $(DIST_ZIG)
	cd $< && $(ZIG) build $(ZIG_TARGET) --prefix $(TD)/dist/depsout

dist/deps/libyyjson: deps/libyyjson $(DIST_ZIG)
	mkdir -p $(TD)/$@
	cp -a $</* $(TD)/$@

dist/depsout/lib/libyyjson.a: dist/deps/libyyjson $(DIST_ZIG)
	cd $< && $(ZIG) build $(ZIG_TARGET) --prefix $(TD)/dist/depsout


ifeq ($(ARCH),x86_64)
ZIG_ARCH_ARG=-mcpu=x86_64
endif
builder/builder: builder/build.zig $(ZIG_DEP) $(DEPS_DIRS)
	rm -rf builder/zig-cache builder/zig-out
	(echo 'const root = @import("build.zig");'; tail -n +2 dist/zig/lib/build_runner.zig) > builder/build_runner.zig
	cd builder && $(ZIG) build-exe build_runner.zig -femit-bin=builder $(ZIG_ARCH_ARG)

.PHONY: base/out/rel/lib/libActon.a base/out/dev/lib/libActon.a
base/out/rel/lib/libActon.a: $(ACTONC) $(DEPS)
	cd base && ../dist/bin/actonc build --auto-stub --ccmd

base/out/dev/lib/libActon.a: $(ACTONC) $(DEPS)
	cd base && ../dist/bin/actonc build --auto-stub --dev --ccmd

base/out/types/__builtin__.ty: $(ACTONC)
	cd base && ../dist/bin/actonc src/__builtin__.act


# top level targets

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


.PHONY: clean clean-all clean-base
clean: clean-distribution clean-base

clean-all: clean clean-compiler
	rm -rf lib/* $(ZIG_LOCAL_CACHE_DIR)

clean-base:
	rm -rf base/build-cache base/out builder/build_runner* builder/builder* builder/zig-cache builder/zig-out
	rm -rf $(OFILES) builtin/__builtin__.h builtin/__builtin__.c builtin/ty/out stdlib/out/

# == DIST ==
#

BACKEND_FILES = backend/build.zig $(wildcard backend/*.c backend/*.h backend/failure_detector/*.c backend/failure_detector/*.h)
DIST_BACKEND_FILES = $(addprefix dist/,$(BACKEND_FILES)) dist/backend/deps dist/bin/actondb
dist/backend%: backend/%
	mkdir -p $(dir $@)
	cp -a $< $@

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
dist/bin/actonc: compiler/actonc $(DIST_ZIG)
	@mkdir -p $(dir $@)
	cp $< $@.tmp
	mv $@.tmp $@

#
dist/bin/actondb: $(DIST_ZIG) $(DEPS) $(DIST_INC)
	$(ZIG) build --build-file $(TD)/dist/backend/build.zig $(ZIG_TARGET) --prefix $(TD)/dist/depsout -Dsyspath_include=$(TD)/dist/depsout/include
	mv $(TD)/dist/depsout/bin/actondb $@
	rmdir $(TD)/dist/depsout/bin

dist/bin/runacton: bin/runacton
	@mkdir -p $(dir $@)
	cp $< $@.tmp
	mv $@.tmp $@

dist/builder: builder/builder
	@mkdir -p $@
	cp -a builder/builder builder/*.zig $@/

DIST_DEPS=$(addprefix dist/deps/,libargp libbsdnt libgc libnetstring libprotobuf_c libutf8proc libuuid libuv libxml2 libyyjson pcre2)
dist/deps/%: deps/% $(DEPS)
	@mkdir -p $(dir $@)
	cp -a $< $@

dist/rts/%: base/rts/%
	@mkdir -p $(dir $@)
	cp $< $@

dist/lib/dev/libActon.a: base/out/dev/lib/libActon.a
	@mkdir -p $(dir $@)
	cp $< $@

dist/lib/rel/libActon.a: base/out/rel/lib/libActon.a
	@mkdir -p $(dir $@)
	cp $< $@

dist/completion/acton.bash-completion: completion/acton.bash-completion
	mkdir -p $(dir $@)
	cp $< $@

dist/zig: deps-download/zig-$(OS)-$(ARCH)-$(ZIG_VERSION).tar.xz
	mkdir -p $@
	cd $@ && tar Jx --strip-components=1 -f ../../$^
	rm -rf $@/doc $@/lib/libcxx $@/lib/libcxxabi $@/lib/libc/include/any-windows-any
	cp -a deps/zig-extras/* $@


# Check if ZIG_VERSION contains -dev, in which case we pull down a nightly,
# otherwise its a release
deps-download/zig-$(OS)-$(ARCH)-$(ZIG_VERSION).tar.xz:
	mkdir -p deps-download
ifeq ($(findstring -dev,$(ZIG_VERSION)),-dev)
	curl -o $@ https://ziglang.org/builds/zig-$(OS)-$(ARCH)-$(ZIG_VERSION).tar.xz
else
	curl -o $@ https://ziglang.org/download/$(ZIG_VERSION)/zig-$(OS)-$(ARCH)-$(ZIG_VERSION).tar.xz
endif

.PHONY: distribution clean-distribution
distribution: dist/base $(DIST_BACKEND_FILES) $(DEPSA) dist/lib/dev/libActon.a dist/lib/rel/libActon.a dist/builder $(DIST_INC) $(DIST_BINS) $(DIST_HFILES) $(DIST_ZIG)
	$(MAKE) $(DIST_DEPS)

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
.PHONY: acton-$(PLATARCH)-$(ACTONC_VERSION).tar.xz
acton-$(PLATARCH)-$(ACTONC_VERSION).tar.xz:
	tar cv $(TAR_TRANSFORM_OPT) --exclude .gitignore dist | xz -z -0 --threads=0 > $@

.PHONY: release
release: distribution
	$(MAKE) acton-$(PLATARCH)-$(ACTONC_VERSION).tar.xz

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
