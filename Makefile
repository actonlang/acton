include common.mk
TD:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
CHANGELOG_VERSION=$(shell grep '^\#\# \[[0-9]' CHANGELOG.md | sed 's/\#\# \[\([^]]\{1,\}\)].*/\1/' | head -n1)
GIT_VERSION_TAG=$(shell git tag --points-at HEAD 2>/dev/null | grep "v[0-9]" | sed -e 's/^v//')

ZIG_LOCAL_CACHE_DIR ?= $(TD)/zig-cache
export ZIG_LOCAL_CACHE_DIR

ACTON=$(TD)/dist/bin/acton
ACTONC=dist/bin/actonc
ACTC=$(TD)/dist/bin/actonc
ZIG_VERSION:=0.12.0-dev.2236+32e88251e
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
export DEB_DIST=stable
else
export VERSION_INFO?=$(VERSION).$(BUILD_TIME)
export DEB_DIST=tip
endif

ifdef CPEDANTIC
CPEDANTIC=--cpedantic
endif

# rewrite arm64 to aarch64
ifeq ($(shell uname -m),arm64)
ARCH:=aarch64
else
ARCH:=$(shell uname -m)
endif

# -- Apple Mac OS X ------------------------------------------------------------
ifeq ($(shell uname -s),Darwin)
OS:=macos
ifeq ($(shell uname -m),arm64)
ZIG_CPU := -Dcpu=apple_a15
endif
endif

# -- Linux ---------------------------------------------------------------------
ifeq ($(shell uname -s),Linux)
OS:=linux
ifeq ($(shell uname -m),x86_64)
ZIG_TARGET := -Dtarget=x86_64-linux-gnu.2.27
ZIG_CPU := -Dcpu=skylake
ACTONC_TARGET := --target x86_64-linux-gnu.2.27
else
$(error "Unsupported architecture for Linux?")
endif
endif # -- END: Linux ----------------------------------------------------------

.PHONY: all
all: version-check
	$(MAKE) $(DIST_ZIG)
	$(MAKE) distribution

.PHONY: help
help:
	@echo "Available make targets:"
	@echo "  all     - build everything"
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
# NOTE: we're unsetting CC & CXX to avoid using zig cc & zig c++ for stack /
# ghc, which doesn't seem to work properly
compiler/actonc: compiler/package.yaml.in compiler/stack.yaml dist/builder $(ACTONC_HS)
	cd compiler && unset CC && unset CXX && unset CFLAGS && stack build --dry-run 2>&1 | grep "Nothing to build" || \
		(sed 's,^version:.*,version:      "$(VERSION_INFO)",' < package.yaml.in > package.yaml \
		&& stack build $(STACK_OPTS) --ghc-options='-j4 $(ACTC_GHC_OPTS)' \
		&& stack --local-bin-path=. install 2>/dev/null)

.PHONY: clean-compiler
clean-compiler:
	cd compiler && stack clean >/dev/null 2>&1 || true
	rm -f compiler/actonc compiler/package.yaml compiler/acton.cabal

# /deps --------------------------------------------------
DEPS_DIRS += dist/deps/mbedtls
DEPS_DIRS += dist/deps/libargp
DEPS_DIRS += dist/deps/libbsdnt
DEPS_DIRS += dist/deps/libgc
DEPS_DIRS += dist/deps/libnetstring
DEPS_DIRS += dist/deps/pcre2
DEPS_DIRS += dist/deps/libprotobuf_c
DEPS_DIRS += dist/deps/tlsuv
DEPS_DIRS += dist/deps/libutf8proc
DEPS_DIRS += dist/deps/libuuid
DEPS_DIRS += dist/deps/libuv
DEPS_DIRS += dist/deps/libxml2
DEPS_DIRS += dist/deps/libyyjson
DEPS_DIRS += dist/deps/libsnappy_c

# TODO: depend on include file rather than lib directory and remove lib files from distribution?
DEPS += dist/depsout/lib/libactongc.a
DEPS += dist/depsout/lib/libargp.a
DEPS += dist/depsout/lib/libbsdnt.a
DEPS += dist/depsout/lib/libmbedcrypto.a
DEPS += dist/depsout/lib/libmbedtls.a
DEPS += dist/depsout/lib/libmbedx509.a
DEPS += dist/depsout/lib/libpcre2.a
DEPS += dist/depsout/lib/libprotobuf-c.a
DEPS += dist/depsout/lib/libtlsuv.a
DEPS += dist/depsout/lib/libutf8proc.a
DEPS += dist/depsout/lib/libuuid.a
DEPS += dist/depsout/lib/libuv.a
DEPS += dist/depsout/lib/libxml2.a
DEPS += dist/depsout/lib/libnetstring.a
DEPS += dist/depsout/lib/libyyjson.a
DEPS += dist/depsout/lib/libsnappy-c.a

.PHONE: clean-downloads
clean-downloads:
	rm -rf deps-download

# /deps/libargp --------------------------------------------
LIBARGP_REF=a0e41ac937fe5d5d573fdeb16353c87e4d1e3998
deps-download/$(LIBARGP_REF).tar.gz:
	mkdir -p deps-download
	curl -f -L -o $@ https://github.com/actonlang/argp-standalone/archive/$(LIBARGP_REF).tar.gz

dist/deps/libargp: deps-download/$(LIBARGP_REF).tar.gz
	mkdir -p $@
	cd $@ && tar zx --strip-components=1 -f $(TD)/$<
	rm -rf $@/testsuite
	touch $(TD)/$@

dist/depsout/lib/libargp.a: dist/deps/libargp $(DIST_ZIG)
	cd $< && $(ZIG) build $(ZIG_TARGET) $(ZIG_CPU) --prefix $(TD)/dist/depsout

# /deps/libbsdnt --------------------------------------------
LIBBSDNT_REF=1f31f6903378022fec8f09a3b3f3446e961eacbc
deps-download/$(LIBBSDNT_REF).tar.gz:
	mkdir -p deps-download
	curl -f -L -o $@ https://github.com/actonlang/bsdnt/archive/$(LIBBSDNT_REF).tar.gz

dist/deps/libbsdnt: deps-download/$(LIBBSDNT_REF).tar.gz
	mkdir -p $@
	cd $@ && tar zx --strip-components=1 -f $(TD)/$<
	touch $(TD)/$@

dist/depsout/lib/libbsdnt.a: dist/deps/libbsdnt $(DIST_ZIG)
	cd $< && $(ZIG) build $(ZIG_TARGET) $(ZIG_CPU) --prefix $(TD)/dist/depsout

# /deps/libgc --------------------------------------------
LIBGC_REF=16ad56e4ba1cc71b6683aca5558405612bf7c259
deps-download/$(LIBGC_REF).tar.gz:
	mkdir -p deps-download
	curl -f -L -o $@ https://github.com/actonlang/bdwgc/archive/$(LIBGC_REF).tar.gz

dist/deps/libgc: deps-download/$(LIBGC_REF).tar.gz
	mkdir -p $@
	cd $@ && tar zx --strip-components=1 -f $(TD)/$<
	rm -rf $@/cord $@/tests $@/tools
	touch $(TD)/$@

dist/depsout/lib/libactongc.a: dist/deps/libgc $(DIST_ZIG)
	cd $< && $(ZIG) build $(ZIG_TARGET) $(ZIG_CPU) --prefix $(TD)/dist/depsout -DBUILD_SHARED_LIBS=false -Denable_large_config -Denable_mmap
	mv dist/depsout/lib/libgc.a $@

# /deps/libmbedtls --------------------------------------------
LIBMBEDTLS_REF=9009ebc3d89f4acc43267280b7db637c4a4c7c51
deps-download/$(LIBMBEDTLS_REF).tar.gz:
	mkdir -p deps-download
	curl -f -L -o $@ https://github.com/actonlang/mbedtls/archive/$(LIBMBEDTLS_REF).tar.gz

dist/deps/mbedtls: deps-download/$(LIBMBEDTLS_REF).tar.gz
	mkdir -p $@
	cd $@ && tar zx --strip-components=1 -f $(TD)/$<
	touch $(TD)/$@

dist/depsout/lib/libmbedtls.a: dist/deps/mbedtls $(DIST_ZIG)
	cd $< && $(ZIG) build $(ZIG_TARGET) $(ZIG_CPU) --prefix $(TD)/dist/depsout
dist/depsout/lib/libmbedcrypto.a: dist/depsout/lib/libmbedtls.a
dist/depsout/lib/libmbedx509.a: dist/depsout/lib/libmbedtls.a

# /deps/libprotobuf_c --------------------------------------------
LIBPROTOBUF_C_REF=054ad7def7a9d6132a85747a746dde122c98b78d
deps-download/$(LIBPROTOBUF_C_REF).tar.gz:
	mkdir -p deps-download
	curl -f -L -o $@ https://github.com/actonlang/protobuf-c/archive/$(LIBPROTOBUF_C_REF).tar.gz

dist/deps/libprotobuf_c: deps-download/$(LIBPROTOBUF_C_REF).tar.gz
	mkdir -p $@
	cd $@ && tar zx --strip-components=1 -f $(TD)/$<
	touch $(TD)/$@

dist/depsout/lib/libprotobuf-c.a: dist/deps/libprotobuf_c $(DIST_ZIG)
	cd $< && $(ZIG) build $(ZIG_TARGET) $(ZIG_CPU) --prefix $(TD)/dist/depsout

# /deps/tlsuv ---------------------------------------------
TLSUV_REF=f30ff355c06616b050002a12889b2cde4beccb19
deps-download/$(TLSUV_REF).tar.gz:
	mkdir -p deps-download
	curl -f -L -o $@ https://github.com/actonlang/tlsuv/archive/$(TLSUV_REF).tar.gz

dist/deps/tlsuv: deps-download/$(TLSUV_REF).tar.gz
	mkdir -p $@
	cd $@ && tar zx --strip-components=1 -f $(TD)/$<
	touch $(TD)/$@

dist/depsout/lib/libtlsuv.a: dist/deps/tlsuv $(DIST_ZIG) dist/depsout/lib/libmbedtls.a dist/depsout/lib/libuv.a
	cd $< && $(ZIG) build $(ZIG_TARGET) $(ZIG_CPU) --prefix $(TD)/dist/depsout --search-prefix $(TD)/dist/depsout

# /deps/libutf8proc --------------------------------------
LIBUTF8PROC_REF=ca34e8c7db7c7c28bddcf2a821d3c02e259c2215
deps-download/$(LIBUTF8PROC_REF).tar.gz:
	mkdir -p deps-download
	curl -f -L -o $@ https://github.com/actonlang/utf8proc/archive/$(LIBUTF8PROC_REF).tar.gz

dist/deps/libutf8proc: deps-download/$(LIBUTF8PROC_REF).tar.gz
	mkdir -p $@
	cd $@ && tar zx --strip-components=1 -f $(TD)/$<
	touch $(TD)/$@

dist/depsout/lib/libutf8proc.a: dist/deps/libutf8proc $(DIST_ZIG)
	cd $< && $(ZIG) build $(ZIG_TARGET) $(ZIG_CPU) --prefix $(TD)/dist/depsout

# /deps/libuuid ------------------------------------------
dist/deps/libuuid: deps/libuuid
	mkdir -p $(TD)/$@
	cp -a $</* $(TD)/$@

dist/depsout/lib/libuuid.a: dist/deps/libuuid $(DIST_ZIG)
	cd $< && $(ZIG) build $(ZIG_TARGET) $(ZIG_CPU) --prefix $(TD)/dist/depsout

# /deps/libuv --------------------------------------------
LIBUV_REF=f85e52114e5149a3108123a601061d65e9c209be
deps-download/$(LIBUV_REF).tar.gz:
	mkdir -p deps-download
	curl -f -L -o $@ https://github.com/actonlang/libuv/archive/$(LIBUV_REF).tar.gz

dist/deps/libuv: deps-download/$(LIBUV_REF).tar.gz
	mkdir -p $@
	cd $@ && tar zx --strip-components=1 -f $(TD)/$< libuv-$(LIBUV_REF)/build.zig libuv-$(LIBUV_REF)/include libuv-$(LIBUV_REF)/src
	touch $(TD)/$@

dist/depsout/lib/libuv.a: dist/deps/libuv $(DIST_ZIG)
	cd $< && $(ZIG) build $(ZIG_TARGET) $(ZIG_CPU) --prefix $(TD)/dist/depsout

# /deps/libxml2 ------------------------------------------
LIBXML2_REF=14100c19fd1d754bb39c9584fe223b2e2e1ffe77
deps-download/$(LIBXML2_REF).tar.gz:
	mkdir -p deps-download
	curl -f -L -o $@ https://github.com/actonlang/libxml2/archive/$(LIBXML2_REF).tar.gz

dist/deps/libxml2: deps-download/$(LIBXML2_REF).tar.gz
	mkdir -p $@
	cd $@ && tar zx --strip-components=1 -f $(TD)/$<
	rm -rf $@/doc $@/example $@/fuzz $@/os400 $@/python $@/test*
	touch $(TD)/$@

dist/depsout/lib/libxml2.a: dist/deps/libxml2 $(DIST_ZIG)
	cd $< && $(ZIG) build $(ZIG_TARGET) $(ZIG_CPU) --prefix $(TD)/dist/depsout

# /deps/pcre2 --------------------------------------------
LIBPCRE2_REF=369f485229f211d596d742b69d3c742a65436b41
deps-download/$(LIBPCRE2_REF).tar.gz:
	mkdir -p deps-download
	curl -f -L -o $@ https://github.com/actonlang/pcre2/archive/$(LIBPCRE2_REF).tar.gz

dist/deps/pcre2: deps-download/$(LIBPCRE2_REF).tar.gz
	mkdir -p $@
	cd $@ && tar zx --strip-components=1 -f $(TD)/$<
	touch $(TD)/$@

dist/depsout/lib/libpcre2.a: dist/deps/pcre2 $(DIST_ZIG)
	cd $< && $(ZIG) build $(ZIG_TARGET) $(ZIG_CPU) --prefix $(TD)/dist/depsout

# /deps/libsnappy_c --------------------------------------------
LIBSNAPPY_C_REF=519ab55d5338c47d85452c6b0fa97ef2d0e96396
deps-download/$(LIBSNAPPY_C_REF).tar.gz:
	mkdir -p deps-download
	curl -f -L -o $@ https://github.com/actonlang/snappy/archive/$(LIBSNAPPY_C_REF).tar.gz

dist/deps/libsnappy_c: deps-download/$(LIBSNAPPY_C_REF).tar.gz
	mkdir -p $@
	cd $@ && tar zx --strip-components=1 -f $(TD)/$<
	touch $(TD)/$@

dist/depsout/lib/libsnappy-c.a: dist/deps/libsnappy_c $(DIST_ZIG)
	cd $< && $(ZIG) build $(ZIG_TARGET) $(ZIG_CPU) --prefix $(TD)/dist/depsout


# --
dist/deps/libnetstring: deps/libnetstring $(DIST_ZIG)
	mkdir -p $(TD)/$@
	cp -a $</* $(TD)/$@

dist/depsout/lib/libnetstring.a: dist/deps/libnetstring $(DIST_ZIG)
	cd $< && $(ZIG) build $(ZIG_TARGET) $(ZIG_CPU) --prefix $(TD)/dist/depsout

dist/deps/libyyjson: deps/libyyjson $(DIST_ZIG)
	mkdir -p $(TD)/$@
	cp -a $</* $(TD)/$@

dist/depsout/lib/libyyjson.a: dist/deps/libyyjson $(DIST_ZIG)
	cd $< && $(ZIG) build $(ZIG_TARGET) $(ZIG_CPU) --prefix $(TD)/dist/depsout


ifeq ($(ARCH),x86_64)
ZIG_ARCH_ARG=-mcpu=x86_64
endif
builder/builder: builder/build.zig backend/build.zig base/build.zig $(ZIG_DEP) $(DEPS_DIRS)
	rm -rf builder/zig-cache builder/zig-out
	(echo 'const root = @import("build.zig");'; tail -n +2 dist/zig/lib/build_runner.zig | sed -e 's/@dependencies/dependencies.zig/') > builder/build_runner.zig
	cd builder && $(ZIG) build-exe build_runner.zig -femit-bin=builder $(ZIG_ARCH_ARG)

.PHONY: base/out/types/__builtin__.ty
base/out/types/__builtin__.ty: $(ACTONC) $(DEPS)
	cd base && ../dist/bin/actonc build --auto-stub --skip-build $(CPEDANTIC)

# top level targets

.PHONY: test test-builtins test-compiler test-db test-examples test-lang test-regressions test-rts test-stdlib
test:
	cd compiler && stack test
	$(MAKE) test-stdlib
	$(MAKE) -C backend test
	$(MAKE) test-rts-db

test-builtins:
	cd compiler && stack test --ta '-p "Builtins"'

test-compiler:
	cd compiler && stack test --ta '-p "compiler"'

test-typeerrors:
	cd compiler && stack test --ta '-p "type errors"'

test-typeerrors-accept:
	cd compiler && stack runghc -- test.hs -p "type errors" --accept

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

test-stdlib: dist/bin/acton
	cd compiler && stack test --ta '-p "stdlib"'
	cd test/stdlib_tests && $(ACTON) test


.PHONY: clean clean-all clean-base
clean: clean-cli clean-distribution clean-base

clean-cli:
	rm -rf cli/out cli/build-cache

clean-all: clean clean-compiler
	rm -rf lib/* $(ZIG_LOCAL_CACHE_DIR)

clean-base:
	rm -rf base/build-cache base/out builder/build_runner* builder/builder* builder/zig-cache builder/zig-out

bin/acton: cli/out/bin/acton
	cp -a $< $@

cli/out/bin/acton: distribution1
	cd cli && $(ACTC) build $(ACTONC_TARGET)

# == DIST ==
#

BACKEND_FILES = backend/build.zig $(wildcard backend/*.c backend/*.h backend/failure_detector/*.c backend/failure_detector/*.h)
DIST_BACKEND_FILES = $(addprefix dist/,$(BACKEND_FILES)) dist/backend/deps dist/bin/actondb
dist/backend%: backend/%
	mkdir -p $(dir $@)
	cp -a $< $@

# We depend on __builtin__.ty because the base/out directory will be populated
# as a result of building it, and we want to copy those files!
.PHONY: dist/base
dist/base: base dist/base/out/types/__builtin__.ty
	@mkdir -p $@ $@/out
	cp -a base/Acton.toml base/builtin base/rts base/src base/stdlib dist/base/
	cp -a base/out/types dist/base/out/

dist/bin/acton: bin/acton
	@mkdir -p $(dir $@)
	cp -a $< $@.tmp
	mv $@.tmp $@

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
	@mkdir -p $(dir $@)
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

DIST_DEPS=$(addprefix dist/deps/,libargp libbsdnt libgc libnetstring libprotobuf_c libutf8proc libuuid libuv libxml2 libyyjson pcre2 libsnappy_c)
dist/deps/%: deps/% $(DEPS)
	@mkdir -p $(dir $@)
	cp -a $< $@

dist/rts/%: base/rts/%
	@mkdir -p $(dir $@)
	cp $< $@

dist/base/out/types/__builtin__.ty: base/out/types/__builtin__.ty
	@mkdir -p $(dir $@)
	cp -a $< $@

dist/completion/acton.bash-completion: completion/acton.bash-completion
	mkdir -p $(dir $@)
	cp $< $@

dist/zig: deps-download/zig-$(OS)-$(ARCH)-$(ZIG_VERSION).tar.xz
	mkdir -p $@
	cd $@ && tar Jx --strip-components=1 -f ../../$^
	rm -rf $@/doc
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

.PHONY: distribution1 distribution clean-distribution
distribution1: dist/base $(DIST_BACKEND_FILES) $(DEPSA) dist/builder $(DIST_INC) $(DIST_BINS) $(DIST_HFILES) $(DIST_ZIG)
	$(MAKE) $(DIST_DEPS)

distribution: dist/bin/acton

clean-distribution:
	rm -rf dist

# == release ==
# This is where we take our distribution and turn it into a release tar ball
GNU_TAR := $(shell sed --version 2>&1 | grep GNU >/dev/null 2>&1; echo $$?)
ifeq ($(GNU_TAR),0)
TAR_TRANSFORM_OPT=--transform 's,^dist,acton,'
else
TAR_TRANSFORM_OPT=-s ,^dist,acton,
endif

# Do grep to only get a version number. If there's an error, we get an empty
# string which is better than getting the error message itself.
ACTONC_VERSION=$(shell $(ACTONC) --numeric-version 2>/dev/null | grep -E "^[0-9.]+$$")
.PHONY: acton-$(OS)-$(ARCH)-$(ACTONC_VERSION).tar.xz
acton-$(OS)-$(ARCH)-$(ACTONC_VERSION).tar.xz:
	tar cv $(TAR_TRANSFORM_OPT) --exclude .gitignore dist | xz -z -0 --threads=0 > $@

.PHONY: release
release: distribution
	$(MAKE) acton-$(OS)-$(ARCH)-$(ACTONC_VERSION).tar.xz

# This target is used by the debian packaging
.PHONY: install
install:
	mkdir -p $(DESTDIR)/usr/bin $(DESTDIR)/usr/lib/acton
	cp -a dist/. $(DESTDIR)/usr/lib/acton/
	cd $(DESTDIR)/usr/bin && ln -s ../lib/acton/bin/acton
	cd $(DESTDIR)/usr/bin && ln -s ../lib/acton/bin/actonc
	cd $(DESTDIR)/usr/bin && ln -s ../lib/acton/bin/actondb
	cd $(DESTDIR)/usr/bin && ln -s ../lib/acton/bin/runacton

.PHONY: debian/changelog
debian/changelog: debian/changelog.in CHANGELOG.md
	cat $< | sed -e 's/VERSION/$(VERSION_INFO)/' -e 's/DEB_DIST/$(DEB_DIST)/' > $@

.PHONY: debs
debs: debian/changelog
	debuild --preserve-envvar VERSION_INFO -i -us -uc -b
