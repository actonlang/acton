include version.mk
TD := $(CURDIR)
CHANGELOG_VERSION=$(shell grep '^\#\# \[[0-9]' CHANGELOG.md | sed 's/\#\# \[\([^]]\{1,\}\)].*/\1/' | head -n1)
GIT_VERSION_TAG=$(shell git tag --points-at HEAD 2>/dev/null | grep "v[0-9]" | sed -e 's/^v//')

ifdef HOME
ZIG_LOCAL_CACHE_DIR ?= $(HOME)/.cache/acton/zig-local-cache
else
# TODO: Windows?
ZIG_LOCAL_CACHE_DIR ?= $(TD)/zig-cache
endif
export ZIG_LOCAL_CACHE_DIR

ACTON=$(TD)/dist/bin/acton
ACTONC=dist/bin/actonc
ACTC=$(TD)/dist/bin/actonc
ZIG_VERSION:=0.13.0
ZIG=$(TD)/dist/zig/zig
CURL:=curl --fail --location --retry 5 --retry-delay 2 --retry-max-time 120 --retry-all-errors --retry-connrefused
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
export CONTAINER_TAG?=$(VERSION)
else
export VERSION_INFO?=$(VERSION).$(BUILD_TIME)
export DEB_DIST=tip
export CONTAINER_TAG?=tip
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
endif

# -- Linux ---------------------------------------------------------------------
ifeq ($(shell uname -s),Linux)
OS:=linux
ifeq ($(shell uname -m),x86_64)
ACTONC_TARGET := --target x86_64-linux-gnu.2.27
else ifeq ($(shell uname -m),aarch64)
ACTONC_TARGET := --target aarch64-linux-gnu.2.27
else
$(error "Unsupported architecture for Linux?" $(shell uname -m))
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
	$(error Version in version.mk ($(VERSION)) differs from last version in CHANGELOG.md ($(CHANGELOG_VERSION)))
endif
ifneq ($(GIT_VERSION_TAG),) # if we are on a git tag..
ifneq ($(VERSION),$(GIT_VERSION_TAG)) # ..ensure the git tag is same as version in version.mk
	$(error Current git tag ($(GIT_VERSION_TAG)) differs from version in version.mk ($(VERSION)))
endif
endif

BUILTIN_HFILES=$(wildcard base/builtin/*.h)

DIST_BINS=$(ACTONC) dist/bin/actondb dist/bin/runacton dist/bin/lsp-server-acton
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
ACTONC_HS=$(wildcard compiler/lib/src/*.hs compiler/lib/src/*/*.hs compiler/actonc/Main.hs)
ACTONLSP_HS=$(wildcard compiler/lsp-server/*.hs)
# NOTE: we're unsetting CC & CXX to avoid using zig cc & zig c++ for stack /
# ghc, which doesn't seem to work properly
dist/bin/actonc: compiler/lib/package.yaml.in compiler/actonc/package.yaml.in compiler/lsp-server/package.yaml.in compiler/stack.yaml $(ACTONC_HS) $(ACTONLSP_HS) version.mk
	mkdir -p dist/bin
	cd compiler && sed 's,^version: BUILD_VERSION,version: "$(VERSION)",' < lib/package.yaml.in > lib/package.yaml
	cd compiler && unset CC && unset CXX && unset CFLAGS && stack build actonc lsp-server-acton --dry-run 2>&1 | grep "Nothing to build" || \
		(sed 's,^version: BUILD_VERSION,version: "$(VERSION_INFO)",' < actonc/package.yaml.in > actonc/package.yaml \
		&& sed 's,^version: BUILD_VERSION,version: "$(VERSION_INFO)",' < lsp-server/package.yaml.in > lsp-server/package.yaml \
		&& stack build actonc lsp-server-acton $(STACK_OPTS) --ghc-options='-j4 $(ACTC_GHC_OPTS)')
	cd compiler && unset CC && unset CXX && unset CFLAGS && stack --local-bin-path=../dist/bin install actonc lsp-server-acton

dist/bin/lsp-server-acton: dist/bin/actonc
	@true

.PHONY: clean-compiler
clean-compiler:
	cd compiler && stack clean >/dev/null 2>&1 || true
	rm -f dist/bin/actonc compiler/package.yaml compiler/acton.cabal

# /deps --------------------------------------------------
DEPS += dist/deps/mbedtls
DEPS += dist/deps/libargp
DEPS += dist/deps/libbsdnt
DEPS += dist/deps/libgc
DEPS += dist/deps/libnetstring
DEPS += dist/deps/pcre2
DEPS += dist/deps/libprotobuf_c
DEPS += dist/deps/tlsuv
DEPS += dist/deps/libutf8proc
DEPS += dist/deps/libuuid
DEPS += dist/deps/libuv
DEPS += dist/deps/libxml2
DEPS += dist/deps/libyyjson
DEPS += dist/deps/libsnappy_c

.PHONE: clean-downloads
clean-downloads:
	rm -rf deps-download


# /deps/libargp --------------------------------------------
LIBARGP_REF=a30e99cda3fabc591727a8df3aee5524c2392e15
deps-download/$(LIBARGP_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/actonlang/argp-standalone/archive/$(LIBARGP_REF).tar.gz

dist/deps/libargp: deps-download/$(LIBARGP_REF).tar.gz
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	rm -rf "$@/testsuite"
	touch "$(TD)/$@"

# /deps/libbsdnt --------------------------------------------
LIBBSDNT_REF=282f774e1e664ea7c23cc0bb9f313c1054874a97
deps-download/$(LIBBSDNT_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/actonlang/bsdnt/archive/$(LIBBSDNT_REF).tar.gz

dist/deps/libbsdnt: deps-download/$(LIBBSDNT_REF).tar.gz
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	touch "$(TD)/$@"

# /deps/libgc --------------------------------------------
LIBGC_REF=0a23b211b558137de7ee654c5527a54113142517
deps-download/$(LIBGC_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/actonlang/bdwgc/archive/$(LIBGC_REF).tar.gz

dist/deps/libgc: deps-download/$(LIBGC_REF).tar.gz
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	rm -rf "$@/tests" "$@/tools"
	touch "$(TD)/$@"

# /deps/libmbedtls --------------------------------------------
LIBMBEDTLS_REF=737c3d8e8a9f52c4adaecadb2ecdec3ccab4255d
deps-download/$(LIBMBEDTLS_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/actonlang/mbedtls/archive/$(LIBMBEDTLS_REF).tar.gz

dist/deps/mbedtls: deps-download/$(LIBMBEDTLS_REF).tar.gz
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	touch "$(TD)/$@"

# /deps/libprotobuf_c --------------------------------------------
LIBPROTOBUF_C_REF=4e4bfc7ec44e6ac746b05f3251f59610822bc95c
deps-download/$(LIBPROTOBUF_C_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/actonlang/protobuf-c/archive/$(LIBPROTOBUF_C_REF).tar.gz

dist/deps/libprotobuf_c: deps-download/$(LIBPROTOBUF_C_REF).tar.gz
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	touch "$(TD)/$@"

# /deps/tlsuv ---------------------------------------------
TLSUV_REF=8854f5626963e443fd8647793f18173140de6a7e
deps-download/$(TLSUV_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/actonlang/tlsuv/archive/$(TLSUV_REF).tar.gz

dist/deps/tlsuv: deps-download/$(TLSUV_REF).tar.gz dist/deps/libuv dist/deps/mbedtls
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	touch "$(TD)/$@"

# /deps/libutf8proc --------------------------------------
LIBUTF8PROC_REF=e914c63b43d5f283090a63a307fccd25acbe37f0
deps-download/$(LIBUTF8PROC_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/actonlang/utf8proc/archive/$(LIBUTF8PROC_REF).tar.gz

dist/deps/libutf8proc: deps-download/$(LIBUTF8PROC_REF).tar.gz
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	touch "$(TD)/$@"

# /deps/libuuid ------------------------------------------
dist/deps/libuuid: deps/libuuid
	mkdir -p "$(TD)/$@"
	cp -a "$</"* "$(TD)/$@"

# /deps/libuv --------------------------------------------
LIBUV_REF=f20620733fb8fb5fb261699bbb858887ac6ec0bb
deps-download/$(LIBUV_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/actonlang/libuv/archive/$(LIBUV_REF).tar.gz

dist/deps/libuv: deps-download/$(LIBUV_REF).tar.gz
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<" "libuv-$(LIBUV_REF)/build.zig" "libuv-$(LIBUV_REF)/include" "libuv-$(LIBUV_REF)/src"
	touch "$(TD)/$@"

# /deps/libxml2 ------------------------------------------
LIBXML2_REF=56e4e62c077b2c5285b0eec4d6d4497f9b2e6e8f
deps-download/$(LIBXML2_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/actonlang/libxml2/archive/$(LIBXML2_REF).tar.gz

dist/deps/libxml2: deps-download/$(LIBXML2_REF).tar.gz
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	rm -rf "$@/doc" "$@/example" "$@/fuzz" "$@/os400" "$@/python" $@/test*
	touch "$(TD)/$@"

# /deps/pcre2 --------------------------------------------
LIBPCRE2_REF=e9967bee566dedc213d467660bc25ed495bb693b
deps-download/$(LIBPCRE2_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/actonlang/pcre2/archive/$(LIBPCRE2_REF).tar.gz

dist/deps/pcre2: deps-download/$(LIBPCRE2_REF).tar.gz
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	touch "$(TD)/$@"

# /deps/libsnappy_c --------------------------------------------
LIBSNAPPY_C_REF=3f5b95957558a35c2becbe6b628c8219477dd5a4
deps-download/$(LIBSNAPPY_C_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/actonlang/snappy/archive/$(LIBSNAPPY_C_REF).tar.gz

dist/deps/libsnappy_c: deps-download/$(LIBSNAPPY_C_REF).tar.gz
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	touch "$(TD)/$@"

dist/deps/libnetstring: deps/libnetstring $(DIST_ZIG)
	mkdir -p "$(TD)/$@"
	cp -a "$</"* "$(TD)/$@"

dist/deps/libyyjson: deps/libyyjson $(DIST_ZIG)
	mkdir -p "$(TD)/$@"
	cp -a "$</"* "$(TD)/$@"

# top level targets
.PHONY: test test-builtins test-compiler test-db test-examples test-lang test-regressions test-rts test-stdlib
test:
	cd compiler && stack test
	$(MAKE) test-stdlib
	$(MAKE) -C backend test
	$(MAKE) test-rts-db

test-builtins:
	cd compiler && stack test actonc --ta '-p "Builtins"'

test-compiler:
	cd compiler && stack test acton
	cd compiler && stack test actonc --ta '-p "compiler"'

test-compiler-accept:
	cd compiler && stack test acton --test-arguments "--golden-start --golden-reset"

test-cross-compile:
	cd compiler && stack test actonc --ta '-p "cross-compilation"'

test-rebuild: dist/bin/actonc
	cd compiler && stack test actonc:rebuild

.PHONY: test-rebuild-accept
test-rebuild-accept: dist/bin/actonc
	cd compiler && stack test actonc:rebuild --ta "--accept"

test-syntaxerrors:
	cd compiler && stack test actonc --ta '-p "syntax errors"'

test-syntaxerrors-accept:
	cd compiler/actonc && stack runghc -- test.hs -p "syntax errors" --accept

test-typeerrors:
	cd compiler && stack test actonc --ta '-p "type errors"'

test-typeerrors-accept:
	cd compiler/actonc && stack runghc -- test.hs -p "type errors" --accept

test-db:
	cd compiler && stack test actonc --ta '-p "DB"'

test-examples:
	cd compiler && stack test actonc --ta '-p "Examples"'

test-lang:
	cd compiler && stack test actonc --ta '-p "Core language"'

test-regressions:
	cd compiler && stack test actonc --ta '-p "Regression"'

test-rts:
	cd compiler && stack test actonc --ta '-p "RTS"'

test-rts-db:
	$(MAKE) -C test

test-stdlib: dist/bin/acton
	cd compiler && stack test actonc --ta '-p "stdlib"'
	$(MAKE) -C test tls-test-server
	cd test/stdlib_tests && "$(ACTON)" test


.PHONY: clean clean-all clean-base
clean: clean-cli clean-distribution clean-base

clean-cli:
	rm -rf cli/out

clean-all: clean clean-compiler
	rm -rf $(ZIG_LOCAL_CACHE_DIR)

clean-base:
	rm -rf base/out

cli/out/bin/acton: distribution1
	cd cli && rm -f build.zig build.zig.zon && "$(ACTC)" build $(ACTONC_TARGET)

# == DIST ==
#

BACKEND_FILES = backend/build.zig backend/build.zig.zon $(wildcard backend/*.c backend/*.h backend/failure_detector/*.c backend/failure_detector/*.h)
DIST_BACKEND_FILES = $(addprefix dist/,$(BACKEND_FILES)) dist/backend/deps dist/bin/actondb
dist/backend%: backend/%
	mkdir -p "$(dir $@)"
	cp -a "$<" "$@"

.PHONY: dist/base
dist/base: base base/.build base/__root.zig base/acton.zig base/build.zig base/build.zig.zon base/acton.zig dist/bin/actonc $(DEPS)
	mkdir -p "$@" "$@/.build" "$@/out"
	cp -a base/__root.zig base/Acton.toml base/acton.zig base/build.zig base/build.zig.zon base/builtin base/rts base/src dist/base/
	cd dist/base && ../bin/actonc build --skip-build && rm -rf .build

# This does a little hack, first copying and then moving the file in place. This
# is to avoid an error if the executable is currently running. cp tries to open
# the file and modify it, which the Linux kernel (and perhaps others?) will
# prevent if the file to be modified is an executable program that is currently
# running.  We work around it by moving / renaming the file in place instead!
dist/bin/acton: cli/out/bin/acton
	@mkdir -p $(dir $@)
	cp -a $< $@.tmp
	mv $@.tmp $@

dist/bin/actondb: $(DIST_ZIG) $(DEPS)
	@mkdir -p $(dir $@)
	cd dist/backend && "$(ZIG)" build -Donly_actondb --prefix "$(TD)/dist"

dist/bin/runacton: bin/runacton
	@mkdir -p $(dir $@)
	cp $< $@.tmp
	mv $@.tmp $@

dist/builder: builder/build.zig builder/build.zig.zon
	@mkdir -p "$@"
	cp -a $^ "$@/"

dist/deps/%: deps/% $(DEPS)
	@mkdir -p "$(dir $@)"
	cp -a "$<" "$@"

dist/completion/acton.bash-completion: completion/acton.bash-completion
	mkdir -p "$(dir $@)"
	cp "$<" "$@"

dist/zig: deps-download/zig-$(OS)-$(ARCH)-$(ZIG_VERSION).tar.xz
	mkdir -p "$@"
	cd "$@" && tar Jx --strip-components=1 -f "../../$^"
	rm -rf "$@/doc"
	cp -a deps/zig-extras/* "$@"


# Check if ZIG_VERSION contains -dev, in which case we pull down a nightly,
# otherwise its a release
deps-download/zig-$(OS)-$(ARCH)-$(ZIG_VERSION).tar.xz:
	mkdir -p deps-download
ifeq ($(findstring -dev,$(ZIG_VERSION)),-dev)
	$(CURL) -o $@ https://github.com/actonlang/zigballs/raw/main/zig-$(OS)-$(ARCH)-$(ZIG_VERSION).tar.xz
#	$(CURL) -o $@ https://ziglang.org/builds/zig-$(OS)-$(ARCH)-$(ZIG_VERSION).tar.xz
else
	$(CURL) -o $@ https://ziglang.org/download/$(ZIG_VERSION)/zig-$(OS)-$(ARCH)-$(ZIG_VERSION).tar.xz
endif

.PHONY: distribution1 distribution clean-distribution
distribution1: dist/base $(DIST_BACKEND_FILES) dist/builder $(DIST_BINS) $(DIST_ZIG)
	$(MAKE) $(DEPS)

distribution: dist/bin/acton dist/lldb/acton.py

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
	tar cv $(TAR_TRANSFORM_OPT) --exclude .gitignore dist | xz -z -0 --threads=0 > "$@"

.PHONY: release
release: distribution
	$(MAKE) acton-$(OS)-$(ARCH)-$(ACTONC_VERSION).tar.xz

# This target is used by the debian packaging
.PHONY: install
install:
	mkdir -p "$(DESTDIR)/usr/bin" "$(DESTDIR)/usr/lib/acton"
	cp -a dist/. "$(DESTDIR)/usr/lib/acton/"
	cd "$(DESTDIR)/usr/bin" && ln -s ../lib/acton/bin/acton
	cd "$(DESTDIR)/usr/bin" && ln -s ../lib/acton/bin/actonc
	cd "$(DESTDIR)/usr/bin" && ln -s ../lib/acton/bin/actondb
	cd "$(DESTDIR)/usr/bin" && ln -s ../lib/acton/bin/runacton
	cd "$(DESTDIR)/usr/bin" && ln -s ../lib/acton/bin/lsp-server-acton

dist/lldb/acton.py: utils/lldb/acton.py
	@mkdir -p dist/lldb
	cp -a $< $@

.PHONY: debian/changelog
debian/changelog: debian/changelog.in CHANGELOG.md
	cat $< | sed -e 's/VERSION/$(VERSION_INFO)/' -e 's/DEB_DIST/$(DEB_DIST)/' > $@

.PHONY: debs
debs: debian/changelog
	debuild --preserve-envvar VERSION_INFO -i -us -uc -nc -b

.PHONY: container-image image image-deb push-image
container-image: all
	podman build -f Containerfile -t acton:$(CONTAINER_TAG) --volume $(TD):/src:ro .

image: container-image

# Build container from locally built .deb (useful to mirror CI build path)
image-deb: debs
	podman build -f Containerfile.deb -t acton:$(CONTAINER_TAG) --build-arg TARGETARCH=$(ARCH) --volume $(TD):/src:ro .

push-image:
	@echo "Pushing container image to GitHub Container Registry"
	podman tag acton:$(CONTAINER_TAG) ghcr.io/actonlang/acton:$(CONTAINER_TAG)
	podman push ghcr.io/actonlang/acton:$(CONTAINER_TAG)
