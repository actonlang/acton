include version.mk
TD := $(CURDIR)
CHANGELOG_VERSION=$(shell grep '^\#\# \[[0-9]' CHANGELOG.md | sed 's/\#\# \[\([^]]\{1,\}\)].*/\1/' | head -n1)
GIT_VERSION_TAG=$(shell git tag --points-at HEAD 2>/dev/null | grep "v[0-9]" | sed -e 's/^v//')
UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

ifdef HOME
ZIG_LOCAL_CACHE_DIR ?= $(HOME)/.cache/acton/zig-local-cache
else
ZIG_LOCAL_CACHE_DIR ?= $(TD)/zig-cache
endif
export ZIG_LOCAL_CACHE_DIR

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

# This is the version we will stamp into acton
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
ifeq ($(UNAME_M),arm64)
ARCH:=aarch64
else
ARCH:=$(UNAME_M)
endif

# -- Apple Mac OS X ------------------------------------------------------------
ifeq ($(UNAME_S),Darwin)
OS:=macos
endif

# -- Linux ---------------------------------------------------------------------
ifeq ($(UNAME_S),Linux)
OS:=linux
ifeq ($(UNAME_M),x86_64)
ACTONC_TARGET := --target x86_64-linux-gnu.2.27
else ifeq ($(UNAME_M),aarch64)
ACTONC_TARGET := --target aarch64-linux-gnu.2.27
else
$(error "Unsupported architecture for Linux?" $(UNAME_M))
endif
endif # -- END: Linux ----------------------------------------------------------

# -- Windows (MSYS2 / MINGW / Cygwin) ------------------------------------------
ifneq (,$(filter MINGW% MSYS% CYGWIN%,$(UNAME_S)))
OS:=windows
endif

EXEEXT :=
ifeq ($(OS),windows)
EXEEXT := .exe
endif

ACTON_BIN := dist/bin/acton$(EXEEXT)
ACTONC_BIN := dist/bin/actonc$(EXEEXT)
LSP_BIN := dist/bin/lsp-server-acton$(EXEEXT)
ACTONDB_BIN := dist/bin/actondb$(EXEEXT)

ACTON := $(TD)/$(ACTON_BIN)
ACTONC := $(ACTONC_BIN)
ACTONC_ABS := $(TD)/$(ACTONC_BIN)

ZIG_VERSION:=0.15.2
ZIG=$(TD)/dist/zig/zig$(EXEEXT)
CURL:=curl --fail --location --retry 5 --retry-delay 2 --retry-max-time 120 --retry-all-errors --retry-connrefused
AR=$(ZIG) ar
CC=$(ZIG) cc
CXX=$(ZIG) c++
export CC
export CXX

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

DIST_BINS=$(ACTONC_BIN) $(ACTONDB_BIN) dist/bin/runacton $(LSP_BIN)
ifeq ($(OS),windows)
# ActonDB deps aren't Windows-ready yet; skip the binary for now.
DIST_BINS=$(ACTONC_BIN) dist/bin/runacton $(LSP_BIN)
endif
DIST_ZIG=dist/zig
ZIG_ARCHIVE_NAME := zig-$(ARCH)-$(OS)-$(ZIG_VERSION).tar.xz
ifeq ($(OS),windows)
ZIG_ARCHIVE_NAME := zig-$(ARCH)-$(OS)-$(ZIG_VERSION).zip
endif
ZIG_ARCHIVE_URL := https://ziglang.org/download/$(ZIG_VERSION)/$(ZIG_ARCHIVE_NAME)
ZIG_ARCHIVE_URL_ALT :=
ifeq ($(OS),windows)
ZIG_ARCHIVE_URL_ALT := https://ziglang.org/download/$(ZIG_VERSION)/zig-$(OS)-$(ARCH)-$(ZIG_VERSION).zip
endif
ifeq ($(findstring -dev,$(ZIG_VERSION)),-dev)
ZIG_ARCHIVE_URL := https://github.com/actonlang/zigballs/raw/main/$(ZIG_ARCHIVE_NAME)
ZIG_ARCHIVE_URL_ALT :=
endif
ZIG_ARCHIVE := deps-download/$(ZIG_ARCHIVE_NAME)

.PHONY: test-backend
test-backend: $(BACKEND_TESTS)
	@echo DISABLED TEST: backend/failure_detector/db_messages_test
	./backend/test/actor_ring_tests_local
	./backend/test/actor_ring_tests_remote
	./backend/test/db_unit_tests
	@echo DISABLED test: ./backend/test/queue_unit_tests
	./backend/test/skiplist_test

# /compiler ----------------------------------------------
ACTONC_HS=$(wildcard compiler/lib/src/*.hs compiler/lib/src/*/*.hs compiler/acton/Main.hs)
ACTONLSP_HS=$(wildcard compiler/lsp-server/*.hs)
# NOTE: we unset CC/CXX on non-Windows to avoid Zig for stack/ghc.
STACK_ENV_PREFIX := unset CC && unset CXX && unset CFLAGS &&
STACK_GHC_OPTS := -j4 $(ACTC_GHC_OPTS)
ifeq ($(OS),windows)
STACK_CC ?= gcc
STACK_CXX ?= g++
STACK_CFLAGS ?= -Wno-error -Wno-pragma-pack -Wno-error=pragma-pack
ACTC_GHC_OPTS += -optc-Wno-error -optc-Wno-pragma-pack -optc-Wno-error=pragma-pack
STACK_GHC_OPTS := -j4 $(ACTC_GHC_OPTS)
STACK_MINGW64_WIN := $(shell cygpath -m /mingw64)
STACK_PATH_WIN := $(shell cygpath -w -p "$$PATH")
STACK_ENV_PREFIX := PATH="$(STACK_PATH_WIN)" CC=$(STACK_CC) CXX=$(STACK_CXX) CFLAGS=$(STACK_CFLAGS)
STACK_OPTS += --skip-msys \
	--extra-lib-dirs=$(STACK_MINGW64_WIN)/lib \
	--extra-lib-dirs=$(STACK_MINGW64_WIN)/bin
endif
# NOTE: we're unsetting CC & CXX to avoid using zig cc & zig c++ for stack /
# ghc, which doesn't seem to work properly
$(ACTON_BIN): compiler/lib/package.yaml.in compiler/acton/package.yaml.in compiler/lsp-server/package.yaml.in compiler/stack.yaml $(ACTONC_HS) $(ACTONLSP_HS) version.mk
	mkdir -p dist/bin
	rm -f $(ACTONC_BIN)
	cd compiler && sed 's,^version: BUILD_VERSION,version: "$(VERSION)",' < lib/package.yaml.in > lib/package.yaml
	cd compiler && $(STACK_ENV_PREFIX) stack build acton lsp-server-acton --dry-run 2>&1 | grep "Nothing to build" || \
		(sed 's,^version: BUILD_VERSION,version: "$(VERSION_INFO)",' < acton/package.yaml.in > acton/package.yaml \
		&& sed 's,^version: BUILD_VERSION,version: "$(VERSION_INFO)",' < lsp-server/package.yaml.in > lsp-server/package.yaml \
		&& stack build acton lsp-server-acton $(STACK_OPTS) --ghc-options='$(STACK_GHC_OPTS)')
	cd compiler && $(STACK_ENV_PREFIX) stack --local-bin-path=../dist/bin install acton lsp-server-acton $(STACK_OPTS) --ghc-options='$(STACK_GHC_OPTS)'

$(ACTONC_BIN): $(ACTON_BIN)
	@mkdir -p $(dir $@)
ifeq ($(EXEEXT),.exe)
	cp -a $(ACTON_BIN) $@
else
	ln -sf acton $@
endif

$(LSP_BIN): $(ACTON_BIN)
	@true

.PHONY: clean-compiler
clean-compiler:
	cd compiler && stack clean >/dev/null 2>&1 || true
	rm -f $(ACTON_BIN) $(ACTONC_BIN) compiler/package.yaml compiler/acton.cabal \
		compiler/acton/package.yaml compiler/acton/acton.cabal \
		compiler/lib/*.cabal compiler/acton/*.cabal compiler/lsp-server/*.cabal

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
LIBARGP_REF=137154fb257055beb11f3283021d8eccc3c4f470
deps-download/$(LIBARGP_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/actonlang/argp-standalone/archive/$(LIBARGP_REF).tar.gz

dist/deps/libargp: deps-download/$(LIBARGP_REF).tar.gz
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	rm -rf "$@/testsuite"
	touch "$(TD)/$@"

# /deps/libbsdnt --------------------------------------------
LIBBSDNT_REF=cf7db3414867b8b4a5561bc9aa94a8050d0225c4
deps-download/$(LIBBSDNT_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/actonlang/bsdnt/archive/$(LIBBSDNT_REF).tar.gz

dist/deps/libbsdnt: deps-download/$(LIBBSDNT_REF).tar.gz
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	touch "$(TD)/$@"

# /deps/libgc --------------------------------------------
LIBGC_REF=5ef334ab8f9ef9e23d6b9c99fbd2b621bd52789b
deps-download/$(LIBGC_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/actonlang/bdwgc/archive/$(LIBGC_REF).tar.gz

dist/deps/libgc: deps-download/$(LIBGC_REF).tar.gz
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	rm -rf "$@/tests" "$@/tools"
	touch "$(TD)/$@"

# /deps/libmbedtls --------------------------------------------
LIBMBEDTLS_REF=c7d83538d3d359b05a9331bb2c9217977b5856ac
deps-download/$(LIBMBEDTLS_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/actonlang/mbedtls/archive/$(LIBMBEDTLS_REF).tar.gz

dist/deps/mbedtls: deps-download/$(LIBMBEDTLS_REF).tar.gz
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	touch "$(TD)/$@"

# /deps/libprotobuf_c --------------------------------------------
LIBPROTOBUF_C_REF=faa19a6f6ca393fea01077fb37011a949bc6a3ee
deps-download/$(LIBPROTOBUF_C_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/actonlang/protobuf-c/archive/$(LIBPROTOBUF_C_REF).tar.gz

dist/deps/libprotobuf_c: deps-download/$(LIBPROTOBUF_C_REF).tar.gz
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	touch "$(TD)/$@"

# /deps/tlsuv ---------------------------------------------
TLSUV_REF=5af699b033776ec6a21b32c90e7aa7bf08c9929f
deps-download/$(TLSUV_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/actonlang/tlsuv/archive/$(TLSUV_REF).tar.gz

dist/deps/tlsuv: deps-download/$(TLSUV_REF).tar.gz dist/deps/libuv dist/deps/mbedtls
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	touch "$(TD)/$@"

# /deps/libutf8proc --------------------------------------
LIBUTF8PROC_REF=a78677e855f0a282e79da6164db4ce1cf0789237
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
LIBUV_REF=d760a3f23511ebe7b1935fe1429147d4fca27bb4
deps-download/$(LIBUV_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/actonlang/libuv/archive/$(LIBUV_REF).tar.gz

dist/deps/libuv: deps-download/$(LIBUV_REF).tar.gz
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<" "libuv-$(LIBUV_REF)/build.zig" "libuv-$(LIBUV_REF)/include" "libuv-$(LIBUV_REF)/src"
	touch "$(TD)/$@"

# /deps/libxml2 ------------------------------------------
LIBXML2_REF=358ca4e6e34dd2b386aab1fdeb74a641c54940a0
deps-download/$(LIBXML2_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/actonlang/libxml2/archive/$(LIBXML2_REF).tar.gz

dist/deps/libxml2: deps-download/$(LIBXML2_REF).tar.gz
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	rm -rf "$@/doc" "$@/example" "$@/fuzz" "$@/os400" "$@/python" $@/test*
	touch "$(TD)/$@"

# /deps/pcre2 --------------------------------------------
LIBPCRE2_REF=b82656c5b28658ce1d75489a1b67ba0de5f531ec
deps-download/$(LIBPCRE2_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/actonlang/pcre2/archive/$(LIBPCRE2_REF).tar.gz

dist/deps/pcre2: deps-download/$(LIBPCRE2_REF).tar.gz
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	touch "$(TD)/$@"

# /deps/libsnappy_c --------------------------------------------
LIBSNAPPY_C_REF=9d77a3136e271b709eeac4b1db2d27c281b330b2
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
.PHONY: test test-builtins test-compiler test-db test-examples test-lang test-regressions test-rts test-stdlib online-tests
test: $(ACTON_BIN)
	cd compiler && stack test libacton acton:test_acton acton:incremental
	$(MAKE) test-stdlib
	$(MAKE) -C backend test
	$(MAKE) test-rts-db

test-builtins:
	cd compiler && stack test acton --ta '-p "Builtins"'

test-compiler:
	cd compiler && stack test libacton
	cd compiler && stack test acton --ta '-p "compiler"'

test-compiler-accept:
	cd compiler && stack test acton --test-arguments "--golden-start --golden-reset"

test-cross-compile:
	cd compiler && stack test acton --ta '-p "cross-compilation"'

test-incremental: $(ACTONC_BIN)
	cd compiler && stack test acton:incremental

.PHONY: test-incremental-accept
test-incremental-accept: $(ACTONC_BIN)
	cd compiler && stack test acton:incremental --ta "--accept"

.PHONY: test-rebuild test-rebuild-accept
test-rebuild: test-incremental

test-rebuild-accept: test-incremental-accept

test-syntaxerrors:
	cd compiler && stack test acton --ta '-p "syntax errors"'

test-syntaxerrors-accept:
	cd compiler/acton && stack runghc -- test.hs -p "syntax errors" --accept

test-typeerrors:
	cd compiler && stack test acton --ta '-p "type errors"'

test-typeerrors-accept:
	cd compiler && stack test acton:test_acton --ta '-p "type errors" --accept'

test-db:
	cd compiler && stack test acton --ta '-p "DB"'

test-examples:
	cd compiler && stack test acton --ta '-p "Examples"'

test-lang:
	cd compiler && stack test acton --ta '-p "Core language"'

test-regressions:
	cd compiler && stack test acton --ta '-p "Regression"'

test-rts:
	cd compiler && stack test acton --ta '-p "RTS"'

test-rts-db:
	$(MAKE) -C test

test-stdlib: $(ACTON_BIN)
	cd compiler && stack test acton --ta '-p "stdlib"'
	$(MAKE) -C test tls-test-server
	cd test/stdlib_tests && "$(ACTON)" test

online-tests: $(ACTONC_BIN)
	cd compiler && stack test acton:test_acton_online


.PHONY: clean clean-all clean-base
clean: clean-distribution clean-base

clean-all: clean clean-compiler
	rm -rf $(ZIG_LOCAL_CACHE_DIR)

clean-base:
	rm -rf base/out

# == DIST ==
#

BACKEND_FILES = backend/build.zig backend/build.zig.zon $(wildcard backend/*.c backend/*.h backend/failure_detector/*.c backend/failure_detector/*.h)
DIST_BACKEND_FILES = $(addprefix dist/,$(BACKEND_FILES)) dist/backend/deps $(ACTONDB_BIN)
ifeq ($(OS),windows)
DIST_BACKEND_FILES = $(addprefix dist/,$(BACKEND_FILES)) dist/backend/deps
endif
dist/backend%: backend/%
	mkdir -p "$(dir $@)"
	cp -a "$<" "$@"

ifeq ($(OS),windows)
# Windows tools won't follow MSYS2 .lnk "symlinks", so materialize deps.
dist/backend/deps: $(DEPS)
	rm -rf "$@"
	mkdir -p "$@"
	cp -a dist/deps/* "$@"
endif

.PHONY: dist/base
dist/base: base base/.build base/__root.zig base/acton.zig base/build.zig base/build.zig.zon base/acton.zig $(ACTONC_BIN) $(DEPS)
	mkdir -p "$@" "$@/.build" "$@/out"
	cp -a base/__root.zig base/Build.act base/acton.zig base/build.zig base/build.zig.zon base/builtin base/rts base/src dist/base/
	cd dist/base && "$(ACTONC_ABS)" build --skip-build && rm -rf .build

# This does a little hack, first copying and then moving the file in place. This
# is to avoid an error if the executable is currently running. cp tries to open
# the file and modify it, which the Linux kernel (and perhaps others?) will
# prevent if the file to be modified is an executable program that is currently
# running.  We work around it by moving / renaming the file in place instead!
$(ACTONDB_BIN): $(DIST_ZIG) $(DEPS)
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

dist/zig: $(ZIG_ARCHIVE)
	mkdir -p "$@"
ifeq ($(OS),windows)
	cd "$@" && unzip -q "../../$<" && \
		top_dir=$$(ls -d zig-* | head -n1) && \
		mv "$$top_dir"/* . && rmdir "$$top_dir"
else
	cd "$@" && tar Jx --strip-components=1 -f "../../$<"
endif
	rm -rf "$@/doc"
	cp -a deps/zig-extras/* "$@"


# Check if ZIG_VERSION contains -dev, in which case we pull down a nightly,
# otherwise its a release
$(ZIG_ARCHIVE):
	mkdir -p deps-download
	$(CURL) -o $@ $(ZIG_ARCHIVE_URL) || \
		{ if [ -n "$(ZIG_ARCHIVE_URL_ALT)" ]; then $(CURL) -o $@ $(ZIG_ARCHIVE_URL_ALT); else exit 1; fi; }

.PHONY: distribution1 distribution clean-distribution
distribution1: dist/base $(DIST_BACKEND_FILES) dist/builder $(DIST_BINS) $(DIST_ZIG)
	$(MAKE) $(DEPS)

distribution: distribution1 dist/lldb/acton.py

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
	cd "$(DESTDIR)/usr/bin" && ln -s ../lib/acton/bin/acton actonc
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
