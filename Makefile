include version.mk
TD := $(CURDIR)
CHANGELOG_VERSION=$(shell grep '^\#\# \[[0-9]' CHANGELOG.md | sed 's/\#\# \[\([^]]\{1,\}\)].*/\1/' | head -n1)
GIT_VERSION_TAG=$(shell git tag --points-at HEAD 2>/dev/null | grep "v[0-9]" | sed -e 's/^v//')

ifdef HOME
ZIG_LOCAL_CACHE_DIR ?= $(HOME)/.cache/acton/zig-local-cache
ZIG_GLOBAL_CACHE_DIR ?= $(HOME)/.cache/acton/zig-global-cache
else
# TODO: Windows?
ZIG_LOCAL_CACHE_DIR ?= $(TD)/zig-cache
ZIG_GLOBAL_CACHE_DIR ?= $(TD)/zig-global-cache
endif
export ZIG_LOCAL_CACHE_DIR
export ZIG_GLOBAL_CACHE_DIR

ACTON=$(TD)/dist/bin/acton
ACTONC=dist/bin/actonc
ZIG_VERSION:=0.16.0
ZIG=$(TD)/dist/zig/zig
CURL:=curl --fail --location --retry 5 --retry-delay 2 --retry-max-time 120 --retry-all-errors --retry-connrefused
AR=$(ZIG) ar
CC=$(ZIG) cc
CXX=$(ZIG) c++
export CC
export CXX
ACTON_STACK_CC=$(TD)/compiler/tools/zig-cc.sh
ACTON_STACK_CXX=$(TD)/compiler/tools/zig-cxx.sh
ACTON_STACK_NEEDS_ZIG=
ACTON_ZIG_TARGET=
STACK=unset CC && unset CXX && unset CFLAGS && unset CPPFLAGS && unset LDFLAGS && unset ACTON_REAL_LD && stack

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
BUILD_TIME:=$(shell date "+%Y%m%d.%-H.%-M.%-S")
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

PROFILE ?= 0
ACTON_STACK_BUILD_OPTS := $(STACK_OPTS)
ifeq ($(PROFILE),1)
ACTON_STACK_BUILD_OPTS += --profile --library-profiling --executable-profiling
ACTC_GHC_OPTS += -fprof-auto -fprof-cafs
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
ACTON_ZIG_GLIBC_VERSION ?= 2.28
export ACTON_ZIG_GLIBC_VERSION
ACTON_STACK_NEEDS_ZIG=1
STACK=CC="$(ACTON_STACK_CC)" CXX="$(ACTON_STACK_CXX)" CFLAGS= CPPFLAGS= LDFLAGS= ACTON_REAL_LD="$(ACTON_STACK_CC)" stack --with-gcc="$(ACTON_STACK_CC)"
ifeq ($(shell uname -m),x86_64)
ACTON_ZIG_TARGET := x86_64-linux-gnu.$(ACTON_ZIG_GLIBC_VERSION)
else ifeq ($(shell uname -m),aarch64)
ACTON_ZIG_TARGET := aarch64-linux-gnu.$(ACTON_ZIG_GLIBC_VERSION)
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
	@echo "  make PROFILE=1 dist/bin/acton - build profiled acton binary"
	@echo "  rpms    - build RPM package from existing dist/"
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
ifeq ($(ACTON_STACK_NEEDS_ZIG),1)
ACTON_STACK_PREREQS=$(DIST_ZIG)
else
ACTON_STACK_PREREQS=
endif

# bdeps: static libs we build ourselves and link into the acton compiler. These
# variables must be defined here, before the dist/bin/acton rule, because GNU
# make expands a rule's prerequisite list when the rule is read -- defining
# BDEPS later would leave $(BDEPS) empty there and the archives would never be
# built. The build rules and the full rationale live in the /bdeps section below.
# ACTON_BDEPS_DIR must be absolute since compiler/tools/ld-wrapper.sh (the GHC
# -pgml link wrapper, used on both Linux and macOS) reads it from the environment
# and needs a fixed location independent of the directory the link runs from.
ACTON_BDEPS_DIR := $(TD)/bdeps/out
export ACTON_BDEPS_DIR
# zlib is pulled in by the Haskell zlib package on every platform, so we build
# and statically link it everywhere.
BDEPS += bdeps/out/lib/libz.a
# liblmdb backs the compiler's .tydb interface cache; GHC links it into acton on
# every platform, so it is also an unconditional bdep.
BDEPS += bdeps/out/lib/liblmdb.a
ifeq ($(OS),linux)
# gmp and tinfo are only pulled in by GHC on Linux (macOS GHC uses the native
# bignum backend and the SDK's libncurses), so they are Linux-only bdeps.
BDEPS += bdeps/out/lib/libgmp.a
BDEPS += bdeps/out/lib/libtinfo.a
endif

.PHONY: test-backend
test-backend: $(BACKEND_TESTS)
	@echo DISABLED TEST: backend/failure_detector/db_messages_test
	./backend/test/actor_ring_tests_local
	./backend/test/actor_ring_tests_remote
	./backend/test/db_unit_tests
	@echo DISABLED test: ./backend/test/queue_unit_tests
	./backend/test/skiplist_test

# /compiler ----------------------------------------------
ACTON_HS=$(wildcard compiler/lib/src/*.hs compiler/lib/src/*/*.hs compiler/acton/*.hs compiler/acton/*/*.hs)
ACTONLSP_HS=$(wildcard compiler/lsp-server/*.hs)
dist/bin/acton: compiler/lib/package.yaml.in compiler/acton/package.yaml.in compiler/lsp-server/package.yaml.in compiler/stack.yaml $(ACTON_HS) $(ACTONLSP_HS) version.mk dist/builder $(ACTON_STACK_PREREQS) $(BDEPS)
	mkdir -p dist/bin
	rm -f dist/bin/actonc
	cd compiler && sed 's,^version: BUILD_VERSION,version: "$(VERSION)",' < lib/package.yaml.in > lib/package.yaml
	cd compiler && $(STACK) build acton lsp-server-acton $(ACTON_STACK_BUILD_OPTS) --ghc-options='-j4 $(ACTC_GHC_OPTS)' --dry-run 2>&1 | grep "Nothing to build" || \
		(sed 's,^version: BUILD_VERSION,version: "$(VERSION_INFO)",' < acton/package.yaml.in > acton/package.yaml \
		&& sed 's,^version: BUILD_VERSION,version: "$(VERSION_INFO)",' < lsp-server/package.yaml.in > lsp-server/package.yaml \
		&& unset CC && unset CXX && unset CFLAGS && unset CPPFLAGS && unset LDFLAGS && unset ACTON_REAL_LD && stack setup \
		&& $(STACK) build acton lsp-server-acton $(ACTON_STACK_BUILD_OPTS) --ghc-options='-j4 $(ACTC_GHC_OPTS)')
	cd compiler && $(STACK) --local-bin-path=../dist/bin install acton lsp-server-acton $(ACTON_STACK_BUILD_OPTS) --ghc-options='-j4 $(ACTC_GHC_OPTS)'
	# Keep actonc as a symlink for compatibility
	ln -sf acton dist/bin/actonc

dist/bin/actonc: dist/bin/acton
	@mkdir -p $(dir $@)
	ln -sf acton $@

dist/bin/lsp-server-acton: dist/bin/acton
	@true

.PHONY: clean-compiler
clean-compiler:
	cd compiler && stack clean >/dev/null 2>&1 || true
	rm -f dist/bin/acton dist/bin/actonc compiler/package.yaml compiler/acton.cabal \
		compiler/acton/package.yaml compiler/acton/acton.cabal \
		compiler/lib/*.cabal compiler/acton/*.cabal compiler/lsp-server/*.cabal

ACTON_LINKAGE_BINS ?= dist/bin/acton dist/bin/lsp-server-acton dist/bin/actondb
ACTON_ALLOWED_NEEDED_RE ?= ^(libc\.so\.6|libm\.so\.6|libdl\.so\.2|libpthread\.so\.0|librt\.so\.1|libutil\.so\.1|ld-linux-x86-64\.so\.2|ld-linux-aarch64\.so\.1)$$
.PHONY: ldd
ldd: $(ACTON_LINKAGE_BINS)
ifeq ($(OS),linux)
	@for bin in $(ACTON_LINKAGE_BINS); do \
		echo "== $$bin =="; \
		ldd "$$bin"; \
		if command -v readelf >/dev/null 2>&1; then \
			unexpected_needed=$$(readelf -d "$$bin" | sed -n 's/.*Shared library: \[\(.*\)\].*/\1/p' | grep -Ev '$(ACTON_ALLOWED_NEEDED_RE)' || true); \
			if [ -n "$$unexpected_needed" ]; then \
				echo "unexpected dynamic libraries:" >&2; \
				echo "$$unexpected_needed" >&2; \
				exit 1; \
			fi; \
			max_glibc=$$(readelf --version-info "$$bin" | grep -o 'GLIBC_[0-9][.0-9]*' | sed 's/GLIBC_//' | sort -Vu | tail -n 1); \
			if [ -n "$$max_glibc" ]; then \
				echo "max GLIBC version required: $$max_glibc"; \
				if [ "$$(printf '%s\n%s\n' "$$max_glibc" "$(ACTON_ZIG_GLIBC_VERSION)" | sort -V | tail -n 1)" != "$(ACTON_ZIG_GLIBC_VERSION)" ]; then \
					echo "ERROR: $$bin requires GLIBC $$max_glibc, newer than target $(ACTON_ZIG_GLIBC_VERSION)" >&2; \
					exit 1; \
				fi; \
			fi; \
		fi; \
	done
else ifeq ($(OS),macos)
	@for bin in $(ACTON_LINKAGE_BINS); do \
		echo "== $$bin =="; \
		otool -L "$$bin"; \
		dynamic_lmdb=$$(otool -L "$$bin" | awk 'NR > 1 {print $$1}' | grep -E '(^|/|@rpath/|@loader_path/|@executable_path/)liblmdb' || true); \
		if [ -n "$$dynamic_lmdb" ]; then \
			echo "unexpected dynamic LMDB dependency (liblmdb must be linked statically):" >&2; \
			echo "$$dynamic_lmdb" >&2; \
			exit 1; \
		fi; \
	done
else
	@echo "ldd target is only meaningful on Linux and macOS"
endif

# /deps --------------------------------------------------
# Downloaded dependencies use source tarballs. Acton-owned Zig build glue lives
# under deps/<name> and is overlaid into dist/deps.
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

# /bdeps -------------------------------------------------
# Build-time dependencies of the acton compiler executable itself. These are
# libraries that GHC statically links into `acton`; compiler/tools/*.sh now
# requires these archives under bdeps/out instead of falling back to host .a files.
# Building them ourselves with zig lets us target an older/chosen libc instead of
# inheriting whatever the build machine has installed. Each bdeps/<name> is a
# standalone zig project that fetches its upstream source via build.zig.zon and
# installs (via zig --prefix) into the shared bdeps/out prefix. They are
# statically linked into `acton`, so nothing here is ever shipped in dist/. The
# GHC link wrappers find the archives/headers under bdeps/out (ACTON_BDEPS_DIR).
#
# Both platforms link acton through the -pgml=ld-wrapper.sh override (see
# compiler/acton/package.yaml.in). On Linux the wrapper additionally uses GNU ld
# -Bstatic/-Bdynamic toggling and ACTON_REAL_LD (zig cc) to target an older
# chosen glibc -- a Linux-specific problem. On macOS ld64 has no -Bstatic, so the
# wrapper instead rewrites each -lfoo to the full path of bdeps/out/lib/libfoo.a
# (a positional arg ld64 links statically) and execs the system clang/ld64 GHC is
# configured for; zig only builds the archive, since its bundled linker rejects
# the ld64 flags GHC emits. zlib and LMDB are linked on every platform; gmp and
# tinfo are Linux-only because macOS GHC uses the native bignum backend and the
# SDK's libncurses. The mechanism generalises: any .a placed in bdeps/out/lib
# links statically on either platform.
#
# The BDEPS list and ACTON_BDEPS_DIR are defined earlier (near
# ACTON_STACK_PREREQS) so they're in scope when the dist/bin/acton rule is read;
# only the build rules live here.
#
# bdeps_align_macho repacks a zig-built archive ($(1), absolute path) so the
# final acton link succeeds on macOS. Apple's newer ld64 (Xcode 16 / macOS 26+)
# requires every 64-bit Mach-O archive member to start at an 8-byte-aligned
# offset; zig's archiver does not guarantee that, so the link otherwise fails with
#   ld: 64-bit mach-o member '...' not 8-byte aligned in 'lib....a'
# zig ar's darwin format pads members to 8 bytes. Extracted members come out with
# 0 permissions, hence the chmod before re-archiving. No-op off macOS (GNU ld does
# not care), and generic so any future bdeps lib gets the same treatment.
ifeq ($(OS),macos)
define bdeps_align_macho
	rm -rf "$(1).repack" && mkdir -p "$(1).repack" && cd "$(1).repack" && \
		"$(ZIG)" ar x --output=. "$(1)" && chmod u+rw *.o && \
		"$(ZIG)" ar --format=darwin rcs "$(1).aligned" *.o && cd "$(TD)" && \
		mv "$(1).aligned" "$(1)" && rm -rf "$(1).repack"
endef
else
define bdeps_align_macho
	@:
endef
endif

# /bdeps/zlib --------------------------------------------
bdeps/out/lib/libz.a: bdeps/zlib/build.zig bdeps/zlib/build.zig.zon $(DIST_ZIG)
	cd bdeps/zlib && "$(ZIG)" build -Doptimize=ReleaseFast --prefix "$(TD)/bdeps/out" \
		$(if $(ACTON_ZIG_TARGET),-Dtarget=$(ACTON_ZIG_TARGET))
	$(call bdeps_align_macho,$(abspath $@))

# /bdeps/gmp ---------------------------------------------
bdeps/out/lib/libgmp.a: bdeps/gmp/build.zig bdeps/gmp/build.zig.zon $(DIST_ZIG)
	cd bdeps/gmp && "$(ZIG)" build -Doptimize=ReleaseFast --prefix "$(TD)/bdeps/out" \
		$(if $(ACTON_ZIG_TARGET),-Dtarget=$(ACTON_ZIG_TARGET))
	$(call bdeps_align_macho,$(abspath $@))

# /bdeps/ncurses (libtinfo) ------------------------------
bdeps/out/lib/libtinfo.a: bdeps/ncurses/build.zig bdeps/ncurses/build.zig.zon \
		bdeps/ncurses/gencaps.c bdeps/ncurses/tinfo.c $(DIST_ZIG)
	cd bdeps/ncurses && "$(ZIG)" build -Doptimize=ReleaseFast --prefix "$(TD)/bdeps/out" \
		$(if $(ACTON_ZIG_TARGET),-Dtarget=$(ACTON_ZIG_TARGET))
	$(call bdeps_align_macho,$(abspath $@))

# /bdeps/lmdb (compiler .tydb interface cache) -----------
bdeps/out/lib/liblmdb.a: bdeps/lmdb/build.zig bdeps/lmdb/build.zig.zon $(DIST_ZIG)
	cd bdeps/lmdb && "$(ZIG)" build -Doptimize=ReleaseFast --prefix "$(TD)/bdeps/out" \
		$(if $(ACTON_ZIG_TARGET),-Dtarget=$(ACTON_ZIG_TARGET))
	$(call bdeps_align_macho,$(abspath $@))


# /deps/libargp --------------------------------------------
LIBARGP_REF=137154fb257055beb11f3283021d8eccc3c4f470
LIBARGP_BUILD_ZIG=deps/libargp/build.zig
deps-download/$(LIBARGP_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/actonlang/argp-standalone/archive/$(LIBARGP_REF).tar.gz

dist/deps/libargp: deps-download/$(LIBARGP_REF).tar.gz $(LIBARGP_BUILD_ZIG)
	rm -rf "$@"
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	cp "$(TD)/$(LIBARGP_BUILD_ZIG)" "$@/build.zig"
	rm -rf "$@/testsuite"
	touch "$(TD)/$@"

# /deps/libbsdnt --------------------------------------------
LIBBSDNT_REF=cf7db3414867b8b4a5561bc9aa94a8050d0225c4
LIBBSDNT_BUILD_ZIG=deps/libbsdnt/build.zig
deps-download/$(LIBBSDNT_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/actonlang/bsdnt/archive/$(LIBBSDNT_REF).tar.gz

dist/deps/libbsdnt: deps-download/$(LIBBSDNT_REF).tar.gz $(LIBBSDNT_BUILD_ZIG)
	rm -rf "$@"
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	cp "$(TD)/$(LIBBSDNT_BUILD_ZIG)" "$@/build.zig"
	rm -rf "$@/.github" "$@/configure" "$@/test"
	touch "$(TD)/$@"

# /deps/libgc --------------------------------------------
LIBGC_REF=0a23b211b558137de7ee654c5527a54113142517
LIBGC_BUILD_ZIG=deps/libgc/build.zig
deps-download/$(LIBGC_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/bdwgc/bdwgc/archive/$(LIBGC_REF).tar.gz

dist/deps/libgc: deps-download/$(LIBGC_REF).tar.gz $(LIBGC_BUILD_ZIG)
	rm -rf "$@"
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	cp "$(TD)/$(LIBGC_BUILD_ZIG)" "$@/build.zig"
	rm -rf "$@/.github" "$@/autogen.sh" "$@/cord/tests" "$@/docs" "$@/tests" "$@/tools"
	touch "$(TD)/$@"

# /deps/libmbedtls --------------------------------------------
LIBMBEDTLS_REF=c7d83538d3d359b05a9331bb2c9217977b5856ac
LIBMBEDTLS_BUILD_ZIG=deps/mbedtls/build.zig
deps-download/$(LIBMBEDTLS_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/actonlang/mbedtls/archive/$(LIBMBEDTLS_REF).tar.gz

dist/deps/mbedtls: deps-download/$(LIBMBEDTLS_REF).tar.gz $(LIBMBEDTLS_BUILD_ZIG)
	rm -rf "$@"
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	cp "$(TD)/$(LIBMBEDTLS_BUILD_ZIG)" "$@/build.zig"
	rm -rf "$@/.github" "$@/cmake" "$@/docs" "$@/doxygen" "$@/programs" "$@/scripts" "$@/visualc"
	# The TLS test server builds against mbed TLS' certificate fixtures.
	find "$@/tests" -mindepth 1 -maxdepth 1 ! -name include ! -name src -exec rm -rf {} +
	find "$@/tests/src" -mindepth 1 ! -name certs.c ! -name test_certs.h -exec rm -rf {} +
	touch "$(TD)/$@"

# /deps/libprotobuf_c --------------------------------------------
LIBPROTOBUF_C_REF=abc67a11c6db271bedbb9f58be85d6f4e2ea8389
LIBPROTOBUF_C_BUILD_ZIG=deps/libprotobuf_c/build.zig
deps-download/$(LIBPROTOBUF_C_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/protobuf-c/protobuf-c/archive/$(LIBPROTOBUF_C_REF).tar.gz

dist/deps/libprotobuf_c: deps-download/$(LIBPROTOBUF_C_REF).tar.gz $(LIBPROTOBUF_C_BUILD_ZIG)
	rm -rf "$@"
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	cp "$(TD)/$(LIBPROTOBUF_C_BUILD_ZIG)" "$@/build.zig"
	rm -rf "$@/.github" "$@/autogen.sh" "$@/build-cmake" "$@/protoc-c" "$@/t"
	rm -f "$@/.commit_docs.sh"
	chmod a-x "$@/protobuf-c/protobuf-c.h"
	touch "$(TD)/$@"

# /deps/tlsuv ---------------------------------------------
TLSUV_REF=e5765922d95c48937a5caecdd391ff0ad3bc16cd
TLSUV_BUILD_ZIG=deps/tlsuv/build.zig
TLSUV_BUILD_ZON=deps/tlsuv/build.zig.zon
deps-download/$(TLSUV_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/actonlang/tlsuv/archive/$(TLSUV_REF).tar.gz

dist/deps/tlsuv: deps-download/$(TLSUV_REF).tar.gz dist/deps/libuv dist/deps/mbedtls $(TLSUV_BUILD_ZIG) $(TLSUV_BUILD_ZON)
	rm -rf "$@"
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	cp "$(TD)/$(TLSUV_BUILD_ZIG)" "$@/build.zig"
	cp "$(TD)/$(TLSUV_BUILD_ZON)" "$@/build.zig.zon"
	rm -rf "$@/.github" "$@/.idea" "$@/cmake" "$@/deps/uv_link_t/docs" "$@/deps/uv_link_t/example" "$@/deps/uv_link_t/test" "$@/sample" "$@/tests"
	touch "$(TD)/$@"

# /deps/libutf8proc --------------------------------------
LIBUTF8PROC_REF=1cb28a66ca79a0845e99433fd1056257456cef8b
LIBUTF8PROC_BUILD_ZIG=deps/libutf8proc/build.zig
deps-download/$(LIBUTF8PROC_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/JuliaStrings/utf8proc/archive/$(LIBUTF8PROC_REF).tar.gz

dist/deps/libutf8proc: deps-download/$(LIBUTF8PROC_REF).tar.gz $(LIBUTF8PROC_BUILD_ZIG)
	rm -rf "$@"
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	cp "$(TD)/$(LIBUTF8PROC_BUILD_ZIG)" "$@/build.zig"
	rm -rf "$@/.github" "$@/bench" "$@/data" "$@/test"
	touch "$(TD)/$@"

# /deps/libuuid ------------------------------------------
dist/deps/libuuid: deps/libuuid
	mkdir -p "$(TD)/$@"
	cp -a "$</"* "$(TD)/$@"

# /deps/libuv --------------------------------------------
LIBUV_REF=d760a3f23511ebe7b1935fe1429147d4fca27bb4
LIBUV_BUILD_ZIG=deps/libuv/build.zig
deps-download/$(LIBUV_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/actonlang/libuv/archive/$(LIBUV_REF).tar.gz

dist/deps/libuv: deps-download/$(LIBUV_REF).tar.gz $(LIBUV_BUILD_ZIG)
	rm -rf "$@"
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<" "libuv-$(LIBUV_REF)/include" "libuv-$(LIBUV_REF)/src"
	cp "$(TD)/$(LIBUV_BUILD_ZIG)" "$@/build.zig"
	touch "$(TD)/$@"

# /deps/libxml2 ------------------------------------------
LIBXML2_REF=358ca4e6e34dd2b386aab1fdeb74a641c54940a0
LIBXML2_BUILD_ZIG=deps/libxml2/build.zig
deps-download/$(LIBXML2_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/actonlang/libxml2/archive/$(LIBXML2_REF).tar.gz

dist/deps/libxml2: deps-download/$(LIBXML2_REF).tar.gz $(LIBXML2_BUILD_ZIG)
	rm -rf "$@"
	mkdir -p "$@"
	cd "$@" && tar zx --exclude='libxml2-$(LIBXML2_REF)/test*' --strip-components=1 -f "$(TD)/$<"
	cp "$(TD)/$(LIBXML2_BUILD_ZIG)" "$@/build.zig"
	rm -rf "$@/.gitlab-ci" "$@/autogen.sh" "$@/doc" "$@/example" "$@/fuzz" "$@/os400" "$@/python" "$@/result" "$@/vms" "$@/win32" "$@/xstc" $@/test*
	rm -f "$@"/check-*.py "$@"/dbgen*.pl "$@"/gen*.py "$@"/gentest.py "$@"/build_glob.py "$@"/xml2-config.in
	touch "$(TD)/$@"

# /deps/pcre2 --------------------------------------------
LIBPCRE2_REF=e4ccef3034c870342f2d37c928e98bc8c69cd340
LIBPCRE2_BUILD_ZIG=deps/pcre2/build.zig
deps-download/$(LIBPCRE2_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/PCRE2Project/pcre2/archive/$(LIBPCRE2_REF).tar.gz

dist/deps/pcre2: deps-download/$(LIBPCRE2_REF).tar.gz $(LIBPCRE2_BUILD_ZIG)
	rm -rf "$@"
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	cp "$(TD)/$(LIBPCRE2_BUILD_ZIG)" "$@/build.zig"
	rm -rf "$@/.github" "$@/autogen.sh" "$@/doc" "$@/maint" "$@/testdata" "$@/vms"
	rm -f "$@"/132html "$@"/CheckMan "$@"/CleanTxt "$@"/Detrail "$@"/PrepareRelease "$@"/RunGrepTest "$@"/RunTest "$@"/pcre2-config.in "$@"/perltest.sh
	touch "$(TD)/$@"

# /deps/libsnappy_c --------------------------------------------
LIBSNAPPY_C_REF=dc05e026488865bc69313a68bcc03ef2e4ea8e83
LIBSNAPPY_C_BUILD_ZIG=deps/libsnappy_c/build.zig
deps-download/$(LIBSNAPPY_C_REF).tar.gz:
	mkdir -p deps-download
	$(CURL) -o $@ https://github.com/google/snappy/archive/$(LIBSNAPPY_C_REF).tar.gz

dist/deps/libsnappy_c: deps-download/$(LIBSNAPPY_C_REF).tar.gz $(LIBSNAPPY_C_BUILD_ZIG)
	rm -rf "$@"
	mkdir -p "$@"
	cd "$@" && tar zx --strip-components=1 -f "$(TD)/$<"
	cp "$(TD)/$(LIBSNAPPY_C_BUILD_ZIG)" "$@/build.zig"
	rm -rf "$@/.github" "$@/cmake" "$@/docs" "$@/testdata" "$@/third_party"
	touch "$(TD)/$@"

dist/deps/libnetstring: deps/libnetstring $(DIST_ZIG)
	mkdir -p "$(TD)/$@"
	cp -a "$</"* "$(TD)/$@"

dist/deps/libyyjson: deps/libyyjson $(DIST_ZIG)
	mkdir -p "$(TD)/$@"
	cp -a "$</"* "$(TD)/$@"

# top level targets
.PHONY: test test-builtins test-compiler test-db test-examples test-lang test-regressions test-rts test-stdlib online-tests
.PHONY: test-compiler-accept test-lib-accept test-acton-goldens-accept test-goldens-accept
# These run stack against libacton/acton, which link liblmdb, so the bdeps
# archives must exist first. (test, test-stdlib, test-incremental, online-tests
# already pull it in transitively via dist/bin/acton[c].)
test-builtins test-compiler test-lib-accept test-acton-goldens-accept test-cross-compile test-syntaxerrors test-syntaxerrors-accept test-typeerrors test-typeerrors-accept test-db test-examples test-lang test-regressions test-rts: $(BDEPS)
test: dist/bin/acton
	cd compiler && stack test libacton acton:incremental
	# Exclude the cross-compilation group from the default run: it builds for
	# many targets and bloats ~/.cache/acton. Run it via `make test-cross-compile`.
	# Exclude stdlib here because `test-stdlib` below runs the same compiler
	# stdlib group before the standalone stdlib_tests project.
	cd compiler && stack test acton:test_acton --ta '-p "! /cross-compilation/ && ! /stdlib/"'
	$(MAKE) test-stdlib
	$(MAKE) -C backend test
	$(MAKE) test-rts-db

test-builtins:
	cd compiler && stack test acton --ta '-p "Builtins"'

test-compiler:
	cd compiler && stack test libacton
	cd compiler && stack test acton --ta '-p "compiler"'

test-compiler-accept: test-lib-accept test-acton-goldens-accept

test-lib-accept:
	cd compiler && stack test libacton:test_lib --test-arguments "--golden-start --golden-reset"

test-acton-goldens-accept:
	cd compiler && stack test acton:test_acton --test-arguments "--accept"

test-goldens-accept: test-compiler-accept test-incremental-accept test-syntaxerrors-accept test-typeerrors-accept

test-cross-compile:
	cd compiler && stack test acton --ta '-p "/cross-compilation/"'

# Docker-based reproduction: acton must fetch a dependency through an HTTP proxy
# from a network with no direct egress. Defaults to ./dist; override ACTON_DIST
# (e.g. CI points it at the installed release). See test/proxy/README.md.
.PHONY: test-proxy
test-proxy:
	ACTON_DIST="$(or $(ACTON_DIST),$(CURDIR)/dist)" bash test/proxy/run.sh

test-incremental: dist/bin/acton
	cd compiler && stack test acton:incremental

.PHONY: test-incremental-accept
test-incremental-accept: dist/bin/acton
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

test-stdlib: dist/bin/acton dist/std
	cd compiler && stack test acton --ta '-p "stdlib"'
	$(MAKE) -C test tls-test-server
	cd test/stdlib_tests && "$(ACTON)" test

online-tests: dist/bin/acton
	cd compiler && stack test acton:test_acton_online


.PHONY: clean clean-all clean-base clean-std clean-bdeps clean-rpm
clean: clean-distribution clean-base clean-std clean-bdeps clean-rpm

clean-all: clean clean-compiler
	rm -rf $(ZIG_LOCAL_CACHE_DIR)

clean-base:
	rm -rf base/out

clean-std:
	rm -rf std/out

clean-bdeps:
	rm -rf bdeps/out bdeps/*/zig-out bdeps/*/zig-pkg bdeps/*/.zig-cache

clean-rpm:
	rm -rf rpmbuild

# == DIST ==
#

BACKEND_FILES = backend/Build.act backend/build.zig backend/build.zig.zon $(wildcard backend/*.c backend/*.h backend/failure_detector/*.c backend/failure_detector/*.h)
DIST_BACKEND_FILES = $(addprefix dist/,$(BACKEND_FILES)) dist/backend/deps dist/bin/actondb
dist/backend%: backend/%
	mkdir -p "$(dir $@)"
	cp -a "$<" "$@"

.PHONY: dist/base
dist/base: base base/.build base/__root.zig base/acton.zig base/build.zig base/build.zig.zon base/acton.zig dist/bin/acton $(DEPS) dist/backend/Build.act
	mkdir -p "$@" "$@/.build" "$@/out"
	rm -rf "$@/src" "$@/out/types/std"
	cp -a base/__root.zig base/Build.act base/acton.zig base/build.zig base/build.zig.zon base/builtin base/rts base/src dist/base/
	cd dist/base && ../bin/acton build --skip-build && rm -rf .build
	find "$@/out/types" -name lock.mdb -type f -delete
	find "$@/out/types" -name data.mdb -type f -exec chmod 0644 {} +

.PHONY: dist/std
dist/std: std std/Build.act std/build.zig std/build.zig.zon dist/base dist/bin/acton $(DEPS)
	mkdir -p "$@" "$@/.build" "$@/out"
	rm -rf "$@/src" "$@/out/types"
	cp -a std/Build.act std/build.zig std/build.zig.zon std/src dist/std/
	cd dist/std && ../bin/acton build --skip-build && rm -rf .build
	find "$@/out/types" -name lock.mdb -type f -delete
	find "$@/out/types" -name data.mdb -type f -exec chmod 0644 {} +

# This does a little hack, first copying and then moving the file in place. This
# is to avoid an error if the executable is currently running. cp tries to open
# the file and modify it, which the Linux kernel (and perhaps others?) will
# prevent if the file to be modified is an executable program that is currently
# running.  We work around it by moving / renaming the file in place instead!
ifeq ($(OS),macos)
# Workaround for macOS 26.4 CLI tools breaking Zig: point DEVELOPER_DIR to
# /dev/null to prevent Zig from trying to use them and instead falling back to
# its own implementation.
# See https://codeberg.org/ziglang/zig/issues/31658.
dist/bin/actondb: export DEVELOPER_DIR := /dev/null
endif
dist/bin/actondb: $(DIST_ZIG) $(DEPS)
	@mkdir -p $(dir $@)
	cd dist/backend && "$(ZIG)" build -Donly_actondb $(if $(ACTON_ZIG_TARGET),-Dtarget=$(ACTON_ZIG_TARGET)) --prefix "$(TD)/dist"

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

ZIG_TARBALL=zig-$(ARCH)-$(OS)-$(ZIG_VERSION).tar.xz
ZIG_DOWNLOAD_BASE_URL ?=
ifeq ($(strip $(ZIG_DOWNLOAD_BASE_URL)),)
ifeq ($(findstring -dev,$(ZIG_VERSION)),-dev)
ZIG_DOWNLOAD_URL ?= https://ziglang.org/builds/$(ZIG_TARBALL)
else
ZIG_DOWNLOAD_URL ?= https://ziglang.org/download/$(ZIG_VERSION)/$(ZIG_TARBALL)
endif
else
ZIG_DOWNLOAD_URL ?= $(patsubst %/,%,$(ZIG_DOWNLOAD_BASE_URL))/$(ZIG_TARBALL)
endif

dist/zig: deps-download/$(ZIG_TARBALL)
	mkdir -p "$@"
	cd "$@" && tar Jx --strip-components=1 -f "../../$^"
	rm -rf "$@/doc"
	cp -a deps/zig-extras/* "$@"


# By default Zig downloads come from ziglang.org. CI can set
# ZIG_DOWNLOAD_BASE_URL=https://github.com/actonlang/zigballs/raw/main
# to fetch the same tarball filename from our mirror.
deps-download/$(ZIG_TARBALL):
	mkdir -p deps-download
	$(CURL) -o $@ "$(ZIG_DOWNLOAD_URL)"

.PHONY: distribution1 distribution clean-distribution
distribution1: dist/base dist/std $(DIST_BACKEND_FILES) dist/builder $(DIST_BINS) $(DIST_ZIG)
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

.PHONY: acton-$(OS)-$(ARCH)-$(VERSION_INFO).tar.xz
acton-$(OS)-$(ARCH)-$(VERSION_INFO).tar.xz:
	tar cv $(TAR_TRANSFORM_OPT) --exclude .gitignore dist | xz -z -0 --threads=0 > "$@"

.PHONY: release
release: distribution
	$(MAKE) acton-$(OS)-$(ARCH)-$(VERSION_INFO).tar.xz

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

PACKAGE_VERSION_INFO := $(shell test -x dist/bin/acton && dist/bin/acton version || true)

.PHONY: package-dist-validate
package-dist-validate: VERSION_INFO := $(if $(PACKAGE_VERSION_INFO),$(PACKAGE_VERSION_INFO),$(VERSION_INFO))
package-dist-validate:
	@for path in bin/acton bin/actonc bin/actondb bin/runacton bin/lsp-server-acton zig/zig; do \
		test -x "dist/$$path" || { echo "ERROR: dist/$$path is missing; run make or extract a Linux release tarball first" >&2; exit 1; }; \
	done
	@for path in base std; do \
		test -d "dist/$$path" || { echo "ERROR: dist/$$path is missing; run make or extract a Linux release tarball first" >&2; exit 1; }; \
	done
	@dist_version="$$(dist/bin/acton version)"; \
	if [ "$$dist_version" != "$(VERSION_INFO)" ]; then \
		echo "ERROR: dist/bin/acton version ($$dist_version) does not match package version ($(VERSION_INFO))" >&2; \
		exit 1; \
	fi

.PHONY: debs
debs: VERSION_INFO := $(if $(PACKAGE_VERSION_INFO),$(PACKAGE_VERSION_INFO),$(VERSION_INFO))
debs: debian/changelog package-dist-validate
	debuild --preserve-envvar VERSION_INFO --preserve-envvar PATH -i -us -uc -nc -b

RPM_RELEASE ?= 1
.PHONY: rpms rpm-validate

rpms: VERSION_INFO := $(if $(PACKAGE_VERSION_INFO),$(PACKAGE_VERSION_INFO),$(VERSION_INFO))
rpms: package-dist-validate rpm/acton.spec.in LICENSE README.md
	command -v rpmbuild >/dev/null 2>&1 || { echo "ERROR: rpmbuild is required to build RPM packages"; exit 1; }
	command -v tar >/dev/null 2>&1 || { echo "ERROR: tar is required to build RPM packages"; exit 1; }
	command -v xz >/dev/null 2>&1 || { echo "ERROR: xz is required to build RPM packages"; exit 1; }
	rm -rf rpmbuild/payload
	mkdir -p rpmbuild/payload rpmbuild/SOURCES
	$(MAKE) install DESTDIR="$(TD)/rpmbuild/payload"
	install -D -m 0644 completion/acton.bash-completion "rpmbuild/payload/usr/share/bash-completion/completions/acton"
	tar -C rpmbuild/payload -cJf "rpmbuild/SOURCES/acton-rpm-payload-$(VERSION_INFO).tar.xz" .
	mkdir -p rpmbuild/SPECS rpmbuild/SOURCES
	sed -e 's,@VERSION@,$(VERSION_INFO),g' -e 's,@RELEASE@,$(RPM_RELEASE),g' "rpm/acton.spec.in" > rpmbuild/SPECS/acton.spec
	cp LICENSE README.md rpmbuild/SOURCES/
	mkdir -p rpmbuild/BUILD rpmbuild/BUILDROOT rpmbuild/RPMS rpmbuild/SRPMS
	# BuildRequires are checked against the RPM DB, but CI also builds this on Ubuntu.
	rpmbuild --nodeps --define "_topdir $(TD)/rpmbuild" -bb rpmbuild/SPECS/acton.spec
	@find rpmbuild/RPMS -name '*.rpm' -print

rpm-validate:
	@command -v docker >/dev/null 2>&1 || { echo "ERROR: docker is required to validate RPM packages"; exit 1; }
	@rpm_file="$$(find "$(TD)/rpmbuild/RPMS" -name 'acton-[0-9]*.rpm' 2>/dev/null | sort | tail -n1)"; \
	if [ -z "$$rpm_file" ]; then \
		echo "ERROR: no Acton RPM found under rpmbuild/RPMS; run 'make rpms' first"; \
		exit 1; \
	fi; \
	rpm_dir="$$(dirname "$$rpm_file")"; \
	rpm_base="$$(basename "$$rpm_file")"; \
	echo "Validating $$rpm_base in fedora:latest"; \
	docker run --rm -v "$$rpm_dir:/rpms:ro" -e ACTON_RPM="/rpms/$$rpm_base" fedora:latest sh -lc '\
		set -eu; \
		rpm -qip "$$ACTON_RPM"; \
		dnf install -y "$$ACTON_RPM"; \
		rpm -q acton; \
		acton version; \
		acton --help >/dev/null; \
		test -x /usr/lib/acton/bin/acton; \
		test -L /usr/bin/acton; \
		test -L /usr/bin/actonc; \
		test -L /usr/bin/actondb; \
		test -L /usr/bin/runacton; \
		test -L /usr/bin/lsp-server-acton; \
		work="$$(mktemp -d)"; \
		cd "$$work"; \
		printf "%s\n" "#!/usr/bin/env runacton" "actor main(env):" "    print(\"Hello, world\")" "    env.exit(0)" > acton-test.act; \
		chmod a+x acton-test.act; \
		./acton-test.act; \
		./acton-test.act | grep "Hello, world"; \
		acton build acton-test.act; \
		./acton-test; \
		./acton-test | grep "Hello, world"'

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
