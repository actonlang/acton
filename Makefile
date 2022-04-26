include common.mk
CHANGELOG_VERSION=$(shell grep '^\#\# \[[0-9]' CHANGELOG.md | sed 's/\#\# \[\([^]]\{1,\}\)].*/\1/' | head -n1)

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
LDLIBS+=-lprotobuf-c -lm -lpthread

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


# /backend ----------------------------------------------
backend/actondb: backend/actondb.c lib/libActonDB.a
	$(CC) -o$@ $< $(CFLAGS) \
		$(LDFLAGS) \
		-lActonDB \
		$(LDLIBS)

backend/%.o: backend/%.c
	$(CC) -DLOG_USE_COLOR -g -o$@ $< -c $(CFLAGS)

backend/failure_detector/%.o: backend/failure_detector/%.c
	$(CC) -g -o$@ $< -c $(CFLAGS)

# This target is just to override the above target and turn it into a NOP for
# the protobuf files. We have the generated files committed to git, so this
# isn't something we normally want to regenerate anyway.
backend/failure_detector/db_messages.pb-c.c: backend/failure_detector/db_messages.proto
	true

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
ENV_FILES=$(wildcard builtin/env.*)
BUILTIN_HFILES=$(filter-out $(ENV_FILES),$(wildcard builtin/*.h))
BUILTIN_CFILES=$(filter-out $(ENV_FILES),$(wildcard builtin/*.c))
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
# all other modules depend on it, so it must be compiled first. While we use
# wildcard patterns for all other files, we have explicit targets for
# __builtin__.ty to make things work. Other .ty file targets etc depend on this,
# so we get the order right.
dist/types/__builtin__.ty: stdlib/out/types/__builtin__.ty
	@mkdir -p $(dir $@)
	cp $< $@

stdlib/out/types/__builtin__.ty: stdlib/src/__builtin__.act $(ACTONC)
	@mkdir -p $(dir $@)
	$(ACTC) $<

# Build our standard library
# We use a single target as a focal point to get serialization since we cannot
# build these things in parallel
# Compiling these .act files with and with --dev will produce
# stdlib/out/lib/libActonProject_rel.a and stdlib/out/lib/libActonProject_dev.a which we then rename
.PHONY: stdlib_project
stdlib_project: $(STDLIB_ACTFILES_NS) dist/types/__builtin__.ty $(ACTONC)
	echo $(STDLIB_ACTFILES_NS) | $(XARGS) -n1 $(ACTC)
	echo $(STDLIB_ACTFILES_NS) | $(XARGS) -n1 $(ACTC) --dev
	cp -a stdlib/out/types/. dist/types/

stdlib/out/dev/lib/libActonProject.a stdlib/out/rel/lib/libActonProject.a: $(STDLIB_ACTFILES) $(ACTONC)
	$(MAKE) stdlib_project


# /lib --------------------------------------------------
DBARCHIVE=lib/libActonDB.a
ARCHIVES=lib/dev/libActon.a lib/rel/libActon.a

# If we later let actonc build things, it would produce a libActonProject.a file
# in the stdlib directory, which we would need to join together with rts.o etc
# to form the final libActon (or maybe produce a libActonStdlib and link with?)

LIBACTON_DEV_OFILES=builtin/builtin_dev.o builtin/env_dev.o rts/empty.o rts/log.o rts/rts_dev.o deps/netstring_dev.o deps/yyjson_dev.o
OFILES += $(LIBACTON_DEV_OFILES)
lib/dev/libActon.a: stdlib/out/dev/lib/libActonProject.a  $(LIBACTON_DEV_OFILES)
	@mkdir -p $(dir $@)
	cp -a $< $@
	ar rcs $@ $(filter-out stdlib/out/dev/lib/libActonProject.a,$^)

LIBACTON_REL_OFILES=$(LIBACTON_DEV_OFILES:_dev.o=_rel.o)
OFILES += $(LIBACTON_REL_OFILES)
lib/rel/libActon.a: stdlib/out/rel/lib/libActonProject.a $(LIBACTON_REL_OFILES)
	@mkdir -p $(dir $@)
	cp -a $< $@
	ar rcs $@ $(filter-out stdlib/out/rel/lib/libActonProject.a,$^)

COMM_OFILES += backend/comm.o rts/empty.o
DB_OFILES += backend/db.o backend/queue.o backend/skiplist.o backend/txn_state.o backend/txns.o rts/empty.o
DBCLIENT_OFILES += backend/client_api.o rts/empty.o
REMOTE_OFILES += backend/failure_detector/db_messages.pb-c.o backend/failure_detector/cells.o backend/failure_detector/db_queries.o backend/failure_detector/fd.o
VC_OFILES += backend/failure_detector/vector_clock.o
BACKEND_OFILES=$(COMM_OFILES) $(DB_OFILES) $(DBCLIENT_OFILES) $(REMOTE_OFILES) $(VC_OFILES) backend/log.o deps/netstring_rel.o deps/yyjson_rel.o
OFILES += $(BACKEND_OFILES)
lib/libActonDB.a: $(BACKEND_OFILES)
	ar rcs $@ $^


# /rts --------------------------------------------------
OFILES += rts/log.o rts/rts_dev.o rts/rts_rel.o rts/empty.o
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

.PHONY: test
test:
	$(MAKE) -C backend test
	$(MAKE) -C test

.PHONY: clean
clean: clean-compiler clean-distribution clean-backend clean-rts

.PHONY: clean-backend
clean-backend:
	rm -f $(BACKEND_OFILES) backend/actondb

.PHONY: clean-rts
clean-rts:
	rm -rf $(ARCHIVES) $(DBARCHIVE) $(OFILES) $(STDLIB_HFILES) $(STDLIB_OFILES) $(STDLIB_TYFILES) stdlib/out/

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
dist/bin/actondb: backend/actondb
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

DIST_BINS=$(ACTONC) dist/bin/actondb
DIST_HFILES=dist/rts/rts.h \
	dist/builtin/env.h \
	$(addprefix dist/,$(BUILTIN_HFILES))
DIST_DBARCHIVE=$(addprefix dist/,$(DBARCHIVE))
DIST_ARCHIVES=$(addprefix dist/,$(ARCHIVES))

.PHONY: distribution clean-distribution
# Our stdlib (via DIST_ARCHIVES) must not be compiled concurrently, so we avoid
# listing it as a dependency and instead run it in the target
distribution: $(DIST_BINS) $(DIST_HFILES) $(DIST_TYFILES) $(DIST_DBARCHIVE)
	$(MAKE) -j1 $(DIST_ARCHIVES)

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

.PHONY: debian/changelog
debian/changelog: debian/changelog.in CHANGELOG.md
	cat $< | sed 's/VERSION/$(VERSION_INFO)/' > $@

.PHONY: debs
debs: debian/changelog
	debuild --preserve-envvar VERSION_INFO -i -us -uc -b
