MK_PATH:=$(shell dirname $(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
ACTONC=$(MK_PATH)/dist/bin/actonc --cpedantic
DDB_SERVER=../dist/bin/actondb
TESTS= \
	$(ACTONC_TESTS) \
	$(ENV_TESTS) \
	$(RTS_TESTS) \
	$(DDB_TESTS) \
	test_acton_rts_sleep \
	test_random \
	test_time \
	rts_sleep \
	regression \
	stdlib/test_numpy
test:
	$(MAKE) $(TESTS)

regression:
	$(MAKE) -C regression


# -- actonc ---------------------------------------------------------------------
ACTONC_TESTS = $(ACTONC_PROJ_TESTS) $(ACTONC_ROOT_TESTS)
.PHONY: test-actonc

test-actonc:
	$(MAKE) $(ACTONC_TESTS)

# -- actonc project -------------------------------------------------------------
ACTONC_PROJ_TESTS := actonc/project/simple actonc/project/missing_src actonc/project/qualified_root actonc/project/qualified_root§unqualified
.PHONY: actonc/project/simple actonc/project/missing_src actonc/project/qualified_root actonc/project/qualified_root§unqualified
# Verify simple project compiles and we see output files
actonc/project/simple:
	cd $@ && $(ACTONC) build
	@ls $@/out/types/a.h
	@ls $@/out/types/a.ty
	@ls $@/out/types/b.h
	@ls $@/out/types/b.ty

# Verify we get error when there's no src/ directory in project
actonc/project/missing_src:
	cd $@ && $(ACTONC) build; EXIT_CODE=$$?; if [ $$EXIT_CODE -ne 1 ]; then exit 1; fi
	cd $@ && $(ACTONC) build 2>&1 | grep "Missing src/ directory"

# Verify detection of root actor
actonc/project/auto_root:
	cd $@ && $(ACTONC) build
	-@ls $@/out/rel/bin/test

# Verify root argument as a qualified name
# TODO: remove the rm -rf here, it should not be needed, but when removed,
# multiple test invokations fail
actonc/project/qualified_root:
	rm -rf $@/out
	cd $@ && $(ACTONC) build --verbose --root test.main
	@ls $@/out/rel/bin/test
	$@/out/rel/bin/test | grep "qualified_root foo"

N=$(word 1,$(subst §, ,$@))
actonc/project/qualified_root§unqualified:
	rm -rf $(N)/out
	cd $(N) && $(ACTONC) build --root main; EXIT_CODE=$$?; if [ $$EXIT_CODE -ne 1 ]; then exit 1; fi
	cd $(N) && $(ACTONC) build --root main 2>&1 | grep "Project build requires a qualified root actor name"

# -- actonc root actor ----------------------------------------------------------
.PHONY: actonc/root/test
ACTONC_ROOT_TESTS = actonc/root/test
actonc/root/test:
	rm -f $@
	$(ACTONC) $@.act --root test.main
	$@
	rm -f $@
	$(ACTONC) $@.act --root main
	$@


ENV_TESTS=env/listen_err
.PHONY: $(ENV_TESTS)
env/listen_err:
	$(ACTONC) --root main $@.act
	./$@


ddb-tests:
	$(MAKE) $(DDB_TESTS)

DDB_TESTS=test_db_app test_db_app_no_quorum test_db_app_recovery test_db_app_resume_tcp_client test_db_app_resume_tcp_server
.PHONY: $(DDB_TESTS)

# Starts up a database cluster, checks membership is ok before proceeding to run
# a simple app. We do not really verify that the RTS uses the database - we
# assume it does and would fail catastrohpically if it encounters an error.
test_db_app:
	$(ACTONC) --root main test_db_app.act
	./test_db.py TestDbApps.test_app

test_db_app_no_quorum:
	@echo "Skipping because this is essentially broken"
	#$(ACTONC) --root main test_db_app.act
	#./test_db.py TestDbAppsNoQuorum

test_db_app_resume_tcp_server:
	$(ACTONC) --root main --dev rts/ddb_test_server.act
	./test_db.py TestDbApps.test_app_resume_tcp_server

test_db_app_resume_tcp_client:
	$(ACTONC) --root main --dev rts/ddb_test_server.act
	$(ACTONC) --root main --dev rts/ddb_test_client.act
	./test_db.py TestDbApps.test_app_resume_tcp_client


# -- RTS --
RTS_TESTS = rts/argv1 rts/argv2 rts/argv3 rts/exit_0 rts/exit_1
.PHONY: $(RTS_TESTS)

test-rts: $(RTS_TESTS)

# Test normal argument parsing
rts/argv1:
	$(ACTONC) --root main $@.act
	./$@ --rts-verbose foo --bar --rts-verbose
	./$@ --rts-verbose --rts-wthreads 7 foo --bar
	./$@ --rts-verbose --rts-wthreads=7 foo --bar
	./$@ --rts-verbose --rts-wthreads 7 foo --bar 2>&1 | grep "Using 7 worker threads"
	./$@ --rts-verbose --rts-wthreads=7 foo --bar 2>&1 | grep "Using 7 worker threads"

# Test argument parsing when using --
rts/argv2:
	$(ACTONC) --root main $@.act
	./$@ --rts-verbose --rts-wthreads=7 -- foo --bar --rts-verbose

# Test argument parsing for lacking argument to option
rts/argv3:
	$(ACTONC) --root main $@.act
	./$@ --rts-wthreads 2>&1 | grep "ERROR: --rts-wthreads requires an argument."
	@echo "Test success, saw expected error message"

rts/exit_0:
	$(ACTONC) --root main $@.act
	./$@

rts/exit_1:
	$(ACTONC) --root main $@.act
	./$@; EXIT_CODE=$$?; if [ $$EXIT_CODE -ne 1 ]; then exit 1; fi

# Expect 9 threads given 7 workers + main process + IO
rts/wthreads1:
	$(ACTONC) --root main $@.act
	./$@ --rts-wthreads 7 & PID=$$! && ps -o thcount $${PID} | tail -n1 | awk '{ print $$1 }' | grep "^9$$"

#--

test_acton_rts_sleep:
	$(ACTONC) --root main $@.act
	./$@

test_async:
	$(ACTONC) --root main $@.act
	./$@

test_random:
	$(ACTONC) --root main $@.act
	./$@

rts_sleep:
	$(ACTONC) --root main $@.act
	./$@

test_time:
	$(ACTONC) --root main $@.act
	./test_time $(shell date "+%s")

.PHONY: stdlib/test_numpy
stdlib/test_numpy: stdlib/test_numpy.act
	$(ACTONC) --root main $<
	./$@

.PHONY: test_acton_rts_sleep test_async test_random test_time regression rts_sleep
