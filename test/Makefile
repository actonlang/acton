ACTONC=../dist/bin/actonc --cpedantic
DDB_SERVER=../dist/bin/actondb
TESTS= \
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


ENV_TESTS=env/listen_err
.PHONY: $(ENV_TESTS)
env/listen_err:
	$(ACTONC) --root main $@.act
	./$@


ddb-tests:
	$(MAKE) $(DDB_TESTS)

DDB_TESTS=test_db_app test_db_app_no_quorum test_db_app_recovery test_db_app_tcp
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

test_db_app_tcp:
	$(ACTONC) --root main --dev rts/ddb_test_server.act
	./test_db.py TestDbApps.test_app_tcp_recovery


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
	./$@ --rts-verbose --rts-wthreads 7 foo --bar | grep "Using 7 worker threads"
	./$@ --rts-verbose --rts-wthreads=7 foo --bar | grep "Using 7 worker threads"

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
