ACTONC=../dist/bin/actonc --cpedantic
DDB_SERVER=../dist/bin/actondb
TESTS= \
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


ddb-tests:
	$(MAKE) $(DDB_TESTS)

DDB_TESTS=test_db_membership test_db_app test_db_app_recovery
.PHONY: $(DDB_TESTS)
# Starts up a database cluster and checks membership looks ok.
test_db_membership:
	./test_db.py --db-nodes 3

# Starts up a database cluster, checks membership is ok before proceeding to run
# a simple app. We do not really verify that the RTS uses the database - we
# assume it does and would fail catastrohpically if it encounters an error.
test_db_app:
	$(ACTONC) --root main test_db_app.act
	./test_db.py --db-nodes 3 --test-app

test_db_app_recovery:
	$(ACTONC) --root main test_db_recovery.act
	./test_db.py --db-nodes 1 --test-recovery


# -- RTS --
RTS_TESTS = rts/argv1 rts/argv2 rts/argv3

# Test normal argument parsing
rts/argv1:
	$(ACTONC) --root main $@.act
	./$@ --rts-verbose foo --bar --rts-verbose
	./$@ --rts-verbose --rts-wthreads 7 foo --bar
	./$@ --rts-verbose --rts-wthreads=7 foo --bar

# Test argument parsing when using --
rts/argv2:
	$(ACTONC) --root main $@.act
	./$@ --rts-verbose --rts-wthreads=7 -- foo --bar --rts-verbose

# Test argument parsing for lacking argument to option
rts/argv3:
	$(ACTONC) --root main $@.act
	./$@ --rts-wthreads 2>&1 | grep "ERROR: --rts-wthreads requires an argument."
	@echo "Test success, saw expected error message"

# Expect 9 threads given 7 workers + main process + IO
rts/wthreads1:
	$(ACTONC) --root main $@.act
	./$@ --rts-wthreads 7 & PID=$$! && ps -o thcount $${PID} | tail -n1 | awk '{ print $$1 }' | grep "^9$$"


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

.PHONY: rts/argv1 rts/argv2 rts/argv3 rts/wthreads1 test_acton_rts_sleep test_async test_random test_time regression rts_sleep
