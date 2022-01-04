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


foo:
	@echo $(TESTS)

DDB_TESTS=test_db_membership test_db_app
.PHONY: test_db_membership test_db_app
# Starts up a database cluster and checks membership
test_db_membership:
	./test_db.py --db-nodes 3

# Starts up a database cluster and
# Currently using a single (1) DB node since replication is b0rked!
test_db_app:
	$(ACTONC) --root main test_db.act
	./test_db.py --db-nodes 1 --app ./test_db


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

.PHONY: rts/argv1 rts/argv2 rts/argv3 test_acton_rts_sleep test_async test_random test_time regression rts_sleep
