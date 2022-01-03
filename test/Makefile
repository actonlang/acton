ACTONC=../dist/bin/actonc --cpedantic
DDB_SERVER=../dist/bin/actondb
TESTS= \
	$(RTS_TESTS) \
	ddb_count \
	test_acton_rts_sleep \
	test_random \
	test_time \
	rts_sleep \
	regression \
	stdlib/test_numpy
test: $(TESTS)

regression:
	$(MAKE) -C regression

ddb_start:
	$(DDB_SERVER) -p 32000 -m 34000 -s 127.0.0.1:34000 >db1.log 2>&1 & echo $$! > db1.pid
	sleep 0.1
#	$(DDB_SERVER) -p 32001 -m 34001 -s 127.0.0.1:34000 >db2.log 2>&1 &
#	sleep 0.1
#	$(DDB_SERVER) -p 32002 -m 34002 -s 127.0.0.1:34000 >db3.log 2>&1 &
#	sleep 0.1

ddb_stop:
	-kill $$(cat db1.pid); rm db1.pid
	-pkill -f "actondb.*-s 127.0.0.1.34000"


# This is a really naive test. We don't even check the output of the program so
# we do not check that the actual persistence and restoration of state is
# working. We assume that stuff will attempt to do its business and if anything
# goes wrong, it will have catastrophic failure so that something will return an
# incorrect return code and this test case can fail. This should really be
# improved upon :)
ddb_count:
	$(MAKE) ddb_stop
	$(MAKE) ddb_start
	$(ACTONC) --root main count.act
#	./count 8 --rts-verbose --rts-ddb-host 127.0.0.1 --rts-ddb-replication 1 &
#	sleep 5
#	pkill -f count.8
#	sleep 1
	./count 1 --rts-verbose --rts-ddb-host 127.0.0.1 --rts-ddb-replication 1
	$(MAKE) ddb_stop


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
