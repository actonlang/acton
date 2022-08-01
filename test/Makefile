MK_PATH:=$(shell dirname $(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
ACTONC=$(MK_PATH)/dist/bin/actonc --cpedantic
DDB_SERVER=../dist/bin/actondb
TESTS= \
	$(DDB_TESTS)
test:
	$(MAKE) $(TESTS)


ddb-tests:
	$(MAKE) $(DDB_TESTS)

DDB_TESTS=test_db_app_no_quorum
.PHONY: $(DDB_TESTS)

test_db_app_no_quorum:
	@echo "Skipping because this is essentially broken"
	#$(ACTONC) --root main test_db_app.act
	#./test_db.py TestDbAppsNoQuorum


# Expect 9 threads given 7 workers + main process + IO
rts/wthreads1:
	$(ACTONC) --root main $@.act
	./$@ --rts-wthreads 7 & PID=$$! && ps -o thcount $${PID} | tail -n1 | awk '{ print $$1 }' | grep "^9$$"
