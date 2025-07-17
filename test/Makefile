MK_PATH:=$(shell dirname $(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
ACTONC=$(MK_PATH)/dist/bin/actonc --quiet
DDB_SERVER=../dist/bin/actondb
TESTS= \
	$(DDB_TESTS)
test:
	$(MAKE) $(TESTS)


ddb-tests:
	$(MAKE) $(DDB_TESTS)

DDB_TESTS=test_db_app test_db_resume_tcp_server test_db_resume_tcp_client
.PHONY: $(DDB_TESTS)

# Starts up a database cluster, checks membership is ok before proceeding to run
# a simple app. We do not really verify that the RTS uses the database - we
# assume it does and would fail catastrohpically if it encounters an error.
test_db_app:
	$(ACTONC) --db rts_db/ddb_test_app.act
	$(ACTONC) --db rts_db/test_db_app.act
	rts_db/test_db_app

test_db_resume_tcp_server:
	$(ACTONC) --db rts_db/ddb_test_server.act
	$(ACTONC) --db rts_db/test_tcp_server.act
	rts_db/test_tcp_server

test_db_resume_tcp_client:
	$(ACTONC) --db rts_db/ddb_test_server.act
	$(ACTONC) --db rts_db/ddb_test_client.act
	$(ACTONC) --db rts_db/test_tcp_client.act
	rts_db/test_tcp_client


# Expect 9 threads given 7 workers + main process + IO
rts/wthreads1:
	$(ACTONC) --root main $@.act
	./$@ --rts-wthreads 7 & PID=$$! && ps -o thcount $${PID} | tail -n1 | awk '{ print $$1 }' | grep "^9$$"
