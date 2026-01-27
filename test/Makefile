MK_PATH:=$(abspath $(CURDIR)/..)
ACTON="../dist/bin/acton" --quiet
DDB_SERVER=../dist/bin/actondb
TESTS= \
	$(DDB_TESTS)
test:
	$(MAKE) $(TESTS)


ddb-tests:
	$(MAKE) $(DDB_TESTS)

DDB_TESTS=test_db_app test_db_resume_tcp_server test_db_resume_tcp_client
.PHONY: $(DDB_TESTS)

.PHONY: tls-test-server
tls-test-server:
	../dist/zig/zig cc -pthread -I../dist/deps/mbedtls/include -I../dist/deps/mbedtls/tests/include -I../dist/deps/mbedtls/library -Istdlib -DMBEDTLS_USER_CONFIG_FILE=\"tls_test_mbedtls_config.h\" -o stdlib/tls_test_server stdlib/tls_test_server.c ../dist/deps/mbedtls/tests/src/certs.c ../dist/deps/mbedtls/library/*.c

# Starts up a database cluster, checks membership is ok before proceeding to run
# a simple app. We do not really verify that the RTS uses the database - we
# assume it does and would fail catastrohpically if it encounters an error.
test_db_app:
	$(ACTON) --db rts_db/ddb_test_app.act
	$(ACTON) --db rts_db/test_db_app.act
	rts_db/test_db_app

test_db_resume_tcp_server:
	$(ACTON) --db rts_db/ddb_test_server.act
	$(ACTON) --db rts_db/test_tcp_server.act
	rts_db/test_tcp_server

test_db_resume_tcp_client:
	$(ACTON) --db rts_db/ddb_test_server.act
	$(ACTON) --db rts_db/ddb_test_client.act
	$(ACTON) --db rts_db/test_tcp_client.act
	rts_db/test_tcp_client


# Expect 9 threads given 7 workers + main process + IO
rts/wthreads1:
	$(ACTON) --root main $@.act
	./$@ --rts-wthreads 7 & PID=$$! && ps -o thcount $${PID} | tail -n1 | awk '{ print $$1 }' | grep "^9$$"
