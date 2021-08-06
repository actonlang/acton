DDB_SERVER=../backend/server
test:
	$(MAKE) -C regression

ddb_start:
	$(DDB_SERVER) -p 32000 -m 34000 -s 127.0.0.1:34000 >db1.log 2>&1 &
	sleep 0.1
	$(DDB_SERVER) -p 32001 -m 34001 -s 127.0.0.1:34000 >db2.log 2>&1 &
	sleep 0.1
	$(DDB_SERVER) -p 32002 -m 34002 -s 127.0.0.1:34000 >db3.log 2>&1 &
	sleep 0.1

ddb_stop:
	-pkill -f "server.*-s 127.0.0.1.34000"

# This is a really naive test. We don't even check the output of the program so
# we do not check that the actual persistence and restoration of state is
# working. We assume that stuff will attempt to do its business and if anything
# goes wrong, it will have catastrophic failure so that something will return an
# incorrect return code and this test case can fail. This should really be
# improved upon :)
ddb_count:
	$(MAKE) ddb_start
	../actonc --root main count.act
	./count 8 --rts-verbose --rts-ddb-host 127.0.0.1 &
	sleep 5
	pkill -f count.8
	sleep 1
	./count 8 --rts-verbose --rts-ddb-host 127.0.0.1
	$(MAKE) ddb_stop


argv:
	../actonc --root main argv.act
	./argv --rts-verbose foo --bar --rts-verbose

.PHONY: argv
