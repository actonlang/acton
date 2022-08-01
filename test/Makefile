MK_PATH:=$(shell dirname $(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
ACTONC=$(MK_PATH)/dist/bin/actonc --cpedantic
TESTS=test_db_app_no_quorum rts/wthreads1
test:
	$(MAKE) $(TESTS)

.PHONY: $(TESTS)

test_db_app_no_quorum:
	@echo "Skipping because this is essentially broken"
	#$(ACTONC) --root main test_db_app.act
	#./test_db.py TestDbAppsNoQuorum


# Expect 10 threads given 7 workers + main process + IO + new IO
rts/wthreads1:
	$(ACTONC) $@.act
	./$@ --rts-wthreads 7 & PID=$$! && ps -o thcount $${PID} | tail -n1 | awk '{ print $$1 }' | grep "^10$$"
