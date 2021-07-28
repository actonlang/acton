ACTONC=../actonc

test:
	$(MAKE) -C regression

time:
	$(ACTONC) --root main time.act
	./time $(shell date "+%s")

.PHONY: time
