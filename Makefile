all:
	$(MAKE) -C compiler install
	$(MAKE) -C modules
	$(MAKE) -C builtin
	$(MAKE) -C rts
	$(MAKE) -C math
	$(MAKE) -C numpy

test:
	$(MAKE) -C test

clean:
	rm -f actonc
	$(MAKE) -C compiler clean
	$(MAKE) -C modules clean
	$(MAKE) -C builtin clean
	$(MAKE) -C rts clean
	$(MAKE) -C math clean
	$(MAKE) -C numpy clean

.PHONY: all clean test
