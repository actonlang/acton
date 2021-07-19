all: compiler rts

compiler:
	$(MAKE) -C compiler install

backend:
	$(MAKE) -C backend

rts: backend
	$(MAKE) -C modules
	$(MAKE) -C builtin
	$(MAKE) -C rts
	$(MAKE) -C math
	$(MAKE) -C numpy

test:
	$(MAKE) -C test

clean: clean-compiler clean-backend clean-rts

clean-compiler:
	rm -f actonc
	$(MAKE) -C compiler clean

clean-backend:
	$(MAKE) -C backend clean

clean-rts:
	$(MAKE) -C modules clean
	$(MAKE) -C builtin clean
	$(MAKE) -C rts clean
	$(MAKE) -C math clean
	$(MAKE) -C numpy clean

.PHONY: all compiler backend rts clean clean-compiler clean-backend clean-rts test
