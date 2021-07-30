include common.mk

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

ARCH=$(shell uname -s -m | sed -e 's/ /-/' | tr '[A-Z]' '[a-z]')
RELEASE_MANIFEST=actonc backend builtin lib math modules numpy rts
GNU_TAR := $(shell ls --version 2>&1 | grep GNU >/dev/null 2>&1; echo $$?)
ifeq ($(GNU_TAR),0)
TAR_TRANSFORM_OPT=--transform 's,^,acton/,'
else
TAR_TRANSFORM_OPT=-s ,^,acton/,
endif
acton-$(ARCH)-$(VERSION).tar.bz2:
	tar jcvf $@ $(TAR_TRANSFORM_OPT) $(RELEASE_MANIFEST)

release: acton-$(ARCH)-$(VERSION).tar.bz2

.PHONY: all compiler backend rts clean clean-compiler clean-backend clean-rts test release acton-$(ARCH)-$(VERSION).tar.bz2
