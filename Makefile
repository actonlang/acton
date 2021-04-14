all:
	cd compiler && $(MAKE) install
	cd modules && $(MAKE)
	cd builtin && $(MAKE)
	cd rts && $(MAKE)
	cd math && $(MAKE)
	#cd numpy && $(MAKE)

test:
	$(MAKE) -C test

clean:
	rm -f actonc
	cd compiler && $(MAKE) clean
	cd modules && $(MAKE) clean
	cd builtin && $(MAKE) clean
	cd rts && $(MAKE) clean
	cd math && $(MAKE) clean
	cd numpy && $(MAKE) clean


.PHONY: test
