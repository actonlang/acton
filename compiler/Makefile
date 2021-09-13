include ../common.mk
ACTON_BINDIR  := .
BUILD_TIME=$(shell date "+%Y%m%d.%-H.%-M.%-S")
ifdef BUILD_RELEASE
VERSION_INFO:=$(VERSION)
else
VERSION_INFO:=$(VERSION).$(BUILD_TIME)
endif

.SUFFIXES : .o .hs .hi .lhs .hc .s
.PHONY: all build install clean

all: build

build: package.yaml
	stack build

package.yaml: package.yaml.in
	@echo "  ver   " $(VERSION)
	sed 's,^version:.*,version:      "$(VERSION_INFO)",' <$< >$@ || (rm -f $@; exit 1)


install: build
	stack --local-bin-path=$(ACTON_BINDIR) install 2>/dev/null

clean:
	-stack clean
	rm -f $(ACTON_BINDIR)/actonc package.yaml acton.cabal
