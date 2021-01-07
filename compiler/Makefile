#ACTON_BINDIR  := ../bin
ACTON_BINDIR  := ..

VERSION=$(shell awk -F= '/ACTON_VERSION=/ {print $$2}' ../VERSION)


.SUFFIXES : .o .hs .hi .lhs .hc .s
.PHONY: all build install clean

all: build

build: package.yaml
	stack build

package.yaml: package.yaml.in ../VERSION
	@echo "  ver   " $(VERSION)
	@sed 's/^version:.*/version: $(VERSION)/' <$< >$@ || (rm -f $@; exit 1)

install: build
	@stack --local-bin-path=$(ACTON_BINDIR) install 2>/dev/null

clean:
	@stack clean
	@rm -f $(ACTON_BINDIR)/actonc
	@rm -f package.yaml
