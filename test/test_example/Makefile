ACTON_DIR ?= ../..

# source files
#SRC := $(ACTON_DIR)/something.c

# static libs, if any
#LIBS := $(ACTON_DIR)/deps/libpqueue/libpqueue.a

# necessary compiler flags
CFLAGS := -std=c11 \
	-Wall \
	-Werror \
	-pedantic \
	-pedantic-errors \
	-Wno-int-to-void-pointer-cast \
	-Wno-int-to-pointer-cast \
	-Wno-pointer-to-int-cast \
	-Wno-error=int-to-pointer-cast \
	-Wno-unknown-pragmas \
	-I$(ACTON_DIR) \
	-I$(ACTON_DIR)/deps/libpqueue/src \
	-D_XOPEN_SOURCE=600

# necessary linker flags, if any
#LDFLAGS :=

include ../Makefile.test
