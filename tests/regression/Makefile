
R ?= ../..
D := $R/tests/regression

VERS=3.7
CC=clang-$(VERS)
CXX=clang++-$(VERS)

CPPFLAGS=
CFLAGS=-Wall
CXXFLAGS=-Wall -O1
LDLIBS=-lpthread

SRCS:=$(wildcard $D/libc/*.c $D/pthread/*.c)
BINS:=$(SRCS:.c=)
LLS:=$(SRCS:.c=.ll)

all: run

compile : $(LLS) $(BINS)
all-bins : $(BINS)

run : compile
	$D/regtest.sh $R/dist/bin/stid $D

clean :
	rm -f $(BINS) $(LLS)
	rm -f $D/regression.log*
	find $D | grep '.log$$' | xargs rm -f

%.ll : %.c
	$(CC) -S -emit-llvm $(CFLAGS) $< -o $@
%.bc : %.c
	$(CC) -c -emit-llvm $(CFLAGS) $< -o $@
%.ll : %.cc
	$(CC) -S -emit-llvm $(CXXFLAGS) $< -o $@
%.bc : %.cc
	$(CC) -c -emit-llvm $(CXXFLAGS) $< -o $@
%.s : %.bc
	llc-$(VERS) $< -o $@

