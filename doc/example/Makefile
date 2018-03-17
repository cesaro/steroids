
STID_ROOT := ../../dist

CXX := clang++-6.0
CPPFLAGS := -I $(STID_ROOT)/include $(shell llvm-config-6.0 --cppflags)
CXXFLAGS := -Wall -std=c++11 -g
LDFLAGS := $(shell llvm-config-6.0 --ldflags) -L $(STID_ROOT)/lib/
LDLIBS := -lsteroids $(shell llvm-config-6.0 --libs all) -lz -lpthread -lffi -lncurses -ldl -lm

all : example

clean :
	rm -f example program.ll

run : example program.c
	clang-6.0 -emit-llvm -S -c program.c
	llvm-link-6.0 program.ll $(STID_ROOT)/lib/rt.bc -o /tmp/input.bc
	./example /tmp/input.bc
