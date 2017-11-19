
STID_ROOT := ../../dist

CXX := clang++-3.7
CPPFLAGS := -I $(STID_ROOT)/include $(shell llvm-config-3.7 --cppflags)
CXXFLAGS := -Wall -std=c++11 -g
LDFLAGS := $(shell llvm-config-3.7 --ldflags) -L $(STID_ROOT)/lib/
LDLIBS := -lsteroids $(shell llvm-config-3.7 --libs all) -lz -lpthread -lffi -lncurses -ldl -lm

all : example

clean :
	rm -f example program.ll

run : example program.c
	clang-3.7 -emit-llvm -S -c program.c
	llvm-link-3.7 program.ll $(STID_ROOT)/lib/rt.bc -o /tmp/input.bc
	./example /tmp/input.bc
