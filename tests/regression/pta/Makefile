
LLVMVERS=3.7
CC=clang-$(LLVMVERS)
CFLAGS:=-std=c11 -pthread

LL=$(patsubst %.c,%.ll,$(wildcard *.c))

all : run

run : $(LL)
	./run.sh

clean :
	rm -f $(LL)

%.ll : %.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -S -flto $< -o $@
%.bc : %.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -c -flto $< -o $@
%.bc : %.ll
	llvm-as-$(LLVMVERS) $< -o $@
%.ll : %.bc
	llvm-dis-$(LLVMVERS) $< -o $@
