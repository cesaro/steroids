
# Copyright (C) 2010-2016  Cesar Rodriguez <cesar.rodriguez@lipn.fr>
#
# This program is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation, either version 3 of the License, or any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
# more details.
#
# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <http://www.gnu.org/licenses/>.

include defs.mk

.PHONY: fake all g test clean distclean prof dist compile tags run

all : compile run

compile: $(TARGETS)

r run: compile input.ll
	#./tools/pta-dump/pta-dump tests/pta/alloca1.ll
	./tools/test/main

input.ll : program.ll rt/rt.bc
	llvm-link-$(LLVMVERS) -S $^ -o $@

#program.ll : /tmp/cunf3.ll
program.ll : tests/hello.ll
	#opt-3.7 -S -O3 -mem2reg $< > $@
	opt-3.7 -S -verify $< > $@

src/libsteroids.a : $(LIB_OBJS) $(LIB_MOBJS)
	@echo "AR  $@"
	@$(AR) r $@ $^

src/libsteroids.so : $(LIB_OBJS) $(LIB_MOBJS)
	@echo "LD  $@"
	@$(CXX) -shared $(CXXFLAGS) -o $@ $^

$(TOOLS_TEST_TARGETS) : $(TOOLS_TEST_OBJS) $(TOOLS_TEST_MOBJS) src/libsteroids.a
	@echo "LD  $@"
	@$(CXX) $(LDFLAGS) -o $@ $^ $(LDLIBS)

$(TOOLS_STID_TARGETS) : $(TOOLS_STID_OBJS) $(TOOLS_STID_MOBJS) src/libsteroids.a
	@echo "LD  $@"
	@$(CXX) $(LDFLAGS) -o $@ $^ $(LDLIBS)

$(TOOLS_PTADUMP_TARGETS) : $(TOOLS_PTADUMP_OBJS) $(TOOLS_PTADUMP_MOBJS) src/libsteroids.a
	@echo "LD  $@"
	@$(CXX) $(LDFLAGS) -o $@ $^ $(LDLIBS)

$(RT_TARGETS) : $(RT_OBJS) $(RT_MOBJS)
	@echo "LD  $@"
	@llvm-link-$(LLVMVERS) $(if $(findstring .ll, $@), -S, ) -o $@ $^

rt/start.c : rt/start.s
	./utils/as2c.py < $< > $@

prof : $(TARGETS)
	rm gmon.out.*
	src/main /tmp/ele4.ll_net

tags : $(SRCS)
	ctags -R --c++-kinds=+p --fields=+K --extra=+q include/ src/ tools/ rt/ $(shell llvm-config-$(LLVMVERS) --includedir)

g gdb : $(TARGETS)
	gdb ./tools/test/main

c cgdb : $(TARGETS)
	cgdb ./tools/test/main

t test : test.pta

test.pta : $(TOOLS_PTADUMP_TARGETS) $(patsubst %.c,%.ll,$(wildcard tests/pta/*.c))
	export PATH=$$PWD/tools/pta-dump:$$PATH; cd ./tests/pta; ./run.sh

vars :
	@echo xxxxxxxxxxxx
	@echo $(patsubst %.c,%.ll,$(wildcard tests/pta/*.c))
	@echo xxxxxxxxxxxx
	@echo "CC       $(CC)"
	@echo "CXX      $(CXX)"
	@echo "CFLAGS   $(CFLAGS)"
	@echo "CPPFLAGS $(CPPFLAGS)"
	@echo "CXXFLAGS $(CXXFLAGS)"
	@echo "TARGETS  $(TARGETS)"
	@echo "MOBJS    $(MOBJS)"
	@echo "OBJS     $(OBJS)"
	@echo "DEPS     $(DEPS)"
	@echo ""
	@echo "Library:"
	@echo "SRCS     $(LIB_SRCS)"
	@echo "MSRCS    $(LIB_MSRCS)"
	@echo "OBJS     $(LIB_OBJS)"
	@echo "MOBJS    $(LIB_MOBJS)"
	@echo "TARGETS  $(LIB_TARGETS)"
	@echo ""
	@echo "tools/test:"
	@echo "SRCS     $(TOOLS_TEST_SRCS)"
	@echo "MSRCS    $(TOOLS_TEST_MSRCS)"
	@echo "OBJS     $(TOOLS_TEST_OBJS)"
	@echo "MOBJS    $(TOOLS_TEST_MOBJS)"
	@echo "TARGETS  $(TOOLS_TEST_TARGETS)"
	@echo ""
	@echo "rt:"
	@echo "SRCS     $(RT_SRCS)"
	@echo "MSRCS    $(RT_MSRCS)"
	@echo "OBJS     $(RT_OBJS)"
	@echo "MOBJS    $(RT_MOBJS)"
	@echo "TARGETS  $(RT_TARGETS)"

clean :
	@rm -f $(TARGETS) $(MOBJS) $(OBJS)
	@rm -f rt/*.ll rt/start.c input.ll
	@echo Cleaning done.

distclean : clean
	@rm -f $(DEPS)
	@rm -Rf dist/
	@echo Mr. Proper done.

dist : compile
	rm -Rf dist/
	mkdir dist
	mkdir dist/bin
	mkdir dist/lib
	mkdir dist/lib/stid
	cp tools/stid/main dist/bin/stid
	cp tools/pta-dump/pta-dump dist/bin/pta-dump
	cp src/*.a dist/lib/stid
	cp src/*.so dist/lib/stid

PREFIX = ~/x/local

install : dist
	cd dist; cp -Rv * $(PREFIX)

uninstall :
	cd $(PREFIX)
	rm bin/stid
	rm -R lib/stid

-include $(DEPS)

