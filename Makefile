
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
	./tools/test/main

input.ll : tests/hello.ll rt/rt.ll
	llvm-link-$(LLVMVERS) -S $^ -o $@

rt/rt.ll : rt/start.s rt/aha.bc
	./utils/as2c.py < rt/start.s > /tmp/start.c
	make /tmp/start.bc
	llvm-link-$(LLVMVERS) -S rt/aha.bc /tmp/start.bc -o $@

src/libsteroids.a : $(LIB_OBJS) $(LIB_MOBJS)
	@echo "AR  $@"
	@$(AR) r $@ $^

src/libsteroids.so : $(LIB_OBJS) $(LIB_MOBJS)
	@echo "LD  $@"
	@$(CXX) -shared $(CXXFLAGS) -o $@ $^

$(TOOLS_TEST_TARGETS) : $(TOOLS_TEST_OBJS) $(TOOLS_TEST_MOBJS) src/libsteroids.a
	@echo "LD  $@"
	@$(CXX) $(LDFLAGS) -o $@ $^ $(LDLIBS) src/libsteroids.a

#$(MINISAT)/build/release/lib/libminisat.a :
#	cd $(MINISAT); make lr

prof : $(TARGETS)
	rm gmon.out.*
	src/main /tmp/ele4.ll_net

tags : $(SRCS)
	ctags -R --c++-kinds=+p --fields=+K --extra=+q include/ src/ tools/ rt/ $(shell llvm-config-$(LLVMVERS) --includedir)

g gdb : $(TARGETS)
	gdb ./tools/test/main

vars :
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

clean :
	@rm -f $(TARGETS) $(MOBJS) $(OBJS)
	@echo Cleaning done.

distclean : clean
	@rm -f $(DEPS)
	@rm -Rf dist/
	@echo Mr. Proper done.

dist : all
	rm -Rf dist/
	# ...

-include $(DEPS)

