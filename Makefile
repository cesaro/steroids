
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

r run: compile
	./src/main

input.ll : benchmarks/basic/hello.ll src/rt/rtv.ll
	llvm-link-$(LLVMVERS) -S $^ -o $@

src/rt/rtv.ll : src/rt/start.s src/rt/rt.bc
	./utils/as2c.py < src/rt/start.s > /tmp/start.c
	make /tmp/start.bc
	llvm-link-$(LLVMVERS) -S src/rt/rt.bc /tmp/start.bc -o $@

$(TARGETS) : % : %.o $(OBJS)
	@echo "LD  $@"
	@$(CXX) $(LDFLAGS) -o $@ $^ $(LDLIBS)

#$(MINISAT)/build/release/lib/libminisat.a :
#	cd $(MINISAT); make lr

prof : $(TARGETS)
	rm gmon.out.*
	src/main /tmp/ele4.ll_net

tags : $(SRCS)
	ctags -R --c++-kinds=+p --fields=+K --extra=+q src/ $(shell llvm-config-$(LLVMVERS) --includedir)

g gdb : $(TARGETS)
	gdb ./src/main

vars :
	@echo "CC       $(CC)"
	@echo "CXX      $(CXX)"
	@echo "CFLAGS   $(CFLAGS)"
	@echo "CPPFLAGS $(CPPFLAGS)"
	@echo "CXXFLAGS $(CXXFLAGS)"
	@echo "TARGETS  $(TARGETS)"
	@echo "MSRCS    $(MSRCS)"
	@echo "MOBJS    $(MOBJS)"
	@echo "SRCS     $(SRCS)"
	@echo "OBJS     $(OBJS)"
	@echo "DEPS     $(DEPS)"

clean :
	@rm -f $(TARGETS) $(MOBJS) $(OBJS)
	@rm -f src/rt/rt.ll
	@rm -f src/rt/rtv.ll
	@echo Cleaning done.

distclean : clean
	@rm -f $(DEPS)
	@rm -Rf dist/
	@echo Mr. Proper done.

dist : all
	rm -Rf dist/
	# ...

-include $(DEPS)

