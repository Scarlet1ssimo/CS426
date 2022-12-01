SHELL=/bin/bash
LEVEL=..
CC=clang-12
CXX=clang++-12
include ../Makefile

SRCS	= $(wildcard *.ll)
test:	$(SRCS:%.ll=%.diff)
stat:	$(SRCS:%.ll=%.stat)

LLCFLAGS = -regalloc=simple -stats -verify-machineinstrs
LLCFLAGSSTAT = -regalloc=simple -stats
# LLCFLAGS = -regalloc=simple -stats -verify-machineinstrs -debug

%.s: %.ll $(LLC)
	$(LLC) $(LLCFLAGS) $*.ll -O0

%.dbg: %.ll $(LLC)
	$(LLC) $(LLCFLAGS) -debug-only=regalloc $*.ll -O0 2> >(tee stderr)

%.stat: %.ll $(LLC)
	@$(LLC) $(LLCFLAGSSTAT) $*.ll -O0 2> stat-$*
	- grep 'stores\|loads' stat-$* --color

%.test: %.ll $(LLC) 
	$(LLC) $(LLCFLAGS) $*.ll -O0
	$(LLC) $(LLCFLAGS) $*.ll -O1
	$(LLC) $(LLCFLAGS) $*.ll -O2

%.exe: %.s
	$(CXX) -g $+ -o $@

%.run: %.exe
	./$+

%.ref.s: %.ll
	llc-12 $*.ll -o $@

%.diff: %.exe %.ref.exe
	./test $+ $*.in

%.ll: %.cpp
	clang++ -O0 $*.cpp -emit-llvm -S

clean:
	rm *.s stat-* stderr o1 o2

.PHONY: clean
.PRECIOUS: %.s %.exe %.ll