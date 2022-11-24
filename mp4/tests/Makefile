SHELL=/bin/bash
LEVEL=..
CC=clang
CXX=clang++
include ../Makefile

SRCS	= $(wildcard *.ll)
test:	$(SRCS:%.ll=%.diff)
stat:	$(SRCS:%.ll=%.stat)

LLCFLAGS = -regalloc=simple -stats -verify-machineinstrs -debug-only=regalloc
LLCFLAGSSTAT = -regalloc=simple -stats
# LLCFLAGS = -regalloc=simple -stats -verify-machineinstrs -debug

%.s: %.ll $(LLC)
	$(LLC) $(LLCFLAGS) $*.ll -O0

%.dbg: %.ll $(LLC)
	$(LLC) $(LLCFLAGS) $*.ll -O0 2> >(tee stderr)

%.stat: %.ll $(LLC)
	$(LLC) $(LLCFLAGSSTAT) $*.ll -O0 2> >(tee stat-$*) 

%.test: %.ll $(LLC) 
	$(LLC) $(LLCFLAGS) $*.ll -O0
	$(LLC) $(LLCFLAGS) $*.ll -O1
	$(LLC) $(LLCFLAGS) $*.ll -O2

%.exe: %.s
	$(CC) -g $+ -o $@

%.run: %.exe
	./$+

%.ref.s: %.ll
	llc-12 $*.ll -o $@

%.diff: %.exe %.ref.exe
	./test $+

clean:
	rm *.s

.PHONY: clean
.PRECIOUS: %.s %.exe