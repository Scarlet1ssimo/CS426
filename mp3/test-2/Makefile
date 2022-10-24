LEVEL = ..
CGEN    = $(MPDIR)/src/cgen-2
MP3=1
TESTS	= $(wildcard *.cl)
test:	$(TESTS:%.cl=%.test)
REFCGEN = $(MPDIR)/reference-binaries/cgen-2
REFCOOLRT = $(MPDIR)/reference-binaries/coolrt.o

all:	$(TESTS:.cl=.out)

in_int.out: in_int.exe
	echo "12345" | ./$< > $@ || true

in_string.out: in_string.exe
	echo "Welcome to the world." | ./$< > $@ || true

all-exe: $(TESTS:.cl=.exe)

include ../Makefile.common

$(CGEN) ::
	make -C ../src cgen-2
cgen-2 : $(CGEN)

%.ll: %.ast $(CGEN)
	$(CGEN) $(CGENOPTS) < $< > $@

%.ac.ll: %.ast
	$(REFCGEN) $(CGENOPTS) < $< > $@

%.ac.exe: %.ac.s $(REFCOOLRT)
	$(CC) -g $+ -o $@

%.test: %.ac.ll %.ll
	diff -w $^ --color

%.run: %.exe
	./$<

.PHONY: test %.test clean realclean %.run

realclean: clean
	make -C ../src clean