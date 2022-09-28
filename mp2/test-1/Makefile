LEVEL = ..
TESTS	= $(wildcard *.cl)
test:	$(TESTS:%.cl=%.test)

CGEN    = $(MPDIR)/src/cgen-1
REFCGEN = $(MPDIR)/reference-binaries/cgen-1

include ../Makefile.common


$(CGEN) ::
	make -C ../src cgen-1

%.ll: %.ast $(CGEN)
	$(CGEN) $(CGENOPTS) < $< > $@

%.ac.ll: %.ast
	$(REFCGEN) $(CGENOPTS) < $< > $@

%.test: %.ac.ll %.ll
	diff -w $^ --color

.PHONY: test %.test clean realclean

realclean: clean
	make -C ../src clean