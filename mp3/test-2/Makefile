LEVEL = ..
CGEN    = $(MPDIR)/src/cgen-2
MP3=1
TESTS	= $(wildcard *.cl)
all:	$(TESTS:.cl=.out)

in_int.out: in_int.exe
	echo "12345" | ./$< > $@ || true

in_string.out: in_string.exe
	echo "Welcome to the world." | ./$< > $@ || true

all-exe: $(TESTS:.cl=.exe)

include ../Makefile.common

$(CGEN) ::
	make -C ../src cgen-2
