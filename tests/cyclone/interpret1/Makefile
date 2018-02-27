POP_IN_POP=X
USE_RTCG=X
HOME_DIR = ../../..

include ../../Makefile.inc

ifdef POP_IN_POP
POPFLAGS += -no-inline
endif

OUTFILE=test.out
REFFILE=ref.out

ifdef WINDIR
CFLAGS=/c
CFLAGSOPT=/c /O2 /Fo
LINK=link
LINKFLAGS=/nologo /subsystem:console /libpath:$(RUNTIMEDIR) /out:
else
CC=gcc
CFLAGS=-c
CFLAGSOPT=-c -O3 -o
LINK=gcc
LINKFLAGS=-o
endif

POPOBJS = pinterp.$(O) pinterp_gen.$(O) pprog.$(O) test_pop.$(O) bench.$(O)
OBJS = cprog.$(O) test_c.$(O) timer.$(O) \
       cinterp.$(O) cinterp.opt.$(O)

POPTOS = $(POPOBJS:.$(O)=.to)

#       interpret_cts_1.$(O)  interpret_cts_2.$(O) \
#       interpret_cts_1.opt.$(O)  interpret_cts_2.opt.$(O) \
#	test_tempo.$(O) \

ifndef WINDIR
## This apparently does not work on Windows.
OBJS += Linux/interpret_opt.rts.o 
endif

VERIFIES = bench.ver pinterp.ver pinterp_gen.ver pprog.pop test_pop.ver

# benchmark

all: bench.exe

bench.exe: $(OBJS) $(POPOBJS) bench.$(O) test_interface.to
	$(TALC) $(TALCFLAGS) -o $@ $(OBJS) $(POPTOS) $(TALCLIBS) --trusted test_interface_e.tali

verify:  $(VERIFIES)

run:
	./bench.exe TEST > $(OUTFILE)

check:
	diff $(OUTFILE) $(REFFILE)

ref:
	cp $(OUTFILE) $(REFFILE)

# Popcorn files

##%.$(O): %.pop
##	$(POPCORN) $(POPFLAGS) -c -o $@ $<

# C files

%.opt.$(O): %.opt.c
	$(CC) $(CFLAGSOPT)$@ $<

%.$(O): %.c
	$(CC) $(CFLAGS)  $<

# verification

%.ver: %.pop
	$(POPCORN) --elaborate-only $<
	mv $(subst .pop,.tal,$<) $@

# clean

exe_only:
	rm -f *~ *.o *.obj *.opt *.tal* *.lst *.pil

clean: exe_only
	rm -f *.exe
	rm -f *.ver
	rm -f *.tal
	rm -f *.to
	rm -f *.pdb # produced by debugging
	rm -f *.ilk # produced by debugging



