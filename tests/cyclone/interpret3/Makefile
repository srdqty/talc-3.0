USE_RTCG = X
POP_IN_POP= X

HOME_DIR = ../../..

include ../../Makefile.inc

ifdef POP_IN_POP
POPFLAGS += -no-inline
endif
OUTFILE=test.out
REFFILE=ref.out

ifdef WINDIR
CC= cl
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

POPFILES = pinterp pinterp_gen2 pprog test_pop bench
CFILES = cinterp cinterp.opt \
	interpret_cts_1 interpret_cts_1.opt \
	interpret_cts_2 interpret_cts_2.opt \
	interpret_cts_3 interpret_cts_3.opt \
	interpret_cts_4 interpret_cts_4.opt \
	interpret_cts_5 interpret_cts_5.opt \
	cprog test_c test_tempo timer
#OBJS = pinterp.$(O) pinterp_gen2.$(O) \
#       cinterp.$(O) cinterp.opt.$(O) \
#       interpret_cts_1.$(O) interpret_cts_1.opt.$(O) \
#       interpret_cts_2.$(O) interpret_cts_2.opt.$(O) \
#       interpret_cts_3.$(O) interpret_cts_3.opt.$(O) \
#       interpret_cts_4.$(O) interpret_cts_4.opt.$(O) \
#       interpret_cts_5.$(O) interpret_cts_5.opt.$(O) \
#       cprog.$(O) pprog.$(O) \
#       test_c.$(O) test_pop.$(O) test_tempo.$(O) \
#       timer.$(O)
       # Linux/interpret.rts.o \

OBJS =  $(addsuffix .$(O), $(POPFILES)) $(addsuffix .$(O), $(CFILES))
TOS = $(addsuffix .to, $(POPFILES))
TRUSTED = $(addprefix --trusted , $(addsuffix .$(O), $(CFILES)))

VERIFIES = bench.ver pinterp.pop pinterp_gen.ver pprog.ver test.ver test_pop.ver

# bench

compile: bench.exe

bench.exe: $(OBJS) bench_interface.to
	$(TALC) $(TALCFLAGS) -o $@ $(TOS) $(TRUSTED) $(TALCLIBS) --trusted bench_interface_e.tali
##	$(POPCORN) $(POPFLAGS) -o $@ $^

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

##%.ver: %.pop
##	$(POPCORN) --elaborate-only $<
##	mv $(subst .pop,.tal,$<) $@

# clean

exe_only:
	rm -f *~ *.o *.obj *.opt *.tal* *.lst

clean: exe_only
	rm -f *.exe
	rm -f *.ver
	rm -f *.tal
	rm -f *.to *.pil
	rm -f *.pdb # produced by debugging
	rm -f *.ilk # produced by debugging
