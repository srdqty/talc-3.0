POP_IN_POP=X
USE_LOADER=
USE_RTCG=X

-include Makefile.local

HOME_DIR = ../..
include ../Makefile.inc

## Inlining not yet supported for RTCG
ifdef POP_IN_POP 
POPFLAGS += -no-inline
endif
# any alterations to TALCFLAGS or POPFLAGS should go here

OUTFILE=test.out
REFFILE=ref.out

FAILED =
PROGS =  add bsearch cg cg2 cg3 cg4 cg5 cg6 cgh1 cghole dot \
	list_map intswitch-inter itj jmp mexp nested qe2 rec \
	sprintf while-inter sprintf

EXES = $(addsuffix .exe, $(PROGS))
TALS = $(addsuffix .tal, $(PROGS))

# FMS: Interface problem I suspect.
BUGGY2 = test_stdlib.exe 

compile: $(EXES)
ifndef WINDIR
ifdef POP_IN_POP
	cd interpret3; make compile
endif
	cd fastnet; make compile
endif

tal: $(TALS)

##sprintf.exe: $(LIBFLAGS)
##	$(POPCORN) -o $@ $^

run: $(EXES)
	rm -f $(OUTFILE)
	./add.exe >> $(OUTFILE)
	./bsearch.exe >> $(OUTFILE)
	./cg.exe >> $(OUTFILE)
	./cg2.exe >> $(OUTFILE)
	./cg3.exe >> $(OUTFILE)
	./cg4.exe >> $(OUTFILE)
	./cg5.exe >> $(OUTFILE)
	./cg6.exe >> $(OUTFILE)
	./cgh1.exe >> $(OUTFILE)
	./cghole.exe >> $(OUTFILE)
	./dot.exe >> $(OUTFILE)
	./intswitch-inter.exe >> $(OUTFILE)
	./itj.exe >> $(OUTFILE)
	./jmp.exe >> $(OUTFILE)
	./list_map.exe >> $(OUTFILE)
	./mexp.exe >> $(OUTFILE)
	./nested.exe >> $(OUTFILE)
	./qe2.exe >> $(OUTFILE)
	./sprintf.exe >> $(OUTFILE)
	./rec.exe >> $(OUTFILE)
	./while-inter.exe >> $(OUTFILE)
#	./interpret/test.exe >> $(OUTFILE)	    # interpret subdirectory
ifndef WINDIR
ifdef POP_IN_POP
	cd interpret3; make run
endif
	cd fastnet; make run
endif

check:
	diff $(OUTFILE) $(REFFILE)
ifdef POP_IN_POP
	cd interpret3; make check
endif
	cd fastnet; make check

ref:
	cp $(OUTFILE) $(REFFILE)

bench:
	./interpret1/bench.exe

exe_only:
	rm -f *~ *.$(O) *.to *.tal* *.lst

clean_top: exe_only
	rm -f *.exe
	rm -f *.ver
	rm -f *.pdb # produced by debugging
	rm -f *.ilk # produced by debugging
	rm -f *.pil
	rm -f $(OUTFILE)

clean: exe_only
	make clean_top
	cd interpret1; make clean
	cd interpret3; make clean
	cd rsa; make clean
	cd sockets; make clean
	cd fastnet; make clean

#	cd cheb; make clean
#	cd matmult; make clean
#	cd romberg; make clean
#	cd cubic_splines; make clean

# EOF: Makefile
