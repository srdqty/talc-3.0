# contains implicit rules and default declarations
DYN_UPDATE=
COUNT_REFS=
NO_STACK_TRACE=
include Makefile.inc

SRC_BASE = core id set splay dict list hashtable lexing queue arg sockets \
           string sys filename xarray bitvec buffer fn char array math

ifndef POP_IN_POP
SRC_BASE += dlpop dllist
endif

SRC_TAL = $(addsuffix .tal, $(SRC_BASE))
SRC_OBJ = $(addsuffix .$(O), $(SRC_BASE))
SRC_TO  = $(addsuffix .to,  $(SRC_BASE))
SOURCES = $(addsuffix .pop, $(SRC_BASE))

ifdef WINDIR
all: poploader.$(O) $(SRC_OBJ)
else
all: poploader.$(O) $(SRC_OBJ) unixlib
endif

# the DLNATIVE stuff has been obsoleted by the dlpop library.  In 
# particular, the init fun type has changed, making it not useful

#ifdef WINDIR
#DLNATIVE = ../../runtime/loader/dlnative.tal.win
#else
#DLNATIVE = ../../runtime/loader/dlnative.tal.lin
#endif

#../../runtime/loader/dlnative.tal: $(DLNATIVE)
#	cp $< $@

#dlnative.$(O) : ../../runtime/loader/dlnative.tal
#	$(TALC) $(COMPFLAGS) -c $< 
#	cp ../../runtime/loader/dlnative* .

poploader.$(O) : ../../runtime/loader/poploader.tal
	$(TALC) $(COMPFLAGS) -c $< 
	cp ../../runtime/loader/poploader* .

dlpop.$(O) dlpop.to dlpop.tal: dlpop.pop
	$(POPCORN) $(COMPFLAGS) $(POPFLAGS) --no-loadable-fn-ptrs -c $<

clean:
	rm -f *.tal* *.$(O) *.to *~ *.lst *.$(A)
	rm -f ../../runtime/loader/poploader.to
	cd unix; make clean
	rm -f unixlib.h
#	rm -f ../../runtime/loader/dlnative.tal

verify: 
	$(TALC) -c $(SRC_TO)

#RLS 5/26/00#
poplib.$(A): $(SRC_OBJ) poploader.$(O)
	ar r $@ $+

poplib: poplib.$(A)
#/RLS#

unixlib:
	cd unix; $(MAKE)
	cp unix/unixlib.$(A) .
	cp unix/unixlib.tali .
	cp unix/unixlib.h .

depend:
	../../apps/popcorn/build/popcorn.exe -M\
	 $(COMPFLAGS) $(SOURCES) > .depend

include .depend

