HOME_DIR=../../..

POPFLAGS += --stack-trace

USE_CYCLONE=y
USE_LOADER=y
DYN_NATIVE=y

include ../../Makefile.inc
LIBFILES += queue hashtable poploader
OBJ_BASE = planbase lex absyn parse env svc eval network
OBJ = $(addsuffix .to, $(OBJ_BASE))
LIB_BASE = tal_sockets
TALCLIBS += $(addprefix --std-lib , $(LIB_BASE))

############################################################################

         all: single distributed testing
      single: plan.exe
 distributed: pland.exe inject.exe
     testing: dserv simpserv rtcgtest dynlink.exe

### used to build distributed version

pland.exe: pland.to $(OBJ)
	$(TALC) $(TALCFLAGS) -o $@ $^ $(TALCLIBS)
	$(MAKE) MAKE_LOADABLE=y dynlink_linked.$(O)

inject.exe:  $(OBJ) inject.to
	$(TALC) $(TALCFLAGS) -o $@ $^ $(TALCLIBS)

### performs dynamic linking w/o RTCG

dserv: $(OBJ)
	$(MAKE) MAKE_LOADABLE=y dserv.$(O)

simpserv: $(OBJ)
	$(MAKE) MAKE_LOADABLE=y simpserv.$(O)

### performs dynamic linking w/ RTCG

dynlink: dynlink.exe dynlink_running.pop 
dynlink.exe: dynlink_running.to misc.to planbase.to lex.to
	$(TALC) $(TALCFLAGS) -o $@ $^ $(TALCLIBS) 
	$(MAKE) MAKE_LOADABLE=y dynlink_linked.$(O)

rtcgtest: $(OBJ)
	$(MAKE) MAKE_LOADABLE=y rtcgtest.$(O)

#########################################################
# Non-regression suite
#########################################################

compile: plan.exe

REF_FILE=reference.out
CHECK_FILE=check.out

# used locally (not distributed) to run non-regression suite
plan.exe: plan.to $(OBJ) 
	$(TALC) $(TALCFLAGS) -o $@ $^ $(TALCLIBS)

run:
	rm -f $(CHECK_FILE)
	./plan.exe tests/types.plan    >> $(CHECK_FILE)
	./plan.exe tests/valdef.plan   >> $(CHECK_FILE)
	./plan.exe tests/values.plan   >> $(CHECK_FILE)
	./plan.exe tests/unary.plan    >> $(CHECK_FILE)
	./plan.exe tests/binary.plan   >> $(CHECK_FILE)
	./plan.exe tests/exp.plan      >> $(CHECK_FILE)
	./plan.exe tests/services.plan >> $(CHECK_FILE)
#	./plan.exe tests/except.plan

# these tests are from Mike and Jon's PLANet implementation
# tests commented out contain constructs not yet implemented
	./plan.exe interp_tests/Helloworld.plan  >> $(CHECK_FILE)
	./plan.exe interp_tests/binops.plan      >> $(CHECK_FILE)
	./plan.exe interp_tests/chunks1a.plan    >> $(CHECK_FILE)
	./plan.exe interp_tests/chunks1b.plan    >> $(CHECK_FILE)
	./plan.exe interp_tests/chunks2a.plan    >> $(CHECK_FILE)
	./plan.exe interp_tests/chunks2b.plan    >> $(CHECK_FILE)
	./plan.exe interp_tests/chunks3a.plan    >> $(CHECK_FILE)
	./plan.exe interp_tests/chunks3b.plan    >> $(CHECK_FILE)
	./plan.exe interp_tests/cons.plan        >> $(CHECK_FILE)
	./plan.exe interp_tests/consts.plan      >> $(CHECK_FILE)
	./plan.exe interp_tests/foldr.plan       >> $(CHECK_FILE)
	./plan.exe interp_tests/funnest.plan     >> $(CHECK_FILE)
	./plan.exe interp_tests/norecurse.plan   >> $(CHECK_FILE)
	./plan.exe interp_tests/unops.plan       >> $(CHECK_FILE)
#	./plan.exe interp_tests/blob.plan
#	./plan.exe interp_tests/except.plan
#	./plan.exe interp_tests/foldl.plan
#	./plan.exe interp_tests/member.plan
#	./plan.exe interp_tests/store.plan
#	./plan.exe interp_tests/tuples.plan
#	./plan.exe interp_tests/typeerror.plan


check:
	diff $(CHECK_FILE) $(REF_FILE)

REF:
	cp $(CHECK_FILE) $(REF_FILE)


### misc.

clean:
	rm -f *~ *.$(O) *.to *.tal *.tali *.lst *.pdb *.ilk *.exe
