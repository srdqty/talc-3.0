USE_CYCLONE=y
USE_LOADER=
DYN_NATIVE=

LIBFILES = core string list # queue hashtable poploader
POPLIB = ../../../popcorn/lib/
include ../../Makefile.inc

# Is this needed?
PRELUDE = ../../../runtime/prelude.to
#PRELUDE =

OBJ_BASE=lex parse env svc eval misc 
OBJ= $(addsuffix .to, $(OBJ_BASE)) 
LIB_BASE = tal_sockets
TALCLIBS += $(addprefix --std-lib , $(LIB_BASE))

#########################################################
# all
#########################################################

compile: plan.exe

#########################################################

bench.exe: $(PLIB) $(OBJ) bench.to
	$(TALC) $(TALCFLAGS) -o $@ $^ $(TALCLIBS)

clean:
	rm -f *~ *.$(O) *.to *.tal *.tali *.lst *.pdb *.ilk *.exe

#########################################################
# Non-regression suite
#########################################################

REF_FILE=reference.out
CHECK_FILE=check.out
REF_DIST=pland_logfile.out
CHECK_DIST=checkdist.out

plan.exe: $(PLIB) $(OBJ) plan.to
	$(TALC) $(TALCFLAGS) -o $@ $^ $(TALCLIBS)

#plan.exe: $(PLIB) $(OBJ) plan.$(O) misc.$(O)
#	$(POPCORN) $(POPFLAGS) -o $@ -I $(PLIB_DIR) -I $(RT_DIR)  $^ $(RT)

# quick check
qc:
	echo "main()" | ./plan.exe tests/values.plan

run:
	rm -f $(CHECK_FILE)
	./plan.exe tests/types.plan   >> $(CHECK_FILE)
	./plan.exe tests/valdef.plan  >> $(CHECK_FILE)
	./plan.exe tests/values.plan  >> $(CHECK_FILE)
	./plan.exe tests/unary.plan   >> $(CHECK_FILE)
	./plan.exe tests/binary.plan  >> $(CHECK_FILE)
	./plan.exe tests/exp.plan     >> $(CHECK_FILE)
	./plan.exe tests/services.plan   >> $(CHECK_FILE)
#	./plan.exe tests/except.plan

# these tests are from Mike and Jon's PLANet implementation
	./plan.exe interp_tests/Helloworld.plan  >> $(CHECK_FILE)
	./plan.exe interp_tests/binops.plan      >> $(CHECK_FILE)
#	./plan.exe interp_tests/blob.plan
	./plan.exe interp_tests/chunks1a.plan    >> $(CHECK_FILE)
	./plan.exe interp_tests/chunks1b.plan    >> $(CHECK_FILE)
	./plan.exe interp_tests/chunks2a.plan    >> $(CHECK_FILE)
	./plan.exe interp_tests/chunks2b.plan    >> $(CHECK_FILE)
	./plan.exe interp_tests/chunks3a.plan    >> $(CHECK_FILE)
	./plan.exe interp_tests/chunks3b.plan    >> $(CHECK_FILE)
	./plan.exe interp_tests/cons.plan        >> $(CHECK_FILE)
	./plan.exe interp_tests/consts.plan      >> $(CHECK_FILE)
#	./plan.exe interp_tests/except.plan      >> $(CHECK_FILE)
#	./plan.exe interp_tests/foldl.plan
	./plan.exe interp_tests/foldr.plan       >> $(CHECK_FILE)
	./plan.exe interp_tests/funnest.plan     >> $(CHECK_FILE)
#	./plan.exe interp_tests/member.plan
	./plan.exe interp_tests/norecurse.plan   >> $(CHECK_FILE)
#	./plan.exe interp_tests/store.plan
#	./plan.exe interp_tests/tuples.plan
#	./plan.exe interp_tests/typeerror.plan
	./plan.exe interp_tests/unops.plan       >> $(CHECK_FILE)


check:
	diff $(CHECK_FILE) $(REF_FILE)

REF:
	cp $(CHECK_FILE) $(REF_FILE)

initdist:
	./pland.exe -rf c4000 -if c1 &
	./pland.exe -rf c4001 -if c2 &
	./pland.exe -rf c4002 -if c3 &

rundist:
	rm -f $(CHECK_DIST)
	./inject.exe -ip cyclone:4000 ping.plan
	./inject.exe -ip cyclone:4000 traceroute.plan    >> $(CHECK_DIST)
	./inject.exe -ip cyclone:4000 broadcast_msg.plan >> $(CHECK_DIST)
	./inject.exe -ip cyclone:4000 svcs.plan          >> $(CHECK_DIST)

checkdist:
	diff $(CHECK_DIST) $(REF_DIST)

REFDIST:
	cp $(CHECK_DIST) $(REF_DIST)
