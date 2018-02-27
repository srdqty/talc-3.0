# Default flag settings
# Please modify or augment in Makefile.local
# DO NOT CHECK Makefile.local INTO THE REPOSITORY
# If no Makefile.local is present, defaults will be used instead.
POP_IN_POP=
USE_LOADER=

-include Makefile.local

ifndef WINDIR
LIBFILES = array
endif

HOME_DIR = ../..
include ../Makefile.inc

ifdef POP_IN_POP
HASHTABLE_TO = $(POPLIB_DIR)/hashtable_strap.to
else
HASHTABLE_TO = $(POPLIB_DIR)/hashtable.to
endif

out=test.out
ref=sample.out

exes = ast.exe check_array.exe check_array2.exe check_big_fact.exe check_exn.exe\
       check_exn2.exe check_globals.exe check_loops.exe check_spill.exe \
       check_inst.exe check_def_init.exe check_for.exe check_open.exe \
       check_arith.exe check_string.exe \
       combine.exe fact.exe fib.exe \
       hanoi.exe int_msort.exe int_queue.exe \
       msort.exe printf.exe rsa.exe check_abstypes.exe new_features.exe \
       eval_order.exe hashtest.exe
ifndef POP_IN_POP
exes += exncon.exe funvar.exe tuples.exe
endif
exes += check_builtin_exn.exe

compile: $(exes) 

check_globals.exe: check_globals.to check_globals2.to
	$(TALC) $(TALCFLAGS) -o check_globals.exe check_globals.to \
	check_globals2.to $(TALCLIBS)

int_msort.exe: int_list.to int_msort.to
	$(TALC) $(TALCFLAGS) -o int_msort.exe int_list.to int_msort.to \
	$(TALCLIBS)

msort.exe: msort.to
	$(TALC) $(TALCFLAGS) -o msort.exe msort.to $(TALCLIBS)

combine.exe: combine.to
	$(TALC) $(TALCFLAGS) -o combine.exe combine.to $(TALCLIBS)

new_features.exe: new_features.to
	$(TALC) $(TALCFLAGS) -o new_features.exe \
	new_features.to $(TALCLIBS) 

hashtest.exe: hashtest.to
	$(TALC) $(TALCFLAGS) -o hashtest.exe \
	hashtest.to $(HASHTABLE_TO) $(TALCLIBS)

ifndef WINDIR
msort_array.exe: msort_array.to
	$(TALC) $(TALCFLAGS) $^ -o $@ $(TALCLIBS) -T unixlib.$(A)
endif

run:
	rm -f $(out)
	./ast.exe >> $(out)
	./check_array.exe >> $(out)
	./check_array2.exe >> $(out)
	./check_big_fact.exe >> $(out)
	./check_exn.exe >> $(out) 2>&1 || true
	./check_exn2.exe >> $(out)
	./check_globals.exe >> $(out)
	./check_loops.exe >> $(out)
	./check_spill.exe >> $(out)
	./check_inst.exe >> $(out)
	./check_def_init.exe >> $(out)
	./check_for.exe >> $(out)
	./check_open.exe >> $(out)
	./check_arith.exe >> $(out)
	./check_string.exe >> $(out)
	./combine.exe >> $(out)
	./fact.exe 10 >> $(out)
	./fib.exe 10 >> $(out)
	./hanoi.exe >> $(out)
	./int_msort.exe 5 >> $(out)
	./int_queue.exe  >> $(out)
	./msort.exe 5 >> $(out)
	./printf.exe >> $(out)
	./rsa.exe >> $(out)
	./eval_order.exe >> $(out)
	./hashtest.exe >> $(out)
ifndef POP_IN_POP
	./exncon.exe >> $(out)
	./funvar.exe >> $(out)
	./tuples.exe >> $(out)
endif
	./check_builtin_exn.exe >> $(out) 2>&1 || true
check:
	diff $(out) $(ref)

all: clean compile run check

exe_only:
	rm -f *~ *.o *.to *.pil *.obj *.tal* *.lst

clean: exe_only
	rm -f *.exe $(out)

