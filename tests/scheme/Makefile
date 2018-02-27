
# Makefile for running "all" the scheme tests
# Runs both Scheme compilers

# std-lib.ss is a bad test b/c it currently generates no output

ref     = sample.out
ml_out  = ml_test.out
pop_out = pop_test.out

SRC_BASE = std-lib scheme_test

ML_IMP   = scheme
ML_FLAGS = --TALC

POP_IMP  = ../../apps/popcorn/build/scheme.exe
POP_FLAGS= --TALC

all: caml popcorn

caml: clean
	$(system echo "\nUsing ML Implementation\n")
	$(ML_IMP) $(ML_FLAGS) std-lib.ss
	./std-lib.exe >> $(ml_out)
	$(ML_IMP) $(ML_FLAGS) scheme_test.ss
	./scheme_test.exe >> $(ml_out)
	diff $(ml_out) $(ref)

popcorn: clean_again
	$(system echo "\nUsing Popcorn Implementation\n")
	$(POP_IMP) $(POP_FLAGS) std-lib.ss
	./std-lib.exe >> $(pop_out)
	$(POP_IMP) $(POP_FLAGS) scheme_test.ss
	./scheme_test.exe >> $(pop_out)
	diff $(pop_out) $(ref)

clean:
	rm -f *~ *.o *.obj *.tal* *.lst *.exe $(ml_out) $(pop_out)

clean_again:
	rm -f *~ *.o *.obj *.tal* *.lst *.exe $(ml_out) $(pop_out)
