POPCORN=popcorn.exe
POPFLAGS=--TALC
TALC=talc.exe

# System dependencies
ifdef WINDIR
O=obj
A=lib
CC=cl
else
O=o
A=a
CC=gcc
endif

RT_DIR=../../../../talc/runtime
PLIB_DIR=../../../../talc/popcorn/lib

PLIB_BASE=core string list
PLIB=$(addprefix $(PLIB_DIR)/,  $(addsuffix .$(O), $(PLIB_BASE)))
OBJ_BASE=
OBJ= $(addsuffix .$(O), $(OBJ_BASE))
RT_BASE=
RT= $(addsuffix .$(O), $(RT_BASE))

#########################################################
# all
#########################################################

all: bench.exe

#########################################################

#cheb.exe: cheb.$(O)
#	$(POPCORN) $(POPFLAGS) -o $@ -I $(PLIB_DIR) -I $(RT_DIR) $^ $(RT)

bench.exe: bench.$(O) $(PLIB) cheb.$(O) ch.$(O)
	$(POPCORN) $(POPFLAGS) -o $@ -I $(PLIB_DIR) -I $(RT_DIR) $^ $(RT)

ch: ch.$(O)
	$(CC) -o ch -lm $^

%.tal: %.pop
	$(POPCORN) $(POPFLAGS) --generate-tal-only $<

%.to %.$(O) %.tal: %.pop
	$(POPCORN) $(POPFLAGS) -c $<

%.to %.$(O): %.tal
	$(TALC) $(TALCFLAGS) -c $<


%.exe: %.to
	$(TALC) $(TALCFLAGS) -o $@ $< $(TALCLIBS)

%.$(O): %.c 
	$(CC) -c $<

clean:
	rm -f *~ *.$(O) *.to *.tal *.tali *.lst *.pdb *.ilk *.exe

#########################################################
# Non-regression suite
#########################################################

REF_FILE=reference.out
CHECK_FILE=check.out

run-local:
	rm -f $(CHECK_FILE)

check-local:
	diff $(CHECK_FILE) $(REF_FILE)

ref-local:
	cp $(CHECK_FILE) $(REF_FILE)

