# Makefile for popcorn applications
# September 1999
# Dan Grossman

# will not bootstrap and test the Popcorn in Popcorn compiler without
# user acrobatics

############################ shortcomings  ############################
# 1. We don't capture all dependencies:
#     a. No mention of runtime support
#     b. confuses .tal and .obj 
# 2. We don't have tal libraries. Therefore, we should carefully list
#    which .obj files should be linked into an executable.  Since that
#    requires more maintenance, we instead link in entire directories
#    of object files, thus bloating executables.
#####################################################################

# don't use drive names because cl and gnu can't agree on how to parse them!

ifdef WINDIR

# Assume Windows NT/98/95/3.x
O=obj
A=lib
exe_suffix=.exe
DIFF=diff -a

else

# Assume some kind of Unix
O=o
A=a
exe_suffix=
DEFINES     = -D SYS_UNIX
DIFF=diff -q

endif

DEFINES +=

################### applications and options #####################
TALC        = talc.exe
BISON       = tools/bison/bison$(exe_suffix)
LEX         = tools/ocamllex/popocamllex.exe
EXTRAFLAGS  = 
## PIPFLAGS are only available/used for POP_IN_POP compiler
PIPFLAGS =
## SBFLAGS are only for use with the stack-based compiler.
SBFLAGS = --stack-trace
COMPFLAGS   = $(INCLUDES) $(DEFINES) $(EXTRAFLAGS)

PIP_PRELUDE = ../../runtime/preludenew.to
PRELUDE     = ../../runtime/prelude.to

LINKFLAGS   = --verify-link --verify-program --no-internals \
	  -T pop_runtime.$(O) -T stdlib.$(O) cyclonelib.$(O)
PIP_LINKFLAGS = --single-error --verify-link --verify-program --no-internals \
	  -T pop_runtimenew.$(O) -T stdlibnew.$(O) cyclonelib.$(O)

TARGETDIR   = build
INCLUDES    = $(patsubst %, -I %, $(SRC_DIRS))

POPCORNLIB = ../../popcorn/lib

POP_IN_CAML = ../../build/popcorn.exe
POP_IN_POP  = $(TARGETDIR)/popcorn_slow.exe
POP_STRAP   = $(TARGETDIR)/popcorn.exe
POP_TEST    = $(TARGETDIR)/popcorn_test.exe

########################## file names ##############################
SRC_DIRS = talx86 toplevel scheme popcorn

UTIL_BASE     = core id set splay dict list hashtable lexing arg string sys\
         filename xarray bitvec char array math
TAL_BASE      = tal talpp talout
TOPLEVEL_BASE = lineno gcdfec gcd
### Worklist should really be in the popcorn utilities base.
POPCORN_BASE  = popsyntax poperr popparse_tab poplex popdep\
	poptypeenv popunify poptypeexp poptypestmt poptype\
	popil poptranslate popilprint popilanalyze popinline \
	popregalloc poptalenv poptaltypes popiltal popcorn \
	popdead popcfg poprtcg popilcheck worklist popiltype \
	popprofile poptailcall

GENFILES = popcorn/poplex.pop popcorn/popparse_tab.pop popcorn/popparse_tab.h \
        popcorn/popdep.pop \
        toplevel/lineno.pop

UTIL_SLOW_TO    = $(addprefix $(POPCORNLIB)/,$(addsuffix _slow.to,$(UTIL_BASE)))
TAL_SLOW_TO     = $(addprefix talx86/,       $(addsuffix _slow.to,$(TAL_BASE)))
TOPLEVEL_SLOW_TO = $(addprefix toplevel/,$(addsuffix _slow.to,$(TOPLEVEL_BASE)))
POPCORN_SLOW_TO = $(addprefix popcorn/,   $(addsuffix _slow.to,$(POPCORN_BASE)))

UTIL_TO     = $(UTIL_SLOW_TO:_slow.to=_strap.to)
TAL_TO      = $(TAL_SLOW_TO:_slow.to=_strap.to)
TOPLEVEL_TO = $(TOPLEVEL_SLOW_TO:_slow.to=_strap.to)
POPCORN_TO  = $(POPCORN_SLOW_TO:_slow.to=_strap.to)

UTIL_TEST_TO     = $(UTIL_SLOW_TO:_slow.to=_test.to)
TAL_TEST_TO      = $(TAL_SLOW_TO:_slow.to=_test.to)
TOPLEVEL_TEST_TO = $(TOPLEVEL_SLOW_TO:_slow.to=_test.to)
POPCORN_TEST_TO  = $(POPCORN_SLOW_TO:_slow.to=_test.to)

#SCHEME_TO = $(addprefix scheme/,     $(addsuffix .to,$(SCHEME_BASE)))

ALL_SOURCES = $(addsuffix .pop, \
  $(TAL_BASE) $(SCHEME_BASE) $(TOPLEVEL_BASE) $(POPCORN_BASE) $(UTIL_BASE))

############################# targets ###############################
all: bison ocamllex all_modules full_bootstrap
old_all: bison ocamllex all_modules $(POP_IN_POP)
all_modules: util toplevel talx86 popcorn #scheme
full_bootstrap: $(POP_IN_POP) $(POP_STRAP) $(POP_TEST)
	$(DIFF) $(POP_STRAP) $(POP_TEST)

bison: 
	$(MAKE) -C tools/bison
ocamllex:
	$(MAKE) -C tools/ocamllex

popcorn:  $(POPCORN_SLOW_TO)
util:     $(UTIL_SLOW_TO)
toplevel: $(TOPLEVEL_SLOW_TO)
talx86:   $(TAL_SLOW_TO)

$(POP_IN_POP): $(UTIL_SLOW_TO) $(TAL_SLOW_TO) $(TOPLEVEL_SLOW_TO) $(POPCORN_SLOW_TO)
	$(TALC) $(LINKFLAGS) $(PRELUDE) $^ -o $@
$(POP_STRAP): $(UTIL_TO) $(TAL_TO) $(TOPLEVEL_TO) $(POPCORN_TO)
	$(TALC) $(PIP_LINKFLAGS) $(PIP_PRELUDE) $^ -o $@
$(POP_TEST): $(UTIL_TEST_TO) $(TAL_TEST_TO) $(TOPLEVEL_TEST_TO) $(POPCORN_TEST_TO)
	$(TALC) $(PIP_LINKFLAGS) $(PIP_PRELUDE) $^ -o $@

#$(TARGETDIR)/scheme.exe: $(UTIL_TO) $(TAL_TO) $(SCHEME_TO)
#       $(TALC) --verify-link --verify-program --no-internals \
#               $^ --std-lib stdlib \
#               -T pop_runtime.$(O) cyclonelib.obj \
#               -o $(TARGETDIR)/scheme.exe
#scheme: $(TARGETDIR)/scheme.exe


############################ depend ###########################

depend: $(GENFILES)
	$(TARGETDIR)/popcorn.exe -M $(COMPFLAGS) $(ALL_SOURCES) > .depend

include .depend

############################ templates #########################
.PRECIOUS: %.pop %_tab.pop

%.pop: %.popl
	$(LEX) $<

%_tab.pop: %.y
	$(BISON) -d $<

%_slow.pop: %.pop
	ln $< $@
%_strap.pop: %.pop
	ln $< $@
%_test.pop: %.pop
	ln $< $@

%_slow.to: %_slow.pop 
	$(POP_IN_CAML) -c $(COMPFLAGS) $(SBFLAGS) $<
%_strap.to: %_strap.pop 
	$(POP_IN_POP) -c $(COMPFLAGS) $(PIPFLAGS) $<
%_test.to: %_test.pop 
	$(POP_STRAP) -c $(COMPFLAGS) $(PIPFLAGS) $<

%.$(O): %.pop 
	$(POP_IN_POP) -c $(COMPFLAGS) $(PIPFLAGS) $<


############################## clean ###########################
clean:
	for i in $(SRC_DIRS) $(POPCORNLIB); do \
	  (cd $$i;\
	   rm -f *~ $(addprefix *., lst tal tali $(O) to pil);\
	   rm -f *_strap.pop *_test.pop *_slow.pop) \
	done
	rm -f *~
	rm -f $(GENFILES)
	rm -f $(TARGETDIR)/*.exe
	rm -f $(TARGETDIR)/*.pdb

clean_strap: clean_test
	for i in $(SRC_DIRS) $(POPCORNLIB); do \
	  (cd $$i; rm -f *~ *_strap*) \
	done
	rm -f $(POP_STRAP)

clean_test:
	for i in $(SRC_DIRS) $(POPCORNLIB); do \
	  (cd $$i; rm -f *~ *_test*) \
	done
	rm -f $(POP_TEST)

clean_rec: clean
	$(MAKE) clean -C tools/bison
	$(MAKE) clean -C tools/ocamllex
