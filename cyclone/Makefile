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

################### applications and options #####################
TALC        = ../build/talc.exe
BISON       = ../apps/popcorn/tools/bison/bison$(exe_suffix)
LEX         = ../apps/popcorn/tools/ocamllex/popocamllex.exe
TOPLEVEL    = ../apps/popcorn/toplevel

ifdef POP_IN_POP
POPCORN     = ../apps/popcorn/build/popcorn.exe
POPFLAGS    = 
STDLIB      = stdlibnew 
SRC_SUFF    =_strap
else
POPCORN     = ../build/popcorn.exe
POPFLAGS    = --stack-trace
STDLIB      = stdlib 
SRC_SUFF    =
endif

EXTRAFLAGS  = 
PRELUDE     = ../runtime/prelude.tal
LINKFLAGS   = --verify-link --no-internals\
              -T pop_runtime.$(O) $(PRELUDE) cyclonelib.$(O) --verify-program
#LINKFLAGS   = --verify-link --no-internals
#         --std-lib pop_runtime $(PRELUDE) cyclonelib.$(O) --verify-program
#LINKFLAGS   = --std-lib pop_runtime $(PRELUDE) cyclonelib.$(O) 
POPCORNLIB = ../popcorn/lib
SRC_DIR     = src
TARGET_DIR  = bin
SRC_DIRS    = $(SRC_DIR) $(POPCORNLIB) $(TOPLEVEL)
INCLUDES    = $(patsubst %, -I %, $(SRC_DIRS))
COMPFLAGS   = $(INCLUDES) $(DEFINES) $(EXTRAFLAGS)
########################## file names ##############################
UTIL_BASE     = \
    $(addsuffix $(SRC_SUFF),\
         core id set splay dict list hashtable lexing arg string sys\
         filename xarray bitvec fn char array)
TOPLEVEL_BASE = $(addsuffix $(SRC_SUFF), gcdfec lineno)
CYCLONE_BASE  = parse_tab lex absyn cyclone pp synpp\
		evexp tcutil tcexp tcenv tcpat tcstmt tc toc

GENFILES = $(addprefix $(SRC_DIR)/,lex.pop parse_tab.pop parse_tab.h)

ALL_SOURCES = $(addsuffix .pop, $(CYCLONE_BASE) $(UTIL_BASE) $(TOPLEVEL_BASE))

UTIL_TO = $(addprefix $(POPCORNLIB)/,$(addsuffix .to,$(UTIL_BASE)))
TOPLEVEL_TO = $(addprefix $(TOPLEVEL)/,$(addsuffix .to,$(TOPLEVEL_BASE)))
CYCLONE_TO = $(addprefix $(SRC_DIR)/,$(addsuffix .to,$(CYCLONE_BASE)))

############################# targets ###############################
all: bison ocamllex all_modules 
all_modules: $(GENFILES) $(TARGET_DIR)/cyclone.exe

bison: 
	$(MAKE) -C ../apps/popcorn/tools/bison
ocamllex:
	$(MAKE) -C ../apps/popcorn/tools/ocamllex

$(TARGET_DIR)/cyclone.exe: $(CYCLONE_TO) $(UTIL_TO) $(TOPLEVEL_TO)
	$(TALC) $(LINKFLAGS) --std-lib $(STDLIB) $^ -o $@

############################ depend ###########################
depend: $(GENFILES)
	for i in $(ALL_SOURCES); do \
	  ../apps/popcorn/build/popcorn.exe -M $(COMPFLAGS) $$i >> .depend;\
	done

include .depend

############################ templates #########################
.PRECIOUS: %.pop %_tab.pop

%_strap.pop: %.pop
	ln $< $@

%.pop: %.popl
	$(LEX) $<

%_tab.pop: %.y
	$(BISON) -v -d $<

%.to: %.pop
	$(POPCORN) -c $(COMPFLAGS) $(POPFLAGS) $<

%.$(O): %.pop 
	$(POPCORN) -c $(COMPFLAGS) $(POPFLAGS) $<


############################## clean ###########################
clean:
	rm -f *~
	rm -f $(GENFILES)
	rm -f $(TARGET_DIR)/cyclone.exe
	(cd lib; make clean)
	(cd boot; make clean)
	(cd $(SRC_DIR);\
	rm -f *~ $(addprefix *., lst tal tali $(O) to pil output);)

