# Makefile for popcorn applications
# Dan Grossman
# FMS: Modified for SIMP

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
CC=cl.exe
DASH_O = /Fo
else

# Assume some kind of Unix
O=o
A=a
exe_suffix=
DEFINES     = -D SYS_UNIX
DIFF=diff -q
CC=gcc
CFLAGS=-Wall -O2
DASH_O = -o

endif
################### directories              #####################
TALCDIR     = ../..
RUNDIR      = ../../runtime
PIPDIR      = ../popcorn
POPCORNLIB  = ../../popcorn/lib
SRCDIR      = src
C_SRCDIR    = csrc
RTCG_SRCDIR = rtcg_src
TARGETDIR   = build

## There will be more source directories in the future.
SRC_DIRS    = $(SRCDIR) rtcg_$(SRCDIR) $(C_SRCDIR)

################### applications and options #####################
TALC        = talc.exe
BISON       = $(PIPDIR)/tools/bison/bison$(exe_suffix)
LEX         = $(PIPDIR)/tools/ocamllex/popocamllex$(exe_suffix)
PROFILER    = $(PIPDIR)/profiler/profiler.exe
POP_IN_CAML = $(TALCDIR)/build/popcorn.exe
POP_IN_POP  = $(PIPDIR)/build/popcorn_slow.exe

EXTRAFLAGS  = 
## PIPFLAGS are only available/used for POP_IN_POP compiler
## -unsafe-arrays for unsafe arrays.
PIPFLAGS    = -no-inline -no-webs -no-cp
PROFFLAGS   = $(PIPFLAGS) -P
## SBFLAGS are only for use with the stack-base compiler.
SBFLAGS     = --stack-trace

ifdef STAT
PIPFLAGS += -D COLLECT_STATS=X
NEW_GC= $(RUNDIR)/gc_stat.a
CYCLONELIB=cyclonelib_stat.$(O)
else
NEW_GC= 
CYCLONELIB=cyclonelib.$(O)
endif

COMPFLAGS   = $(INCLUDES) $(DEFINES) $(EXTRAFLAGS)

PROF_PRELUDE = $(RUNDIR)/preludeprof.to
PIP_PRELUDE  = $(RUNDIR)/preludenew.to
PRELUDE      = $(RUNDIR)/prelude.to

LINKFLAGS     = --link-debug --verify-link --verify-program --no-internals \
	  -T pop_runtime.$(O) -T stdlib.$(O) cyclonelib.$(O)

PIP_LINKFLAGS = --single-error --verify-link --verify-program --no-internals \
-T pop_runtimenew.$(O) -T stdlibnew.$(O) $(CYCLONELIB)

##PIP_LINKFLAGS = --no-verify --no-internals -T pop_runtimenew.$(O) -T stdlibnew.$(O) cyclonelib.$(O)

PROF_FILE = $(SRCDIR)/prof_simp_i.$(O)
PROF_RTCG_FILE = $(RTCG_SRCDIR)/prof_simp_ir.$(O)

PROF_LINKFLAGS = $(PIP_LINKFLAGS)  -T $(PIPDIR)/profiler/prof.$(O)

INCLUDES    = $(patsubst %, -I %, $(SRC_DIRS))

PROF_GC  = $(RUNDIR)/gcprof.$(A)
PROF_DEP = $(PIPDIR)/profiler/prof.$(O) $(PROF_GC)

########################## file names ##############################

# don't really need all of these yet.
UTIL_BASE = core id set splay dict list hashtable lexing arg string sys \
         filename xarray bitvec char array math
BASE = simp mem loader sim syscall interp
TRUSTED = cloader cmisc cstat

GENFILES =

UTIL_SB_TO = $(addprefix $(POPCORNLIB)/,$(addsuffix _slow.to,$(UTIL_BASE)))
BASE_SB_TO = $(addprefix $(SRCDIR)/,          $(addsuffix _slow.to,$(BASE)))
TRUSTED_SB_TALI = $(addprefix $(C_SRCDIR)/, $(addsuffix _slow.tali, $(TRUSTED)))

UTIL_TO = $(UTIL_SB_TO:_slow.to=_strap.to)
BASE_TO = $(BASE_SB_TO:_slow.to=_strap.to)
TRUSTED_TALI = $(TRUSTED_SB_TALI:_slow.tali=_strap.tali)

UTIL_PROF_TO      = $(UTIL_SB_TO:_slow.to=_prof.to)
BASE_PROF_TO      = $(BASE_SB_TO:_slow.to=_prof.to)
TRUSTED_PROF_TALI = $(TRUSTED_SB_TALI:_slow.tali=_prof.tali)

BASE_RTCG_TO      = $(addprefix rtcg_,$(BASE_TO))
BASE_RTCG_SB_TO   = $(addprefix rtcg_,$(BASE_SB_TO))
BASE_RTCG_PROF_TO = $(addprefix rtcg_,$(BASE_PROF_TO))

TRUSTED_SB_OBJ = $(addprefix -T , $(TRUSTED_SB_TALI:.tali=.$(O)) )
TRUSTED_OBJ = $(addprefix -T , $(TRUSTED_TALI:.tali=.$(O)) )
TRUSTED_PROF_OBJ = $(addprefix -T , $(TRUSTED_PROF_TALI:.tali=.$(O)) )

ALL_SOURCES = $(addsuffix .pop, $(BASE) $(UTIL_BASE))

ALL_PROF_ITALI = $(UTIL_PROF_TO:.to=_i.tali) $(BASE_PROF_TO:.to=_i.tali)
ALL_RTCG_PROF_ITALI = $(UTIL_PROF_TO:.to=_i.tali) $(BASE_RTCG_PROF_TO:.to=_i.tali)

ALL_SB_TO   = $(UTIL_SB_TO)   $(BASE_SB_TO)
ALL_TO      = $(UTIL_TO)      $(BASE_TO)
ALL_PROF_TO = $(UTIL_PROF_TO) $(BASE_PROF_TO)

ALL_RTCG_SB_TO   = $(UTIL_SB_TO)   $(BASE_RTCG_SB_TO)
ALL_RTCG_TO      = $(UTIL_TO)      $(BASE_RTCG_TO)
ALL_RTCG_PROF_TO = $(UTIL_PROF_TO) $(BASE_RTCG_PROF_TO)


############################# targets ###############################
all: $(TARGETDIR)/simp_i.exe $(TARGETDIR)/simp_ip.exe

simp_i: $(TARGETDIR)/simp_i.exe

simp_ip: $(TARGETDIR)/simp_ip.exe

simp_ir: $(TARGETDIR)/simp_ir.exe

simp_irp: $(TARGETDIR)/simp_irp.exe

simp_or: $(TARGETDIR)/simp_or.exe

simp_o: $(TARGETDIR)/simp_o.exe

$(TARGETDIR)/simp_o.exe: $(ALL_SB_TO) $(TRUSTED_SB_TALI)
	$(TALC) $(LINKFLAGS) $(PRELUDE) $(ALL_SB_TO) $(TRUSTED_SB_OBJ) -o $@

$(TARGETDIR)/simp_or.exe: $(ALL_RTCG_SB_TO) $(TRUSTED_SB_TALI)
	$(TALC) $(LINKFLAGS) $(PRELUDE) $(ALL_RTCG_SB_TO) $(TRUSTED_SB_OBJ) -o $@

$(TARGETDIR)/simp_i.exe: $(ALL_TO) $(TRUSTED_TALI)
	$(TALC) $(PIP_LINKFLAGS) $(PIP_PRELUDE) $(ALL_TO) $(TRUSTED_OBJ) $(NEW_GC) -o $@

$(TARGETDIR)/simp_ir.exe: $(ALL_RTCG_TO) $(TRUSTED_TALI)
	$(TALC) $(PIP_LINKFLAGS) $(TRUSTED_OBJ) $(PIP_PRELUDE) $(ALL_RTCG_TO) $(NEW_GC) -o $@

$(TARGETDIR)/simp_ip.exe: $(ALL_PROF_TO) $(TRUSTED_PROF_TALI) $(PROF_FILE) $(PROF_DEP)
	$(TALC) $(PROF_LINKFLAGS) -T $(PROF_FILE) $(TRUSTED_PROF_OBJ) $(PROF_PRELUDE) $(ALL_PROF_TO) $(PROF_GC) -o $@

$(TARGETDIR)/simp_irp.exe: $(ALL_RTCG_PROF_TO) $(TRUSTED_RTCG_PROF_TALI) $(PROF_RTCG_FILE) $(PROF_DEP)
	$(TALC) $(PROF_LINKFLAGS) -T $(PROF_RTCG_FILE) $(TRUSTED_RTCG_PROF_OBJ) $(PROF_PRELUDE) $(ALL_RTCG_PROF_TO) $(PROF_GC) -o $@

## Generate the profiling file
$(PROF_FILE): $(ALL_PROF_ITALI)
	$(PROFILER) -o $(PROF_FILE:.$(O)=.c) $^
	$(CC) $(DASH_O)$(PROF_FILE) -c $(PROF_FILE:.$(O)=.c)

$(PROF_RTCG_FILE): $(ALL_RTCG_PROF_ITALI)
	$(PROFILER) -o $(PROF_RTCG_FILE:.$(O)=.c) $^
	$(CC) $(DASH_O)$(PROF_RTCG_FILE) -c $(PROF_RTCG_FILE:.$(O)=.c)

## This file is trusted.
#csrc/cloader.tali: csrc/pop_cloader_slow.to csrc/cloader.$(O)
#	mv csrc/pop_cloader_slow_e.tali csrc/cloader.tali

test_mem.exe: $(UTIL_TO) src/test_mem_strap.to $(BASE_TO) 
	$(TALC) $(PIP_LINKFLAGS) $(PIP_PRELUDE) $^ -o ./$@

test_loader.exe: $(UTIL_SB_TO) src/test_loader_slow.to $(BASE_SB_TO)
	$(TALC) $(LINKFLAGS) $(PRELUDE) -T csrc/cloader.$(O) $^ -o ./$@

test.exe: $(UTIL_TO) test_strap.to $(TRUSTED_TALI)
	$(TALC) $(PIP_LINKFLAGS) $(PIP_PRELUDE) $(UTIL_TO) $(TRUSTED_OBJ) test_strap.to -o ./$@

############################ depend ###########################

depend: $(GENFILES)
	$(PIPDIR)/build/popcorn.exe -M $(COMPFLAGS) $(ALL_SOURCES) > .depend

-include .depend

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
%_prof.pop: %.pop
	ln $< $@

%_slow.to: %_slow.pop 
	$(POP_IN_CAML) -c $(COMPFLAGS) $(SBFLAGS) $<
%_strap.to: %_strap.pop 
	$(POP_IN_POP) -c $(COMPFLAGS) $(PIPFLAGS) $<
%_prof.to: %_prof.pop 
	$(POP_IN_POP) -c $(COMPFLAGS) $(PROFFLAGS) $<

%.$(O): %.pop 
	$(POP_IN_POP) -c $(COMPFLAGS) $(PIPFLAGS) $<

%.$(O): %.c
	$(CC) $(CFLAGS) -c $< $(DASH_O)$@

.PRECIOUS: %_strap.$(O) %_e.tali %_slow.$(O) %_prof.$(O)

%_slow.$(O): %.$(O)
	ln -f $< $@
%_strap.$(O): %.$(O)
	ln -f $< $@
%_prof.$(O): %.$(O)
	ln -f $< $@

%_e.tali: %.to
	echo Generating $@

%.tali: pop_%_e.tali %.$(O)
	ln -f $< $@

############################## clean ###########################
clean:
	for i in $(SRC_DIRS); do \
	  (cd $$i;\
	   rm -f *~ $(addprefix *., lst tal tali $(O) to pil);\
	   rm -f *_strap.pop *_test.pop *_slow.pop) \
	done
	rm -f *~
	rm -f $(GENFILES)
	rm -f $(TARGETDIR)/simp_i.exe $(TARGETDIR)/simp_ir.exe
	rm -f $(TARGETDIR)/*.pdb $(TARGETDIR)/*.ilk

clean_i:
	for i in src ; do \
	  (cd $$i;\
	   rm -f *~ $(addprefix *., lst tal tali $(O) to pil);\
	   rm -f *_strap.pop *_test.pop *_slow.pop) \
	done
	rm -f $(TARGETDIR)/simp_i.exe

clean_ir:
	for i in rtcg_src ; do \
	  (cd $$i;\
	   rm -f *~ $(addprefix *., lst tal tali $(O) to pil);\
	   rm -f *_strap.pop *_test.pop *_slow.pop) \
	done
	rm -f $(TARGETDIR)/simp_ir.exe

clean_rec: clean
	cd tests; rm -f *.ppm
