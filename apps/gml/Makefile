# Makefile for popcorn applications
# Dan Grossman
# FMS: Modified for GML November 14.

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
DASH_O = -o

endif
################### directories              #####################
TALCDIR     = ../..
RUNDIR      = ../../runtime
PIPDIR      = ../popcorn
POPCORNLIB  = ../../popcorn/lib
SRCDIR      = src
RTCG_SRCDIR = rtcg_src
TARGETDIR   = build

## There will be more source directories in the future.
SRC_DIRS    = $(SRCDIR) rtcg_$(SRCDIR) crsc

################### applications and options #####################
TALC        = talc.exe
BISON       = $(PIPDIR)/tools/bison/bison$(exe_suffix)
LEX         = $(PIPDIR)/tools/ocamllex/popocamllex.exe
PROFILER    = $(PIPDIR)/profiler/profiler.exe
POP_IN_CAML = $(TALCDIR)/build/popcorn.exe
POP_IN_POP  = $(PIPDIR)/build/popcorn_slow.exe

EXTRAFLAGS  = 
## PIPFLAGS are only available/used for POP_IN_POP compiler
PIPFLAGS    = -no-inline -no-cp -no-webs
PROFFLAGS   = $(PIPFLAGS) -P
## SBFLAGS are only for use with the stack-base compiler.
SBFLAGS     = --stack-trace
COMPFLAGS   = $(INCLUDES) $(DEFINES) $(EXTRAFLAGS)

ifdef STAT
PIPFLAGS += -D COLLECT_STATS=X
NEW_GC= $(RUNDIR)/gc_stat.a
CYCLONELIB=cyclonelib_stat.$(O)
else
NEW_GC= 
CYCLONELIB=cyclonelib.$(O)
endif

PIPFLAGS += $(addprefix -D ,$(addsuffix =X,$(DS)))

PROF_PRELUDE = $(RUNDIR)/preludeprof.to
PIP_PRELUDE  = $(RUNDIR)/preludenew.to
PRELUDE      = $(RUNDIR)/prelude.to

LINKFLAGS     = --link-debug --verify-link --verify-program --no-internals \
	  -T pop_runtime.$(O) -T stdlib.$(O) cyclonelib.$(O)
PIP_LINKFLAGS = --single-error --verify-link --verify-program --no-internals \
	  -T pop_runtimenew.$(O) -T stdlibnew.$(O) $(CYCLONELIB)

PROF_FILE = $(SRCDIR)/prof_gml_i.$(O)
PROF_RTCG_FILE = $(RTCG_SRCDIR)/prof_gml_ir.$(O)

PROF_LINKFLAGS = $(PIP_LINKFLAGS)  -T $(PIPDIR)/profiler/prof.$(O)

INCLUDES    = $(patsubst %, -I %, $(SRC_DIRS))

PROF_GC  = $(RUNDIR)/gcprof.$(A)
PROF_DEP = $(PIPDIR)/profiler/prof.$(O) $(PROF_GC)

########################## file names ##############################

# don't really need all of these yet.
UTIL_BASE = core id set splay dict list hashtable lexing arg string sys\
         filename xarray bitvec char array math
BASE = top point matrix affine gmlsyntax parse_tab lex eval gml render ppm

TRUSTED = cstat

GENFILES = lex.pop parse_tab.pop parse_tab.h 

UTIL_SB_TO = $(addprefix $(POPCORNLIB)/,$(addsuffix _slow.to,$(UTIL_BASE)))
BASE_SB_TO = $(addprefix $(SRCDIR)/,          $(addsuffix _slow.to,$(BASE)))
TRUSTED_SB_TALI = $(addprefix csrc/,$(addsuffix _slow.tali, $(TRUSTED)))

UTIL_TO = $(UTIL_SB_TO:_slow.to=_strap.to)
BASE_TO = $(BASE_SB_TO:_slow.to=_strap.to)
TRUSTED_TALI = $(TRUSTED_SB_TALI:_slow.tali=_strap.tali)

UTIL_PROF_TO = $(UTIL_SB_TO:_slow.to=_prof.to)
BASE_PROF_TO = $(BASE_SB_TO:_slow.to=_prof.to)
TRUSTED_PROF_TALI = $(TRUSTED_SB_TALI:_slow.tali=_prof.tali)

BASE_RTCG_TO = $(addprefix rtcg_,$(BASE_TO))
BASE_RTCG_SB_TO = $(addprefix rtcg_,$(BASE_SB_TO))
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

MV_IR = mv $(TARGETDIR)/gml_ir.exe

############################# targets ###############################
all: $(TARGETDIR)/gml_i.exe $(TARGETDIR)/gml_ip.exe

.PHONY: gml_i gml_ip gml_ir gml_irp gml_or raytrace

raytrace:
	cd plclub/src; make raytrace
	mv plclub/src/raytrace $(TARGETDIR)/raytrace

raytrace_p:
	cd plclub/src; make clean
	cd plclub/src; make PROFILE=X raytrace
	mv plclub/src/raytrace $(TARGETDIR)/raytrace_p

all_gml:
	rm -rf build/*.exe
	make clean_i
	make gml_i
	make gml_ip
	make clean_ir
	make DS=FIND_ALL_OPT gml_ir
	$(MV_IR) build/gml_ir_f.exe
	make clean_ir
	make DS=TRACE_OPT gml_ir
	$(MV_IR) build/gml_ir_t.exe
	make clean_ir
	make DS=INTERP_OPT gml_ir
	$(MV_IR) build/gml_ir_i.exe
	make clean_ir
	make DS=HAND_VMUL_OPT gml_ir
	$(MV_IR) build/gml_ir_h.exe
	make clean_ir
	make DS=SIMPLE_VMUL_OPT gml_ir
	$(MV_IR) build/gml_ir_v.exe
	make clean_ir
	make DS=SMART_VMUL_OPT gml_ir
	$(MV_IR) build/gml_ir_V.exe
	make clean_ir
	make STAT=X DS=FIND_ALL_OPT  gml_ir
	$(MV_IR) build/gml_ir_fs.exe
	make clean_ir
	make STAT=X DS=TRACE_OPT gml_ir
	$(MV_IR) build/gml_ir_ts.exe
	make clean_ir
	make STAT=X DS=INTERP_OPT gml_ir
	$(MV_IR) build/gml_ir_is.exe
	make clean_ir
	make STAT=X DS=HAND_VMUL_OPT gml_ir
	$(MV_IR) build/gml_ir_hs.exe
	make clean_ir
	make STAT=X DS=SIMPLE_VMUL_OPT gml_ir
	$(MV_IR) build/gml_ir_vs.exe
	make clean_ir
	make STAT=X DS=SMART_VMUL_OPT gml_ir
	$(MV_IR) build/gml_ir_Vs.exe

gml_i: $(TARGETDIR)/gml_i.exe

gml_ip: $(TARGETDIR)/gml_ip.exe

gml_ir: $(TARGETDIR)/gml_ir.exe

gml_irp: $(TARGETDIR)/gml_irp.exe

gml_or: $(TARGETDIR)/gml_or.exe

$(TARGETDIR)/gml_o.exe: $(ALL_SB_TO) $(TRUSTED_SB_TALI)
	$(TALC) $(LINKFLAGS) $(PRELUDE) $(UTIL_SB_TO) $(BASE_SB_TO) $(TRUSTED_SB_OBJ) -o $@

$(TARGETDIR)/gml_or.exe: $(ALL_RTCG_SB_TO) $(TRUSTED_SB_TALI)
	$(TALC) $(LINKFLAGS) $(PRELUDE) $(ALL_RTCG_SB_TO) $(TRUSTED_SB_TALI) -o $@

$(TARGETDIR)/gml_i.exe: $(ALL_TO) $(TRUSTED_TALI)
	$(TALC) $(PIP_LINKFLAGS) $(TRUSTED_OBJ) $(PIP_PRELUDE) $(ALL_TO) $(NEW_GC) -o $@

$(TARGETDIR)/gml_ir.exe: $(ALL_RTCG_TO) $(TRUSTED_TALI) hand_vmul.to
	$(TALC) $(PIP_LINKFLAGS) $(TRUSTED_OBJ) $(PIP_PRELUDE) $(ALL_RTCG_TO) hand_vmul.to $(NEW_GC) -o $@

$(TARGETDIR)/gml_ip.exe: $(ALL_PROF_TO) $(TRUSTED_PROF_TALI) $(PROF_FILE) $(PROF_DEP)
	$(TALC) $(PROF_LINKFLAGS) -T $(PROF_FILE) $(TRUSTED_PROF_OBJ) $(PROF_PRELUDE) $(ALL_PROF_TO) $(PROF_GC) -o $@

$(TARGETDIR)/gml_irp.exe: $(ALL_RTCG_PROF_TO) $(TRUSTED_RTCG_PROF_TALI) $(PROF_RTCG_FILE) $(PROF_DEP)
	$(TALC) $(PROF_LINKFLAGS) -T $(PROF_RTCG_FILE) $(TRUSTED_PROF_OBJ) $(PROF_PRELUDE) $(ALL_RTCG_PROF_TO) $(PROF_GC) -o $@

$(TARGETDIR)/test_ppm.exe: $(UTIL_SB_TO) ppm_slow.to test_ppm_slow.to
	$(TALC) $(LINKFLAGS) $(PRELUDE) $^ -o $@

test.exe: $(UTIL_TO) rtcg_src/matrix_strap.to rtcg_src/top_strap.to rtcg_src/test_strap.to
	$(TALC) $(PIP_LINKFLAGS) $(PIP_PRELUDE) $^ -o $@

test_prof.exe: $(UTIL_PROF_TO) rtcg_src/matrix_prof.to rtcg_src/top_prof.to rtcg_src/test_prof.to prof_test.$(O) $(PROF_DEP)
	$(TALC) $(PROF_LINKFLAGS) -T prof_test.$(O) $(PROF_PRELUDE) $(UTIL_PROF_TO) rtcg_src/matrix_prof.to rtcg_src/top_prof.to rtcg_src/test_prof.to $(PROF_GC)  -o $@

hand_vmul.to: hand_vmul.tal
	$(TALC) -c hand_vmul.tal

## Generate the profiling file
$(PROF_FILE): $(ALL_PROF_ITALI)
	$(PROFILER) -o $(PROF_FILE:.$(O)=.c) $^
	$(CC) $(DASH_O)$(PROF_FILE) -c $(PROF_FILE:.$(O)=.c)

$(PROF_RTCG_FILE): $(ALL_RTCG_PROF_ITALI)
	$(PROFILER) -o $(PROF_RTCG_FILE:.$(O)=.c) $^
	$(CC) $(DASH_O)$(PROF_RTCG_FILE) -c $(PROF_RTCG_FILE:.$(O)=.c)

prof_test.$(O):  $(UTIL_PROF_TO:.to=_i.tali) rtcg_src/matrix_prof_i.tali rtcg_src/top_prof_i.tali rtcg_src/test_prof_i.tali
	$(PROFILER) -o prof_test.c $^
	$(CC) $(DASH_O)prof_test.$(O) -c prof_test.c

############################ depend ###########################

depend: $(GENFILES)
	$(PIPDIR)/build/popcorn.exe -M $(COMPFLAGS) $(ALL_SOURCES) > .depend

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
	rm -f $(TARGETDIR)/*.exe
	rm -f $(TARGETDIR)/*.pdb $(TARGETDIR)/*.ilk
	rm -f hand_vmul.$(O) hand_vmul.to 

clean_i:
	for i in src ; do \
	  (cd $$i;\
	   rm -f *~ $(addprefix *., lst tal tali $(O) to pil);\
	   rm -f *_strap.pop *_test.pop *_slow.pop) \
	done
	rm -f $(TARGETDIR)/gml_i.exe

clean_ir:
	for i in rtcg_src ; do \
	  (cd $$i;\
	   rm -f *~ $(addprefix *., lst tal tali $(O) to pil);\
	   rm -f *_strap.pop *_test.pop *_slow.pop) \
	done
	rm -f $(TARGETDIR)/gml_ir.exe

clean_rec: clean
	cd tests; rm -f *.ppm
