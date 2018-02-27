# Makefile for TALC
#
# If this fails the first time do make depend before make all.
#
# This file is considerable cleaned up from initial versions written by Dave.
# Should old stuff in the initial makefile be needed, retrieve version 1.6.
# NG 22 Dec 97
#
# Next major cleanup by Dan in Version 1.36 -- retrieve earlier version for
# kml-related targets.
# THIS FILE NOW RELIES VERY HEAVILY ON GNUMAKE! 
# DG 12 Jan 99

# Now in Version 1.37 we can compile to native.	 To do native profiling, must
# be under UNIX (due to limitation of caml).

# To build with profiling, do: make <target> PROFILE=X

#################### Configuration
# N.B. Requires MASM 6.11 and Visual C++ to build the runtime

# note: native/profile combination is not supported under Windows as of 
#	ocaml version 2.01
ifdef NATIVE
OCAMLC	= ocamlopt$(OPT)
OCAMLI	= ocamlopt$(OPT)
OBJSUFF = cmx
LIBSUFF = cmxa
   ifdef PROFILE
      FLAGS = -p -w s
   else
      FLAGS = -w s
   endif
INTERPRETER = libasmrun
else
OBJSUFF = cmo
# cannot use ocamlcp for some reason
OCAMLI = ocamlc$(OPT)
LIBSUFF = cma
   ifdef PROFILE
      OCAMLC = ocamlcp
      FLAGS  = -w s
   else
      OCAMLC = ocamlc$(OPT)
      FLAGS  = -w s
   endif
INTERPRETER = libcamlrun
endif

ifdef DEBUG
  FLAGS += -g
endif

ifdef PROTO
  FLAGS += -i
endif

ifdef WINDIR

ifdef CAMLLIB
OCAMLLIB :=$(subst \,/,$(CAMLLIB))
else
ifdef OCAMLLIB
OCAMLLIB :=$(subst \,/,$(OCAMLLIB))
else
echo OCaml does not appear to be installed.  Neither OCAMLLIB nor CAMLLIB are defined.
endif
endif

else
OCAMLLIB :=`$(OCAMLC) -v | grep library | awk '{ print $$4 }'`
endif

OCAMLLINKLIB=$(OCAMLLIB)/caml
OCAMLRUNLIB=$(OCAMLLIB)/$(INTERPRETER)
OCAMLLEX  = ocamllex
OCAMLYACC = ocamlyacc -v
COMPFLAGS = $(INCLUDES) $(FLAGS)
ifndef NATIVE
LINKFLAGS = -custom $(COMPFLAGS) 
endif
TARGETDIR = build

######################################################################
# O/S Stuff

# NG: This is a hack and probably ought to be replaced by something more
#     robust.  To determine the O/S we see if the environment variable WINDIR
#     is defined, if so, we assume Win32, if not, we assume a *nix.

ifdef WINDIR

# Assume Windows NT/98/95/3.x
O=obj
A=lib
ASM=ml /nologo /coff
CC=cl /nologo
CINCLUDEDIR=/I
EMACS=emacs
MV=mv

else

# Assume some kind of Unix
O=o
A=a
CC=gcc -Wall -O2 -ggdb
CINCLUDEDIR=-I
EMACS=emacs-19	# CUCS has 20 as default
LINUX_GC=gc.libc6 # or gc.libc5
MV=mv

endif

INTERPRETER := $(INTERPRETER).$(A)

######################################################################

all: talc popcorn runtime

world: all runtime scheme_runtime doc/tal.elc

#################### Util

UTIL_BASE = utilities splay dict set xarray stringchan binout bitmatrix \
	    numtypes smallset base64
COMPUTIL_BASE = identifier

UTIL	 = $(addprefix util/,	  $(addsuffix .$(OBJSUFF), $(UTIL_BASE)))
COMPUTIL = $(addprefix computil/, $(addsuffix .$(OBJSUFF), $(COMPUTIL_BASE)))

$(TARGETDIR)/util.$(LIBSUFF):  $(UTIL) $(COMPUTIL) 
	$(OCAMLC) $(LINKFLAGS) -a -o $@ $^ 

################### Gcd
 # this is the top-level directory, but we don't include talc.ml since it
 # isn't generic.

GCD_BASE = lineno gcdfec gcdfe gcd

GCDLIB = $(addprefix toplevel/, $(addsuffix .$(OBJSUFF), $(GCD_BASE)))

$(TARGETDIR)/gcd.$(LIBSUFF): $(GCDLIB)
	$(OCAMLC) $(LINKFLAGS) -a -o $@ $^ 

#################### Tal

TALLIB_BASE = tal talctxt talpp talout

TALLIB = $(addprefix talx86/, $(addsuffix .$(OBJSUFF), $(TALLIB_BASE)))

$(TARGETDIR)/tal.$(LIBSUFF): $(TALLIB)
	$(OCAMLC) $(LINKFLAGS) -a -o $@ $^ 

#################### Talcomp

TALCOMPLIB_BASE = objfile talcon disobjfile cyclone talbin talbinout talparser \
		  tallex tallogic talbinin tallinkchk talverify \
		  talasmx86 talasm \
		  coff elf talbe binin discoff diself disasmx86 \
		  dasm

TALCOMPLIB = $(addprefix talx86/, $(addsuffix .$(OBJSUFF), $(TALCOMPLIB_BASE)))

$(TARGETDIR)/talcomp.$(LIBSUFF): $(TALCOMPLIB)
	$(OCAMLC) $(LINKFLAGS) -a -o $@ $^ 

#################### C support

CFILES = util/float.$(O)

#################### dynlink

DLOBJS = $(UTIL) $(COMPUTIL) $(TALLIB) $(GCDLIB) $(TALCOMPLIB)

runtime/dynlinklib.$(O): $(DLOBJS) toplevel/dynlink.$(OBJSUFF)
	$(OCAMLC) -output-obj $(LINKFLAGS) -o $(notdir $@) $^
	$(MV) $(notdir $@) $@

runtime/dynlink.$(O): runtime/dynlink.c
	$(OCAMLC) -c $< -o $(notdir $@); $(MV) $(notdir $@) $@

# for Windows, we compile the dynlink library in bytecode to avoid
# name conflicts
ifdef NATIVE
ifdef WINDIR
runtime/dynlinklib.$(A): 
	$(MAKE) NATIVE= runtime/dynlinklib.$(A)
else
runtime/dynlinklib.$(A): $(OCAMLIB) $(CFILES) runtime/dynlinklib.$(O) runtime/dynlink.$(O)
	cp $(OCAMLRUNLIB) $@
	ar r $@ $+
endif
else
runtime/dynlinklib.$(A): $(OCAMLIB) $(CFILES) runtime/dynlinklib.$(O) runtime/dynlink.$(O)
	cp $(OCAMLRUNLIB) $@
	ar r $@ $+
endif

dynlink : runtime/dynlinklib.$(A) runtime/c_runtime.$(A) runtime/poplib.$(A)

GENCOBJS = $(addprefix $(TARGETDIR)/, $(addsuffix .$(LIBSUFF), \
	     util tal gcd talcomp)) \
	   toplevel/genCinit.$(OBJSUFF)


#################### genCinit.exe

$(TARGETDIR)/genCinit.exe: $(CFILES) $(GENCOBJS)
	$(OCAMLC) $(LINKFLAGS) -o $@ $^ 


#**#
#
#runtime/webtalfns.$(O): $(DLOBJS) toplevel/dynlink.$(OBJSUFF) toplevel/webtalfns.ml
#	$(OCAMLC) -output-obj $(LINKFLAGS) -o $(notdir $@) $^
#	$(MV) $(notdir $@) $@
#
#**#


#################### talc.exe

TALCOBJS = $(addprefix $(TARGETDIR)/, $(addsuffix .$(LIBSUFF), \
	     util tal gcd talcomp)) \
	   toplevel/talc.$(OBJSUFF)

talc: $(TARGETDIR)/talc.exe

$(TARGETDIR)/talc.exe: $(CFILES) $(TALCOBJS)
	$(OCAMLC) $(LINKFLAGS) -o $@ $^ 

#################### Popcorn

POPCORN_BASE = poperr popsyntax popparse poplex poptype poppeep \
	       popcomptypes popcompenv popcompile popdyntrans \
               popdynpatch popcorn

POPOBJS = $(addprefix $(TARGETDIR)/, $(addsuffix .$(LIBSUFF), util tal gcd talcomp)) \
	  $(addprefix talx86/, objfile.$(OBJSUFF)) \
	  $(addprefix popcorn/, $(addsuffix .$(OBJSUFF), $(POPCORN_BASE)))

popcorn: $(TARGETDIR)/popcorn.exe

$(TARGETDIR)/popcorn.exe: $(CFILES) $(POPOBJS)
	$(OCAMLC) $(LINKFLAGS) -o $@ $^ 

GENPATCH_BASE = poperr popsyntax popparse poplex poptype popdynpatch \
  poppeep popcomptypes popcompenv popdyntrans popgenpatch
GENPATCH_OBJS = $(addprefix $(TARGETDIR)/, $(addsuffix .$(LIBSUFF), util tal gcd talcomp)) \
	  $(addprefix talx86/, objfile.$(OBJSUFF)) \
	  $(addprefix popcorn/, $(addsuffix .$(OBJSUFF), $(GENPATCH_BASE)))

$(TARGETDIR)/popgenpatch.exe: $(CFILES) $(GENPATCH_OBJS)
	$(OCAMLC) $(LINKFLAGS) -o $@ $^ 


#################### Scheme

SCHEME_BASE = sast sil sparse slex scomp scheme

SCHEMEOBJS= $(addprefix $(TARGETDIR)/, $(addsuffix .$(LIBSUFF), util tal gcd)) \
	    $(addprefix scheme/, $(addsuffix .$(OBJSUFF), $(SCHEME_BASE)))

scheme: $(TARGETDIR)/scheme.exe

$(TARGETDIR)/scheme.exe: $(SCHEMEOBJS) 
	$(OCAMLC) $(LINKFLAGS) -o $@ $^ 

#################### Runtimes

#RLS 5/26/00#
WIN_BASE_OBJS = tal_util stdlib pop_runtime loader cyclonelib sclib pop_runtimenew
WIN_BASE = tal_start $(WIN_BASE_OBJS)

#WIN_BASE = tal_start tal_start_nomain tal_util stdlib pop_runtime cyclonelib sclib loader pop_runtimenew
#/RLS#

# tal_sockets only works for Linux (for now)
ifdef WINDIR
RUNTIME_BASE = $(WIN_BASE)
else           
RUNTIME_BASE = $(WIN_BASE) tal_sockets
endif

RUNTIME_LIBS = gc.$(A) objlib.$(A) dynlinklib.$(A)

RUNTIME=$(addprefix runtime/, $(RUNTIME_LIBS) $(addsuffix .$(O), $(RUNTIME_BASE)))

runtime: $(RUNTIME) runtime/stdlibnew.$(O) runtime/prelude.$(O)	runtime/preludenew.$(O) runtime/preludeprof.$(O) $(TARGETDIR)/genCinit.exe
	$(MAKE) -C popcorn/lib

runtime/stdlibnew.$(O): runtime/stdlib.$(O)
	cp runtime/stdlib.$(O) runtime/stdlibnew.$(O) 

runtime/gc.a: runtime/$(LINUX_GC)
	cp $< $@

runtime/objlib.$(A) runtime/loader.$(O): runtime/loader/loader.c
	cd runtime/loader; $(MAKE) WHERE=.. install

#RLS 5/26/00#
NO_MAIN_BASE = $(subst tal_start,tal_start_nomain, $(RUNTIME_BASE) prelude)
NO_MAIN_NO_NEW_BASE = $(filter-out %new, $(NO_MAIN_BASE))
NO_MAIN = $(addprefix runtime/, $(addsuffix .$(O), $(NO_MAIN_NO_NEW_BASE)))

runtime/c_runtime.$(A): $(NO_MAIN)
	ar r $@ $+

runtime/poplib.$(A): popcorn
	$(MAKE) -C popcorn/lib poplib.lib
	mv popcorn/lib/poplib.lib runtime
#/RLS#
#################### Ocamldep

computil/ocamldep.cmo: computil/ocamldep.ml
	$(OCAMLC) -c $<

$(TARGETDIR)/ocamldep.exe: computil/ocamldep.cmo
	$(OCAMLC) $(LINKFLAGS) -o $@ $^ 

# special rules for these, since they don't match the template rule
runtime/tal_start.c: runtime/tal_start.tmpl runtime/tal.tali $(TARGETDIR)/genCinit.exe
	cp $< $@
	genCinit.exe --no-string-def --no-rep-def runtime/tal.tali >> $@

runtime/tal_start_nomain.c: runtime/tal_start_nomain.tmpl runtime/tal.tali $(TARGETDIR)/genCinit.exe
	cp $< $@
	genCinit.exe --no-string-def --no-rep-def runtime/tal.tali >> $@

######################################################################
# Templates

%.ml: %.mll
	$(OCAMLLEX) $< 

%.ml %.mli: %.mly
	$(OCAMLYACC) $< 

%.cmi: %.mli
	$(OCAMLI) -c $(COMPFLAGS) $<

%.cmo %.cmx: %.ml
	$(OCAMLC) -c $(COMPFLAGS) $<

%.obj: %.asm
	$(ASM) /Fo$@ /c $< 

%.obj: %.tal $(TARGETDIR)/talc.exe
	$(TARGETDIR)/talc.exe -c --coff $<

%.o: %.tal $(TARGETDIR)/talc.exe
	$(TARGETDIR)/talc.exe -c --elf $<

%.obj: %.c
	$(CC) -Z7 $(CINCLUDEDIR) $(OCAMLLINKLIB) /MT /Fo$@ /c $<

%.o: %.c
	$(CC) $(CINCLUDEDIR)$(OCAMLLINKLIB) -c -o $@ $<

%.elc: %.el
	$(EMACS) -batch -f batch-byte-compile $<

%.c: %.tmpl %.tali $(TARGETDIR)/genCinit.exe
	cp $< $@
	genCinit.exe --no-string-def --no-rep-def $(@:.c=.tali) >> $@

######################################################################

# Administration

DIRS = util computil talx86 runtime toplevel popcorn scheme

INCLUDES = $(patsubst %,-I %, $(DIRS))

SOURCES = $(addsuffix /*.ml,  $(DIRS)) \
	  $(addsuffix /*.mli, $(DIRS))

LEXFILES  = toplevel/lineno talx86/tallex popcorn/poplex scheme/slex
YACCFILES = talx86/talparser popcorn/popparse scheme/sparse

GENERATINGFILES = $(addsuffix .mll, $(LEXFILES))\
		  $(addsuffix .mly, $(YACCFILES))

GENFILES = $(addsuffix .ml,  $(LEXFILES) $(YACCFILES)) \
	   $(addsuffix .mli, $(YACCFILES))

TMP_SUFF = cmo cmi cmx obj o to output exe

clean:
	for i in $(DIRS); do \
	  (cd $$i; rm -f *~ $(addprefix *., $(TMP_SUFF))) \
	done
	cd $(TARGETDIR); rm -f *.exe *.cma *.cmxa *.$(A)
	rm -f $(GENFILES) *~
	cd tests/popcorn; make clean
	cd tests/cyclone; make clean
	cd tests/loader; make clean
	cd tests/scheme;  make clean
	cd apps/popcorn;  make clean_rec
	cd apps/gml;      make clean_rec
	cd popcorn/lib;   make clean
	cd tests/talx86;  rm -f *.$(O) *.to *.lst *.exe *~
	cd runtime/loader; make clean
	cd runtime; rm -f objlib.$(A) dynlinklib.$(A) stdlib.c loader.tali \
	  tal_start.c tal_start_nomain.c pop_runtime.c tal_sockets.c
	cd runtime/loader; rm -f dlnative.{to,$(O)} dlpop.{to,$(O)}
ifndef WINDIR
	cd doc;		  rm -f *.aux *.dvi *.log *.elc
endif

dlclean:
	cd runtime/loader; make clean
	cd runtime; rm -f objlib.$(A) dynlinklib.$(A) c_runtime.$(A) loader.$(O) dynlink.$(O) dynlinklib.$(O) tal_start_nomain.$(O) poplib.$(A)
	cd runtime/loader; rm -f dlnative.{to,$(O)} dlpop.{to,$(O)}

depend: $(GENFILES) $(TARGETDIR)/ocamldep.exe
	$(TARGETDIR)/ocamldep.exe --unix $(INCLUDES) $(SOURCES) > .depend
#       ocamldep $(INCLUDES) $(SOURCES) > .depend

-include .depend

# This is stuff for stripping out the ^M from NT files shipped to UNIX.
# It is a brittle script (if 2 people use it at the same time then you
# could corrupt your data).  Use at your own risk.

strip:
	for suffix in ml mll mly mli tali c asm tal tex el pop s S ss h out; do \
	  find . -name \*.$$suffix -exec ./stripm {} \; ; \
	done
	for file in Makefile Makefile.inc; do \
	  find . -name $$file -exec ./stripm {} \; ; \
	done

# EOF: Makefile
