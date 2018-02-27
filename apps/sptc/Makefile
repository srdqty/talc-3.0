# Makefile for sptc -- a simple version of ptc.
# PTC is an open source graphics library that runs under both unix and windows.
# It is available from http://www.gaffer.org/ptc
#
# This Makefile relies on feature of GNU make.
#
# January 2000
# Fred Smith

# Assume Windows NT/98/95/3.x
O=obj
A=lib
exe_suffix=.exe
DIFF=diff -a

# Directories
TALCDIR     = ../..
TARGETDIR   = .
POPCORNLIB  = $(TALCDIR)/popcorn/lib

# Tools
CC          = cl /nologo
TALC        = talc.exe
## Warning: Must use popcorn-in-popcorn compiler.  Other compiler does not
## respect C's calling convention.
POPCORN     = ../popcorn/build/popcorn.exe

# Flags
PRELUDE     = $(TALCDIR)/runtime/preludenew.tal
INCLUDES    = -I $(POPCORNLIB)

# Files
PTCLIB      = ptc.lib
## Must include library files you use here.
OBJS        = core

TOS         = $(addprefix $(POPCORNLIB)/,$(addsuffix _strap.to, $(OBJS)))

# Targets

all: sptc sptc_tali

sptc: sptc.c
	$(CC) /c sptc.c /link $(PTCLIB) 

sptc_tali: pop_sptc.pop
	$(POPCORN) -c pop_sptc.pop
	rm -f pop_sptc.tal pop_sptc_i.tali pop_sptc.to
	mv pop_sptc_e.tali sptc.tali

clean:
	rm -f *~ *.obj *.exe *.lst *.pil *.tal *_e.tali *_i.tali *.to

# Generic targets

%.$(O) %.to: %.pop
	$(POPCORN) -c $<


# Generate an executable using sptc.
%.exe: %.to %.$(O)
	$(TALC) --TALC --verify-link --no-internals --verify-program \
	$(INCLUDES) $(TOS) \
        -T pop_runtimenew.$(O) $(PRELUDE) cyclonelib.$(O) -T sptc.$(O) \
	-T stdlibnew.$(O) --trusted $(PTCLIB) $*.to -o $@

