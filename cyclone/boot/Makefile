CY=../bin/cyclone
BOOTCY=./cyclone
BOOT2CY=./boot_cyclone
GCLIB=../bin/gc.a
LEX=../../apps/cyclone/tools/lex/cycocamllex.exe
BISON=../../apps/cyclone/tools/bison/bison.exe
CC=gcc
CFLAGS=-g -fwritable-strings -I../lib
CYCFLAGS=-I ../lib -gcc -tc -toc -pp
LIBDIR=../lib

LIBRAW= arg.o bitvec.o buffer.o character.o core.o \
	dict.o filename.o hashtable.o list.o queue.o ref.o rope.o set.o \
	splay.o string.o lexing.o id.o xarray.o fn.o array.o position.o \
	pp.o lineno.o 
LIBOBJS= $(addprefix $(LIBDIR)/, $(LIBRAW)) \
	$(addprefix $(LIBDIR)/, cyc_helpers.o)
BOOTLIBOBJS = $(addprefix $(LIBDIR)/, $(addprefix boot_, $(LIBRAW))) \
	$(addprefix $(LIBDIR)/, cyc_helpers.o)
BOOT2LIBOBJS = $(addprefix $(LIBDIR)/, $(addprefix boot2_, $(LIBRAW))) \
	$(addprefix $(LIBDIR)/, cyc_helpers.o)
CYCOBJS= absyn.o parse_tab.o lex.o absynpp.o tcenv.o tcutil.o \
	tcstmt.o tcpat.o tcexp.o evexp.o tc.o toc.o cyclone.o
CLIBFILES = $(LIBOBJS:.o=.c)
CYCFILES = $(CYCOBJS:.o=.c)

BOOT_CYCOBJS= $(addprefix boot_, $(CYCOBJS))
BOOT_CYCFILES = $(addprefix boot_, $(CYCFILES))
BOOT2_CYCOBJS= $(addprefix boot2_, $(CYCOBJS))
BOOT2_CYCFILES = $(addprefix boot2_, $(CYCFILES))

all: cyclone $(CYCFILES)
boot: $(BOOT_CYCFILES) $(BOOT_CYCOBJS) boot_cyclone
boot2: $(BOOT2_CYCFILES) $(BOOT2_CYCOBJS) boot2_cyclone

cyclone: $(CYCOBJS)
	$(CC) -g -o $@ $^ $(LIBOBJS) $(GCLIB)

boot_cyclone: $(BOOT_CYCOBJS)
	$(CC) -g -o $@ $^ $(BOOTLIBOBJS) $(GCLIB)

boot2_cyclone: $(BOOT2_CYCOBJS)
	$(CC) -g -o $@ $^ $(BOOT2LIBOBJS) $(GCLIB)

clean:
	rm -f cyclone cyclone.exe *.o *.i $(CYCFILES)
	rm -f lex.cyc parse_tab.h parse_tab.cyc parse.output
	rm -f boot*.c boot*.exe

%.c: %.cyc
	$(CY) $(CYCFLAGS) $^ > $@

boot_%.c: %.cyc
	$(BOOTCY) $(CYCFLAGS) $^ > $@

boot2_%.c: %.cyc
	$(BOOT2CY) $(CYCFLAGS) $^ > $@

%.cyc: %.cyl
	$(LEX) $<

%_tab.cyc: %.y
	$(BISON) -v -d $<

%_tab.h: %.y
	$(BISON) -v -d $<

# dependencies
cyclone.o: parse.h absyn.h absynpp.h
parse.h: absyn.h parse_tab.h
lex.o: parse.h absyn.h parse_tab.h
