CY=../bin/cyclone
BOOTCY=../boot/cyclone
BOOT2CY=../boot/boot_cyclone
GCLIB=../bin/gc.a
LEX=../../apps/cyclone/tools/lex/cycocamllex.exe
BISON=../../apps/cyclone/tools/bison/bison.exe
CC=gcc
CFLAGS=-g -fwritable-strings

OBJS= arg.o bitvec.o buffer.o character.o core.o dict.o filename.o\
	hashtable.o list.o queue.o ref.o rope.o set.o splay.o string.o\
	lexing.o arg.o id.o xarray.o fn.o array.o position.o\
	cyctest.o lineno.o pp.o
CFILES = $(OBJS:.o=.c)

BOOTOBJS=$(addprefix boot_, $(OBJS))
BOOTCFILES=$(addprefix boot_, $(CFILES))
BOOT2OBJS=$(addprefix boot2_, $(OBJS))
BOOT2CFILES=$(addprefix boot2_, $(CFILES))

all: test foo $(OBJS) $(CFILES)
boot: $(BOOTCFILES) $(BOOTOBJS)
boot2: $(BOOT2CFILES) $(BOOT2OBJS)

test: cyctest
	./cyctest

foo: foo_tab.o foo.o list.o string.o character.o core.o cyc_helpers.o $(GCLIB)
	$(CC) -g -o $@ $^
	./foo < foo_input

cyctest: hashtable.o string.o set.o list.o string.o buffer.o queue.o\
	arg.o character.o dict.o splay.o rope.o core.o xarray.o fn.o\
	id.o array.o pp.o filename.o bitvec.o ref.o cyctest.o cyc_helpers.o\
	$(GCLIB)
	$(CC) -g -o $@ $^

precore_c.h: precore.h
	$(CY) -tc -toc -pp -noexpandtypedefs $^ | grep -v __sFILE > $@

clean:
	rm -f cyctest cyctest.exe precore_c.h *.o $(CFILES) *.stackdump
	rm -f foo.exe foo foo_tab.h foo_tab.cyc
	rm -f lineno.cyc
	rm -f boot*.c

%.c : %.cyc
	$(CY) -tc -toc -pp $^ > $@

%.cyc: %.cyl
	$(LEX) $<

boot_%.c: %.cyc
	$(BOOTCY) -tc -toc -pp $^ > $@

boot2_%.c: %.cyc
	$(BOOT2CY) -tc -toc -pp $^ > $@

%_tab.cyc: %.y
	$(BISON) -v -d $<

# dependencies
core.h: precore.h
core.o: precore_c.h core.h cyc_include.h
cyc_helpers.o: cyc_include.h precore_c.h

bitvec.o: set.h dict.h list.h core.h cyc_include.h
buffer.o: buffer.h string.h core.h cyc_include.h
character.o: cyc_include.h
cyctest.o: bitvec.h buffer.h core.h character.h dict.h\
	filename.h hashtable.h list.h queue.h ref.h rope.h set.h\
	id.o array.h fn.h string.h xarray.h cyc_include.h
dict.o: dict.h list.h core.h cyc_include.h
filename.o: filename.h string.h core.h cyc_include.h
hashtable.o: hashtable.h string.h list.h core.h cyc_include.h
list.o: list.h core.h cyc_include.h
queue.o: queue.h list.h core.h cyc_include.h
ref.o: ref.h cyc_include.h
set.o: set.h list.h core.h cyc_include.h
splay.o: splay.h list.h core.h cyc_include.h
string.o: string.h list.h core.h cyc_include.h
fn.o: fn.h list.h cyc_include.h
pp.o: pp.h string.h hashtable.h fn.h core.h list.h cyc_include.h
array.o: array.h core.h cyc_include.h
id.o: id.h core.h string.h list.h cyc_include.h
arg.o: string.h core.h list.h cyc_include.h
lexing.o: lexing.h string.h core.h cyc_include.h
position.o: position.h core.h list.h arg.h lineno.h
