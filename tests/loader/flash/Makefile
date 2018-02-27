USE_CYCLONE=
USE_LOADER=y
DYN_UPDATE=y

HOME_DIR = ../../..
LIBFILES += hashtable buffer array filename
ifdef USE_LOADER
LIBFILES += poploader
endif

include ../../Makefile.inc

SRCFILES= accept.pop cold.pop nameconvert.pop match.pop data.pop \
  common.pop libhttpd.pop loop.pop timer.pop readreq.pop c_string.pop \
  tdate_parse.pop scanf.pop name.pop file.pop maint.pop main.pop 
OBJFILES = $(SRCFILES:.pop=.$(O))
TOFILES = $(SRCFILES:.pop=.to)

out=test.out
ref=sample.out
exes = flash.exe
# maint_main.exe
all: 
	cat README

exes: $(exes)

flash.exe: $(TOFILES)
	$(TALC) $(TALCFLAGS) $^ -o $@ $(TALCLIBS) -T unixlib.$(A)

maint_main.exe: maint_main.to
	$(TALC) $(TALCFLAGS) $^ -o $@ $(TALCLIBS) -T unixlib.$(A)

clean:
	rm -f *.$(O) *.tal* *.to *~ $(exes)
	cd ver2; $(MAKE) clean

