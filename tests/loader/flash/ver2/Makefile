USE_CYCLONE=
USE_LOADER=y
DYN_UPDATE=y

HOME_DIR = ../../../..
LIBFILES += hashtable buffer array filename
ifdef USE_LOADER
LIBFILES += poploader
endif

include ../../../Makefile.inc

SRCFILES= accept.pop cold.pop nameconvert.pop match.pop data.pop \
  common.pop libhttpd.pop loop.pop timer.pop readreq.pop c_string.pop \
  tdate_parse.pop scanf.pop name.pop file.pop maint.pop main.pop 
OBJFILES = $(SRCFILES:.pop=.$(O))
TOFILES = $(SRCFILES:.pop=.to)

out=test.out
ref=sample.out
exes=flash.exe
# maint_main.exe
all: $(exes)

flash.exe: $(TOFILES)
	$(TALC) $(TALCFLAGS) $^ -o $@ $(TALCLIBS) -T unixlib.$(A)

maint_main.exe: maint_main.to
	$(TALC) $(TALCFLAGS) $^ -o $@ $(TALCLIBS) -T unixlib.$(A)

patch:
	for file in $(SRCFILES); do \
	popgenpatch.exe --overwrite-patch --outdir auto-patches prev/$$file $$file; \
	done
#	done > auto-patches/notes 2>&1

test_patch:
	for file in $(SRCFILES); do \
	popgenpatch.exe test/$$file $$file; \
	done

clean:
	rm -f *.$(O) *.tal* *.to *~ $(exes)
	cd auto-patches; $(MAKE) clean clean_patch
# FMS: No patches directory in repository. Cause Makefile to loop.
#	cd patches; $(MAKE) clean

clean_patch:
	rm -f *_patch.pop *.patch TYPENAME_MAP
