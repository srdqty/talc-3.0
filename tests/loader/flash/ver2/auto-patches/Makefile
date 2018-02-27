USE_CYCLONE=
USE_LOADER=y
DYN_UPDATE=y
MAKE_LOADABLE=y

HOME_DIR = ../../../../..
LIBFILES += hashtable buffer array
ifdef USE_LOADER
LIBFILES += poploader
endif

SOURCES=$(wildcard *.patch)
OBJECTFILES=$(SOURCES:.patch=.o)

include ../../../../Makefile.inc

compile: $(OBJECTFILES)

#	for file in *.patch; do \
#	$(MAKE) MAKE_LOADABLE=y $${file%.patch}.$(O); \
#	done

update_cmd:
	echo -n "update "
	for file in *.o; do echo -n "${file%.o} "; done
	echo

clean:
	rm -f *.$(O) *.tal* *.to *~ $(exes) convert.h

clean_patch:
	rm -f *_patch.pop *.patch TYPENAME_MAP
