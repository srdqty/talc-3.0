USE_CYCLONE=
USE_LOADER=y
DYN_UPDATE=y
NO_STACK_TRACE=y

# default is that LIBFILES=core ... add to that def here, if necessary
LIBFILES = hashtable poploader filename
HOME_DIR = ../../..

include ../../Makefile.inc

out=test.out
ref=sample.out

compile: main

run:
	rm -f $(out)
	./main.exe >> $(out)

check:
	diff $(out) $(ref)

main: main.exe events_patch1.patch events_patch2.patch proc_events_patch1.patch
	$(MAKE) MAKE_LOADABLE=y events_patch1.$(O)
	$(MAKE) MAKE_LOADABLE=y events_patch2.$(O)
	$(MAKE) MAKE_LOADABLE=y proc_events_patch1.$(O)
#	$(MAKE) MAKE_LOADABLE=y newqueue_patch1.$(O)

main.exe: main.to proc_events.to events.to queue.to
	$(TALC) $(TALCFLAGS) -o $@ $^ $(TALCLIBS)

events_patch1.$(O): events_fixed.pop
events_patch2.$(O): events2.pop
proc_events_patch1.$(O): proc_events2.pop proc_events2_patch.pop

main2.exe: main2.to proc_events2.to events2.to
	$(TALC) $(TALCFLAGS) -o $@ $^ $(TALCLIBS)

clean:
	rm -f *~ *.$(O) *.to *.exe *.pil *.tal* *.lst $(out)
