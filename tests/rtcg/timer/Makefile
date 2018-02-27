POP_IN_POP=X
USE_LOADER=
USE_RTCG=

-include Makefile.local

HOME_DIR = ../../..

include ../../Makefile.inc
 
## Inlining not yet supported for RTCG
ifdef POP_IN_POP 
POPFLAGS += -no-inline
endif

ifdef POP_IN_POP
timer_new.$(O): timer.$(O) timer_interface.$(O)
	cp timer.$(O) timer_new.$(O)
	cp timer_interface_e.tali timer_new.tali
	rm timer_interface.$(O)
else
timer_old.$(O): timer.$(O) timer_interface.$(O)
	cp timer.$(O) timer_old.$(O)
	cp timer_interface_e.tali timer_old.tali
	rm timer_interface.$(O)
endif

timer.obj: timer.c
	cl -c timer.c
clean:
	rm -f *~ *.tal *.to *.$(O) *.tali *.exe
	rm -f *.pil





