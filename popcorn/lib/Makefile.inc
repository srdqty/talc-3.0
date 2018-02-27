# flags understood:
# DYN_UPDATE : if we would like to notice updates in the popcorn libraries
# NO_STACK_TRACE : shuts off stack tracing

ifdef WINDIR
O = obj
A = lib
CFLAGS = /nologo
CC= cl 
else
O = o
A = a
CFLAGS = -Wall -O2 -ggdb
CC= gcc
DEFINES     = -D SYS_UNIX
endif

POPCORN        = popcorn.exe
TALC           = talc.exe
TALCFLAGS      = --verify-link --verify-program
EXTRAFLAGS     = 
COMPFLAGS      =  $(EXTRAFLAGS)
POPFLAGS       = --export-syms $(DEFINES) 

# for debugging
ifndef NO_STACK_TRACE
POPFLAGS      += --stack-trace
endif

ifdef DYN_UPDATE
POPFLAGS += --notice-updates --export-locals
CFLAGS += -DNOTICE_UPDATES
endif

ifdef COUNT_REFS
POPFLAGS += --count-references
endif

#floats are only understood by the stack-based compiler.
ifndef POP_IN_POP
POPFLAGS += -D FLOATS=X
endif

# implicit rules

%.to %.$(O): %.tal
	$(TALC) $(TALCFLAGS) -c $<

%.tal %.$(O) %.to: %.pop
	$(POPCORN) $(COMPFLAGS) $(POPFLAGS) -c $<

%.tali: %.h
	$(POPCORN) $(COMPFLAGS) $(POPFLAGS) -c $<

%.obj: %.c
	$(CC) $(CFLAGS) -Z7 /MT /Fo$@ /c $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

%.o: %.S
	$(CC) $(CFLAGS) -c -o $@ $<
