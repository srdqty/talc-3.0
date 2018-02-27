POPCORN=popcorn
TALC=talc
CC=cl /nologo

COMPFLAGS=--TALC

plan: plan.exe

PLANFILES=$(TALFILES) planlib.obj
TALFILES=lex.tal parse.tal typecheck.tal plan.tal
OBJFILES=lex.obj parse.obj typecheck.obj plan.obj planlib.obj

plan.exe: $(PLANFILES)
	$(TALC) --verify-link --TALC $(TALFILES) -S planlib
	$(POPCORN) $(COMPFLAGS) -o $@ $(OBJFILES)

%.tal: %.pop
	$(POPCORN) --generate-tal-only $(COMPFLAGS) -c $<

%.obj: %.c
	$(CC) /Fo$@ /c $<

clean:
	rm -f *~ *.obj *.tal *.tali *.lst plan.exe
