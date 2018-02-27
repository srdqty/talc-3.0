POPCORN=popcorn
TALC=talc
CC=cl /nologo

COMPFLAGS=--TALC

calc: calc.exe

CALCFILES=$(TALFILES) calclib.obj
TALFILES=lex.tal parse.tal eval.tal calc.tal
OBJFILES=lex.obj parse.obj eval.obj calc.obj calclib.obj

calc.exe: $(CALCFILES)
	$(TALC) --verify-link --TALC $(TALFILES) -S calclib
	$(POPCORN) $(COMPFLAGS) -o $@ $(OBJFILES)

%.tal: %.pop
	$(POPCORN) --generate-tal-only $(COMPFLAGS) $<

%.obj: %.c
	$(CC) /Fo$@ /c $<

clean:
	rm -f *~ *.obj *.tal *.tali *.lst calc.exe
