
ifdef WINDIR
O         =obj
CC        = cl.exe
CFLAGS    = /Ox
else
O         =o
CC        =gcc
CFLAGS    = -Wall -O2
endif

OCAMLLEX  = ocamllex
OCAMLYACC = ocamlyacc
ifdef NATIVE
OCAMLC    = ocamlopt
OCAMLI    = ocamlopt
OBJSUFF = cmx
LIBSUFF = cmxa
else
OCAMLC	  = ocamlc
OCAMLI    = ocamlc
OBJSUFF = cmo
LIBSUFF = cma
endif

PIP       = ../build/popcorn_slow.exe

LINKFLAGS = str.$(LIBSUFF)

all: profiler.exe prof.$(O) prof.tali

prof.tali: prof.pop
	$(PIP) -c prof.pop
	rm -rf prof.tal prof.pil prof.to prof_i.tali
	mv prof_e.tali prof.tali

prof.$(O): prof.c
	$(CC) -c $(CFLAGS) prof.c

prof: prof.tali prof.$(O)

profiler.exe: profiler.ml
	$(OCAMLC) $(LINKFLAGS) -o $@ $^ 

clean:
	rm -rf *~ profiler.exe *.cmi *.cmo
	rm -rf prof.$(O) prof.to prof.tal prof.pil prof*.tali

######################################################################
# Templates

%.ml: %.mll
	$(OCAMLLEX) $< 

%.ml %.mli: %.mly
	$(OCAMLYACC) $< 

%.cmi: %.mli
	$(OCAMLI) -c $(COMPFLAGS) $<

%.cmo %.cmx: %.ml
	$(OCAMLC) -c $(COMPFLAGS) $<


