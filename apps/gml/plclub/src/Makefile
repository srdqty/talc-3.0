
OCAMLC=ocamlc -g
OCAMLOPT=ocamlopt -inline 10 -unsafe #-p -cclib -ldl
OCAMLDEP=ocamldep

ifdef PROFILE
OCAMLOPT+= -p
endif

BYTELINKFLAGS=-custom bigarray.cma unix.cma
ifeq ($(shell hostname),saul.cis.upenn.edu)
OPTLINKFLAGS=bigarray.cmxa unix.cmxa
else
OPTLINKFLAGS=bigarray.cmxa unix.cmxa -ccopt -static
endif

## FMS: additions.
ifdef WINDIR
O=obj
EXE=.exe
else
O=o
EXE=
endif
#####

OBJS=ppm.cmo math.cmo program.cmo matrix.cmo render.cmo \
     parser.cmo lexer.cmo dump.cmo eval.cmo compile.cmo main.cmo
COMP=ppm.cmx compare.cmx
DIFF=ppm.cmx diff.cmx

all: ../support/raytrace$(EXE) compare$(EXE) difference$(EXE)

# these object files are necessary for the GML compiler
../support/raytrace$(EXE): raytrace$(EXE) ppm.cmx math.cmx program.cmx matrix.cmx render.cmx
	cp raytrace$(EXE) \
	ppm.cmx math.cmx program.cmx matrix.cmx render.cmx \
	ppm.cmi program.cmi matrix.cmi render.cmi \
	ppm.$(O) math.$(O) program.$(O) matrix.$(O) render.$(O) \
	../support/

raytrace$(EXE): $(OBJS:cmo=cmx)
	$(OCAMLOPT) -o $@ $(OPTLINKFLAGS) $(LINKFLAGS) $(OBJS:cmo=cmx)

compare$(EXE): $(COMP)
	$(OCAMLOPT) -o $@ $(COMP)

difference$(EXE): $(DIFF)
	$(OCAMLOPT) -o $@ $(DIFF)

lexer.ml: lexer.mll
	ocamllex $<
parser.mli parser.ml: parser.mly
	ocamlyacc $<
clean::
	rm -f lexer.ml parser.mli parser.ml compare$(EXE) difference$(EXE) raytrace$(EXE)

#####

.SUFFIXES: .cmo .cmi .cmx .ml .mli .idl .$(O) .c

.ml.cmx:
	$(OCAMLOPT) $(COMPFLAGS) -c $<

.ml.cmo:
	$(OCAMLC) $(BYTECOMPFLAGS) $(COMPFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(COMPFLAGS) -c $<

.c.$(O):
	$(OCAMLC) $(COMPFLAGS) -ccopt "$(CFLAGS)" -c $<

depend:
	$(OCAMLDEP) $(DEPFLAGS) *.ml *.mli > .depend

clean::
	rm -f *.cmo *.cmi *.cmx *.$(O)

-include .depend
