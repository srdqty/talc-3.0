# The lexer generator hacked to output popcorn
# This file is only trivially changed from ocamllex.

CAMLC=ocamlc

COMPFLAGS=
LINKFLAGS=
CAMLYACC=ocamlyacc
YACCFLAGS=
CAMLLEX=ocamllex
CAMLDEP=ocamldep
DEPFLAGS=

OBJS=parser.cmo lexer.cmo lexgen.cmo compact.cmo output.cmo main.cmo

all: popocamllex

popocamllex: $(OBJS)
	$(CAMLC) $(LINKFLAGS) -o popocamllex.exe $(OBJS)

clean::
	rm -f popocamllex.exe
	rm -f *.cmo *.cmi *~

parser.ml parser.mli: parser.mly
	$(CAMLYACC) $(YACCFLAGS) parser.mly

clean::
	rm -f parser.ml parser.mli

beforedepend:: parser.ml parser.mli

lexer.ml: lexer.mll
	$(CAMLLEX) lexer.mll

clean::
	rm -f lexer.ml

beforedepend:: lexer.ml

.SUFFIXES:
.SUFFIXES: .ml .cmo .mli .cmi

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

depend: beforedepend
	$(CAMLDEP) *.mli *.ml > .depend

include .depend
