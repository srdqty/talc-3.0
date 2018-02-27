# Hacked to output cyclone, following Daniel's changes

#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

# $Id: Makefile,v 1.1 2000/09/13 04:31:40 tjim Exp $

# The lexer generator

CAMLC=ocamlc
COMPFLAGS=
LINKFLAGS=
CAMLYACC=ocamlyacc
YACCFLAGS=
CAMLLEX=ocamllex
CAMLDEP=ocamldep
DEPFLAGS=

OBJS=parser.cmo lexer.cmo lexgen.cmo compact.cmo output.cmo main.cmo

all: cycocamllex

cycocamllex: $(OBJS)
	$(CAMLC) $(LINKFLAGS) -o cycocamllex.exe $(OBJS)

clean::
	rm -f cycocamllex.exe
	rm -f *.cmo *.cmi

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
