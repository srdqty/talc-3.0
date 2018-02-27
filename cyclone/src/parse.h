#ifndef PARSE_H
#define PARSE_H

#include "list.h"
#include "core.h"
#include "lexing.h"
#include "absyn.h"

extern yyltype yylloc; /* should be put in parse_tab.h automatically */

prefix Parse {
open Lexing {
open Core {

  extern < < <FILE>function_lexbuf_state>lexbuf>Opt lbuf;
  extern <Absyn::decl>List::list parse_file(FILE);
  extern exception ParseError;
  extern struct_or_union;
  extern type_specifier;
  extern storage_class;
  extern decl_spec;
  extern declarator;
  extern abstractdeclarator;
  extern blockitem;
}}}

open Core { open List { open Absyn { open Gcdfec { open Parse {
#include "parse_tab.h"
}}}}}
#endif
