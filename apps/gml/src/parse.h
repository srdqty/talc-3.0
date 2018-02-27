#ifndef PARSE_H
#define PARSE_H

/* don't use c++-style comments here b/c it gets included in a popocamllex file
 * and I haven't hacked in support for that yet
*/ 


/* used in YYSTYPE */
#include "list.h"
#include "core.h"
#include "gmlsyntax.h"

#include "lexing.h"

extern yyltype yylloc;

prefix Parse {
open   Lexing   {
open   Core     {
  extern < < <FILE>function_lexbuf_state>lexbuf>Opt lbuf;
  extern *(<string>list,<Gmlsyntax::syn>list) parse_program(FILE);
  extern exception ParseError;
}}}

open List      {
open Core      {
open Gmlsyntax {
open Parse  {
#include "parse_tab.h" /* from bison -- must be inside the opens */
}}}}

#endif
