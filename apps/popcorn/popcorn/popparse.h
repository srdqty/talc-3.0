#ifndef POPPARSE_H
#define POPPARSE_H

/* don't use c++-style comments here b/c it gets included in a popocamllex file
 * and I haven't hacked in support for that yet
*/ 


/* used in YYSTYPE */
#include "list.h"
#include "core.h"
#include "popsyntax.h"

#include "lexing.h"

extern yyltype yylloc; /* should be put in popparse_tab.h automatically */

prefix Popparse {
open   Lexing   {
open   Core     {
  extern type_modifier;
  extern exp_or_fun;
  extern switch_clause;
  extern < < <FILE>function_lexbuf_state>lexbuf>Opt lbuf;
  extern <Popsyntax::top_decl>List::list parse_program(FILE);
  extern exception ParseError;
}}}

open List      {
open Core      {
open Popsyntax {
open Popparse  {
#include "popparse_tab.h" /* from bison -- must be inside the opens */
}}}}

#endif
