#ifndef POPTYPE_H
#define POPTYPE_H
// Poptype:  entry point for the type-checker
#include "list.h"
#include "popsyntax.h"
#include "poptypeenv.h"

prefix Poptype {
open Poptype {

open Popsyntax {
open Poptypeenv {

extern *(global_env,<top_decl>List::list) type_check(<top_decl>List::list ds);
// needed in poptypeexp
extern void tcFunDecl(global_env g,c_env env,location loc,fndecl fd);
// a list of "inner" functions that are later added to the global top-decls 
extern <top_decl>List::list inner_functions;

}}}}
#endif
