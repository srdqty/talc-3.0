#ifndef POPTYPESTMT_H
#define POPTYPESTMT_H
// Poptypestmt:  type-checking for statements

#include "popsyntax.h"
#include "poptypeenv.h"

prefix Poptypestmt {
open Poptypestmt {

open Popsyntax {
open Poptypeenv {
// returns true when the statement does not "fall off".  The flag
// in_splice indicates whether the statement occurs within a splice
// or not.
extern bool does_return(bool in_splice,stmt statement);
// type-checks a statement.
extern f_stmt_synth tcStmt(global_env g,c_env env,stmt statement);
}}}}
#endif
