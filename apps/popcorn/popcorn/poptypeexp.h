#ifndef POPTYPEEXP_H
#define POPTYPEEXP_H
// Poptypeexp:  type-checking for expressions

#include "popsyntax.h"
#include "poptypeenv.h"

prefix Poptypeexp {
open Poptypeexp {

open Popsyntax {
open Poptypeenv {
// check that an expression used for initialization of globals is
// "constant" (i.e. can be computed at compile time.)
extern void check_constexp(exp e);
// type-check an expression and return its type
extern f_exp_synth tcExp(global_env g,c_env env,exp exp);
}}}}
#endif
