#ifndef POPCFG_H
#define POPCFG_H

#include "core.h"
#include "set.h"
#include "popil.h"

prefix Popcfg {
open Popcfg;

// Compute from scratch the CFG for function f. 
// Assumes the cfg for the function generating f (if any) is correct.
 extern void cfg_fun(Popil::cf_function f);

 // Computes cfg's for all the functions in the file, in order from parent to
 // child.
 extern void cfg_file(Popil::cf_file f);
}

#endif
