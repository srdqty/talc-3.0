#ifndef POPILANALYZE_H
#define POPILANALYZE_H

#include "core.h"
#include "set.h"
#include "popil.h"

prefix Popilanalyze {
open   Popilanalyze {
  
extern FILE ilf;
 
extern void eliminate_nullchecks (Popil::cf_function);
extern void eliminate_dead_refine(Popil::cf_function, <int>Set::set[][]);
extern void smash_blocks         (Popil::cf_function);
extern void webify               (Popil::cf_function);
extern *(int[], int) compute_webs(Popil::cf_function);
extern void dead_code            (Popil::cf_function);

extern <int>Set::set compute_all_liveness(Popil::cf_function)[][];
extern unsigned int  spill_costs         (Popil::cf_function)[];

extern void print_liveness(FILE f, <int>Set::set all_live[][]);
extern void copy_propagate(Popil::cf_function);

 open Popil {
// Expose useful helper functions
extern struct <a>use_def_env {
  a env;
  cf_operand use(a,cf_operand);
  cf_operand def(a,cf_operand);
  bool def_first;
}

extern void instruction_use_def<a>(<a>use_def_env ude, cf_instruction inst);
extern void transfer_use_def<a>(<a>use_def_env ude, cf_transfer trans);

 }

}}

#endif
