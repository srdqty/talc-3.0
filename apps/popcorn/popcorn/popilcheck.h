#ifndef POPILCHECK_H
#define POPILCHECK_H

#include "core.h"
#include "popil.h"

prefix Popilcheck {
open Popilcheck;

// Check that the code for function f doesn't violate invariants.
// Write any violations to the log file.
 extern void check_fun(FILE log, Popil::cf_function f);
}
#endif
