// Header file for popdead.h

// Dead-code elimination
// Currently a no-op

#ifndef __POP_DEAD_H
#define __POP_DEAD_H

#include "popil.h"

prefix Popdead {
open Popdead;

// Eliminate dead blocks from a function.  Someday dead code too.
 extern void do_it(Popil::cf_function f);

}

#endif
