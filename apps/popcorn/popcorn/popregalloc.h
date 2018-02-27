#ifndef POPREGALLOC_H
#define POPREGALLOC_H

#include "popil.h"
#include "poptalenv.h"

prefix Popregalloc {
open   Popregalloc {

extern Poptalenv::reg_assignment register_allocate(Popil::cf_function);

}}
#endif
