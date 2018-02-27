#ifndef POPILPRINT_H
#define POPILPRINT_H

#include "popil.h"
#include "core.h"

prefix Popilprint {
open   Popilprint {

  extern void prn_file(Popil::cf_file);
  extern void prn_id_fun(Id::id,Popil::cf_function fn);
  extern void prn_fun(Popil::cf_function fn);
  extern void prn_block(Popil::cf_block);
  extern void prn_edge_stats(Popil::cf_function);
  extern void set_output(FILE);
  extern void prn_local(int);
  extern void PRN0(string);
  // add much more here

  extern bool suppress_output;

}}

#endif
