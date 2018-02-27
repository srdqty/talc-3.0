#ifndef TC_H
#define TC_H

#include "list.h"
#include "absyn.h"
#include "tcenv.h"

prefix Tc {
  open Tc {

open List;
open Absyn;
open Tcenv;

extern tenv tc(tenv te, <decl>list);

}}
#endif
