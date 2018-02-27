#ifndef TCPAT_H
#define TCPAT_H

#include "absyn.h"
#include "tcenv.h"

prefix Tcpat {
open Tcpat {

open Absyn;
open Tcenv;

extern *(<tvar>list, <*(var,tqual,typ)>list) tcPat(tenv,pat);
extern void check_switch_exhaustive(seg,<switch_clause>list);
extern void check_let_pat_exhaustive(seg,pat p);
extern void check_catch_overlap(seg,<switch_clause>list);

}}

#endif
