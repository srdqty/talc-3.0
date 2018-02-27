#ifndef TCSTMT_H
#define TCSTMT_H

#include "absyn.h"
#include "tcenv.h"

prefix Tcstmt {
open Tcstmt {

open Absyn;
open Tcenv;

extern synth tcStmt(tenv,stmt);

}}

#endif
