#ifndef POPILTAL_H
#define POPILTAL_H

#include "xarray.h"
#include "poptalenv.h"
#include "tal.h"
#include "popil.h"
#include "id.h"

prefix Popiltal {
open   Popiltal {

extern void trans_file(string basename, string modname, Popil::cf_file file);
extern void trans_function(Poptalenv::cg_env);
extern void trans_extern(Poptalenv::dg_env env, Id::id v, Popil::cf_typ t);

}}
#endif
