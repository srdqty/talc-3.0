#ifndef POPPROFILE_H
#define POPPROFILE_H

#include "tal.h"
#include "id.h"
#include "popil.h"
#include "poptalenv.h"

prefix Popprofile {
open Popprofile;
open Poptalenv;
open Popil;

extern void add_generic_imports(dg_env d_ev, tg_env t_ev);
extern void init_fun(cf_function fn, dg_env d_ev);
extern void enter_fun(cg_env ev);
extern void leave_fun(cg_env ev);
extern void enter_handler(cg_env ev);

}

#endif
