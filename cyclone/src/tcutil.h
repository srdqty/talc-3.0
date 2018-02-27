#ifndef TCUTIL_H
#define TCUTIL_H

//#include "core.h"
#include "list.h"
#include "set.h"
#include "absyn.h"
#include "gcdfec.h"
#include "tcenv.h"


prefix Tcutil {
open Tcutil {

//open Core;
open List;
open Absyn;
open Gcdfec;
open Set;
open Tcenv;

extern exception TypeErr;
extern a impos<a>(string);
extern void terr(seg, string);
extern void warn(seg, string);
extern void err_noloc(string);

// returns the type of a function declaration
extern typ fd_type(fndecl fd); 

extern typ compress(typ t);
extern <a>conref compress_conref<a>(<a>conref x);
extern void unchecked_cast(tenv, exp, typ);
extern bool coerce_arg(tenv, exp, typ); 
extern bool coerce_assign(tenv, exp, typ);
extern bool coerce_bool_t(tenv, exp);
extern bool coerce_list(tenv, typ, <exp>list);
extern bool coerce_uint_t(tenv, exp);
extern bool coerce_use(tenv, exp, typ);
extern bool coerceable(typ); // true if numeric or character
// true when expressions of type t1 can be cast to t2
extern bool castable(tenv,seg,typ,typ);

extern bool integral_promote(tenv, exp);
extern bool arithmetic_promote(tenv, exp);
extern bool comparison_promote(tenv, exp);
extern typ max_arithmetic_type(typ, typ);

/*
extern void check_valid_lhs(tenv, exp);

*/

extern bool unify(typ, typ);
extern exception Unify; 
// Raises Unify 
extern void unify_it(typ, typ);
extern typ substitute(<*(var,typ)>list, typ);

extern typ fndecl2typ(fndecl);

extern exp default_initializer(tenv,typ,seg);

extern *(var,typ) make_inst_var(var);

// Check that the type is valid assuming that the free type variables are
// drawn from the given list.  As a side-effect, expand any typedefs.
extern void check_valid_type(seg,tenv,<tvar>list,typ);
// Special cased for function declarations
extern void check_fndecl_valid_type(seg,tenv,fndecl);

extern <var>set repeated_vars(<var>list);
extern void check_unique_tvars(seg,<tvar>list);

extern exception MissingField;
extern *(*(field_name,tqual,typ),<*(field_name,tqual,typ)>list)
  remove_field(field_name,<*(field_name,tqual,typ)>list);
extern bool equal_tqual(tqual tq1, tqual tq2);
}}
#endif
