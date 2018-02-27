#ifndef POPTYPEENV_H
#define POPTYPEENV_H

#include "core.h"
#include "list.h"
#include "set.h"
#include "dict.h"
#include "popsyntax.h"
#include "poperr.h"
#include "gcdfec.h"
#define location Gcdfec::seg

prefix Poptypeenv {
open Poptypeenv {

open Core {
open Popsyntax {
open List {

// post a type error
extern void terr(location loc,string s);
extern void terr2(location loc,string s1,string s2);
extern void terr3(location loc,string s1,string s2,string s3);
extern void terr4(location loc,string s1,string s2,string s3,string s4);
extern void terr5(location loc,string s1,string s2,string s3,string s4,
		  string s5);
// post a type error and return a "dummy" synthesis
extern f_exp_synth exp_err(location loc,string msg);
extern f_exp_synth exp_err2(location loc,string s1,string s2);
extern f_exp_synth exp_err3(location loc,string s1,string s2,string s3);
extern f_exp_synth exp_err4(location loc,string s1,string s2,string s3,
			    string s4);
extern f_exp_synth exp_err5(location loc,string s1,string s2,string s3,
			    string s4, string s5);
extern f_exp_synth exp_err6(location loc,string s1,string s2,string s3,
			    string s4, string s5,string s6);
// used when a supposedly impossible situation arises
extern a impossible<a>(string s);
/////////////////////////////////////////////////////////////////////////////
// 2. Utilities
/////////////////////////////////////////////////////////////////////////////
// association lists where the keys are strings
extern a string_list_assoc<a>(string x,<*(string,a)>list lis);
// returns null if p is not a prefix of n, otherwise returns the rest of n.
extern field_name get_switch_arm_field(switch_arm s);
// returns the type of a function declaration
extern typ fd_type(fndecl fd); 
/////////////////////////////////////////////////////////////////////////////
// 3. Global Environments
/////////////////////////////////////////////////////////////////////////////
extern union type_use { void Unused, Mentioned, Destructed; }
extern struct global_env { 
  // struct definitions in scope
  <type_name,*(structdecl,type_use)> Dict::dict structs; 
  // union definitions in scope
  <type_name,*(uniondecl,type_use)> Dict::dict unions;   
  // abstype definitions in scope
  <type_name,*(absdecl,type_use)> Dict::dict abstypes;
  // external abstract types in scope -- the var list is the type parameters 
  // and the bool indicates whether the type is possibly null
  // obviously the type_use can't be destructed
  <type_name,*(<var>list,bool,type_use)> Dict::dict abstracts;
  // Dictionary of union field names in scope.  Used to automatically
  // expand new when it is not ambiguous.
  // Invalid field names are not in the dictionary.  Ambiguous field names
  // are mapped to null. 
  <field_name, <uniondecl>Opt> Dict::dict union_fs;
  // global (value) variables in scope -- The bool is initially false
  // for external globals but set to true if the variable is ever 
  // looked up indicating a use.  The bool is true for internal globals.
  // Thus, the external globals can be trimmed by looking at the bools.
  <var,*(typ,bool)> Dict::dict globals;
  // global exceptions in scope
  // the bool indicates use
  <var,*(typ,scope,bool)> Dict::dict exceptions;
  // type names that have been opened that are in scope -- maps to the
  // fully qualified name (i.e., with the prefix)
  <type_name,type_name> Dict::dict open_typs;
  // value names that have been opened that are in scope -- maps to the
  // fully qualified name (i.e., with the prefix)
  <var,var> Dict::dict open_vals;
}
extern global_env initial_global_env(<top_decl>list ds);
extern void add_global(global_env g, var x, typ t);
/////////////////////////////////////////////////////////////////////////////
// 4. Opening a prefix in a global environment
/////////////////////////////////////////////////////////////////////////////
// Given a global environment g, and a prefix p, open p by generating
// a new environment with the opened identifiers arranged appropriately.
// Note that we must copy the environment because the open may have a
// local scope.
extern global_env open_prefix(global_env g,string p);

/////////////////////////////////////////////////////////////////////////////
// 5. Environment and Synthesis local to a function 
/////////////////////////////////////////////////////////////////////////////
extern f_env; 
extern union c_env {
  f_env          Outermost;
  *(f_env,c_env) Frame;
  *(f_env,c_env) Hidden;
}
extern union f_exp_unassigned {
  <var>Set::set                  Always;  // unassigned after expression
  *(<var>Set::set,<var>Set::set) Boolean; // unassigned when true, when false
}
extern struct f_exp_synth {
  typ              typ;
  bool             does_raise;
  f_exp_unassigned unassigned;
  <var>list        assigned_to; // a list b/c we expect it to be very small
}
extern struct f_stmt_synth {
  bool          does_jmp;
  <var>Set::set unassigned;
}

extern f_env fndecl2f_env(fndecl fd);
extern f_env bogus_f_env();
extern c_env mk_env(f_env f);

extern bool inloop(c_env c);
extern bool label_bound(c_env c,var x);
extern <var>list tyvars(c_env cenv);
extern typ retType(c_env cenv);
extern <var>Set::set unassigned(c_env cenv);

extern <var>Set::set un_after_exp(f_exp_synth exp_synth);
extern *(<var>Set::set,<var>Set::set) un_after_bool(f_exp_synth exp_synth);


extern c_env set_inloop(c_env c);
extern c_env set_outloop(c_env c);
extern c_env add_label(c_env c,var x);
extern c_env add_tyvars(c_env c,location loc,<var>list x);
extern c_env add_var_init(c_env c,var x,typ t);
extern c_env add_var_uninit(c_env c,var x,typ t);
extern c_env set_unassigned(c_env c, <var>Set::set un);
extern typ lookup(c_env cenv,var id);

extern <<var>Set::set>Opt mt_varset_g;
extern <f_stmt_synth>Opt  jump_synth_g;

}}}}}
#endif
