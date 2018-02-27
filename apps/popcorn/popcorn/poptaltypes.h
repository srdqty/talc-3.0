#ifndef POPTALTYPES_H
#define POPTALTYPES_H

#include "core.h"
#include "list.h"
#include "id.h"

#include "popil.h"
#include "poptalenv.h"
#include "tal.h"

prefix Poptaltypes {
open   Poptaltypes {

open   Popil     {
open   Poptalenv {
open   Tal       {
open Core;

#define list List::list

extern con tyvar2con(string v);
extern Id::id tid_val(string p);
extern Id::id tid_fun(string p,cf_convention c,<cf_typ>list args); 
extern Id::id tid_tyv(string p);
extern Id::id tid_exn(string p);
extern Id::id tid_typ(string p);
extern Id::id tid_pkt(string p);
extern Id::id tid_tptr(int i);
extern Id::id tid_region(int i);
extern Id::id stack1_var();

extern con tyvars2lam   (<string>list tyvars, con c);
extern con ids2lam      (<Id::id>list tyvars, con c);
extern con tyvars2exists(<string>list tyvars, con c);
extern con ids2exists   (<Id::id>list tyvars, con c);

extern con    stack1_con();
extern con    stack2_con();
extern con    cap1_con();
extern con    cap2_con();
extern Id::id array_size_var();
extern Id::id exnname_arg_var();
extern con    exnname_arg_con();
extern con    callee_save1_con();
extern con    callee_save2_con();
extern con    callee_save3_con();

extern con mem_name_con(string name, <con>list cs);
extern con name_con    (string name, <con>list cs);

extern con app_cons (con c, <con>list cs);

extern con array_packed     (con c);
extern con string_packed    ();
extern con exnname_con      (con c);
extern con exn_packed       ();
extern con handler_and_below();

extern <con>     list fallthru_arg(cg_env, cf_block dest);
extern <coercion>list branch_tapp (cg_env, cf_block dest);

extern <coercion>list typ2constCoerce(cf_typ t);
extern <coercion>list unrefineTyp(cg_env env, int operandName, cf_refinement r);
extern <coercion>list unrefineOperand(cg_env env, cf_operand op);

extern <coercion>list rollOperand  (cg_env env, cf_operand op);
extern <coercion>list unrollOperand(cg_env env, cf_operand op);

extern bool   needs_indirect(cf_typ t);
extern Id::id op2cname(cg_env env, cf_operand op);

// Memoize the tyvars free in the type of each operand.
// We now derive the bound variables from the live variables as each program
// point.
extern void memoize_tyvars(cg_env env, cf_function fn);

// Return the con for the current stack -- used during function prologue
extern con env2stack_con(cg_env env, bool no_handler_slot);

extern machine_state  env2open_code_type(cg_env);
extern con            env2code_type (cg_env env);
extern con            env2code_type_hole(cg_env,int); // This instruction not the first instruction in the block. With hole numbered int.
extern <coercion>list env2call_inst(cg_env env, <cf_typ>list tyvars, 
				    int num_params);

// Next 2 used by the profiler.
extern <coercion>list env2call_inst_special(cg_env env, int num_params,
					    int frame_size);

extern <coercion>list env2call_inst_handler(cg_env env, int num_params, 
					    int extra_cons);

extern <coercion>list env2tailcall_inst(cg_env, <cf_typ>list tyvars,
					 int num_params);

extern con env2template(cg_env env, cf_template t,<code_block>Xarray::xarray blocks);

extern <coercion>list raise_instantiation(cg_env env);

extern con typ2con(cf_typ t);
extern con typ2con_roll(cf_typ t);
extern con var2con(cg_env env, int v);
extern con fun_con(cf_convention,<string>list, <cf_typ>Opt, <cf_typ>list);
extern con struct2ptrtype(cf_structdecl decl);

extern void trans_abstract(tg_env env, string name, cf_absdecl     decl);
extern void trans_struct  (tg_env env, string name, cf_structdecl  decl);
extern void trans_union   (tg_env env, string name, cf_uniondecl   decl);
extern void trans_abstype (tg_env env, string name, cf_abstypedecl decl);
extern void trans_exn     (dg_env env, Id::id name, cf_exndecl     decl);

/* extern void trans_builtin_exn(dg_env env); */

extern *(Id::id,con) get_std_abbrevs()[];

#undef list
}}}}}
#endif
