(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Chris Hawblitzel,Frederick Smith    *)
(*     Dan Grossman                                                   *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* Prior to Release 1.7, this was part of Popcompile; now it is opened by 
   that module.
   Here we make the environment an abstract data type.
 *)

type con = Tal.con
type typ = Popsyntax.typ
type id  = Identifier.identifier

(* types *)
type fenv (* too bad this is exposed *)
type env

(* constructors *)
val env_empty       : Poptype.global_env -> env
val mk_fenv         : Popsyntax.fndecl   -> fenv
val env_add_structs : 
    env -> (Popsyntax.type_name * Popcomptypes.struct_info) list -> env
val env_add_unions  : 
    env -> (Popsyntax.type_name * Popcomptypes.union_info) list  -> env
val env_add_abstypes : 
    env -> (Popsyntax.type_name * Popcomptypes.abstype_info) list  -> env
val env_add_tyvars  : env -> Popsyntax.var list -> env
val env_fun_start   : env -> Popsyntax.fndecl -> env

(* selectors *)
val env_convention  : env -> Popsyntax.convention
val env_tyvars      : env -> Popsyntax.var list
val env_names       : env -> id list
val env_regs        : env -> Tal.machine_state
val env_stack1      : env -> con
val env_stack2      : env -> con
val env_cap1        : env -> con (* capability after handler. *)
val env_cap2        : env -> con (*            before         *)
val env_cap         : env -> con (* overall capability.       *)
val env_s1len       : env -> int
val env_s1cons      : env -> int (* number of type constructors in stack 1 *)
val env_break_label : env -> id * int * env
val env_cont_label  : env -> id * int * env
val env_args_on_stk : env -> int
val lookup_label    : env -> Popsyntax.var -> id * id * int * env
val lookup_struct   : env -> Popsyntax.type_name -> Popcomptypes.struct_info
val lookup_union    : env -> Popsyntax.type_name -> Popcomptypes.union_info
val lookup_abstype  : env -> Popsyntax.type_name -> Popcomptypes.abstype_info
val lookup_global   : env -> Popsyntax.var       -> typ
val is_fun_decl     : env -> Popsyntax.var       -> bool
(* returns the constructor for the exnname, and whether the given
   variable is local (otherwise global) *)
type exncon_loc = LocalExncon | GlobalExncon | ExnconDecl
val lookup_exn      : env -> Popsyntax.var       -> exncon_loc * con
val typ2struct_info : env -> typ                 -> Popcomptypes.struct_info
val typ2union_info  : env -> typ                 -> Popcomptypes.union_info
    
val env_local_var_offset : env -> Popsyntax.var -> int
val env_local_depth      : env -> int
val env_next_handler     : env -> int -> int option
                            (* return first handler slot after input *)
val env_handler_con : env -> con
val env_handler_gop : env -> Tal.genop Tal.coerce option
val env_in_try           : env -> bool
val env_top_handler      : env -> int option
(* modifiers *)
val env_set_undefined    : env -> Popsyntax.varset -> env
val env_add_local_var    : env -> Popsyntax.var    -> con -> env
val env_push_con         : env -> con              ->        env
val env_add_reg          : env -> Tal.reg          -> con -> env
val env_set_loop_labels  : env -> id               -> id  -> env
val env_add_label        : env -> Popsyntax.var    -> id  -> id  -> env
val env_try_body         : env -> Tal.con -> Tal.genop Tal.coerce -> env
val env_add_name         : env -> id      -> con -> env
val env_change_name      : env -> id      -> con -> env

(* Cyclone *)
val env_stack_height : env -> int (* returns the stack height *)
val cyc_get_cg       : env -> (int * con) (* int = offset on real stack. *)
val cyc_get_cg_id    : env -> id
val cyc_set_cg       : env -> con -> env
val cyc_push_cg      : env -> con -> env (* push a new cg_region. *)

(* FMS: surplanted by cyc_set_cg:
   val put_vis_cg_type  : env -> con  -> env
*)

val outermost        : env -> bool
val in_frame         : env -> bool (* Dan *)
val get_vis_fenv     : env -> fenv
val put_vis_fenv     : env -> fenv -> env
val flush_vis_fenv   : env -> env
          
        (* First result is for template, second is for hole-filling *)
val env_codegen_body : env -> Popsyntax.fndecl -> env * env (* Dan *)
val env_cut_body     : env -> env                           (* Dan *)
val env_splice_body  : env -> env                           (* Dan *)

(* End Cyclone *)

(* dynamic linking *)
val env_add_global   : env -> Popsyntax.var -> Popsyntax.typ -> env
val env_global       : env -> Poptype.global_env

