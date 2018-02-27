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

open Popcomptypes
module P  = Popsyntax
module T  = Tal
module PT = Poptype

(***************** Utilities *****************)
let deSome x = match x with Some v -> v | None -> impos "deSome"

(***************** Type Definitions **********)

type type_info =
    { unions:  (P.type_name, union_info)  Dict.dict;
      structs: (P.type_name, struct_info) Dict.dict;
      abstypes: (P.type_name, abstype_info) Dict.dict;
    } 

(* The global_env tracks definitions of types and types of global variables.
 * The local_env tracks the types and stack positions 
 * of local variables.  
 * 
 * The break label, when Some(lab,n), is the label that a break should
 * jump to.  However, before jumping, the break should pop s - n local 
 * variables from the stack, where s is the current size of the stack.
 * Similarly, the continue label, when Some(lab,n) is the label that a 
 * continue should jump to.  
 *
 * sptr_offsets tracks the location of the stack pointers pushed in
 * a try.  A continue or break must restore the appropriate one into EBP.
 * Similarly a return must restore the outermost one. (last one)
 *
 * JGM: we need to track the type variables (and names) that were actually
 * in scope at each label so that when we jump to the label, we generate
 * the right type instantiations.  The easiest way to do this as far as
 * I can tell is to simply record the current environment with the label.
 * 
 * Dave: Doubles require that we track the actual size of arguments, not
 * just the number of arguments:
 * f_args_on_stack represents the size of the args on the stack. The
 *   size is the number of 32-bit words.
 * f_s1len represents the size of the s1 part in number of 32-bit words.
 * f_break_label, f_continue_label, f_labels indicate size of stack to chop in 
 *   32-bit words.
 *)
type shadow_info = Unshadowed | ShadowedUndefined | ShadowedDefined
type fenv = (* Environment local to a function *)
    { f_convention:     P.convention;
      f_tyvars:         P.var list;
      f_stack_bottom:   T.con;
      f_args_on_stack:  int; (* size in number of 32-bit words *)

      f_names:          id list; (* names of malloc'd things *)
      f_local_vars:     (P.var * T.con * shadow_info) list;
      f_handler_con:    T.con;
      f_handler_gop:    T.genop T.coerce option;

      f_cap1_type:      T.con; 
      f_cap2_type:      T.con; (* Capability available before handler. *)
      f_s1len:          int * int; (* size in number of 32-bit words,number of s1 cons *)
      f_break_label:    (id*int*env) option;
      f_continue_label: (id*int*env) option;
      f_regs:           T.machine_state;
      f_labels:         (P.var,id * id * int * env) Dict.dict;
(* Cyclone : FMS *)
      f_cg_stack:       (P.var*id) list; (* "stack" of cg_regions *)
(* Cyclone Env *)
    } 
and cenv =
    Hidden    of fenv
  | Frame     of fenv
  | Outermost of fenv
(* End Cyclone *)
and env = 
    { global_env:     PT.global_env;
      f_env:          fenv;
      sptr_offsets:   int list;
      type_info:      type_info;
(* Cyclone *) cenvs:  cenv list; (* End Cyclone *)
      undefined:      P.varset; (*grabbed from AST and squirreled in here*)
    } 

(*************** Constructors **************)
let env_empty global_env =
  { global_env = global_env;
    f_env = { f_convention     = P.default_convention;
	      f_tyvars         = [];
	      f_stack_bottom   = T.cempty;
	      f_args_on_stack  = 0;
	      f_names          = [];
	      f_local_vars     = [];
	      f_handler_con    = T.cempty;
	      f_handler_gop    = None;
	      f_cap1_type      = T.cempty_cap;
	      f_cap2_type      = T.cempty_cap;
	      f_s1len          = (0,0);
	      f_break_label    = None;
	      f_continue_label = None;
	      f_regs           = T.ms_empty;
	      f_labels         = Dict.empty compare;
(* Cyclone : FMS *)
	      f_cg_stack       = []
(* Cyclone *)
	    };
    sptr_offsets = [];
    type_info    = { structs=Dict.empty compare; unions=Dict.empty compare;
		     abstypes=Dict.empty compare};
(* Cyclone *) cenvs = []; (* End Cyclone *)
    undefined    = P.mt_varset
  } 

let mk_fenv fd =
  let convention = fd.P.fn_convention in
  let tyargs    = fd.P.fn_tyvars     in
  let ret_typ   = fd.P.fn_ret_type   in
  let args      = fd.P.fn_args       in
  let params    = List.map snd args  in
  let param_sz  = P.sumtyps params   in
  let param_num = List.length params in
  let (_,stack1_type,stack2_type) = fun_con convention tyargs ret_typ params in
  let local_env = 
    List.map (fun (x,t) -> (x, typ2con t, Unshadowed)) args in
  { f_convention     = convention;
    f_tyvars         = tyargs;
    f_stack_bottom   = stack1_type;
    f_args_on_stack  = param_sz + 1;

    f_names          = [];
    f_local_vars     = ("*BOGUS*",stack1_type,Unshadowed)::local_env;
    f_handler_con    = T.csptr (exn_stack_con stack2_c cap2_c);
    f_handler_gop    = None;

    f_cap1_type      = cap1_c;
    f_cap2_type      = cap2_c;
    f_s1len          = (param_sz + 1, param_num + 1); (* 1 for retn addr *)
    f_break_label    = None;
    f_continue_label = None;
    f_labels         = Dict.empty compare;
    f_regs           = T.ms_empty;
    (* Cyclone : FMS *)
    f_cg_stack       = [];
    (* Cyclone End *)
  }
let env_add_structs env s_info_list =
  { env with type_info =
      { env.type_info with 
          structs = Dict.inserts env.type_info.structs s_info_list
      } 
  } 
let env_add_unions env u_info_list =
  { env with type_info =
      { env.type_info with 
          unions = Dict.inserts env.type_info.unions u_info_list
      } 
  } 
let env_add_abstypes env a_info_list = 
  { env with type_info =
      { env.type_info with 
          abstypes = Dict.inserts env.type_info.abstypes a_info_list
      } 
  } 

let env_fun_start env fd =
  { global_env   = env.global_env;
    f_env        = mk_fenv fd;
    sptr_offsets = [];
    type_info    = env.type_info;
(* Cyclone *) cenvs = []; (* End Cyclone *)
    undefined    = P.mt_varset;
  } 

(***************** Selectors **************)
let env_convention  env = env.f_env.f_convention
let env_tyvars      env = env.f_env.f_tyvars
let env_args_on_stk env = env.f_env.f_args_on_stack
let env_names       env = env.f_env.f_names
let env_regs        env = env.f_env.f_regs
let env_cap1        env = env.f_env.f_cap1_type
let env_cap2        env = env.f_env.f_cap2_type
let env_cap         env = (T.cjoin [env_cap1 env; env_cap2 env])
let env_s1len       env = fst env.f_env.f_s1len
let env_s1cons      env = snd env.f_env.f_s1len
let env_break_label env = deSome env.f_env.f_break_label
let env_cont_label  env = deSome env.f_env.f_continue_label
let env_handler_con env = env.f_env.f_handler_con
let env_handler_gop env = env.f_env.f_handler_gop

let env_in_try      env = 
  match env.sptr_offsets with [] -> false | _ -> true
let env_top_handler env = 
  match env.sptr_offsets with [] -> None | hd::tl -> Some hd

(* Careful: this stuff is O(nm) where n is stack depth and m is size of un *)
let local_to_stacktype un (v,c,si) tail =
  let mk_junk c =
    match words_in_con c with
      1 -> T.ccons T.pcjunk4 tail
    | 2 -> T.ccons (T.pcjunk Numtypes.i32_8) tail
    | _ -> failwith "impossible: Popcompenv.local_to_types" in
  match si with
    Unshadowed        -> 
      if P.Varset.member un v then mk_junk c else T.ccons c tail
  | ShadowedUndefined -> mk_junk c
  | ShadowedDefined   -> T.ccons c tail
  
let local_sz (v,c,si) = words_in_con c
let locals_sz vs = List.fold_left (fun sum v -> local_sz v + sum) 0 vs

let env_stack1 env = 
  let un    = env.undefined     in
  let fenv  = env.f_env         in
  let s1len = env_s1len env     in
  let n     = if env_in_try env then s1len 
                                else s1len - fenv.f_args_on_stack in
  let vs   = fenv.f_local_vars in
  let rec aux count_up vs =
    if count_up >= n
    then (if env_in_try env then T.cempty else fenv.f_stack_bottom)
    else match vs with
           []     -> failwith "impossible: Popcompenv.env_stack1"
         | hd::tl -> local_to_stacktype un hd (aux (count_up + local_sz hd) tl)
  in
  aux 0 vs

let env_stack2 env = 
  if not (env_in_try env)
  then stack2_c
  else
    let un   = env.undefined in
    let fenv = env.f_env     in
    let n    = env_s1len env in
    let rec take_n n l =
      match n,l with
	0,_       -> l
      | n,[]      -> failwith "impossible: Popcompenv.env_stack2"
      | n,hd::tl  -> take_n (n - local_sz hd) tl in
    let vs = take_n (n+1) fenv.f_local_vars in
    let n  = (locals_sz vs) - fenv.f_args_on_stack in
    let rec aux n vs =
      match n,vs with
	0,_        -> T.cappend fenv.f_stack_bottom 
	                  (exn_stack_con stack2_c cap2_c)
      |	_,[]       -> failwith "impossible: Popcompenv.env_stack2"
      |	n,(hd::tl) -> local_to_stacktype un hd (aux (n - local_sz hd) tl)
    in
    aux n vs

(* number of cons in stack1
let env_s1cons env =
  let s1 = env_stack1 env in
  let rec aux s =
    match s.T.rcon with
      T.Ccons (hd,tl) -> 1 + aux tl
    | _ -> 0 in
  aux s1
*)

let lookup_label    env x  = Dict.lookup env.f_env.f_labels           x
let lookup_struct   env n  = Dict.lookup env.type_info.structs        n
let lookup_union    env n  = Dict.lookup env.type_info.unions         n
let lookup_abstype  env n  = Dict.lookup env.type_info.abstypes       n 
let lookup_global   env x  = snd(Dict.lookup env.global_env.PT.globals x)
let is_fun_decl     env n  = Dict.member env.global_env.PT.functions  n

(* This now has to look through the local environment as well,
   as the given name may be a variable of type excon *)
type exncon_loc = LocalExncon | GlobalExncon | ExnconDecl
let lookup_exn env v = 
  (* first check the local environment *)
  let rec aux l =
    match l with
      []     -> (* not there, check the global environment *)
	(match (snd (Dict.lookup env.global_env.PT.globals v)) with
	  P.ExnconType t ->
	    (* see if it's an exception declaration or a global variable *)
	    let loc =
	      if (Dict.member env.global_env.PT.exceptions v) then
		ExnconDecl
	      else
		GlobalExncon in
	    (loc, try typ2con t with Void_Type -> T.cbyte4)
	| _ -> raise Dict.Absent)
	  (* XXX check shadowing here ? *)

    | (v',c,_)::tl -> 
	if v' = v then (LocalExncon,exnname_arg_con_of c) else aux tl
  in aux env.f_env.f_local_vars

let typ2struct_info env nt = lookup_struct env (get_name nt)
let typ2union_info  env nt = lookup_union  env (get_name nt)

let env_local_depth env = 
  (* Must take into account doubles and longs on the stack. *)
  let rec aux n l =
    match l with
    | [] -> n
    | (v,c,_)::tl -> aux (n + words_in_con c) tl
  in
  aux 0 env.f_env.f_local_vars

(* List.length env.f_env.f_local_vars *)

let env_local_var_offset env v = (* for globals, raises Not_found *)
  let rec aux n l =
    match l with
      []     -> raise Not_found
    | (v',c,_)::tl -> if v' = v then n else aux (n + words_in_con c) tl
  in aux 0 env.f_env.f_local_vars

let env_next_handler env below_here = 
(* fixed by Dan for when in a handler but break does _not_ leave it *)
  match env.sptr_offsets with
    []      -> None
  | hd::tl  -> 
	(let rec aux prev rest =
	  match rest with
	    hd::tl when hd>below_here -> aux (Some hd) tl
	  | _ -> prev in
	aux None env.sptr_offsets)

(****************** Modifiers ***************)
let env_set_undefined env un = 
  if not (un = env.undefined)
  then { env with undefined = un; }
  else env

let env_add_tyvars env vs = 
  { env with f_env = { env.f_env with f_tyvars = vs @ env.f_env.f_tyvars } }
;;

let env_add_local_var env v con = (* v has been pushed, now update env *)
  let un         = env.undefined          in
  let old_locals = env.f_env.f_local_vars in
  let old_locals = 
    if List.exists (fun (v',c,si) -> v = v') old_locals
    then 
      let rec replace vs =
	match vs with
	  [] -> failwith "impossible: Popcompenv.env_add_local_var"
	| (v',c,si)::tl when v = v' ->
	    let new_si = if P.Varset.member un v then ShadowedUndefined 
	                                         else ShadowedDefined    in
	    (v,c,new_si)::tl
	| hd::tl -> hd::(replace tl)
      in replace old_locals
    else
      old_locals
  in
  { env with f_env =
     { env.f_env with f_local_vars = (v,con,Unshadowed)::old_locals;
                      f_s1len      = (env_s1len env + words_in_con con,
				      env_s1cons env + 1);
     }
  }  

let env_push_con env reg_con = 
  { env with f_env =
     { env.f_env with 
        f_local_vars = ("*BOGUS*",reg_con,Unshadowed)::env.f_env.f_local_vars;
        f_s1len      = (env_s1len env + words_in_con reg_con,
			env_s1cons env + 1);
     }
  }

let env_add_reg  env reg c   =
  { env with f_env =
    { env.f_env with f_regs = T.ms_set_reg (env_regs env) reg c }
  } 
let env_set_loop_labels env loopend looptest =
  let stack_depth = env_local_depth env in
  { env with f_env =
    { env.f_env with 
       f_break_label    = Some(loopend,  stack_depth, env);
       f_continue_label = Some(looptest, stack_depth, env); 
    } 
  } 

let env_add_label env popL startL endL =
  let stack_depth = env_local_depth env in
  { env with f_env = 
      { env.f_env with f_labels = 
           Dict.insert env.f_env.f_labels popL 
             (startL,endL,stack_depth,env) }
  }

let env_try_body env handler_con handler_gop = 
(* call on try entry,  stack already has shape handler::old_ebp::.... 
   but env does not yet record the location of old_ebp. *)
  let new_cap1 = T.cempty_cap in
  let new_cap2 = env_cap env  in
  let old_ebp_offset = env_local_depth env in
  let env = env_push_con env handler_con   in
  { env with
     f_env = { env.f_env with
                f_handler_gop = Some handler_gop;
                f_handler_con = handler_con;
                f_cap1_type   = new_cap1;
                f_cap2_type   = new_cap2;
                f_s1len       = (0,0);
              };
    sptr_offsets = old_ebp_offset :: env.sptr_offsets
  }

(* add x as a unique "name" that has type c *)
let env_add_name env x c = 
  let names2 = x :: env_names env in
  let cap = env_cap1 env in
  let cap' = 
    match T.cap_set_name cap x (T.Unique,c) with
      None -> failwith "popcompenv.ml:env_add_name: cap1 not a capability."
    | Some c' -> c'
  in
  let f_env2 = {env.f_env with f_names = names2; f_cap1_type = cap' } in
  {env with f_env = f_env2}
;;

(* change the type of a name x *)
let env_change_name env x c = env_add_name env x c 
(* FMS: Unless we check to make sure x is already bound, this is equivalent to
   the above function.  May want to add this check later. *)
;;  

(* Cyclone *)

let env_stack_height env =
  let rec aux con =
    match con.T.rcon with
      T.Ccons(c1,con) -> words_in_con c1 + aux con
    | _              -> 0 in
  aux (env_stack1 env)

let cyc_name_counter = ref 0 ;;
let cyc_new_name () = 
  (cyc_name_counter:=!cyc_name_counter + 1;
   "cg_"^(string_of_int (!cyc_name_counter)))
;;
let cyc_id nm = tid_name nm

let cyc_get_cg_name env =
  let cg_stack = env.f_env.f_cg_stack in
  match cg_stack with hd::tl-> hd | []-> impos "empty cg_stack"
;;

let cyc_get_cg env =
  let (cg_name,cg_id) = cyc_get_cg_name env in
  let cg_offset = env_local_var_offset env cg_name in
  let cg_con = 
    match T.cap_get_name env.f_env.f_cap1_type cg_id  with
      None -> impos "no con for cg_rgn"
    | Some (T.Unique,c) -> c
    | Some _ -> impos "con for cg_rgn is may_alias?"
  in
  (cg_offset,cg_con)
;;

let cyc_get_cg_id env = (snd (cyc_get_cg_name env)) ;;

(* FMS: This plays the same role as put_vis_cg_type used to. *)
let cyc_set_cg env con = 
  let (cg_name,cg_id) = cyc_get_cg_name env in
  let cap1 = env.f_env.f_cap1_type in
  let cap1' = T.cap_set_name cap1 cg_id (T.Unique,con) in
  match cap1' with
    None -> impos "env.f_env.f_cap1_type not a capability"
  | Some c' -> { env with f_env = { env.f_env with f_cap1_type = c' } }
;;

(* Returns an environment where the cg_region has been pushed on the stack. *)
let cyc_push_cg env con =
  let name = cyc_new_name () in
  let id = cyc_id name in
  let name_con = T.cname (T.cvar id) in
  let env = env_add_local_var env name name_con in
  let env = env_add_name env id con in
  let env = {env with f_env = { env.f_env with 
             f_cg_stack = (name,id)::env.f_env.f_cg_stack}} in
  env
;;
    
let outermost env =
  let rec aux cs =
    match cs with
      [] -> true
    | (Outermost _)::_  -> true
    | (Hidden    _)::cs -> aux cs
    | (Frame     _)::_  -> false in
  aux env.cenvs
    
let in_frame  env = match env.cenvs with (Frame _)::_ -> true | _ -> false
    
let get_vis_fenv env      = env.f_env
let put_vis_fenv env fenv =
  let new_cenvs =
    let rec aux cs =
      match cs with
	[]                    -> []
      | (Hidden    fenv2)::tl -> (Hidden    fenv2) ::(aux tl)
      | (Frame     _)    ::tl -> (Frame     fenv)  ::tl
      | (Outermost _)    ::tl -> (Outermost fenv)  ::tl in
    aux env.cenvs in
  { env with f_env = fenv; cenvs = new_cenvs } 

let flush_vis_fenv env =
  let new_cenvs =
    let rec aux cs =
      match cs with
        []                 -> []
      | (Frame     _)::tl  ->let fenv = get_vis_fenv env in (Frame     fenv)::tl
      | (Outermost _)::tl  ->let fenv = get_vis_fenv env in (Outermost fenv)::tl
      | (Hidden fenv2)::tl ->(Hidden fenv2)::(aux tl) 
    in
    aux env.cenvs in
  { env with cenvs = new_cenvs} 

let env_codegen_body env fd = (* env should already be flushed *)
  let fenv2 = mk_fenv fd in
  let cenvs =
    match env.cenvs with
      []     -> [ Frame fenv2; Outermost(get_vis_fenv env) ]
    | cenvs' ->  (Frame fenv2)::cenvs' in
  let env2 = { env with f_env = fenv2; cenvs = cenvs} in
  let env3 = { env with cenvs = (Hidden fenv2)::env.cenvs } in
  env2, env3

let env_cut_body env = (* env should already be flushed *)
  let fenv,cenvs  = 
    match env.cenvs with (Frame x)::y -> x,y
    | _ -> impos "cut: bad cenvs type in env_cut_body" in
  let fenv2 =
    let rec aux cs =
      match cs with
        []                    -> impos "cut: can't find previous env"
      | (Outermost fenv2)::_  -> fenv2
      | (Frame     fenv2)::_  -> fenv2
      | (Hidden        _)::tl -> aux tl in
    aux cenvs in
  { env with f_env = fenv2; cenvs = (Hidden fenv)::cenvs }
    
let env_splice_body env = (* env should already be flushed *)
  match env.cenvs with
    (Hidden fenv2)::cenvs ->
      let fenv0 = get_vis_fenv env in
      { env with f_env = fenv2; cenvs = (Frame fenv2)::cenvs } 
  | _ -> impos "splice can only be used within a cut"

(* End Cyclone *)


(* dynamic linking *)
let env_add_global env var typ = 
   { env with 
   global_env = { env.global_env with 
   PT.globals = Dict.insert env.global_env.PT.globals var (ref false, typ) }}

let env_global env = env.global_env 
