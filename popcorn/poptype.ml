(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Chris Hawblitzel, Frederick Smith   *)
(*     Dan Grossman                                                   *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* Contents:
 *  Utilities
 *  Environment Types, Etc.
 *  Hack for producing stack dumps COMMENTED OUT
 *  Name Resolutions Using Namespaces
 *  Cyclone Environment Additions
 *  Type Well-formedness
 *  Substitution
 *  "Does Return" -- Deprecated since I'll fold it into type-checking
 *  Unification
 *  Default Initialization -- Deprecated
 *  Constant Folding
 *  
 *  And then some actual type-checking
 *)

(* Now attempts Java-like definite assignment checking *)
(* To do: 
 *   o better loop rules
 *   o splice rules (and rest of Cyclone)
 *   o more like Java on true==true and company
 *)

(*
 * 
 * Still need to verify that exported values do not have types that are not
 * exported (i.e., types that are static.)
*)

(* 
 * Open and prefix provide crude mechanisms for encapsulation.  The names in
 * TypeName are refs so that they can be updated with their
 * fully qualified path, once known.
 *
 * Type checking proceeds as follows:
 *   1. Eliminate Prefixes by embedding them in the names.
 *   2. Blindly build an initial global environment from the top decls.
 *   3. Check that all types mentioned in global_env are well-formed, and
 *      expand their names to be fully qualified.
 *   4. Type check the function bodies and global variable initializers.
 *
 * The following represent unique namespaces:
 *   1. Unions/Structures/Abstypes/Abstracts
 *   2. Globals/Exception
 * No duplicates are allowed within one namespace except abstract/transparent.
 * We always keep the declaration with the most information.
 *)

(* Stack dumps for debugging are out-of-date -- the code doesn't interact
   with definite assignment.  It was a kludge anyway.  I've left the code
   commented out in case someone is interested
 *)

(* MWH: Note about exceptions:

   The exceptions field of the global_env struct has been deprecated
   to just hold the exception declarations in the environment; for
   type checking, each of these exceptions is added to the globals
   field with type ExnconType _.  We need to keep this exceptions
   list around so that they can be differentiated during code
   generation from globals of the same type. 

   I've added a similarly defined "functions" field for differentiating
   between locally defined global function variables and function
   declarations.
*)

(*
let debug = ref false (* true means insert stack dump hack *)
*)

open Numtypes
open Popsyntax

exception Unimplemented
exception UnboundVar of (var*location)

(***************************** Utilities *************************)

(* Some generic things *)
let deSome opt = 
  (match opt with Some v -> v
  | None -> (Printf.eprintf "Compiler Error: deSome failed."; 
	     flush stderr;
	     raise Not_found)) 

  (* check that a list is unique using leq for sorting and equals for equality *)
let check_unique leq equals xs = 
  let rec loop xs = 
    match xs with
      [] -> None
    | [_] -> None
    | x1::((x2::_) as tail) ->
	if equals x1 x2 then Some(x1) else loop tail
  in loop (Sort.list leq xs)

  (* determine if two lists are the same where test compares elements *)
let rec list_match test t1 t2 =
  match t1,t2 with
    [],[] -> true
  | [],_ | _,[] -> false
  | hd1::_,hd2::_ when not (test hd1 hd2) -> false
  | hd1::tl1,hd2::tl2 -> list_match test tl1 tl2

  (* returns (true,log_2(y)) if y is an even power of 2, otherwise returns
   * (false,?).  Assumes y >= 0. *)
let is_power_of_2 y = 
  let rec power2 log x y = 
    if y < x then (false,log)
    else if y = x then (true,log)
    else power2 (log+1) (x lsl 1) y
  in power2 0 1 y

(* Some compiler specific things *)
let terr loc s =
    let e = Poperr.Etypecheck (Poperr.TypeError s) in
    let e = Gcdfec.mk_err_elab loc (Poperr.error_message e) in
    Gcdfec.post_error e

let scope_match s1 s2 =
  match s1,s2 with
    Static,Static | Public,Public | Extern,Extern | Abstract,Abstract
  | Extern,Public | Public,Extern -> true
  | _ -> false

(**************************** Environment Types, Etc. *****************)

(* Global environment *) 
(* union_fs is used to reverse lookup union field names.  Not in dictionary
   means no such field name. If present but None means ambiguous.
*)

type 'a internal_global_env = { 
    structs    : (type_name, structdecl)       Dict.dict;
    unions     : (type_name, uniondecl)        Dict.dict;
    abstypes   : (type_name, absdecl)          Dict.dict;
    abstracts  : (type_name, var list * bool)  Dict.dict;
    union_fs   : (field_name, (uniondecl option)) Dict.dict;
    globals    : (var,       (bool ref * typ)) Dict.dict;
    exceptions : (var,       typ)              Dict.dict;
    functions  : (var,       'a)               Dict.dict;
    open_typs  : (type_name, type_name)        Dict.dict;
    open_vals  : (var,       var)              Dict.dict;
  }
(* same throughout file except to add inner functions *)
type global_env = typ internal_global_env
(* global_env' only used during the initial environment construction. *)
type global_env' = (bool ref * typ) internal_global_env

(* Environment for type-checking -- this is what's passed down *)
type f_env0 = { (* same throughout function *)
    f_name     : var;
    f_convention : convention;
    f_tyvars   : var list;
    f_args     : (var * typ) list;
    f_ret_type : typ;
  } 
type f_env = {
    f_env0         : f_env0;
    f_local_tyvars : var list;
    f_locals       : (var * typ) list;
    f_inloop       : bool;
    f_labels       : var list; (* break/continue destination in scope *)
    f_un_before    : varset;  (* May be unassigned before stmt or exp *)
  }
(* Synthesized by type-checking -- this is what's passed up *)
type f_exp_unassigned = 
    Un_Always  of varset           (* unassigned after exp *)
  | Un_Boolean of varset * varset (* unassigned when true, when false *)
type f_exp_synth = { 
    f_typ            : typ;
    f_exp_raise      : bool; (* Does exp definitely throw an exception *)
    f_exp_unassigned : f_exp_unassigned;
  } 
type f_stmt_synth = { 
    f_stmt_jmp : bool;   (* Does exp definitely break/cont/return/raise *)
    f_un_after : varset; (* May be unassigned after statement *)
  } 

let jump_synth = { f_stmt_jmp = true; f_un_after = mt_varset; }

(* JGM:  I need an imperative version of this to add inner functions and
 * static variables -- we rely upon name mangling and gensyming to 
 * prevent access to these "globals" outside their scope. *)
let add_global g x t = {g with globals = Dict.insert g.globals x (ref true,t)}

(* is n (which should be fully named) nullable *)
let possibly_null global_env n =
  (try (Dict.lookup global_env.structs n).st_possibly_null with
    Dict.Absent ->
      (try snd (Dict.lookup global_env.abstracts n) with
	Dict.Absent -> false))

let fndecl2f_env fd=
  { f_env0 = { f_name     = fd.fn_name;
	       f_convention = fd.fn_convention;
	       f_tyvars   = fd.fn_tyvars;
	       f_args     = fd.fn_args;
	       f_ret_type = fd.fn_ret_type;
	     };
    f_local_tyvars = [];
    f_locals       = fd.fn_args;
    f_inloop       = false;
    f_labels       = [];
    f_un_before    = mt_varset;
  } 
let set_inloop     fenv     = { fenv with f_inloop = true }
let set_outloop    fenv     = { fenv with f_inloop = false }
let add_label      fenv x   = { fenv with 
                                 f_labels     = x::fenv.f_labels }
let add_tyvars     fenv vs  =
  let add_var tvs v = 
    if (List.mem v fenv.f_env0.f_tyvars) or (List.mem v tvs)
    then raise (Invalid_argument v) else v::tvs
  in { fenv with 
        f_local_tyvars = List.fold_left add_var fenv.f_local_tyvars vs }

let add_var_init   fenv x t = { fenv with 
                                f_locals    = (x,t)::fenv.f_locals;
                                f_un_before = Varset.delete fenv.f_un_before x }
let add_var_uninit fenv x t = { fenv with 
                                f_locals    = (x,t)::fenv.f_locals;
                                f_un_before = Varset.insert fenv.f_un_before x }
let set_unassigned fenv un =
  (* avoid allocation in common case of no change *)
  if fenv.f_un_before = un
  then fenv
  else { fenv with f_un_before = un }

let retType      fenv = fenv.f_env0.f_ret_type
let fn_tyvars    fenv = fenv.f_env0.f_tyvars
let all_tyvars   fenv = fenv.f_env0.f_tyvars @ fenv.f_local_tyvars
let inloop       fenv = fenv.f_inloop
let unassigned   fenv = fenv.f_un_before
let label_bound  fenv x  = List.mem x fenv.f_labels
let mk_env       fenv = fenv

let lookup       fenv id = List.assoc id fenv.f_locals

let un_after_exp exp_synth =
  match exp_synth.f_exp_unassigned with
    Un_Always  vs -> vs
  | Un_Boolean(vs1,vs2) -> Varset.union vs1 vs2
let un_after_bool exp_synth =
  match exp_synth.f_exp_unassigned with
    Un_Always  vs -> failwith "poptype.ml: called un_after_bool on non-bool"
  | Un_Boolean(vs1,vs2) -> vs1,vs2

let dummy_location = Gcdfec.seg_of_abs 42 42
let make_exp  re un = { exp_typ = None; raw_exp = re; exp_loc = dummy_location;
  	                exp_un_after = un;}
let make_stmt rs un = { raw_stmt = rs; stmt_loc = dummy_location;
		        un_after = un; un_before = un;}

(************************** to print out environments *********************)

module F = Format
let pr = F.pp_print_string
let ps = F.pp_print_space
let psori f () = F.pp_print_break f 1 2
let pn = F.pp_force_newline

let pr_global_env f opt env =
  let hovbox () = F.pp_open_hovbox f 0 in
  let obox0() = if not opt.flat then F.pp_open_hvbox f 0 in
  let cbox() = if not opt.flat then F.pp_close_box f () in
  let prty f t = pr_poptype f opt None t; () in
  let prsd f sd = pr_poptopdecl f opt (StructDecl sd,dummy_location) in
  let prud f ud = pr_poptopdecl f opt (UnionDecl ud,dummy_location) in
  let prab f ad = pr_poptopdecl f opt (AbsDecl ad,dummy_location) in
  let prvs f vs =
    let pr_poplist f opt p prsep obox l r ts = 
      let rec aux = function 
	  [] -> ()
	| [t] -> p t
	| t::ts -> p t; prsep f; aux ts in
      if l <> "" then pr f l;
      if not opt.flat then obox ();
      aux ts;
      if not opt.flat then F.pp_close_box f ();
      if r <> "" then pr f r in
    let pr_poptyvars = 
      pr_poplist f opt (pr f) (fun f -> pr f ",") in
    pr_poptyvars hovbox "<" ">" vs in
  let print_block name dict prk prv =
    obox0 ();
    pr f name;
    cbox ();
    psori f (); (* break *)
    Dict.print prk prv f dict;
    ps f () in
  (* ------------------------------------------------------------- *)
  print_block "structs" env.structs pr prsd;
  print_block "unions" env.unions pr prud;
  print_block "abstypes" env.abstypes pr prab;
  print_block "abstracts" env.abstracts pr
    (fun f (vs,b) -> prvs f vs; if b then pr f "?");
  print_block "globals" env.globals pr (fun f (bref,t) -> prty f t);
  print_block "exceptions" env.exceptions pr prty;
  print_block "functions" env.functions pr prty

let global_env2string =
  let x2string px opt obox x =
    let f = F.str_formatter in
    obox f;
    px f opt x;
    F.pp_close_box f ();
    F.pp_print_flush f ();
    F.flush_str_formatter () in
  let obox f = F.pp_open_hvbox f 0 in
  x2string pr_global_env std_opts obox
    
(*
(************************** Hack for producing stack dumps ****************)
(* DEBUG *)
let make_typed_exp t re = { exp_typ = Some t; raw_exp = re;
			    exp_loc = dummy_location }
let make_call name args =
  make_exp(FunCall(make_exp(Var name), ref (Some []), args))
let make_call_stmt name args =
  make_stmt(Exp(make_call name args))
let debug_push name =
  make_call_stmt "callStackPush" [make_exp(Const (String name))]
let debug_pop () = 
  make_call_stmt "callStackPop"  []
let debug_dump() =
  make_call_stmt "callStackDump" []
(* END DEBUG *)
*)

(************************* Cyclone Environment Additions **********************)

(* Cyclone *)

(****************************************************************)
(* We define Cyclone environments, and redefine functions that  *)
(* depend on the representation of environments.                *)
(*                                                              *)
(* A version of poptype without Cyclone support can be          *)
(* recovered simply by commenting out this code, and subsequent *)
(* code marked CYCLONE.                                         *)
(*                                                              *)
(* VARIABLE CONVENTIONS: fenv is used to name f_env's, cenv is  *)
(* used to name c_env's, and env is used where it could be      *)
(* either (depending on whether the Cyclone code is commented   *)
(* out).  Global environments are always named by global_env.   *)
(****************************************************************)
type c_env =
    Outermost of f_env
  | Frame of f_env * c_env
  | Hidden of f_env * c_env

let rec fenv_of_cenv cenv =
  match cenv with
    Outermost fenv -> fenv
  | Frame(fenv,_) -> fenv
  | Hidden(_,cenv) -> fenv_of_cenv cenv

let rec put_fenv cenv fenv =
  match cenv with
    Outermost _ -> Outermost fenv
  | Frame(_,cenv) -> Frame(fenv,cenv)
  | Hidden(fenv',cenv) -> Hidden(fenv',put_fenv cenv fenv)

let set_inloop     cenv    = put_fenv cenv (set_inloop  (fenv_of_cenv cenv))
let set_outloop    cenv    = put_fenv cenv (set_outloop (fenv_of_cenv cenv))
let add_label      cenv x  = put_fenv cenv (add_label   (fenv_of_cenv cenv) x)
let add_tyvars     cenv vs = put_fenv cenv (add_tyvars  (fenv_of_cenv cenv) vs)
let add_var_init   cenv x t = 
  put_fenv cenv (add_var_init (fenv_of_cenv cenv) x t)
let add_var_uninit cenv x t = 
  put_fenv cenv (add_var_uninit (fenv_of_cenv cenv) x t)
let set_unassigned cenv un = 
  put_fenv cenv (set_unassigned (fenv_of_cenv cenv) un)

let of_cenv old cenv = old (fenv_of_cenv cenv)

let retType    = of_cenv retType
let fn_tyvars  = of_cenv fn_tyvars
let all_tyvars = of_cenv all_tyvars
let inloop     = of_cenv inloop
let unassigned = of_cenv unassigned
let label_bound cenv x = label_bound (fenv_of_cenv cenv) x

let mk_env fenv = Outermost fenv

let lookup cenv id =
  (********************************************************************)
  (* Special case for Cyclone: if we are looking up a function, we    *)
  (* need to look in the frame.  All Popcorn functions are outermost, *)
  (* and their types are given in the global environment as well as   *)
  (* the frame.  In Cyclone, an inner function won't be in the        *)
  (* global environment.                                              *)
  (********************************************************************)
  (* JGM:  problem if we declare a local variable that shadows the 
   * function name -- so, I'm doing the lookup first and then the
   * check. *)
  let fenv = fenv_of_cenv cenv in
  (try lookup fenv id
  with Not_found ->
    let f0 = fenv.f_env0 in
    if f0.f_name = id then
      FnType(f0.f_convention,f0.f_tyvars,f0.f_ret_type,List.map snd f0.f_args)
    else raise Not_found)

(* End Cyclone *)

(*************************Rewrite special constants****************************)
(* A "special" is a variable that is understood by the compiler and turned
   into an internal operator.  Right now these consist of a bunch of floating 
   point constants.  

   These variables are not keywords. 

   Specials of function type are only changed into operators when they occur
   in a function application.  The library provides real functions that can be
   used if the special is to be put in a data structure. *)
let rewrite_specials exp =
  match exp.raw_exp with
  | Var v ->
      (match is_special v with
      | Some(0,op) -> exp.raw_exp <- Primop(op,[])
      | _ -> ())
  | FunCall(e,tor,es) when (!tor = None || !tor = Some []) ->
      (match e.raw_exp with
	Var v ->
	  (match is_special v with
	    Some(arity,op) ->
	      exp.raw_exp <- Primop(op,es);
	      if(List.length es != arity) then 
		terr (exp.exp_loc) "Builtin operator used with wrong arity."; 
	      ()
	  | _ -> ())
      | _ -> ())
  | _ -> ()
	;;

(************************ Name Resolutions Using Namespaces ******************)

exception Unbound

(*Given name and a function defined : var -> bool. Returns fully qualified name*)
let complete_typ_name global_env defined n =
  begin try
    if defined n then n else Dict.lookup global_env.open_typs n
  with Dict.Absent -> raise Unbound
  end

let complete_val_name global_env defined n =
  begin try
    if defined n then n else Dict.lookup global_env.open_vals n
  with Dict.Absent -> raise Unbound
  end
  
(* looks up a name in the local and global environment; if found it
   completes the name if necessary and returns it as long as it's of
   type <a>exncon.  Will only return if n is of exncon type; otherwise 
   will raise Unbound. *)

let complete_exncon_name global_env env n =
  let (n,t) =
    try 
      let n' = 
	complete_val_name global_env (Dict.member global_env.globals) n in
      let _,t = Dict.lookup global_env.globals n' in
      (n',t)
    with Unbound ->
      (try 
	(n,lookup env n)
      with Not_found -> raise Unbound) in
  (match t with
    ExnconType _ -> n
  | _ -> raise Unbound)

let open_prefix global_env p =
  begin
    let pLen = String.length p in
    let has_prefix n = 
      if String.length n <= pLen then None
      else 
	let rec aux i = 
	  (* foo?bar foo is the prefix.
	     When opened we get bar not ?bar so we need to add 1. *)
	  if i=pLen then Some (String.sub n (i+1) (String.length n - pLen - 1))
	  else if n.[i] <> p.[i] then None else aux (i+1)
	in
	aux 0
    in
    let process_dict opened d =
      let process_elt id b opened =
	match has_prefix id with
	  None -> opened
	| Some id' -> (Dict.insert opened id' id)
      in
      Dict.fold_dict process_elt d opened
    in
    let process_opened opened od =
      let process_elt id id' opened =
	(* id' is the fully-qualified name of this thing.  It just happens to
	   have been opened twice. *)
	match has_prefix id with
	  None -> opened
	| Some id'' -> (Dict.insert opened id'' id')
      in
      Dict.fold_dict process_elt od opened
    in
    let open_typs = process_opened global_env.open_typs global_env.open_typs in
    let open_vals = process_opened global_env.open_vals global_env.open_vals in
    let open_typs = process_dict open_typs global_env.structs in
    let open_typs = process_dict open_typs global_env.unions in
    let open_typs = process_dict open_typs global_env.abstypes in
    let open_typs = process_dict open_typs global_env.abstracts in
    let open_vals = process_dict open_vals global_env.globals in
    { global_env with open_typs = open_typs; open_vals = open_vals }
  end


(************************** Type Well-formedness ***************************)

(* Determines the kind of type:
 * - either 4 byte type passed in general purpose registers
 * - or unconstrained type variable
 * - or something else (a floating point type or void)
 *)
type popkind = K4 | Kany | Kother

let rec kind_of_typ global_env t =
  begin match t with
    VoidType -> Kother
  | Evar(c,tor) ->
      begin match c with
	Any ->
	  begin match !tor with
	    Some t -> kind_of_typ global_env t
	  | None -> Kany
	  end
      |	Byte4 | Option -> K4
      end
  | DoubleType | FloatType -> Kother
  | _ -> K4
  end 

let rec check_arraysize_expression loc e =
  (* Check that an expression is a "constant-expression" suitable for *)
  (* an array size in a type or declaration. *)
  (* We should also check that it has type int!! Sometimes but not
     always this is done elsewhere... *)
  let err s = terr loc s in
  let raise_err s = err s; raise Gcdfec.Exit in
  match e.raw_exp with
    Const(Int _) -> ()
  | Primop(p,[e1;e2]) ->
      begin
        check_arraysize_expression loc e1;
        check_arraysize_expression loc e2;
        match p with
          Plus | Times | Minus | Div | Mod | Bitand | Bitor | Bitxor
        | Bitlshift | Bitlrshift | Bitarshift -> ()
        | _ -> raise_err "non-constant-expression in type"
      end
  | _ -> raise_err "non-constant-expression in type"

let rec check_valid_type loc global_env (tyvars:var list) t =
  (* Check that t is a valid type in the current global environment.
   * Resolve UnresolvedTyId via destructive update of MutableTyp *)
  let err s = terr loc s in
  let raise_err s = err s; raise Gcdfec.Exit in
  match t with
    MutableTyp tr ->
      (match !tr with
	UnresolvedTyId(n,tl) ->
	  if List.mem n tyvars
	  then 
	    (match tl with
	       [] -> tr := VarType n
	     | _ -> raise_err "type variable cannot take type parameters")
	  else 
	    (let nt = NamedType(ref n,tl) in
	     tr := nt;
	     check_valid_type loc global_env tyvars nt)
      |	t -> check_valid_type loc global_env tyvars t)
  | ArrayType(t,None) ->
      check_valid_type loc global_env tyvars t
  | ArrayType(t,Some e) ->
      check_valid_type loc global_env tyvars t;
      check_arraysize_expression loc e
  | FnType(c,vs,t,ts) -> check_valid_types loc global_env (vs@tyvars) (t::ts)
  | NamedType(n,ts) ->
      check_valid_types loc global_env tyvars ts;
      let defined name =
	(Dict.member global_env.structs   name ||
	 Dict.member global_env.unions    name ||
	 Dict.member global_env.abstypes  name ||
	 Dict.member global_env.abstracts name)
      in
      n :=
	 begin try
	   complete_typ_name global_env defined !n
	 with Unbound -> raise_err ("bad type "^ !n)
	 end;
      let vs =
      	(try (Dict.lookup global_env.structs !n).st_tyvars with
	  Dict.Absent ->
	    (try (Dict.lookup global_env.unions !n).un_tyvars with
	      Dict.Absent ->
		(try (Dict.lookup global_env.abstypes !n).abs_all_tyvars with
		  Dict.Absent -> 
		    (try fst (Dict.lookup global_env.abstracts !n) with
		      Dict.Absent -> raise_err ("bad type "^ !n)))))
      in if (List.length vs) = (List.length ts) then ()
      else raise_err ("wrong # of type arguments for "^ !n)
  (* Evars should only occur as pointers to other actual
   * types -- in fact, they shouldn't occur at all but what the hell. *)
  | Evar(_,r) ->
      (match !r with
	Some t -> check_valid_type loc global_env tyvars t
      |	_ -> ())
  | VarType v ->
      if List.mem v tyvars then () else
      raise_err ("free type variable "^v) (* cannot occur w/ current parser *)
  | TupleType (c,ts) -> 
      check_valid_types loc global_env tyvars ts
(* LR *)
  | RepType t ->
      check_valid_type loc global_env tyvars t
(* end LR *)
  | ExnconType t ->
      check_valid_type loc global_env tyvars t
  | _ -> ()
and check_valid_types loc global_env (tyvars:var list) ts =
  List.iter (check_valid_type loc global_env tyvars) ts

let check_fundecl_valid_type global_env loc fd =
  check_valid_type loc global_env fd.fn_tyvars fd.fn_ret_type;
  List.iter (fun (_,t) -> check_valid_type loc global_env fd.fn_tyvars t)
    fd.fn_args;
  FnType(fd.fn_convention,fd.fn_tyvars,fd.fn_ret_type,List.map snd fd.fn_args)

(************************* Substitution  *************************************)

(* substitution -- used to rename inner functions so they may be safely      *)
(* hoisted.  We use the fact that the new name of the function is guaranteed *)
(* to be unique to simplify the translation.  We also do not allow Cyclone   *)
(* constructs within the nested function to avoid strange scoping issues.    *)

(* Dan: we could do this lazily via the environment, but we'll save that for
   later.
 *)

(* a global list of inner functions found so far -- should be reset to []
 * at the beginning of a type-check.
 *)
let inner_functions : top_decl list ref = ref [];;
(* used to generate unique function names *)
let counter, reset_counter = 
  let count = ref 0 in 
  ((fun () -> let i = !count in incr count; i),
   (fun () -> count := 0))
let new_inner_fn_id x = let c = counter() in "inner?"^(string_of_int c)^"?"^x
let new_exn_id () = let c = counter() in "exn?"^(string_of_int c)
(* substitute a new_name for an old_name, respecting bound variables, and
 * using the fact that new_name is guaranteed not to conflict. *)
let subst_stmt new_name old_name stmt = 
  let rec ss stmt = 
    match stmt.raw_stmt with
      Skip -> ()
    | Exp e -> se e
    | Seq(s1,s2) -> ss s1; ss s2
    | Return None -> ()
    | Return (Some e) -> se e
    | IfThenElse(e,s1,s2) -> se e; ss s1; ss s2;
    | While(e,s) -> se e; ss s
    | Break _ -> ()
    | Continue _ -> ()
    | For(e1,e2,e3,s) -> se e1; se e2; se e3; ss s
    | IntSwitch(e,arms,s) -> se e; List.iter (fun (_,s) -> ss s) arms; ss s
    | CharSwitch(e,arms,s) -> se e; List.iter (fun (_,s) -> ss s) arms; ss s
    | UnionSwitch(e,arms,sopt) ->
	se e; List.iter sarm arms; 
	(match sopt with None -> () | Some s -> ss s)
    | ExnSwitch(e,arms,sopt) -> 
	se e; List.iter sarm arms; 
	(match sopt with None -> () | Some s -> ss s)
    | Decl(x,_,eopt,s) -> 
	(match !eopt with None -> () | Some e -> se e); 
	if x = old_name then () else ss s
    | Label(_,s) -> ss s
    | Cut _ -> terr stmt.stmt_loc "can't use cut in inner function"
    | Splice _ -> terr stmt.stmt_loc "can't use splice in inner function"
    | Do(s,e) -> ss s; se e
    | TryHandle(s1,x,s2) -> ss s1; if x = old_name then () else ss s2
    | TryCatchFinally(s,arms,sopt1,sopt2) -> 
	ss s; List.iter sarm arms; 
	(match sopt1 with None -> () | Some s -> ss s);
	(match sopt2 with None -> () | Some s -> ss s);
    | With(x,_,_,e,s) -> se e; if x = old_name then () else ss s
    | Rdtsc(e1,e2) -> se e1; se e2 
  and ses exps = List.iter se exps
  and se exp = 
    match exp.raw_exp with
      Const _ -> ()
    | ConstArray (es,_) -> ses es
    | Var x -> 
	if x = old_name then exp.raw_exp <- (Var new_name) else ()
(* LR *)
    | RepTerm -> ()
(* end LR *)
    | Primop(_,es) -> ses es
    | Conditional(e1,e2,e3) -> se e1; se e2; se e3
    | AssignOp(e1,_,e2) -> se e1; se e2
    | FunCall(e,_,es) -> se e; ses es
    | TypInst(e,_) -> se e
    | NewStruct(_,_,fs) -> List.iter (fun (_,e) -> se e) fs
    | StructMember(e,_) -> se e
    | NewUnion(_,_,_,None) -> ()
    | NewUnion(_,_,_,Some e) -> se e
    | UnionMember(e,_) -> se e
    | NewTuple es -> ses es
    | TupleMember(e,_) -> se e
    | NewAbstype(_,_,_,e) -> se e
    | Subscript(e1,e2) -> se e1; se e2
    | Codegen(_) -> terr exp.exp_loc "can't use codegen in inner function"
    | Fill _ -> terr exp.exp_loc "can't use fill in inner function"
    | NewExn(_,None) -> ()
    | NewExn(_,Some e) -> se e
    | Raise e -> se e
    | SeqExp es -> ses es
    | Nop -> ()
    | Cast(_,e) -> se e
    | Fun fd -> 
	if fd.fn_name = old_name then ()
	else if List.mem old_name (List.map fst fd.fn_args) then ()
	else ss fd.fn_body
  and sarm arm = 
    (match arm.arm_pat with
      No_pat -> ss arm.arm_body
    | Prim_pat (Wild_pat _) -> ss arm.arm_body
    | Prim_pat (Var_pat(x,_)) -> 
	if x = old_name then () else ss arm.arm_body
    | Tuple_pat ps ->
	let rec loop ps = 
	  match ps with
	    (Var_pat(x,_))::rest -> if old_name = x then () else loop rest
	  | (Wild_pat _)::rest -> loop rest
	  | [] -> ss arm.arm_body 
	in loop ps) in
  ss stmt
;;

(* This is for types -- should be renamed to reflect that *)
let rec type_subst inst t =
  match t with
    VarType v ->
      (try List.assoc v inst with Not_found -> t)
  | ArrayType(t,iopt) -> ArrayType(type_subst inst t,iopt)
  | FnType(c,vs,t,ts) ->
      let inst = (List.map (fun v -> (v,VarType v)) vs) @ inst in
      FnType(c,vs,type_subst inst t,type_substs inst ts)
  | TupleType (c,ts) -> TupleType(c, type_substs inst ts)
  | NamedType(n,ts) -> NamedType(n,type_substs inst ts)
  | MutableTyp(tr) -> type_subst inst (!tr)
  | Evar(v,toptref) ->
      (match !toptref with
	None -> t
      |	Some t -> type_subst inst t)
  | RepType(t) -> RepType(type_subst inst t)
  | ExnconType(t) -> ExnconType(type_subst inst t)
  | _ -> t
and type_substs inst ts = List.map (type_subst inst) ts

(* doesRet is now deprecated since I'm folding it into type-checking *)

(*************************************************************************)
(* Check a statement to make sure it does a return rather than fall off  *)
(* the end without doing a return. This is a conservative approximation: *)
(* doesRet returns false for some statements that always do a return.    *)
(*                                                                       *)
(*   EXAMPLE:                              WE RETURN:      CONSERVATIVE? *)
(*   { return(5); 0; }                     true            no            *)
(*   while (1==2) return(5);               false           no            *)
(*   while (1<2) return(5);                false           yes           *)
(*   codegen(int f()                                                     *)
(*           { cut return(5); });          false           yes           *)
(*                                                                       *)
(*************************************************************************)
let rec doesRet {raw_stmt=s} =
  match s with
    Skip -> false
  | Exp e ->
      begin match e.raw_exp with
	Raise _ -> true
      |	_ -> false
      end
  | Seq(s1,s2) -> doesRet s1 or doesRet s2
  | Return _ -> true
  | IfThenElse(_,s1,s2) -> doesRet s1 & doesRet s2
  | While(e,s) -> false (* s may never execute *)
  | Break _ -> false
  | Continue _ -> true (* FMS : Changed to true. *)
  | For(e1,e2,e3,s) -> false (* s may never execute *)
  | IntSwitch(_,ss,s) ->
      (List.for_all (fun (_,s) -> doesRet s) ss) & (doesRet s)
  | CharSwitch(_,ss,s) ->
      (List.for_all (fun (_,s) -> doesRet s) ss) & (doesRet s)
  | UnionSwitch(_,ss,sopt) ->
      (List.for_all (fun a -> doesRet a.arm_body) ss) & (doesRetOpt sopt)
  | ExnSwitch(_,ss,sopt) ->
      (List.for_all (fun a -> doesRet a.arm_body) ss) & (doesRetOpt sopt)
  | Decl(_,_,_,s) -> doesRet s
  | Label(_,s) -> false (* may break out of s *)
  | Do(s,e) -> false (* may break out of s *)
  | TryHandle(s1,_,s2) -> doesRet s1 & doesRet s2
  | TryCatchFinally(s1,ss,s2opt,s3opt) ->
      (doesRet s1) & (List.for_all (fun a -> doesRet a.arm_body) ss) & 
      (doesRetOpt s2opt) & (doesRetOpt s3opt)
(* Cyclone *)
  | Cut s -> doesSpliceRet s
  | Splice s -> false (* this will be a type error *)
  | With (_,_,_,_,s) -> doesRet s
  | Rdtsc (_,_) -> false
and doesRetOpt sopt = 
  match sopt with
    None -> true
  | Some s -> doesRet s
(**********************************************************************)
(* Check a statement to be sure that it does a splice followed by a   *)
(* return.  This is needed for the codegen, cut, splice, and fill     *)
(* special forms.  Like doesRet, this is a conservative analysis.     *)
(**********************************************************************)

(* QUITE BROKEN -- USER BEWARE *)
and doesSpliceRet {raw_stmt=s} =
  match s with
    Skip -> false
  | Exp _ -> false
  | Seq(s1,s2) -> doesSpliceRet s1 or doesSpliceRet s2
  | Return _ -> false (* return in the wrong context *)
  | IfThenElse(_,s1,s2) -> doesSpliceRet s1 & doesSpliceRet s2
  | While(e,s) -> false (* s may never execute *)
  | Break _ -> false
  | Continue _ -> false
  | For(e1,e2,e3,s) -> false (* s may never execute *)
  | IntSwitch(_,ss,s) ->
      (List.for_all (fun (_,s) -> doesSpliceRet s) ss) & (doesSpliceRet s)
  | CharSwitch(_,ss,s) ->
      (List.for_all (fun (_,s) -> doesSpliceRet s) ss) & (doesSpliceRet s)
  | UnionSwitch(_,ss,sopt) ->
      (List.for_all (fun a -> doesSpliceRet a.arm_body) ss) &
      (doesSpliceRetOpt sopt)
  | ExnSwitch(_,ss,sopt) ->
      (List.for_all (fun a -> doesSpliceRet a.arm_body) ss) &
      (doesSpliceRetOpt sopt)
  | Decl(_,_,_,s) -> doesSpliceRet s
  | Label(_,s) -> false
  | Cut s -> false (* this will be a type error *)
  | Splice s -> doesRet s
  | Do(s,e) -> false
  | TryHandle(s1,_,s2) -> doesSpliceRet s1 & doesSpliceRet s2
  | TryCatchFinally(s1,ss,s2opt,s3opt) ->
      (doesSpliceRet s1) & 
      (List.for_all (fun a -> doesSpliceRet a.arm_body) ss) & 
      (doesSpliceRetOpt s2opt) & (doesSpliceRetOpt s3opt)
  | With (_,_,_,_,s) -> doesSpliceRet s
  | Rdtsc(_,_) -> false
and doesSpliceRetOpt sopt = 
  match sopt with
    None -> true
  | Some s -> doesSpliceRet s
(* End Cyclone *)

(***************************** Unification ***************************)

(* Returns true iff t1 and t2 unify. 
 * Destructive so not suitable for backtracking.
 *)
exception Unify

let unify global_env t1 t2 =
  (* check to see if an Evar occurs in a type, raising Unify if so. *)
  let rec occurs r t =
    match t with
      Evar (_,r') -> 
	if r == r' then raise Unify 
	else (match !r' with None -> () | Some t -> occurs r t)
    | ArrayType(t,_) -> occurs r t
    | FnType(_,_,t,ts) -> occurslist r (t::ts)
    | TupleType (_,ts) -> occurslist r ts
    | NamedType(_,ts) -> occurslist r ts
    | MutableTyp tr -> occurs r (!tr)
(* LR *)
    | RepType(t) -> occurs r t
(* end LR *)
    | ExnconType(t) -> occurs r t
    | _ -> ()
  and occurslist r ts =
    match ts with
      [] -> ()
    | t::ts -> occurs r t; occurslist r ts in
  (* inner loop of unify -- raises Unify if the two types do not unify. *)
  let rec un t1 t2 =
    (* compress the types to get rid of any indirection *)
    let t1 = compress t1 in
    let t2 = compress t2 in
    (* check for structural/physical equality.  This is the common case and
     * in particular, succeeds when we have the same refs for Evars.
     *)
    if t1 == t2 then () else
    match t1,t2 with
    | VoidType,VoidType -> ()
    | Evar(c1,r1),_ ->
	begin match !r1 with
	  (* this case shouldn't really happen due to compression *)
	  Some t1' -> un t1' t2
	  (* as long as r1 doesn't occur in t2, set r1 to t2 *)
	| None -> 
	    begin match c1 with
	      Any -> occurs r1 t2; r1 := (Some t2)
	    | Byte4 -> 
		begin 
		  occurs r1 t2; 
		  match t2 with
		    Evar(Option,_) -> r1 := (Some t2)
		  | Evar(Any,r2)   -> 
		      (match !r2 with
			Some t2' -> un t1 t2' 
		      |	None -> (r2 := (Some t1)))
		  | Evar(Byte4,_)  -> r1 := (Some t2)
		  | _ -> 
                      let k = kind_of_typ global_env t2 in
		      if k <> K4 
		      then raise Unify;
		      r1 := (Some t2)
		end
	    | Option -> 
		begin match t2 with
		  Evar(c2,r2) ->
		    (match c2,!r2 with
		    | Any,_        -> r2 := (Some t1)
		    |   _,None     -> r2 := (Some t1)
		    |   _,Some t2' -> un t1 t2')
		| NamedType(tn,ts) ->
		    if possibly_null global_env !tn then r1 := (Some t2)
		    else raise Unify
		| _ -> raise Unify
		end
	    end
	end
    | _,Evar _ -> un t2 t1
    | VarType(x),VarType(y) -> if x = y then () else raise Unify
    | IntType(b1,s1),IntType(b2,s2) -> 
	if (b1=b2) && (s1=s2) then () else raise Unify
    | BooleanType,BooleanType -> ()
    | StringType, StringType -> ()
    | CharType, CharType -> ()
    | FloatType, FloatType -> ()
    | DoubleType, DoubleType -> ()
    | ArrayType(t1,_),ArrayType(t2,_) -> un t1 t2
    | FnType(c1,[],t1,ts1),FnType(c2,[],t2,ts2) when c1=c2 -> uns (t1::ts1) (t2::ts2)
    | FnType(c1,vs1,t1,ts1),FnType(c2,vs2,t2,ts2) when c1=c2 ->
	let inst = 
	  try List.combine vs1 (List.map (fun v -> VarType v) vs2) 
	with Invalid_argument _-> raise Unify in
	uns (t1::ts1) (type_substs inst (t2::ts2))
    | TupleType(c1,ts1),TupleType(c2,ts2) -> 
	if c1 = c2 then uns ts1 ts2 else raise Unify
    | NamedType(tn1,ts1),NamedType(tn2,ts2) ->
	if !tn1 = !tn2 then uns ts1 ts2 else raise Unify
    | ExnType, ExnType -> ()
(* LR *)
    | RepType(t1),RepType(t2) -> un t1 t2
(* end LR *)
    | ExnconType(t1),ExnconType(t2) -> un t1 t2
    | _,_ -> raise Unify
  and uns ts1 ts2 =
    match ts1, ts2 with
      [],[] -> ()
    | t1::ts1,t2::ts2 -> un t1 t2; uns ts1 ts2
    | _,_ -> raise Unify
  in try un t1 t2; true with Unify -> false
;;

(* Insert a cast. *)
let coerce e t =
  let e_new = { exp_typ = e.exp_typ; 
		raw_exp = e.raw_exp; 
		exp_loc = e.exp_loc;
	        exp_un_after = e.exp_un_after; } 
  in
  e.raw_exp <- Cast(t,e_new);
  e.exp_typ <- Some t;
  ()

let less_unsigned t =
  match compress t with
    IntType (true,_) | CharType -> true
  | _ -> false
;;

let less_float t =
  match compress t with
    IntType _ | CharType -> true
  | _ -> false
;;

let less_double t =
  match compress t with
    IntType _ | CharType | FloatType -> true
  | _ -> false
;;

let coercable t =
  match compress t with
    IntType _ | CharType | FloatType | DoubleType -> true
  | _ -> false
;;

(* Coercions 
 * There are 3 basic coercion schemes used in ANSI C:
 *
 * 1. "the integral promotions:" chars, shorts, enumerated types
 *     are promoted to int. If either argument is unsigned int, convert the
 *     other to unsigned int. 
 *     See K&R A6.1 for definition.
 *     Used in integer operations (bit shifting) subscripting, 
 *     switch statements, etc.  
 *     Implemented here in function unify_coerce_int.
 *
 * 2. "the usual arithmetic conversions:" two arithmetic types 
 *     (ie: char,int(signed/unsigned),float,double) are compared and promoted.
 *     There is a hierarchy: signed int < unsigned int < float < double.
 *     Promote one argument to the type of the higher one in the hierarchy.
 *     Types of size smaller than int are always promoted to signed int.
 *     See K&R A6.5 for definition.
 *     Used in general arithmetic operations (add,subtract,mul,div).
 *     Implemented here in function unify_coerce2.
 * 
 * 3. Any arithmetic type converts with any other arithmetic type in
 *    assignment, as a function argument, as a return value.  These
 *    coercions happen implicitly, even if precision is lost.
 *    Implemented here in function unify_coerce1.
 *
 *)

(* unify two the types of two expressions if possible.  
 * If not attempt to coerce one to the other as per
 * "the usual arithmetic conversions" -- see K&R A6.5 
 * We assume the expressions already have types on them. 
 *)
let unify_coerce2 global_env e1 e2 =
  let t1 = compress (deSome e1.exp_typ) in
  let t2 = compress (deSome e2.exp_typ) in
  let def_coerce e = coerce e (IntType(true,B4)) in
  if (unify global_env t1 t2) then true
  else
    begin
      match t1,t2 with
      |	DoubleType,DoubleType -> true
      |	DoubleType,_ -> 
	  if less_double t2 then (coerce e2 DoubleType; true) else false
      |	_,DoubleType -> 
	  if less_double t1 then (coerce e1 DoubleType; true) else false
      |	FloatType,FloatType -> true
      |	FloatType,_ -> 
	  if less_float t2 then (coerce e2 FloatType; true) else false
      |	_,FloatType -> 
	  if less_float t1 then (coerce e1 FloatType; true) else false
      |	IntType(false,B4),IntType(false,B4) -> true
      |	IntType(false,B4),_ -> 
	  if less_unsigned t2 then (coerce e2 (IntType(false,B4)); true)
	  else true
      |	_,IntType(false,B4) -> 
	  if less_unsigned t1 then (coerce e1 (IntType(false,B4)); true)
	  else true
      |	IntType(b1,s1)   ,IntType(b2,s2)    ->
	  (* Wow C made this really easy.
	     Everything becomes an int.
	     Dave: I hope that was sarcasm. *)
	  if s1 <> B4 then def_coerce e1;
	  if s2 <> B4 then def_coerce e2;	  
	  true
      |	CharType         ,IntType(b2,s2)    ->
	  if s2 <> B4 then def_coerce e2;
	  def_coerce e1;
	  true
      |	IntType(b1,s1)   ,CharType          ->
	  if s1 <> B4 then def_coerce e1;
	  def_coerce e2;
	  true
      |	CharType        ,CharType           ->
	  def_coerce e1;
	  def_coerce e2;
	  true
      |	_ -> false
    end
;;

(* Given a typed expression e, perform "the integral promotions" only:

   K&R rule A.6:  A char, short, enumerated type may be used wherever an 
   integer may be used. If an int can represent all values of the
   original type then the value is converted to an int.  Otherwise
   the value is converted to unsigned int.

   Return true if e can be promoted to an integral type. *)

let unify_coerce_int global_env e =
  let te = compress (deSome e.exp_typ) in
  let unsigned_int = IntType(false,B4) in
  let signed_int = IntType(true,B4) in
  if unify global_env te unsigned_int then true
  else
    begin match te with
      IntType(true,B4) -> true
    | IntType(_,(B1|B2)) -> coerce e signed_int; true 
    | CharType -> coerce e signed_int; true
    | _ -> false
    end

(* attempt to unify type of e with t; 
 * if unsuccessful, promote e up to the arithmetic type t 
 *
 * Note: Currently unused -- Replace unify_coerce1 with this procedure
 * to implement a type checker that does not coerce downwards except
 * at the programmer's indication.
 *)
let unify_coerce_arith global_env e t =
  let te = compress (deSome e.exp_typ) in
  if unify global_env te t then true
  else
    match t with
      CharType -> 
	(match te with
	  IntType(_,B1) -> (coerce e t; true)
	| _ -> false)
    | IntType (true,B4) -> 
	(* already checked for equal to signed int *)
	if less_unsigned te then (coerce e t; true) else false
    | IntType (false,B4) -> 
	if less_unsigned te then (coerce e t; true) else false
    | IntType(b,B2) -> 
	(match te with
	  IntType(_,B1) | CharType -> (coerce e t; true) 
	| _ -> false)
    | IntType(b,B1) ->
	(match te with 
	  CharType -> (coerce e t; true)
	| _ ->  false)
    | FloatType ->
	if less_float te then (coerce e t; true) else false
    | DoubleType ->
	if less_double te then (coerce e t; true) else false
    | _ -> false

(* Given a target type and a typed expression coerce e to the target type *)
(* May be promoted up or down *)
let unify_coerce1 global_env e t =
  let t = compress t in
  let te = compress (deSome e.exp_typ) in
  if unify global_env te t then true
  else
    if coercable t && coercable te then
      (coerce e t; true)
    else (* see if it's a tuple-type *)
      (match t,te with
	TupleType (cap,ts), TupleType (ecap,ets) ->
	  if cap = ReadOnly || cap = ecap then
	    (* XXX might consider deep casts -- that is
	       do a unify_coerce2 of each ts here instead *)
	    if unify global_env (TupleType (ecap,ts)) te then
	      (coerce e t; true)
	    else
	      false
	  else
	    false
      |	_ -> false)

(************************* Constant Folding *************************)
(* This is actually all wrong since int32 isn't really 32 bit *)

let rec optimize_binop p e1 e2 = 
  let default_r = Primop(p,[e1;e2]) in
  match p,e1.raw_exp,e2.raw_exp with
    Plus,Const(Int i1),Const(Int i2) -> Const(Int(i1 +$ i2))
  | Plus,Const(Int i1),r when (i1 = i32_0) -> r
  | Plus,r,Const(Int i1) when (i1 = i32_0) -> r
  | Times,Const(Int i1),Const(Int i2) -> Const(Int(i1 *$ i2))
  | Times,Const(Int i1),r when (i1 = i32_1) -> r
  | Times,r,Const(Int i1) when (i1 = i32_1) -> r
  | Times,r,Const(Int i2) when (i2 >$ i32_1) ->
      let (isp2,log2) = is_power_of_2 (int32_to_int i2)
      in if isp2 then
	Primop(Bitlshift,[e1;{ exp_typ = e2.exp_typ;
			       raw_exp = Const(Int (int_to_int32 log2));
			       exp_loc = e2.exp_loc;
                               exp_un_after = e2.exp_un_after}])
      else default_r
  | Times,Const(Int i2),r -> optimize_binop p e2 e1
  | Minus,Const(Int i1),Const(Int i2) -> Const(Int(i1 -$ i2))
  | Minus,r,Const(Int i2) when (i2 = i32_0) -> r
  | Div,Const(Int i1),Const(Int i2) ->
      if (i2 = i32_0) then
	(terr e2.exp_loc ("compile-time division by zero"); default_r)
      else (Const(Int(i1 /$ i2)))
  | Mod,Const(Int i1),Const(Int i2) -> Const(Int(mod32 i1 i2))
  | Bitand,Const(Int i1),Const(Int i2) -> Const(Int(land32 i1 i2))
  | Bitor,Const(Int i1),Const(Int i2) -> Const(Int(lor32 i1 i2))
  | Bitxor,Const(Int i1),Const(Int i2) -> Const(Int(lxor32 i1 i2))
  | Bitlshift,Const(Int i1),Const(Int i2) -> Const(Int(lsl32 i1 i2))
  | Bitlrshift,Const(Int i1),Const(Int i2) -> Const(Int(lsr32 i1 i2))
  | Bitarshift,Const(Int i1),Const(Int i2) -> Const(Int(asr32 i1 i2))
  | _,_,_ -> default_r

let optimize_unop p e = 
  let default_r = Primop(p,[e]) in
  match p,e.raw_exp with
    Not,Const(Bool b) -> Const(Bool(not b))
  | Bitnot,Const(Int i) -> Const(Int (lnot32 i))
  | Ord,Const(Char c) -> Const(Int (int_to_int32 (Char.code c))) 
  | Chr,Const(Int i) -> Const(Char (Char.chr ((int32_to_int i) land 255)))
  | Size,Const(String s) -> Const(Int (int_to_int32 (String.length s)))
  | _,_ -> default_r

(************************** Default Initialization -- Deprecated **********)

(* returns an expression for a default initializer for a type t (if any) *)
let rec default_initializer global_env t loc un =
  let def_init t = default_initializer global_env t loc un in
  begin
    let err() = terr loc ("declaration of type "^(typ2string t)
			  ^" requires initializer") in
    let abort () = err (); raise Gcdfec.Exit in
    let make_exp re = { exp_typ = None; raw_exp = re; exp_loc = loc;
		        exp_un_after = un; } in
    let exp = 
      match compress t with
      IntType(true,_) -> make_exp(Const (Int i32_0))
    | IntType(false,_) -> make_exp(Cast(t,make_exp(Const (Int i32_0))))
    | BooleanType     -> make_exp(Const (Bool false))
    | StringType      -> make_exp(Const (String ""))
    | CharType        -> make_exp(Const (Char '\000'))
    | FloatType       -> make_exp(Const (Float zero_f32))
    | DoubleType      -> make_exp(Const (Double zero_f64))
    | ArrayType(t,None) ->
        make_exp(ConstArray([],Some t))
    | ArrayType(t,Some e) ->

	let const_array i =
	  let rec nlist idx acc =
	    if idx <= 0 then acc
	    else nlist (idx-1) ((def_init t)::acc) in
	  make_exp(ConstArray(nlist (Numtypes.int32_to_int i) [],Some t)) in
	let fail () = (* non-constant array size *)
          make_exp(FunCall(make_exp(Var "new_array"),
                           ref None, [e;def_init t])) in

	(match e.raw_exp with

	(* integer constant *)
	  Const(Int i) -> const_array i

	(* binop -- see if we can constant-fold it *)
	| Primop(p,([e1;e2] as es)) ->
	    let re = optimize_binop p e1 e2 in
	    (match re with
	      Const(Int i) -> e.raw_exp <- re; const_array i
	    | _ -> fail ())

	(* unop -- see if we can constant-fold it *)
	| Primop(p,[e1]) ->
	    let re = optimize_unop p e1 in
	    (match re with
	      Const(Int i) -> e.raw_exp <- re; const_array i
	    | _ -> fail ())

	(* no good ... *)
	| _ -> fail ())

    | TupleType (ReadOnly,ts) -> 
	make_exp(Cast(TupleType(ReadOnly,ts),
		      make_exp(NewTuple (List.map def_init ts))))
    | TupleType (_,ts)    -> make_exp(NewTuple (List.map def_init ts))
    | NamedType (n,ts) ->
(* let n = !n in
   should suffice but relies on tricky invariants.  So we burn a few
   cycles to cover up our sloppiness. *)
	let n = complete_typ_name global_env (* Dan *)
	    (fun v -> (Dict.member global_env.structs v ||
	               Dict.member global_env.unions  v ||
                       Dict.member global_env.abstracts v))
	    (!n) in
	if (possibly_null global_env n) then 
	  make_exp(Const(Null))
	else 
	  begin try 
	    let sd = Dict.lookup global_env.structs n in
	    let init = List.combine sd.st_tyvars ts in
	    let proc_field (f,c,t) =
	      (None,def_init (type_subst init t))
	    in
	    let es = List.map proc_field sd.st_fields in
	    make_exp (NewStruct (n,ref (Some ts),es))
	  with Dict.Absent ->
	    begin try 
	      let ud = Dict.lookup global_env.unions n in
	      let (field,e_opt) = 
		match ud.un_fields with 
		  [] -> terr loc "Union with no cases."; raise Gcdfec.Exit 
		| (f,t)::tl -> 
		    if t=VoidType then (f,None)
		    else 	      
		      let init = List.combine ud.un_tyvars ts in
		      (f,Some (def_init (type_subst init t)))
	      in
	      make_exp (NewUnion (n,ref (Some ts),field,e_opt))
	    with Dict.Absent -> 
	      terr loc "Abstract types require initializers.";
	      abort ()
	    end
	  end
    | FnType _  -> abort () (* Should initialize with dummy function. *)
    | ExnType   -> abort () (* Should initialize with default exception. *)
(* LR *)
    | RepType(t) -> abort () (* Should init with approp term *)
(* end LR *)
    | _ -> abort () in 
    exp.exp_typ <- Some t;
    exp
  end

(**********************************************************************)
(* The following recursive function typecheck Stmts, Expressions, ... *)
(* given a global environment and an initial function environment.    *)
(**********************************************************************)

(* We now do "doesRet" and definite assignment during the type-checking pass.
   Here is how we deviate from Chapter 16 of the Java spec:
  * We are less permissive on what's assigned after loops:
     Java says whatever is defined after the guard is false and after every
     break is defined after.
     We say whatever is defined always after the guard is defined after.
     To be Java-like we need to remember what's defined at every break.
     An in-between solution would be to notice when the while has no breaks
     and in that case consider what is defined after the guard is false.
 *)

(* Note that in some dead code, everything is assigned.  Eg. return; x; *)

let rec tcStmt global_env env ({raw_stmt = s;stmt_loc = loc} as stmt) =
    
  let tcExp'            = tcExp            global_env in
  let tcStmt'           = tcStmt           global_env in
  let unify'            = unify            global_env in
  let unify_coerce1'    = unify_coerce1    global_env in
  let unify_coerce_int' = unify_coerce_int global_env in
    
  let join_synth synth1 synth2 unreachable = (* Used by If and Switches *)
    let merge_unassigned = (* if both jump, then will be empty *)
      if      unreachable       then mt_varset
      else if synth1.f_stmt_jmp then synth2.f_un_after
      else if synth2.f_stmt_jmp then synth1.f_un_after
      else Varset.union synth1.f_un_after synth2.f_un_after
    in { f_stmt_jmp = (synth1.f_stmt_jmp && synth2.f_stmt_jmp) || unreachable;
	 f_un_after = merge_unassigned;
       }  in

  let decl_synth vs init outer_env s = (* list of vs b/c of tuple patterns *)
    let vset      = Varset.from_list (List.map fst vs) in
    let inner_un  = Varset.diff      (unassigned outer_env) vset in
    let shadows   = Varset.intersect (unassigned outer_env) vset in
    let initer    = if init then add_var_init else add_var_uninit in
    let inner_env = set_unassigned outer_env inner_un in
    let inner_env = 
      List.fold_left (fun env (v,t) -> initer env v t) inner_env vs in
    let synth_s   = tcStmt' inner_env s in
    if synth_s.f_stmt_jmp
    then synth_s
    else { f_stmt_jmp = false; 
	   f_un_after = Varset.union synth_s.f_un_after shadows; }
  in

  let check_bool e_synth e s = (* Used by If and loop forms *)
    (* for now, just exit, but should think about how to reasonable keep going
     * in the presence of definite assignment *)
    if not (unify' BooleanType e_synth.f_typ)
    then (terr e.exp_loc (s ^" argument has type "^(typ2string e_synth.f_typ)
			 ^ " instead of bool");
          raise Gcdfec.Exit) in
  
  stmt.un_before <- unassigned env;

  let ans = 
  match s with
    Skip  -> { f_stmt_jmp = false; f_un_after = unassigned env; }
  | Exp e -> 
      let e_synth = tcExp' env e in
      { f_un_after = un_after_exp e_synth;
	f_stmt_jmp = e_synth.f_exp_raise;
      }	
  | Seq(s1,s2) -> 
      let synth1 = tcStmt' env                                    s1 in
      let synth2 = tcStmt' (set_unassigned env synth1.f_un_after) s2 in
      {	f_stmt_jmp = synth1.f_stmt_jmp || synth2.f_stmt_jmp;
	f_un_after = synth2.f_un_after;
      } 
  | Return(eopt) ->
(* DEBUG 
 (* Dan: When debug is true, we'll miss anything assigned by the e, but that
    shouldn't matter since this is a return anyway
  *)
      if !debug
      then 
	stmt.raw_stmt <-
	   (match eopt with 
	     None   -> Seq(debug_pop(), {raw_stmt=s; stmt_loc=loc});
	   | Some e -> 
	       let t       = retType env   in
	       let synth_e = tcExp'  env e in
	       let t'      = synth_e.f_typ in
	       if not (unify_coerce1' e t)
	       then terr loc ("returns value of type "^(typ2string t')^
			      " but requires " ^(typ2string t));
	       match t with
		 VoidType -> Seq(make_stmt(Exp e), debug_pop())
	       | _ ->
		   let vname = "?R" in 
		   Decl(vname, t, ref (Some e),
		    	make_stmt 
			  (Seq (debug_pop(),
			       {raw_stmt = Return (Some (make_exp (Var vname)));
				stmt_loc = loc}))));
 END DEBUG *)
      (match eopt, compress (retType env) with
	None,VoidType -> jump_synth
      | None,t -> 
	  terr loc ("must return a value of type "^(typ2string t));
	  jump_synth
      | Some e,t ->
	  let synth_e = tcExp' env e in
	  (if not (unify_coerce1' e t) then
	    terr loc ("returns value of type "
		      ^(typ2string synth_e.f_typ)^" but requires "
		      ^(typ2string t)));
	  jump_synth)
  | IfThenElse(e,s1,s2) ->
      let e_synth = tcExp' env e in
      check_bool e_synth e "if";
      let un_true,un_false = un_after_bool e_synth in
      let synth1 = tcStmt' (set_unassigned env un_true)  s1 in
      let synth2 = tcStmt' (set_unassigned env un_false) s2 in
      join_synth synth1 synth2 e_synth.f_exp_raise
  | While(e,s) -> (* Definite assignment not like Java yet; see comment above *)
      let e_synth = tcExp' env e in
      check_bool e_synth e "while";
      let un_true,_ = un_after_bool e_synth in
      tcStmt' (set_inloop (set_unassigned env un_true)) s;
      { f_stmt_jmp = e_synth.f_exp_raise;
	f_un_after = un_after_exp e_synth; (* deviation: as if break immed *)
      }	
  | Break None ->
      (if not (inloop env) then terr loc "break not inside loop");
      jump_synth
  | Break (Some x) ->
      (if not (label_bound env x) then terr loc ("Label "^x^" undefined."));
      jump_synth
  | Continue None ->
      (if not (inloop env) then terr loc "continue not inside loop");
      jump_synth
  | Continue (Some x) ->
      (if not (label_bound env x) then terr loc ("Label "^x^" undefined."));
      jump_synth
  | For(e1,e2,e3,s)-> (* Definite assignment not like Java; see comment above *)
      let synth1 = tcExp' env                                        e1 in
      let synth2 = tcExp' (set_unassigned env (un_after_exp synth1)) e2 in
      check_bool synth2 e2 "2nd for";
      let un_true,_ = un_after_bool synth2 in
      let env_true  = set_inloop (set_unassigned  env un_true) in
      let synth_s = tcStmt' env_true s  in
      let synth3  = tcExp'  env_true e3 in (* deviation: as if continue immed *)
      { f_stmt_jmp = synth1.f_exp_raise || synth2.f_exp_raise;
	f_un_after = un_after_exp synth2;  (* deviation: as if break immed *)
      } 
  | IntSwitch(e,ss,s) ->
      let e_synth = tcExp' env e in
      if not (unify_coerce_int' e) 
	then terr loc ("switch has argument type "^(typ2string e_synth.f_typ)
		       ^" but int cases");
      let leq    (i1,_) (i2,_) = i1 <=$ i2 in
      let equals (i1,_) (i2,_) = i1 =$  i2 in
      (match check_unique leq equals ss with
	None -> ()
      | Some(i,s) -> terr s.stmt_loc ("duplicate case: "^(string_of_int32 i)));
      let env' = set_unassigned env (un_after_exp e_synth) in
	(* order is bad here for error messages (does default first) *)
      List.fold_left
	(fun all_synth (_,s) ->
	  let this_synth = tcStmt' env' s in
	  join_synth all_synth this_synth e_synth.f_exp_raise)
	(tcStmt' env' s)
	ss
  | CharSwitch(e,ss,s) ->
      let e_synth = tcExp' env e in
      if not (unify' CharType e_synth.f_typ)
	then terr loc ("switch has argument type "^(typ2string e_synth.f_typ)
		       ^" but char cases");
      let leq    (i1,_) (i2,_) = i1 <= i2 in
      let equals (i1,_) (i2,_) = i1 =  i2 in
      (match check_unique leq equals ss with
	None -> ()
      | Some(i,s) -> 
	  terr s.stmt_loc ("duplicate character case:"^(Char.escaped i)));
      let env' = set_unassigned env (un_after_exp e_synth) in
	(* order is bad here for error messages (does default first) *)
      List.fold_left
	(fun all_synth (_,s) ->
	  let this_synth = tcStmt' env' s in
	  join_synth all_synth this_synth e_synth.f_exp_raise)
	(tcStmt' env' s)
	ss
  | UnionSwitch(e,ss,def) ->
      (* UnionSwitch's can actually be one of two things:  a switch on a
       * union value or a switch on an exn value.  We can't tell syntactically *)
    let err t = terr loc ("switch has argument type "^(typ2string t)
			  ^" but union cases") in
      
    let rec check_complete fields = (* find missing field for error message *)
      match fields with
	[] -> ()
      | (f,_)::rest ->
	  if List.exists (fun arm -> f = arm.arm_field) ss 
	  then check_complete rest
	  else terr loc ("switch missing case for field "^f) in
    let rec check_exhaustive fields =
	(* check that fields are complete -- assumes default is empty *)
      List.for_all (fun f->List.exists (fun a ->(fst f)=a.arm_field) ss)
	fields in

    begin
      let e_synth = tcExp' env e in
      let env     = set_unassigned env (un_after_exp e_synth) in

      match compress e_synth.f_typ with
	NamedType(u,ts) ->
	  (try
	    let u    = !u in
	    let ud   = Dict.lookup global_env.unions u in
	    let inst = List.combine ud.un_tyvars ts in (* type instantiation *)

	    let leq    a1 a2 = a1.arm_field <= a2.arm_field in
	    let equals a1 a2 = a1.arm_field =  a2.arm_field in
	    (match check_unique leq equals ss with
	      None   -> ()
	    | Some a -> terr a.arm_body.stmt_loc
		  ("duplicate case for constructor "^(a.arm_field)));


	    let tc_arm ({arm_field=f;arm_pat=pat;arm_body=s} as arm) =
	        (* type-check a clause of the switch *)
	      let loc = s.stmt_loc in
	      let t = (try List.assoc f ud.un_fields with
		         Not_found -> 
			   terr loc ("bad switch field "^f); raise Gcdfec.Exit)
	      in
		(* add the pattern to the environment *)
	      let vs =
		(match pat,compress(type_subst inst t) with
		  No_pat,VoidType -> []
		| _,VoidType ->
		    terr loc ("field "^f^" has type void"); raise Gcdfec.Exit
		| No_pat,_ ->
		    terr loc ("field "^f^
			       " has non-void type -- a pattern is required");
		    raise Gcdfec.Exit
		| Prim_pat(Var_pat(x,tref)),t -> 
		    tref := t; [x,t]
		| Prim_pat(Wild_pat tref),t -> tref := t; []
		| Tuple_pat ps,TupleType (_,ts) -> 
		    let vars = 
		      List.fold_left 
			(fun vs p -> 
			  match p with Var_pat(v,_) -> v::vs | _ -> vs) [] ps
		    in (match check_unique (<=) (=) vars with
		      None -> ()
		    | Some x -> 
			terr loc ("duplicate variable in pattern: "^x));
		      begin
			try
			  List.fold_left2 
			    (fun vs p t -> 
			      match p with
				Var_pat(x,tref) -> tref := t; (x,t)::vs
			      |	Wild_pat tref   -> tref := t; vs)
			    [] ps ts
			with Invalid_argument _ -> 
			  (terr loc ("tuple pattern does not match "^
				      typ2string t); raise Gcdfec.Exit)
		      end
		  | _,_ -> (terr loc ("tuple pattern does not match "^
				       (typ2string t)); []))
	      in decl_synth vs true env s
	    in
	    let e_raise = e_synth.f_exp_raise in
	    let arms_synth = 
	      let rec loop arms =
		match arms with
		  []   -> { f_stmt_jmp = e_raise; f_un_after = unassigned env }
		| [arm]  -> tc_arm arm
		| hd::tl -> join_synth (tc_arm hd) (loop tl) e_raise in
	      loop ss
	    in
	    (match def with
	      None ->
		if ud.un_possibly_null then
		  terr loc ("switch on ? type "^u^" requires default");
	(*	if not (check_exhaustive ud.un_fields) then
		  terr loc ("non-exhaustive switch requires default");*)
		check_complete ud.un_fields;
		arms_synth
	    | Some s ->
		let exhaustive = check_exhaustive ud.un_fields in
		(if not ud.un_possibly_null & exhaustive then
		  terr loc ("switch has redundant default"));
		join_synth arms_synth (tcStmt' env s) e_raise)
	  with Dict.Absent -> err (NamedType(u,[])); jump_synth )
	
      | ExnType -> (* the switch is really an exception switch *)
	  stmt.raw_stmt <- ExnSwitch(e,ss,def);
	  tcStmt' env stmt
      | t -> (err t; jump_synth)
    end	
  | ExnSwitch(e,ss,def) ->

      let def = 
	(* If no default and we're switching on a variable, re-raise.
	   With DEBUG, we restore the stack on re-raise.
	   If we have a default, then no stack restoration.
	*)
	match e.raw_exp, def with 
	  Var v, None ->
            let un = unassigned env in
	    let reraise_stmt = make_stmt (Exp(make_exp(Raise e) un)) un in
	    let inserted_default =
	      Some (*
		(if !debug then 
		  make_stmt
		    (Seq (make_call_stmt 
			    "callStackRestore" [make_exp(Var "?old")],
			  reraise_stmt))
		else reraise_stmt) *)
	    reraise_stmt in
	    (stmt.raw_stmt <- ExnSwitch(e, ss, inserted_default);
	     inserted_default)
	| _ -> def 
      in

      let e_synth = tcExp' env e in
      let env     = set_unassigned env (un_after_exp e_synth) in

      (* make sure we're switching something of exn type *)
      (match compress e_synth.f_typ with 
	ExnType -> ()
      | t -> terr loc 
	    ("exception switch has non-exception type "^(typ2string t)));
	  
      (* complete the exception names of the switch arms, and make sure
	 they are all variables of type exncon *)
      let ss = 
	(* replace the field name with the fully qualified one *)
	(* type check and complete the field names *)
	List.map
	  (fun a -> 
	    { a with arm_field = 
	      (try
		complete_exncon_name global_env env a.arm_field 
	      with Unbound ->
		(terr loc ("exception field "^a.arm_field^" is not an exncon");
	         "bogus"))
	    }) ss in

      (* Now rewrite the statement with the completed names *)
      stmt.raw_stmt <- ExnSwitch(e,ss,def);

      (* check that the cases are unique *)
      let leq    a1 a2 = a1.arm_field <= a2.arm_field in
      let equals a1 a2 = a1.arm_field =  a2.arm_field in
      (match check_unique leq equals ss with
	None -> ()
      | Some a -> (terr loc ("exn switch has two cases for "^a.arm_field)));

      (* check a case, adding x to the environment with the appropriate
       * type.  If the exception f carries void, then x shouldn't
       * really be there. *)
      let tc_arm ({arm_field=f;arm_pat=pat;arm_body=s} as arm) =
	(* make sure the arm field name is of type excon 'a; return 'a *)
	let tc_arm_var n = 
          let un = unassigned env in
	  let e_synth = tcExp' env (make_exp (Var n) un) in
	  let t = compress e_synth.f_typ in
	  match t with
	    ExnconType t -> 
	      (* make sure t is closed *)
	      check_valid_type loc global_env [] t; t
	  | _ -> 
	      (terr loc
		("exn switch arm type "^(typ2string t)^" not an exncon"); t) in
	let loc = s.stmt_loc in
	let t = tc_arm_var f in
	let vs =
	  (match pat,t with
	    No_pat,VoidType -> []
	  | _,VoidType ->
	      terr loc ("exception "^f^" carries no type"); 
	      raise Gcdfec.Exit
	  | No_pat,_ ->
	      terr loc ("exception "^f^
			" has non-void type -- an identifier is required");
	      raise Gcdfec.Exit
	  | (Prim_pat(Var_pat (x,tref)),t) -> 
	      tref := t; [x,t]
	  | (Prim_pat(Wild_pat tref),t) -> tref := t; []
	  | Tuple_pat ps,TupleType (_,ts) ->
	      let vars = 
		List.fold_left 
		  (fun vs p -> 
		    match p with Var_pat(v,_) -> v::vs | _ -> vs) [] ps
	      in (match check_unique (<=) (=) vars with
		None -> ()
	      | Some x -> 
		  terr loc ("duplicate variable in pattern: "^x));
	      List.fold_left2 
		(fun vs p t ->
		  match p with
		    Var_pat(x,tref) -> tref := t; (x,t)::vs
		  | Wild_pat(tref)  -> tref := t; vs) 
		[] ps ts
	  | _,_ -> 
	    (terr loc ("exception "^f^" has non-tuple type"); env, []); []) in 
	decl_synth vs true env s in

      let e_raise = e_synth.f_exp_raise in
      let arms_synth = 
	let rec loop arms =
	  match arms with
	    []   -> { f_stmt_jmp = e_raise; f_un_after = unassigned env; }
	  | [arm]  -> tc_arm arm
	  | hd::tl -> join_synth (tc_arm hd) (loop tl) e_raise in
	loop ss
      in
      (match def with
	None -> (* With Dan's hack, only possible when e not a variable *)
	  terr loc "exception switch requires default";
	  arms_synth
      |	Some s -> join_synth arms_synth (tcStmt' env s) e_raise)

  | Decl(x,t,eopt,s) ->
      (* check_valid_type completes type names in t *)
      check_valid_type loc global_env (all_tyvars env) t;

      (* As an intermediate step, we default initialize only variables 
         declared with the form: t x[e].
         We may soon also require that t is some sort of primitive type.
       *)
      (match !eopt,t with
        None,ArrayType(t',Some e') ->
          let e = default_initializer global_env t loc (unassigned env) in
          eopt := Some e;
      | _ -> ());
      let env,init =
        match !eopt with
          None   -> env,false
	| Some e -> 
	    let e_synth = tcExp' env e in
	    (if not (unify_coerce1' e t)
	    then terr loc (x^" declared with type "^(typ2string t)
	      ^" but initializer has type "^(typ2string e_synth.f_typ)));
	    set_unassigned env (un_after_exp e_synth), true
      in
      decl_synth [x,t] init env s

    | Label(x,s) -> (* Deviates from Java as though immediate break *)
	tcStmt' (add_label env x) s;
	{ f_stmt_jmp = false; f_un_after = unassigned env; }
(* Cyclone *)
    | Cut s ->
        begin
          match env with
            Frame(fenv,cenv) ->
              (* set_outloop to prevent break/continue across cut *)
              tcStmt' (set_outloop(Hidden(fenv,cenv))) s;
	      (* Very conservative in presence of Cyclone right now *)
	      { f_stmt_jmp = false; f_un_after = unassigned env; }
          | Hidden _ -> 
	      terr loc "can't cut while codegen is in cut"; jump_synth
          | Outermost _ ->
	      terr loc "cut can only be used within codegen"; jump_synth
        end
    | Splice s ->
        begin
          match env with
            Hidden(fenv,cenv) ->
              (* set_outloop to prevent break/continue across splice *)
              tcStmt' (set_outloop(Frame(fenv,cenv))) s;
	      (* Very conservative in presence of Cyclone right now *)
	      { f_stmt_jmp = false; f_un_after = unassigned env; }
          | Frame _ -> 
	      terr loc "can't splice while already in codegen"; jump_synth
          | Outermost _ ->
	      terr loc "splice can only be used within cut"; jump_synth
        end
(* End Cyclone *)
    | Do(s,e) -> (* deviates from Java *)
	let e_synth = tcExp' env e in (* env b/c body might continue immed *)
	check_bool e_synth e "do-while";
	tcStmt' (set_inloop env) s;
	{ f_stmt_jmp = false; f_un_after = unassigned env; }(* may break immed *)

    | TryHandle(s1,x,s2) ->
(* DEBUG 
	let env = 
	  if !debug
	  then 
	    (let vname = "?Q" in
	    stmt.raw_stmt <- 
	       Decl(vname, IntType(true,B4),
		    ref (Some(make_call "callStackSave" [])), 
		    {raw_stmt = stmt.raw_stmt; stmt_loc = stmt.stmt_loc});
	    s2.raw_stmt <-
	       (let oldname = "?old" in (* used in inserted default *)
	       Decl(oldname, IntType(true,B4),
		    ref(Some (make_call "callStackSave" [])),
		    make_stmt
		      (Seq(make_call_stmt "callStackRestore" 
			     [make_exp (Var vname) (unassigned env)],
			   {raw_stmt = s2.raw_stmt; stmt_loc = s2.stmt_loc}))));
	    add_var_init env vname (IntType(true,B4)))
	  else
	    env in
 END DEBUG *)
	let synth1 = tcStmt' env s1 in
	let synth2 = decl_synth [x,ExnType] true env s2 in
	join_synth synth1 synth2 false
    | TryCatchFinally(s,arms,default,Some _) -> 
	print_string "finally clauses unimplemented";
	raise Unimplemented
    | TryCatchFinally(s,arms,default,None) ->
	(* to simplify my life -- I just translate this into 
	 * a try/handle expression. *)
	(* XXX new_exn_id assumes ?exn naming convention in popcompile ... *)
	let exn_var = new_exn_id() in
	let exn_var_exp = make_exp (Var exn_var) (unassigned env) in
	let switch = 
	  make_stmt (ExnSwitch(exn_var_exp,arms,default)) (unassigned env) in
	let rs = TryHandle(s,exn_var,switch) in
	stmt.raw_stmt <- rs;
	tcStmt' env stmt
    | With(x,r,vs,e,s) ->
	begin
	  let synth_e = tcExp' env e           in
	  let t       = compress synth_e.f_typ in
	  match t with
	    NamedType(n,ts) ->
	      begin
		let n = !n in
		try 
		  let ad   = Dict.lookup  global_env.abstypes n   in
		  let inst = List.combine ad.abs_all_tyvars   ts  in
		  let inst2 = 
		    begin
		      try 
			List.combine ad.abs_exist_tyvars 
			  (List.map (fun v -> VarType v) vs) 
		      with Invalid_argument _ -> 
			terr loc 
			  ("wrong number of abstract type variables for "^n);
			[]
		    end in
                  let inst_t = type_subst (inst @ inst2) ad.abs_defn in
		  r := (Some inst_t);
		  let env = set_unassigned env (un_after_exp synth_e) in
		  let env = add_tyvars env vs in
		  decl_synth [x,inst_t] true env s
		with Dict.Absent -> 
		  (terr e.exp_loc ((typ2string t) ^ " is not an abstype"));
		  jump_synth
		| Invalid_argument v -> 
		    terr loc("duplicate type variable "^v); jump_synth
	      end
	  | _ -> 
	      terr e.exp_loc ((typ2string t) ^ " is not an abstype"); jump_synth
	end
    | Rdtsc(e1,e2) ->
	begin
	  (* Both expressions must be variables. The first will contain the
	     high bits of the counter, the second the low bits.
	     The code generator isn't factored properly to support arbitrary
	     l-values as e1 or e2.
	     
	     XXX - threading of synth is probably incorrect.  Doesn't matter
	     since both variables are assigned to.
	     *)
	     
	  let v1,v2 = match e1.raw_exp,e2.raw_exp with
	    Var v1,Var v2 -> v1,v2
	  | _,_    -> 
	      terr loc ("Both arguments to rdtsc must be variables.");
	      "bogus1","bogus2"
	  in

	  let env_hi = set_unassigned env (Varset.delete (unassigned env) v1) in
	  let env_lo = set_unassigned env (Varset.delete (unassigned env) v2) in

	  let synth_hi = tcExp' env_hi e1 in
       	  let synth_lo = tcExp' env_lo e2 in
	
	  if not (unify_coerce_int' e1) or not (unify_coerce_int' e2) then
	    terr loc ("rdtsc expects two integers arguments, not arguments"^
		      " of type "^(typ2string synth_hi.f_typ)^
		      " and "^ (typ2string synth_lo.f_typ));
	  { f_un_after = 
	    (Varset.delete (Varset.delete (un_after_exp synth_lo) v1) v2);
	    f_stmt_jmp = synth_hi.f_exp_raise or synth_lo.f_exp_raise;
	  }	

	end
  in
  stmt.un_after <- ans.f_un_after;
  ans
  (* end of tcStmt *)
and tcExp global_env env exp =
  let e   = exp.raw_exp in
  let loc = exp.exp_loc in

  let tcExp'           = tcExp            global_env in
  let unify            = unify            global_env in
  let unify_coerce1    = unify_coerce1    global_env in
  let unify_coerce2    = unify_coerce2    global_env in
  let unify_coerce_int = unify_coerce_int global_env in
  let unify_un t1 t2   = (unify t1 t2; ())           in

  let err s   = terr loc s in
  let abort s = terr loc s; raise Gcdfec.Exit in
  let t2s     = typ2string in

  let base t = 
    { f_typ = t; f_exp_raise = false; 
      f_exp_unassigned = Un_Always(unassigned env); } in
  let rec tcExpList env el = 
    (* order matters! *)
    (* Note the caller must check that the elements' types are acceptable *)
    match el with
      []  -> false, unassigned env
    | [e] -> let synth = tcExp' env e in synth.f_exp_raise, un_after_exp synth
    | hd::tl ->
	let hd_synth    = tcExp' env hd in
	let un_after_hd = 
	  if hd_synth.f_exp_raise
	  then mt_varset
	  else un_after_exp hd_synth in
	tcExpList (set_unassigned env un_after_hd) tl
  in
  let tcExpList env el = let x,y = tcExpList env el in x,Un_Always y in
  let synth =
    match e with
      Const(Int _)     -> base (IntType(true,B4))
    | Const(String _)  -> base StringType
    | Const(Char _)    -> base CharType
    | Const(Null)      -> base (Evar(Option,ref None))
    | Const(Float _)   -> base FloatType
    | Const(Double _ ) -> base DoubleType
    | Const(Bool true)  -> 
	{ f_typ = BooleanType; f_exp_raise = false; 
	  f_exp_unassigned = Un_Boolean(unassigned env, mt_varset); }
    | Const(Bool false) -> 
	{ f_typ = BooleanType; f_exp_raise = false; 
	  f_exp_unassigned = Un_Boolean(mt_varset, unassigned env); }
    | ConstArray([],Some t) ->
	(check_valid_type loc global_env (all_tyvars env) t; 
	 base (ArrayType(t,None)))
    | ConstArray([],None) -> abort "empty array has no type"
    | ConstArray((e1::rest) as es,_) ->
	let raises,un_after = tcExpList env es in
	let t = deSome e1.exp_typ in
	List.iter
	  (fun e -> if unify_coerce1 e t then () 
	            else terr e.exp_loc "type mismatch in array constant")
	  rest;
	{ f_typ = ArrayType(t,None); f_exp_raise = raises;
	  f_exp_unassigned = un_after; }
    | Var x ->
	begin try 
	  let t = lookup env x in
	  (if Varset.member (unassigned env) x
	  then err ("Variable "^x^" not definitely assigned\n"));
	  base t
	with Not_found -> 
	  try 
	    let (r,t) = Dict.lookup global_env.globals x
	    in r := true; 
	    rewrite_specials exp;
	    base t
	  with Dict.Absent ->
	    begin
	      let x' = try
		let ge = global_env in
		complete_val_name ge (Dict.member ge.globals) x
	      with Unbound -> raise (UnboundVar (x,loc))
	      in
	      exp.raw_exp <- Var x';
	      let (r,t) = Dict.lookup global_env.globals x' in
	      r := true; 
	      rewrite_specials exp;
	      base t
	    end
	end
	
    | Primop(p,([e1;e2] as es)) ->
	(* Unlike Java, the only binops on bools are == and !=
	 * (The parser expands && and || into Conditionals and there's no 
	 * ^ or | or & on booleans)
	 * For == and != on booleans we do nothing special yet, unlike Java
	 * so for example, true == true will behave differently.
	 *)
	let raises,un_after = tcExpList env es in
	let p,t = tcBinPrimop global_env env loc p e1 e2 in
	let r = optimize_binop p e1 e2 in
	exp.raw_exp <- r;
	{ f_typ = t; f_exp_raise = raises;
	  f_exp_unassigned = un_after (* Deviation here! *)
	} 
    | Primop(p,[e]) ->
	let e_synth = tcExp' env e in
	let t = tcUnPrimop global_env env loc p e in
	let r = optimize_unop p e in
	exp.raw_exp <- r;
	(match p with
	  Not ->
	    { e_synth with
              f_exp_unassigned =
	      match e_synth.f_exp_unassigned with
		Un_Always vs -> failwith "Not subterm should be a boolean!"
	      |	Un_Boolean(vs1,vs2) -> Un_Boolean(vs2,vs1);
	    } 
	| _ -> { e_synth with f_typ = t; })
    | Primop(p,[]) ->
	(match p with
	  PiF | Log2_eF | Log2_10F | Log10_2F | Loge_2F ->
	    base DoubleType
	| _ -> abort "primop not nullary.")
    | Primop(_,_) -> abort "primop wrong # of args"

    | Conditional(e1,e2,e3) -> (* gotta do this well b/c it encodes && and || *)
	let synth1 = tcExp' env e1 in
	let t1     = synth1.f_typ in
	if not (unify BooleanType t1)
	then err ("conditional argument has type "^(t2s t1)
		  ^" instead of bool");
	let un_true,un_false = un_after_bool synth1 in
	let synth2 = tcExp' (set_unassigned env un_true)  e2 in
	let synth3 = tcExp' (set_unassigned env un_false) e3 in
	let t2     = synth2.f_typ in
	let t3     = synth3.f_typ in
	let t      = 
	  if unify_coerce2 e2 e3 then (deSome e2.exp_typ)
	  else (abort ("clauses of conditional do not match type: "
		       ^(t2s t2)^" != "^(t2s t3))) in
	let un_merge =
	  if unify BooleanType t
	  then 
	    match synth2.f_exp_unassigned,synth3.f_exp_unassigned with
	      Un_Boolean(unt1,unf1),Un_Boolean(unt2,unf2) ->
		Un_Boolean(Varset.union unt1 unt2, Varset.union unf1 unf2)
	    | _ -> failwith "Boolean conditional subterms should be boolean!"
	  else
	    Un_Always (Varset.union (un_after_exp synth2) (un_after_exp synth3))
	in
	{ f_typ = t;
	  f_exp_raise = synth1.f_exp_raise || 
	                 (synth2.f_exp_raise && synth3.f_exp_raise);
	  f_exp_unassigned = un_merge;
        } 

    | AssignOp(e1,po,e2) ->
	(* Note that as in Primop, we violate Java by not considering
	   various boolean operators to have meaning.
	 *)
	(* must check lhs before rhs, but if lhs is a Var, and there's no Op,
           then the Var may be undefined. *)
	let env_lhs = 
	  match e1.raw_exp,po with
	    Var v,None -> set_unassigned env (Varset.delete (unassigned env) v)
	  | _          -> env in
	let synth_lhs = tcExp' env_lhs e1 in
	let t1        = synth_lhs.f_typ   in
	check_valid_lhs global_env env e1;
	
	let env_rhs =
	  match e1.raw_exp with
	    Var v -> env
	  | _     -> set_unassigned env (un_after_exp synth_lhs) in
	let synth_rhs = tcExp' env_rhs e2 in
	let t2        = synth_rhs.f_typ   in
	
	let un_after =
	  match e1.raw_exp with
	    Var v -> Varset.delete (un_after_exp synth_rhs) v
	  | _     -> un_after_exp synth_rhs in

	begin match po with
	  None -> 
	    if not (unify_coerce1 e2 t1)
	    then err ("type mismatch: "^(t2s t1)^" != "^(t2s t2));
	| Some p -> 
	    let (p',t_result) = tcBinPrimop global_env env loc p e1 e2 in
	    if not (unify t_result t1 or coercable t_result)
	    then err "Cannot use this operator in front of an assignment.";
	    if p'<>p then exp.raw_exp <- (AssignOp (e1,Some p',e2));
	end;
	{ f_typ            = t1;
	  f_exp_raise      = synth_lhs.f_exp_raise || synth_rhs.f_exp_raise;
	  f_exp_unassigned = Un_Always un_after;
        } 
    | FunCall(e,ts,es) ->
	let raises,un_after = tcExpList env (e::es) in
	begin match compress(deSome e.exp_typ) with
	  FnType(c,vs,rt,argts) ->
		(* inst is the instantiation of the bound type variables vs.
		 * we substitute Evar's for them to be unified.  *)
	    let inst = List.map (fun v -> (v,Evar(Byte4,ref None))) vs in
	    let rt = (type_subst inst) rt in
	    let argts = List.map (type_subst inst) argts in
	    (try
	      (match !ts with
		None -> (* implicit instantiation *)
		  ts := Some(List.map snd inst)
	      | Some ts -> (* explicit instantiation *)
		  List.iter2 unify_un ts (List.map snd inst));
	      (List.iter2
		 (fun e t ->
		   if not (unify_coerce1 e t)
		   then terr e.exp_loc ("argument type mismatch.  "^
					"actual has type "
					^(t2s (deSome e.exp_typ))
					^" but formal has type "
					^(t2s t)))
		 es argts);
	      rewrite_specials exp;
	      { f_typ            = rt;
		f_exp_raise      = raises;
		f_exp_unassigned = un_after;
	      }	
	    with Invalid_argument _ -> abort "argument number mismatch")
	| t -> abort ("attempt to apply non function type " ^(t2s t))
	end
(* LR *)
    | TypInst ({ raw_exp = RepTerm },ts) ->
	(match ts with
	  [t] -> 
	    (* check type t in the empty type var environment
	       to make sure that it's closed *)
	    check_valid_type loc global_env [] t;
	    base (RepType (compress t))
	| ts -> abort ("RepTerm has more than one type arg: "^
		       (String.concat ", " (List.map t2s ts))))
    | RepTerm ->
	abort "RepTerm has no type arguments"
(* end LR *)
    | TypInst (e,ts) ->
	(* MWH 6/14/00 -- changed this to descend into tuple types
	   to find a function to instantiate.  Facilitates passing
	   polymorphic functions "by reference."  Due to limitations
	   in the TAL typechecker, we only allow looking into singleton
	   tuples. *)
	let rec inst_type t =
	  (match t with
	    FnType(c,vs,rt,argts) ->
	      let vs,inst = 
		let rec aux vs ts inst =
		  match vs,ts with
		    vs,[] -> (vs,inst) (* Allows partial instantiation *)
		  | [],_  -> abort "instantiation with too many arguments"
		  | hd1::tl1,hd2::tl2 -> aux tl1 tl2 ((hd1,hd2)::inst)
		in aux vs ts []
	      in
	      let rt = (type_subst inst) rt in
	      let argts = List.map (type_subst inst) argts in
	      let new_t = FnType(c,vs,rt,argts) in 
	      check_valid_type loc global_env (all_tyvars env) new_t;
	      new_t
	  | TupleType (c,[t']) ->
	      if c = ReadOnly then
		TupleType (c,[(inst_type t')])
	      else
		abort 
		  ("attempt to instantiate under non-readonly tuple"^(t2s t))
	  | TupleType (c,ts) ->
	      abort 
		("attempt to instantiate under non-singleton tuple type "^
	         (t2s t))
	  | t -> 
	      abort ("attempt to instantiate non function type "^(t2s t))) in
	let synth_e = tcExp' env e in
	{ synth_e with f_typ = inst_type (compress synth_e.f_typ); }
  
    | NewStruct(n,ts,es) ->
	    (* The parser does not distinguish NewExn or NewAbstype from 
               NewStruct so we have to make this distinction here.  If a 
               struct and an exception/abstype have the same name we will 
               consider that an error.
	     *)
	begin 
	  let is_exn = ref false in
	  let name =
	    (try
	      (* will only return if n is an exception *)
	      let n' = complete_exncon_name global_env env n in
	      is_exn := true;
	      n'
	    with Unbound ->
	      (try 
		complete_typ_name global_env (Dict.member global_env.structs) n
	      with Unbound ->
		(try 
                  complete_typ_name global_env 
                    (Dict.member global_env.abstypes) n
	      with Unbound -> 
		abort (n ^ " refers to neither a struct nor an exception."))))
	  in

	match (Dict.member global_env.structs name,
	       Dict.member global_env.abstypes name,
	       !is_exn)
	with
	  (true,true,true) -> 
	    abort ("Ambiguous new (struct or exception or abstype) " ^ n)
	| (true,_,true) -> 
	    abort ("Ambiguous new (struct or exception) " ^ n)
	| (_,true,true) ->
	    abort ("Ambiguous new (exception or abstype) " ^ n)
	| (true,true,_) ->
	    abort ("Ambiguous new (struct or abstype) " ^ n)
	| (false,false,false) -> 
	     (* Dan bug fix -- occurs if n is an opened union or abstract *)
	     (* Was thought to be Impossible *)
	    abort (n ^ " refers to neither a struct, abstypes, nor exception.")
	| (false,false,true) ->
	   (* It is a NewExn *)
	    let eopt =
	      match (!ts,es) with
		(None,[])  -> None
	      | (None,[(None,e)]) -> (Some e)
	      | (Some _,_) ->
		  abort (n^" is an exception. Type arguments are not allowed.")
	      | _ ->
		  abort (n^" is an exception. Too many arguments")
	    in
	    exp.raw_exp <- NewExn (name,eopt);
	    tcExp' env exp

	| (false,true,false) ->
	   (* It is a NewAbstype *)
	    let e = 
	      match es with
		[(None,e)] -> e
	      | [] -> abort (n^" is an abstype -- an argument is required")
	      | _ ->  abort (n^" is an abstype") in
	    exp.raw_exp <- NewAbstype(name,ts,ref None,e);
	    tcExp' env exp

	| (true,false,false) ->
 	  (* It is a NewStruct. *)
	    begin try
	      (* update with the full name *)
	      exp.raw_exp <- NewStruct (name,ts,es);
	      let sd = Dict.lookup global_env.structs name in
	      (* determine the type instantiation *)
	      let inst = List.map (fun v->v,Evar(Byte4,ref None)) sd.st_tyvars in
	      begin match !ts with
		None -> ts := Some(List.map snd inst)(* implicit instantiation *)
	      | Some ts ->  (* explicit instantiation *)
		  (try List.iter2 unify_un ts (List.map snd inst)
		  with Invalid_argument _ ->
		    abort "Explicit instantiation has wrong # of args")
	      end;
	      (* type-check before shuffling fields b/c of definite assignment *)
	      let stripped_es = List.map snd es in
	      let raises,un_after = tcExpList env stripped_es in

	      (* if there are labels, make sure they match up properly *)
	      let es = 
		match es with
		  [] -> []
		| [(_,e)] -> [e]
		| (None,e)::rest ->
			(* the first expression has no label, so none of them
			 * should and the order is implied to be the order
			 * in which the fields are declared in the struct *)
		    let rec check_none e = 
		      match e with
			(None,e) -> e
		      | (Some _,e)-> 
			  terr e.exp_loc
			    "bad initialization expression for struct"; e
		    in e::(List.map check_none rest)
		| (Some f,e)::rest ->
			(* the first expression has a label, so all of them
  		         * should have labels.  All of the labels should
			 * represent fields and vice versa.  We return the
			 * list of associated expressions in the order in
			 * which the fields are declared in the struct to
			 * match up with the case where there are no labels.*)
		    let fs = List.map (fun (f,_,x) -> f) sd.st_fields in
		    let check fe = 
		      match fe with 
			(Some f,e) -> 
			  if List.mem f fs then (f,e)
			  else (terr e.exp_loc
				  ("struct "^n^" has no "^f^" field");
				(f,e))
		      | (None,e) -> 
			  terr e.exp_loc
			    "bad initialization expresson for struct";
			  ("*bogus*",e) 
		    in
		    let fes = List.map check es in
		    let find f = 
		      try List.assoc f fes 
		      with Not_found ->
			(terr e.exp_loc("struct "^n^" missing field "^f);
			 raise Gcdfec.Exit) 
		    in
		    List.map find fs 
	      in
	      let checkexp e (f,_,t) =
		(*already type-checked e, now just check it's the correct type*)
		let t = type_subst inst t in
		if not (unify_coerce1 e t)
		then terr e.exp_loc
		    ("struct "^n^", field "^f^" has type "^(t2s t)^
		     " but argument has type "^(t2s (deSome e.exp_typ)))
	      in
	      (try List.iter2 checkexp es sd.st_fields
	      with Invalid_argument _ -> 
		abort ("struct "^n^" argument # mismatch"));
	      { f_typ            = NamedType(ref name,List.map snd inst);
		f_exp_raise      = raises;
		f_exp_unassigned = un_after;
	      }	
	    with
	      Dict.Absent -> failwith "poptype.ml:NewStruct:Impossible!"
	    end
	end

    | StructMember(e,f) ->
	let synth_e = tcExp' env e in
	begin match compress synth_e.f_typ with
	  NamedType(n,ts) ->
	    begin try
	      let n = !n in
	      let sd = Dict.lookup global_env.structs n in
	      let rec aux fs =
		(match fs with
		  ((f',_,t)::rest) ->
		    if f = f' then
		      let inst = List.map2 (fun v t -> (v,t)) sd.st_tyvars ts in
		      { synth_e with f_typ = type_subst inst t; }
		    else aux rest
		| [] -> abort ("struct "^n^" has no "^f^" field"))
	      in
	      aux sd.st_fields
            with Dict.Absent -> 
	      exp.raw_exp <- UnionMember(e,f);
	      tcExp' env exp
	    end
	| t -> abort ((t2s t)^" not a struct or union.")
	end 

    | UnionMember(e,f) ->
	let synth_e = tcExp' env e in
	begin match compress synth_e.f_typ with
	  NamedType(n,ts) ->
	    begin try 
	      let n = !n in
	      let ud = Dict.lookup global_env.unions n in
	      let rec aux fs =
		match fs with
		  (f',t)::rest -> 
		    if f = f' then
		      let inst = List.map2 (fun v t -> (v,t)) ud.un_tyvars ts in
		      (match type_subst inst t with
			VoidType -> abort ("union projection has void type")
		      |        t -> { synth_e with f_typ = t; })
		    else aux rest
		    | [] -> abort ("union "^n^" has no "^f^" field")
		  in 
		  aux ud.un_fields
		with Dict.Absent -> 
		  abort (!n ^ " not a struct or union!")
		end
	| t -> abort ((t2s t)^" not a struct or union type")
	end
  
    | NewUnion(n,ts,f,eopt) ->
	let name =
	  try 
	    if n <> "" then 
	      complete_typ_name global_env (Dict.member global_env.unions) n
	    else (* Use the union_fs to figure out what the name should be. *)
	      begin
		if Dict.member global_env.union_fs f then
		  (match Dict.lookup global_env.union_fs f with
		  | Some ud -> ud.un_name 
		  | None -> abort ("The field name "^f^" is ambiguous"))
		else abort (f^" is not the name of a union field.")
	      end
	  with Unbound -> abort (n^" is not a union")
	in
	exp.raw_exp <- NewUnion (name,ts,f,eopt);
	begin try
	  (* similar to NewStruct *)
	  let ud    = Dict.lookup global_env.unions name in
	  let inst  = List.map (fun v->(v,Evar(Byte4,ref None))) ud.un_tyvars in
	  let evars = List.map snd inst in
	  let rty   = NamedType(ref name,evars) in
	  (match !ts with
	    None -> ts := Some(evars)
	  | Some ts -> 
	      (try List.iter2 unify_un evars ts
	      with Invalid_argument _ -> 
		abort "Explicit instantiation has wrong # of arguments"));
	  let t = 
	    try type_subst inst (List.assoc f ud.un_fields)
	    with Not_found -> 
	      let udstr = topdecl2string (UnionDecl ud,dummy_location) in
	      abort ("union field "^f^" not found in "^udstr) in
	  begin match compress t,eopt with
	    VoidType,None -> base rty
	  | _,None -> abort ("union "^n^", field "^f^
			     " requires argument of type "^(t2s t))
	  | _,Some e ->
	      let synth_e = tcExp' env e in
	      (if unify_coerce1 e t then ()
	      else err ("union "^n^", field "^f^ " requires argument of type "
			^(t2s t)^" not "^(t2s (synth_e.f_typ))));
	      { synth_e with f_typ = rty; }
	  end
	with
	  Dict.Absent -> abort(n^" is not a union type")
	| Not_found   -> abort("union "^n^" has no "^f^" field")
	end

    | NewTuple es -> 
	let raises,un_after = tcExpList env es in
	{ f_typ = TupleType(ReadWrite,
			    List.map (fun e -> deSome e.exp_typ) es);
	  f_exp_raise = raises;
	  f_exp_unassigned = un_after;
	} 
  
    | TupleMember(e,i) ->
	let synth_e = tcExp' env e in
	(match compress synth_e.f_typ with
	  TupleType (c,ts) ->
	    (try { synth_e with f_typ = List.nth ts (i - 1) } with
	      Invalid_argument _ -> 
		if i=0 then abort ("First element of tuple is 1 not 0")
		else abort ("Invalid offset of tuple")
	    | Failure _ ->
		let num_fields = string_of_int (List.length ts) in
		abort("tuple has "^num_fields^" fields"))
	| Evar _ ->
	    abort ("cannot determine # of fields in tuple type")
	| t -> abort ("tuple projection applied to "^(t2s t)^" value"))
    | NewAbstype(n,all_ts_ref,ex_ts_ref,e) ->
	let synth_e = tcExp' env e in
	let ad = Dict.lookup global_env.abstypes n in 
	let inst1 = 
	  List.map (fun v -> (v,Evar(Byte4,ref None))) ad.abs_all_tyvars   in
	let evars1 = List.map snd inst1 in
	let inst2 = 
	  List.map (fun v -> (v,Evar(Byte4,ref None))) ad.abs_exist_tyvars in
	let evars2 = List.map snd inst2 in
	let rty = NamedType(ref n,evars1) in
	let inst = inst1 @ inst2 in
	(match !all_ts_ref with
	  None -> all_ts_ref := (Some evars1)
	| Some ts ->
	    (try List.iter2 unify_un evars1 ts
	    with Invalid_argument _ ->
	      abort "Explicit instantiation has wrong # of arguments"));
	(match !ex_ts_ref with
	  None -> ex_ts_ref := (Some evars2)
	| Some ts ->
	    (try List.iter2 unify_un evars2 ts
	    with Invalid_argument _ ->
	      abort "Explicit instantiation has wrong # of arguments"));
	let t2 = type_subst inst ad.abs_defn in
	if not (unify_coerce1 e t2) then
	  terr e.exp_loc
	    ("expression has type "^(t2s synth_e.f_typ)^" <> "^(t2s t2)); 
	{ synth_e with f_typ = rty }

    | Subscript(e1,e2) ->
	begin
	  let raises,un_after = tcExpList env [e1;e2] in
	  if not (unify_coerce1 e2 (IntType(false,B4))) (* XXX *)
	  then terr e2.exp_loc 
	      ("subscript type "^(t2s (deSome e2.exp_typ))^" not an int");
	  let t1 = deSome e1.exp_typ in
	  match compress t1 with
	    StringType -> 
	      { f_typ=CharType; f_exp_raise=raises; f_exp_unassigned=un_after;}
	  | _ ->
	      let t = Evar(Any,ref None) in
	      if unify (ArrayType(t,None)) t1 
	      then { f_typ=t; f_exp_raise=raises; f_exp_unassigned=un_after;}
	      else (terr e1.exp_loc
		      ("subscript on non-array/string type "^(t2s t1));
		    raise Gcdfec.Exit)
	end
	
    | NewExn(id,eopt) ->
	    (* id is completed when converting from NewStruct to
	       NewExn!!! However sometimes we parse NewExn's directly so
	       we still need to complete the name. *)
	begin try
	  let name = 
	    try
	      complete_exncon_name global_env env id 
	    with Unbound -> 
	      (terr loc ("exception name "^id^" is not an exncon");
	       "bogus")
	  in
	  exp.raw_exp <- NewExn(name,eopt);	      
	  let t1 =
            let un = unassigned env in
	    let e_synth = tcExp' env (make_exp (Var id) un) in
	    let t = compress e_synth.f_typ in
	    match t with
	      ExnconType t -> t
	    | _ -> (terr loc
		("exn switch arm type "^(typ2string t)^" not an exncon"); t) in
	  begin match compress t1,eopt with
	    VoidType,None -> base ExnType
	  | t,Some e ->
	      let synth_e = tcExp' env e  in
	      let t2      = synth_e.f_typ in
	      if not (unify_coerce1 e t)
	      then err ("exception "^id^" requires "^(t2s t)^"!="^(t2s t2));
	      { synth_e with f_typ = ExnType; }
	  | _,None -> 
	      err ("exception "^id^" requires "^(t2s t1)^" value");
	      base ExnType
	  end;
        with Dict.Absent -> 
	  failwith("Impossible: Bound exception without type.")
	end
	
    | Raise e ->
	let t = (tcExp' env e).f_typ in
	if not (unify t ExnType)
	then err ("expected type exn found type " ^ (t2s t));
	{ f_typ = Evar(Any,ref None); 
	  f_exp_raise = true; 
	  f_exp_unassigned = Un_Always mt_varset; }

    | SeqExp es ->
	let raises,un_after = tcExpList env es in
	let last_t =
	  let rec aux es = 
	    match es with
	      []     -> abort ("Impossible. Sequence exp with no expressions.")
	    | [hd]   -> deSome (hd.exp_typ)
	    | hd::tl -> aux tl
	  in aux es
	in
	{ f_typ = last_t; f_exp_raise = raises; f_exp_unassigned = un_after; }

    | Nop -> base VoidType
		   
    | Cast(t,e) -> 
	let synth_e = tcExp' env e in

	(* first check if both are numeric types *)
	if (coercable t && coercable synth_e.f_typ) then 
	  { synth_e with f_typ = t; }

	(* check if this is from tuple to constant tuple *)
	else
	  let te = compress synth_e.f_typ in
	  (check_valid_type loc global_env (all_tyvars env) t;
	  (* let t = compress t in *)
	   match t,te with
	    TupleType (cap,ts), TupleType (ecap,ets) ->
	      if cap = ReadOnly || cap = ecap then
		(* XXX might consider deep casts -- that is
		   do a unify_coerce2 of each ts here instead *)
		if unify t (TupleType (cap,ets)) then
		  { synth_e with f_typ = t }
		else
		  abort (Printf.sprintf "Cannot cast from %s to %s"
			   (t2s te) (t2s t))
	      else
		abort (Printf.sprintf 
			 "Cannot cast read-only type %s to read-write type %s"
			 (t2s te) (t2s t))
	  | _,_ -> abort "Cast is only allowed on integral types")

(* Cyclone *)
    | Codegen fd ->
	    (* Notice we don't track any definite assignment under a codegen *)
	    (* I.e. we're very conservative *)
        let env' = Frame(fndecl2f_env fd, env) in
	let _ = check_fundecl_valid_type global_env loc fd in
        tcFunDecl global_env env' loc fd;
        base (FnType(fd.fn_convention,
		     fd.fn_tyvars,fd.fn_ret_type,List.map snd fd.fn_args))
    | Fill e ->
        begin
          match env with
            Frame(fenv,cenv) -> 
		  (* Very conservative w.r.t. outer frame *)
	      let t = (tcExp' (Hidden(fenv,cenv)) e).f_typ in
	      base t
          | Hidden _ ->
              abort "fill cannot be used while codegen is in cut"
          | Outermost _ ->
	      abort "fill can only be used within codegen"
        end
(* End Cyclone *)
    | Fun fd ->
	    (* here, we rename the function to a unique name (substituting
	     * for it appropriately within the body of the function) and
	     * then type-check the function as if it was defined globally.
	     * In particular, we add the function to the global environment
	     * so that nested functions may call it.  The function definition
	     * gets placed on a global list (inner_functions) and is replaced
	     * with (Var new_name).   Renaming first will give worse error
             * messages but seems necessary to avoid a separate pass to
             * extract nested functions. *)
	let t = check_fundecl_valid_type global_env loc fd in
	let new_name = new_inner_fn_id fd.fn_name in
	subst_stmt new_name fd.fn_name fd.fn_body;
	let new_fd = { fn_static = false;
		       fn_convention = fd.fn_convention;
		       fn_name = new_name;
		       fn_tyvars = fd.fn_tyvars;
		       fn_ret_type = fd.fn_ret_type;
		       fn_args = fd.fn_args;
		       fn_body = fd.fn_body (* note subst above *)
		     } in
        let env' = Outermost(fndecl2f_env new_fd) in
	    (* necessary so that nested functions can refer to this one *)
	let global' = add_global global_env new_name t in
        tcFunDecl global' env' loc new_fd;
	let td = (FunDecl new_fd, loc) in
	inner_functions := td :: (!inner_functions);
	exp.raw_exp <- (Var new_name);
	base t
  in
  let synth = (* outer context may expect Un_Boolean *)
    (* unification wrong b/c may be unconstrained!!! *)
    if BooleanType == compress synth.f_typ 
    then
      match synth.f_exp_unassigned with
	Un_Always vs -> { synth with f_exp_unassigned = Un_Boolean(vs,vs) }
      |	_            -> synth
    else
      synth
  in
  (exp.exp_typ <- Some synth.f_typ; 
   exp.exp_un_after <- un_after_exp synth;
   synth);
  
  (* end of tcExp *)
and tcBinPrimop global_env env loc p e1 e2 =
  begin
    let t1 = deSome e1.exp_typ in
    let t2 = deSome e2.exp_typ in 
    let err = terr loc in
    let unify_coerce2 = unify_coerce2 global_env in
    let abort s = terr loc s; raise Gcdfec.Exit in
    match p with
      (Plus | Times | TimesU | Minus | Div | DivU | Mod | ModU | Bitand | 
      Bitor | Bitxor | Bitlshift | Bitlrshift | Bitarshift |
      PlusF | MinusF | TimesF | DivF) ->
	 if not (unify_coerce2 e1 e2)
	 then err "operator applied to non-numeric type";
	 let t = compress (deSome e1.exp_typ) in
	 if not (coercable t)
	 then err "operator applied to non-numeric type";
	 begin match t with 
	   IntType(false,_) ->
	     begin match p with
	       Times        -> (TimesU,t) 
	     | Div          -> (DivU  ,t)
	     | Mod          -> (ModU  ,t)
	     | _            -> (p     ,t)
	     end
	 | FloatType | DoubleType ->
	     begin match p with
	       Plus   -> (PlusF ,t)
	     | Minus  -> (MinusF,t)
	     | Times  -> (TimesF,t)
	     | TimesU -> (TimesF,t)
	     | Div    -> (DivF  ,t)
	     | DivU   -> (DivF  ,t)
	     | PlusF | MinusF | TimesF | DivF -> (p,t)
	     | _      -> 
		 (* bitwise operations do not apply to floats *)
		 err "operator does not apply to floating type"; 
		 raise Gcdfec.Exit
	     end
	 | _                -> (p     ,t)
	 end
    | (Gt | GtU | Lt | LtU | Gte | GteU | Lte | LteU
    | GtF | GteF | LtF | LteF) ->
	if not (unify_coerce2 e1 e2)
	then err "comparison applied to non-numeric type";
        let t = compress (deSome e1.exp_typ) in
	if not (coercable t)
	then err "operator applied to non-numeric type";
	let p =
	  match t with
	    IntType(false,_) ->
	      (match p with
		Gt -> GtU | Lt -> LtU | Gte -> GteU | Lte -> LteU | p -> p)
	  | FloatType | DoubleType ->
	      (match p with
		Eq | EqF -> EqF 
	      | Neq | NeqF -> NeqF 
	      | Gt | GtU | GtF -> GtF 
	      | Lt | LtU | LtF -> LtF
	      |	Gte | GteU | GteF -> GteF 
	      | Lte | LteU | LteF -> LteF 
	      | _ -> failwith "tcBinPrimop: impossible")
	  | _ -> p
	in
	(p,BooleanType)
    | (Eq | Neq | EqF | NeqF) ->
	if not (unify_coerce2 e1 e2)
	then err ("comparison of two types: "^(typ2string t1)^" != "
		       ^(typ2string t2));
	let p =
	  match compress (deSome e1.exp_typ) with
	    FloatType | DoubleType ->
	      (match p with
		Eq | EqF -> EqF
	      |	Neq | NeqF -> NeqF
	      |	_ -> failwith "tcBinPrimop: impossible")
	  | _ -> p in
	(p,BooleanType)
    | AtanF | FremF | Fyl2xF | Fyl2xp1F  -> 
	if not (unify_coerce1 global_env e1 DoubleType)
	    then err ("Could not coerce " ^(typ2string t1)^" to double.");
	if not (unify_coerce1 global_env e2 DoubleType)
	    then err ("Could not coerce " ^(typ2string t2)^" to double.");
	(p,DoubleType)
    | _ -> abort ("wrong # of args to primop");
  end
and tcUnPrimop global_env env loc p e =
  begin
    let t = deSome e.exp_typ in
    let err = terr loc in
    let abort s = terr loc s; raise Gcdfec.Exit in
    let unify_t = unify global_env t in
    let unify_coerce1 = unify_coerce1 global_env in
    match p with
      Not ->
	if not (unify_t BooleanType)
	then err ("! requires bool <> "^(typ2string t));
	BooleanType
    | Bitnot ->
	if not (unify_coerce1 e (IntType(true,B4)))
	then err ("bitwise negation requires int <> "^(typ2string t));
	IntType(true,B4)
    | Size ->
	begin match compress t with
	  StringType -> ()
	| ArrayType _ -> ()
	| _ -> err ("size requires string/array <> "^(typ2string t))
	end;
	IntType(true,B4)
    | Ord ->
	if not (unify_t CharType)
	then err ("ord requires char <> "^(typ2string t));
	IntType(true,B4)
    | Chr ->
	if not (unify_coerce1 e (IntType(true,B4)))
	then err ("chr requires int <> "^(typ2string t));
	CharType
    | AddrOf ->
	let rec aux e =
	  match e.raw_exp with
	    Var v -> ()
	  | StructMember (e2,f) -> aux e2
	  | TupleMember (e2,i) -> aux e2
	  | _ -> abort "& applied to invalid expression" in
	(match e.raw_exp with
	  Var v -> (* should have been completed (if nec.) by caller *)
	    if Dict.member global_env.globals v then
	        (* & is a no-op on function types and exception decls *)
	      (match t with 
		FnType(_,_,_,_) -> 
		  if Dict.member global_env.functions v then t
		  else TupleType (ReadWrite,[t])
		| ExnconType _ ->
		    if Dict.member global_env.exceptions v then t
		    else TupleType (ReadWrite,[t])
		| _ -> TupleType (ReadWrite,[t]))
	    else
	      abort "& only valid on global variables"
	| StructMember (e2,f) ->
	    let _ = aux e2 in (* will abort if not rooted globally *)
	    (match e.exp_typ with
	      Some t -> TupleType (ReadWrite,[t])
	    | None -> failwith "poptype.ml: AddrOf---e was not typechecked")
	| TupleMember (e2,i) ->
	    let _ = aux e2 in (* will abort if not rooted globally *)
	    (match e.exp_typ with
	      Some t -> TupleType (ReadWrite,[t])
	    |	None -> failwith "poptype.ml: AddrOf---e was not typechecked")
        (* XXX should also include TypeInst, Conditional, Subscript,
	   Cast, SeqExp *)
	| _ -> abort "& applied to invalid expression")
    | CosF | SinF | TanF | SqrtF | F2xm1F | FabsF | FroundF ->
	if not (unify_coerce1 e DoubleType) then
	  err ("Floating point op requires double <> " ^ (typ2string t));
	DoubleType
    | _ -> abort "wrong # of args for primop"
  end
and check_valid_lhs global_env env e =
  begin
    match e.raw_exp with
      Var x -> 
	(* Bug fix: make sure we're not assigning to a function! *)
	(match deSome e.exp_typ with
	  FnType _ -> 
	    (try 
	      lookup env x; () (* local vars are fine *)
	    with Not_found -> 
	      if (Dict.member global_env.functions x) then
		terr e.exp_loc "cannot assign to function"
	      else ())
	  | _ -> ())
    | Cast(t,e) -> check_valid_lhs global_env env e
    | StructMember(e1,f) ->
	(match e1.exp_typ with
	  Some t ->
	    (match compress t with
	      NamedType(n,_) ->
		let n = !n in
	    	let rec aux fs =
		  match fs with
		    ((f',rw,_)::rest) ->
		      if f = f' then
		    	(match rw with
			  ReadWrite -> ()
		    	| ReadOnly -> terr e.exp_loc ("field "^f^" is const"))
		      else aux rest
		  | [] -> failwith "field lookup"
	    	in aux (Dict.lookup global_env.structs n).st_fields
	    | _ -> failwith "type lookup 0")
	| _ -> failwith "type lookup")
    | Subscript(e1,e2) ->
	(match e1.exp_typ with
	  Some t ->
	    (match compress t with
	      StringType -> ()
	    | ArrayType _ -> ()
	    | _ -> terr e.exp_loc "not a valid left-hand-side")
	| None -> failwith "type lookup 1")
    | TupleMember(e,i) -> 
	(match e.exp_typ with
	  Some t ->
	    (match compress t with
	      TupleType (ReadOnly,_) ->
		terr e.exp_loc "read-only tuple not a valid left-hand-side"
	    | TupleType (_,_) -> ()
	    | _ -> failwith "type lookup 2")
	| None -> failwith "type lookup 3")
    | _ -> terr e.exp_loc "not a valid left-hand-side"
  end
and tcFunDecl global_env env loc fd =
  begin
(*    let returns = doesRet fd.fn_body in
    if fd.fn_ret_type <> VoidType & not returns
    then terr loc ("function body must return a value of type "^
		   (typ2string fd.fn_ret_type));
*)
    try
      let stmt_synth = tcStmt global_env env fd.fn_body in
      let returns    = stmt_synth.f_stmt_jmp in
(* Hack since splice returning isn't being synthesized yet!!! *)
      let returns = match env with
	Frame _ -> doesRet fd.fn_body
      |	_       -> returns 
      in
      if fd.fn_ret_type <> VoidType & not returns
      then terr loc ("function body must return a value of type "^
		     (typ2string fd.fn_ret_type));
(* DEBUG 
      if !debug
      then 
	(fd.fn_body.raw_stmt <- 
	   Seq(debug_push fd.fn_name, 
	       {raw_stmt=fd.fn_body.raw_stmt; stmt_loc = fd.fn_body.stmt_loc});
	 if (not returns)
	 then fd.fn_body.raw_stmt <- 
	    Seq({raw_stmt=fd.fn_body.raw_stmt; stmt_loc = fd.fn_body.stmt_loc},
		debug_pop()));
 END DEBUG *)
    with UnboundVar (x,loc) ->
      (terr loc ("unbound variable "^x); raise Gcdfec.Exit)
  end
(* end of tcFunDecl *)

let tcGlobalDecl loc global_env (s,v,t,eor) =
  let env =
    mk_env { f_env0 = { f_name = "*BOGUS*";
			f_convention = Stdcall;
			f_tyvars = [];
			f_args = [];
			f_ret_type = VoidType;
		      };
	     f_local_tyvars = [];
	     f_locals = [];
	     f_inloop = false;
	     f_labels = [];
	     f_un_before = mt_varset;
	   } in
  let rec check_constexp e =
  (* check that an expression is compile-time computable *)
    let loc = e.exp_loc in
    match e.raw_exp with
      Const _ -> ()
    | ConstArray (el,t) -> List.iter check_constexp el
    | Var x -> (* only allowed for function and exncon names (not variables) *)
	let n = 
	  (try
	    complete_val_name global_env (Dict.member global_env.globals) x
	  with Unbound -> 
	    terr loc ("Variable "^x^" in top-level initializer not defined");
	    "bogus")
	in
	if (Dict.member global_env.exceptions n) ||
	   (Dict.member global_env.functions n) then ()
	else terr loc 
	    "Global variable initializer must be a constant expression."
    | Primop(AddrOf,[e]) ->
	(match e.raw_exp with
	  Var x -> ()
	| _ -> terr loc 
	      "Global variable initializer under & not a variable.")
    | NewStruct (_,_,el) -> 
	List.iter (fun (_,e) -> check_constexp e) el
    | NewUnion (_,_,_,eo) ->
	(match eo with
	  None -> ()
	| Some e -> check_constexp e)
    | NewTuple el -> List.iter check_constexp el;
    | NewAbstype(_,_,_,e) -> check_constexp e
    | NewExn(xname,None) -> ()
    | NewExn(xname,Some e) -> check_constexp e
(* LR *)
    | TypInst ({ raw_exp = RepTerm },ts) -> ()
(* end LR *)
    | Cast (t,e) -> check_constexp e
    | _ ->
 	terr loc "Global variable initializer must be a constant expression."
  in
  begin
    let e =
      match !eor with
	None -> default_initializer global_env t loc (unassigned env)
      |	Some e -> check_constexp e; e
    in
    let t' = (tcExp global_env env e).f_typ in
    (* We need to unify this so the uninstantiated type variables in
       t' get instantiated for null.
       It is unfortunate but we must do this even when we create
       a default initializer.
       *)
    if not (unify_coerce1 global_env e t)
    (* if not (unify global_env t t') *)
    then (terr loc ("The type of the initializer ("
		    ^ (typ2string t')
		    ^ ") does not match the declared type ("
		    ^ typ2string t ^").");
	  raise Gcdfec.Exit);
    eor := Some(e);
  end

let rec eliminate_open tds =
  match tds with
    [] -> []
  | (OpenDecl(v,tds'),_)::tl -> 
      List.rev_append (eliminate_open tds') (eliminate_open tl)
  | hd::tl -> hd :: (eliminate_open tl)
;;

(* decls is used to check that values are only declared a single time. *)
let rec add_global_env (e,decls) (d,loc) =
  let abort s = terr loc s; raise Gcdfec.Exit in
  begin match d with
    StructDecl sd ->
      let n = sd.st_name in
      let keep_sd () = Dict.insert e.structs n sd in      
      if Dict.member e.abstracts n 
      then 
	({e with structs = keep_sd (); abstracts = Dict.delete e.abstracts n},
	 decls)
      else if Dict.member e.structs n then 
	begin
	  let sd_g = Dict.lookup e.structs n in
	  match sd_g.st_scope,sd.st_scope with
	    Extern,Public ->
	      ({e with structs = keep_sd()}, decls)
	  | _ -> (e,decls) 
	end
      else if Dict.member e.unions n or Dict.member e.abstypes n then  
	abort ("struct and union or abstype share name "^(var2string n))
      else ({e with structs = keep_sd ()},decls)
  | UnionDecl ud ->
      let n = ud.un_name in
      let keep_ud () = Dict.insert e.unions n ud in
      let add_field d (n,t) =
	if Dict.member d n then 
	  match Dict.lookup d n with
	  | None -> d
	  | Some ud when n = ud.un_name -> d
	  | _ -> Dict.insert d n None
	else Dict.insert d n (Some ud)
      in
      let add_fields () = List.fold_left add_field e.union_fs ud.un_fields in
      if Dict.member e.abstracts n 
      then 
	({e with unions = keep_ud(); 
                 union_fs = add_fields ();
                 abstracts = Dict.delete e.abstracts n }, 
	 decls)
      else if Dict.member e.unions n then 
	begin
	  let ud_g = Dict.lookup e.unions n in
	  match ud_g.un_scope, ud.un_scope with
	    Extern,Public -> (* We keep the public declaration*)
	      ({e with unions = keep_ud();
                       union_fs = add_fields ()}, decls)
	  | _ -> (e,decls) (* Other error conditions are checked later. *)
	end
      else if Dict.member e.structs n or Dict.member e.abstypes n then
	abort ("union and struct or abstype share name " ^(var2string n))
      else ({e with unions = keep_ud();
                    union_fs = add_fields ()}, decls)
  | AbsDecl ad ->
      let n = ad.abs_name in
      let keep_ad () = Dict.insert e.abstypes n ad in
      if Dict.member e.abstracts n then
	({e with abstypes = keep_ad(); abstracts = Dict.delete e.abstracts n}, 
	 decls)
      else if Dict.member e.abstypes n then 
	begin
	  let ad_g = Dict.lookup e.abstypes n in
	  match ad_g.abs_scope, ad.abs_scope with
	    Extern,Public -> (* We keep the public declaration*)
	      ({e with abstypes = keep_ad()},decls)
	  | _ -> (e,decls) (* Other error conditions are checked later. *)
	end
      else if Dict.member e.structs n or Dict.member e.unions n then
	abort ("abstype and struct or union share name " ^(var2string n))
      else ({ e with abstypes = keep_ad() }, decls)
  | ExternType (n,vs,b) ->
      if Dict.member e.structs   n or 
	 Dict.member e.unions    n or 
 	 Dict.member e.abstypes  n or
	 Dict.member e.abstracts n
      then (e,decls) 
      else ({e with abstracts = Dict.insert e.abstracts n (vs,b)},decls)
  | ExceptionDecl(v,s,t) ->
      if s <> Extern then
	if Dict.member decls v 
	then abort ("Multiple declarations of "^(var2string v));
      if Dict.member e.globals v then (e,decls)
      else 
	({e with globals=Dict.insert e.globals v (ref (s<>Extern),ExnconType t);
                 exceptions = Dict.insert e.exceptions v t },
	 if s<>Extern then Dict.insert decls v () else decls)
  | FunDecl fd ->
      let v = fd.fn_name in
      let arg_typs = List.map snd fd.fn_args in
      let t = FnType(fd.fn_convention,fd.fn_tyvars,fd.fn_ret_type,arg_typs) in
      if Dict.member decls v
      then abort ("Multiple declarations of "^(var2string v));
      if Dict.member e.functions v
      then (e, Dict.insert decls v ())
      else if Dict.member e.exceptions v then
	abort ("function and exception share name "^(var2string v))
      else
	({ e with functions = Dict.insert e.functions v (ref true,t); },
	 Dict.insert decls v ())
  | ExternVal (v,t) -> 
      if Dict.member e.globals v then (e,decls)
      else if Dict.member e.exceptions v then
	abort ("extern and exception share name "^(var2string v))
      else
	({ e with globals = Dict.insert e.globals v (ref false,t);
                  functions = (match t with 
		  | FnType _ -> Dict.insert e.functions v (ref false, t)
		  | _ -> e.functions) }, decls)
  | GlobalDecl (s,v,t,eor) ->
      if Dict.member decls v
      then abort ("Multiple declarations of "^(var2string v));
      if Dict.member e.globals v then (e,decls)
      else if Dict.member e.exceptions v then
	abort ("global and exception share name " ^(var2string v))
      else if Dict.member e.functions v then
	abort ("global and function share name " ^(var2string v))
      else ({ e with globals = Dict.insert e.globals v (ref true,t) },
	    Dict.insert decls v ())
  | PrefixDecl _ -> failwith "poptype.ml:add_global_env: prefixes should have been eliminated"
  | OpenDecl(prefix,ds') -> add_globals_env (e,decls) ds'
  end
and add_globals_env e ds = List.fold_left add_global_env e ds

let initial_global_env ds =
  let d () = Dict.empty compare in
  let empty_env : global_env' =
    {   structs = d();    unions = d();   abstypes = d(); abstracts = d();
       union_fs = d();   globals = d(); exceptions = d(); functions = d(); 
      open_typs = d(); open_vals = d(); 
    } in
  let ( (env : global_env'),decls) = add_globals_env (empty_env,d()) ds in
  (* add function decls to the global environment *)
  let globals' = Dict.update env.globals env.functions in
  { structs = env.structs; 
    unions = env.unions; 
    abstypes = env.abstypes; 
    abstracts = env.abstracts; 
    union_fs = env.union_fs;
    globals = globals'; 
    exceptions = env.exceptions; 
    functions = Dict.map_dict (function (s,t) -> t) env.functions;
    open_typs = d(); 
    open_vals = d(); 
  }

(* Must check_valid_type first to ensure that names are fully expanded. *)
let rec check_fundecl global_env loc fd =
  let fd_type = check_fundecl_valid_type global_env loc fd in
  let fd_genv = snd(Dict.lookup global_env.globals fd.fn_name) in
  if not (unify global_env fd_type fd_genv)
  then 
    begin 
      terr loc ("Inconsistent declarations for " ^ (var2string fd.fn_name));
      raise Gcdfec.Exit;
    end;
  ()
;;

let check_structdecl global_env loc sd =
  let abort s = 
    let n = var2string sd.st_name in
    terr loc ("Multiple declarations of "^n^" disagree on " ^ s); 
    raise Gcdfec.Exit 
  in
  begin
    List.iter (fun (_,_,t) -> check_valid_type loc global_env sd.st_tyvars t)
      sd.st_fields;
    let sd_genv = Dict.lookup global_env.structs sd.st_name in
    if not (scope_match sd.st_scope sd_genv.st_scope)
    then abort "scope.";
    if not (list_match (=) sd.st_tyvars sd_genv.st_tyvars)
    then abort "type variables.";
    if sd.st_possibly_null <> sd_genv.st_possibly_null
    then abort "?.";
    (try List.iter2 (fun (f1,c1,t1) (f2,c2,t2) ->
      if f1<>f2 
      then abort ("fields ("^(var2string f1)^" != "^(var2string f2)^").")
      else if c1<>c2 or not (unify global_env t1 t2)
      then abort ("field "^(var2string f1)^".");
      ()) sd.st_fields sd_genv.st_fields	
    with Invalid_argument _ -> abort " # of fields")
  end

let check_uniondecl global_env loc ud =
  begin
    let abort s = 
      let n = var2string ud.un_name in
      terr loc ("Multiple declarations of "^n^" disagree on "^s);
      raise Gcdfec.Exit
    in
    List.iter (fun (_,t) -> check_valid_type loc global_env ud.un_tyvars t)
      ud.un_fields;
    let ud_genv = Dict.lookup global_env.unions ud.un_name in
    if not (scope_match ud.un_scope ud_genv.un_scope)
    then abort "scope";
    if not (list_match (=) ud.un_tyvars ud_genv.un_tyvars)
    then abort "type variables";
    if ud.un_possibly_null <> ud_genv.un_possibly_null
    then abort "?";
    (try List.iter2 
      (fun (f1,t1) (f2,t2) ->
	if f1 <> f2 
	then abort ("fields ("^(var2string f1)^" != "^(var2string f2)^")");
	if not (unify global_env t1 t2)
	then abort ("type of field " ^ (var2string f1));
	())
      ud.un_fields ud_genv.un_fields
    with Invalid_argument _ -> abort "# of fields")
  end
  
let check_absdecl global_env loc ad =
  begin
    let abort s = 
      let n = var2string ad.abs_name in
      terr loc ("Multiple declarations of abstype "^n^" disagree on "^s);
      raise Gcdfec.Exit
    in
    let tvars = Sort.list (<=) (ad.abs_all_tyvars @ ad.abs_exist_tyvars) in
    let rec check_dup tvs = 
      match tvs with
	[] -> ()
      |	[_] -> ()
      |	x::((y::rest) as tl) -> 
	  if x = y then terr loc ("Duplicate type variable "^x^
				  " in abstype declaration");
	  check_dup tl in
    check_dup tvars;
    check_valid_type loc global_env tvars ad.abs_defn;
    let ad_genv = Dict.lookup global_env.abstypes ad.abs_name in
    if not (scope_match ad.abs_scope ad_genv.abs_scope)
    then abort "scope";
    if not (list_match (=) ad.abs_all_tyvars ad_genv.abs_all_tyvars)
	or not (list_match (=) ad.abs_exist_tyvars ad_genv.abs_exist_tyvars)
    then abort "type variables";
    let t1 = ad.abs_defn in
    let t2 = ad_genv.abs_defn in
    if not (unify global_env t1 t2)
    then abort "definition"
  end
  
let check_globaldecl loc global_env (s,v,t,eor) =
  begin 
    check_valid_type loc global_env [] t;
    (* if this is of function type, then make sure it's static or extern *)
    match t with
      FnType _ ->
	if (s = Static) || (s = Extern) then ()
	else terr loc "global function variables must be static"
    | _ -> ();
    (* Should only be one declaration of v although there may be many externs.*)
    (* So I shouldn't have to check this, but what the hell. *)
    let t_genv = snd(Dict.lookup global_env.globals v) in
    if not (unify global_env t t_genv)
    then begin
      terr loc ("Global "^(var2string v)^" has types "
		^(typ2string t)^" != "^(typ2string t_genv));
      raise Gcdfec.Exit
    end
  end

let check_top_decls ds =
  let global_env = initial_global_env ds in
  let rec check_typ_decls global_env ds =
    let check_typ_decl (d,loc) =
      let abort s = terr loc s; raise Gcdfec.Exit in
      begin match d with
    	FunDecl fd -> check_fundecl global_env loc fd
      | StructDecl sd -> check_structdecl global_env loc sd
      | UnionDecl ud -> check_uniondecl global_env loc ud
      |	AbsDecl ad -> check_absdecl global_env loc ad
      | ExceptionDecl(v,s,t) ->
	  begin
	    if s = Abstract
	    then terr loc "Exceptions cannot be declared abstract.";
	    check_globaldecl loc global_env (s,v,ExnconType t,None)
(*
	    check_valid_type loc global_env [] t;
	    let t_g = 
	      let _,t = Dict.lookup global_env.globals v in
	      match t with
		ExnconType t -> t
	      |	_ -> abort ("Exception "^(var2string v)^" has non-excon type "
			^(typ2string t)) in
	    if not (unify global_env t t_g)
	    then abort ("Exception "^(var2string v)^" has type "
			^(typ2string t)^" != "^(typ2string t_g));
*)
	  end
      | ExternType (tn,vs,null) ->
	  begin
	    (* Check whether this is defined anywhere and defn matches. *)
	    if Dict.member global_env.abstracts tn
	    then begin
	      let (vs',null') = Dict.lookup global_env.abstracts tn in
	      if null <> null'
	      then abort ("External type "^(var2string tn)^" disagree on ?");
	      if not (list_match (=) vs vs') 
	      then abort ("Abstract type "^(var2string tn)
			  ^"disagree on type variables");
	      ()
	    end
	    else if Dict.member global_env.structs tn
	    then begin
	      let sd = Dict.lookup global_env.structs tn in
	      if not (list_match (=) vs sd.st_tyvars)
	      then abort ("Extern and struct declaration of "^(var2string tn)
			  ^" disagree on type variables");
	      if null <> sd.st_possibly_null 
	      then abort ("Extern and declaration of "^
			  (var2string tn)^"disagree on ?");
	      ()
	    end
	    else if Dict.member global_env.unions tn
	    then begin
	      let ud = Dict.lookup global_env.unions tn in
	      if not (list_match (=) vs ud.un_tyvars)
	      then abort ("Extern and union declaration of "^(var2string tn)
			  ^" disagree on type variables.");
	      if null <> ud.un_possibly_null 
	      then abort ("Extern and union declaration of "^(var2string tn)
			  ^" disagree on ?");
	      ()
	    end
	    else if Dict.member global_env.abstypes tn
	    then begin
	      let ad = Dict.lookup global_env.abstypes tn in
	      if not (list_match (=) vs ad.abs_all_tyvars)
	      then abort ("Extern and abstype declaration of "^(var2string tn)
			  ^" disagree on type variables");
	      ()
	    end
	    else failwith "poptype.ml:check_top_decls: Unbound."
	  end
      | ExternVal (v,t) -> 
	  begin 
	    check_valid_type loc global_env [] t;
	    let (_,t_g) = Dict.lookup global_env.globals v in
	    if not (unify global_env t t_g)
	    then abort ("Extern has types "^(typ2string t)^" != "
			^(typ2string t_g))	     
	  end
      | GlobalDecl d ->  check_globaldecl loc global_env d
      | PrefixDecl _ -> failwith "poptype.ml:check_decl:prefixes should have been eliminated.\n"
      |	OpenDecl (prefix,ds) ->
	  check_typ_decls (open_prefix global_env prefix) ds
      end in
    List.iter check_typ_decl ds
  in
  let rec check_val_decls global_env ds =
    let check_val_decl (d,loc) =
      begin match d with
    	FunDecl fd -> 
	  let env = mk_env (fndecl2f_env fd) in
	  tcFunDecl global_env env loc fd; ()
      | GlobalDecl d -> tcGlobalDecl loc global_env d
      | PrefixDecl _ -> failwith "poptype.ml:check_decl:prefixes should have been eliminated.\n"
      |	OpenDecl (prefix,ds) ->
	  check_val_decls (open_prefix global_env prefix) ds
      |	_ -> ()
      end in
    List.iter check_val_decl ds
  in
  check_typ_decls global_env ds;
  check_val_decls global_env ds;
  let ds = eliminate_open ds in
  (* PrefixDecl and OpenDecl have both been eliminated. *)
  let less_decl (d1,l1) (d2,l2) =
    let val_d d = 
      let val_sc sc =
	match sc with
	  Static -> 0 | Public -> 1 | Extern -> 2 | Abstract -> 3
      in
      let (vals,typs) = (1,0) in
      let (funs,exns,globals,extern_vals) = (0,1,2,3) in
      let (structs,unions,absdecls,extern_types) = (0,1,2,3) in
      match d with
	FunDecl fd             -> (vals,fd.fn_name ,        funs,0)
      |	StructDecl sd          -> (typs,sd.st_name ,     structs,
				   val_sc sd.st_scope)
      |	UnionDecl ud           -> (typs, ud.un_name,      unions,
				   val_sc ud.un_scope)
      |	AbsDecl ad             -> (typs, ad.abs_name,     absdecls,
				   val_sc ad.abs_scope)
      |	ExceptionDecl (v,sc,_) -> (vals,          v,        exns,
				   val_sc sc)
      |	ExternType (tn,_,_)    -> (typs,         tn,extern_types,
				   val_sc Extern)
      |	ExternVal (v,t)        -> (vals,          v, extern_vals,
				   val_sc Extern) 
      |	GlobalDecl(sc,v,_,_)   -> (vals,          v,     globals,
				   val_sc sc) 
      |	_ -> failwith "poptype.ml:less_decl:prefix/open should be eliminated.\n"
    in
    let vd1,vd2 = val_d d1, val_d d2 in
    vd1 <= vd2
  in
  let ds = Sort.list less_decl ds in
  let rec remove_dups ds =
    let id_decl d =
      match d with
	FunDecl fd            -> fd.fn_name
      |	StructDecl sd         -> sd.st_name
      | UnionDecl ud          -> ud.un_name
      |	AbsDecl ad            -> ad.abs_name
      |	ExceptionDecl (v,_,_) -> v
      |	ExternType(tn,_,_)    -> tn
      |	ExternVal(v,_)        -> v
      |	GlobalDecl(_,v,_,_)   -> v
      |	_ -> failwith "poptype.ml:remove_dups:prefix/open eliminated\n"
    in
    match ds with
      ((d1,_) as dl1)::(d2,_)::tl when (id_decl d1) = (id_decl d2) ->
	remove_dups (dl1::tl)
    | dl1 :: (dl2 :: tl' as tl) -> dl1 :: (remove_dups tl)
    | l -> l
  in
  (remove_dups ds,global_env)

(* This approach has the drawback that error messages will show funny names
   but it is the easiest to implement and seems appropriate at this time. *)
let eliminate_prefixes ds =
  let rec aux current_prefix_opt ds =
    let aux' = aux current_prefix_opt in
    match (current_prefix_opt,ds) with
      (None,(PrefixDecl (prefix,ds'),loc) :: tl) ->
	(aux (Some prefix) ds') @ (aux' tl)
    | (None,(OpenDecl (prefix,ds'),loc) :: tl) ->
	(OpenDecl (prefix,aux' ds'),loc) :: (aux' tl)
    | (None,hd::tl) -> hd :: (aux' tl)
    | (Some p,(hd,loc)::tl) ->
	begin
	  let loop td = td :: (aux' tl) in
	  match hd with
	    FunDecl fd -> loop
	      	(FunDecl { fd with fn_name = add_prefix p fd.fn_name}, loc)
	  | StructDecl sd -> loop
		(StructDecl { sd with st_name = add_prefix p sd.st_name }, loc)
	  | UnionDecl ud -> loop
		(UnionDecl { ud with un_name = add_prefix p ud.un_name }, loc)
	  | AbsDecl ad -> loop
		(AbsDecl { ad with abs_name = add_prefix p ad.abs_name }, loc)
	  | ExceptionDecl(v,s,t) -> loop
	      	(ExceptionDecl(add_prefix p v, s,t), loc)
	  | ExternType (tn,vs,b) -> loop
	      	(ExternType (add_prefix p tn,vs,b), loc)
	  | ExternVal (v,t) -> loop
	      	(ExternVal (add_prefix p v,t), loc)
	  | GlobalDecl (sc,v,t,eor) -> loop
	      	(GlobalDecl (sc, add_prefix p v,t,eor),loc)
	  | PrefixDecl (p',ds') ->
	      (aux (Some (add_prefix p p')) ds') @ (aux' tl)
	  | OpenDecl (p',ds') -> loop
		(OpenDecl (p',aux' ds'),loc)
  	end
    | (_,[]) -> []
  in
  aux None ds

let type_check ifc_only ds = 
  inner_functions := [];
  reset_counter ();
  let builtins = 
    let f (x,y) = (ExceptionDecl(x,Extern,y),dummy_location) in
    List.map f 
      [ null_pointer_exn , VoidType;
	union_variant_exn, VoidType;
	array_bounds_exn , VoidType] in
  let ds = if ifc_only then ds else (List.append builtins ds) in
  let (ds,gs) = check_top_decls (eliminate_prefixes ds) in
  let add_inner_function g td = 
    match td with
      (FunDecl fd,_) -> 
	let t = FnType(fd.fn_convention,fd.fn_tyvars,fd.fn_ret_type,
	               List.map snd fd.fn_args) in
	{ g with 
	  globals = Dict.insert g.globals fd.fn_name (ref true,t);
	  functions = Dict.insert g.functions fd.fn_name t } 
    | _ -> (print_string "poptype:add_inner_function"; raise Unimplemented) in
  let gs = List.fold_left add_inner_function gs (!inner_functions) in  
  (* append inner function declarations *)
  let ds = ds @ !inner_functions in
  (* trim external declarations, if necessary *)
  let rec trim_externs ds = 
    match ds with
      [] -> []
    | (((ExternVal (v,t)),_) as d)::rest ->
	let (r,_) = Dict.lookup gs.globals v in
	if !r then d::(trim_externs rest)
	else (trim_externs rest)
    | (((ExceptionDecl (v,s,t)),_) as d)::rest ->
	let (r,_) = Dict.lookup gs.globals v in
	if !r || s <> Extern then d::(trim_externs rest)
	else (trim_externs rest)
    | ((OpenDecl (p',ds),loc))::rest ->
	(match trim_externs ds with
	  [] -> trim_externs rest
	| ds -> (OpenDecl(p',ds),loc)::(trim_externs rest))
    | d::rest -> d::(trim_externs rest) in
  ((if ifc_only then ds else trim_externs ds),gs)





