(**********************************************************************)
(* (c) Michael Hicks                                                  *)
(*     April 2000, all rights reserved.                               *)
(**********************************************************************)

(* The translation for dynamically loadable files consists of a number
   of transformations to the source file.  In essence, all "externs" are
   translated into members of a single structure global to the file, 
   called the global offset table (GOT).  Accesses to externs are changed
   to be indirected through the GOT.  The GOT is filled in at load-time
   by an added "init" function.  This function looks up the missing
   symbols using functions provided by the running program, and fills
   the GOT appropriately.  Any symbols that should be made available to
   future loaded code are exported using the passed functions.

   The code implements this translation as follows:

    - generate import table
      - slots and default entries
    - change references to externals to be through import table
    - generate init function
      - exports
      - imports
*)

(* NOTES

   I need to rewrite some of this stuff to make it more clear.  Here
   are some invariants I should exploit for carefully:

   - the range of flags.add_varmap is a subset of the range of
     flags.upd_varmap.  upd_varmap consists of the list of functions
     to be added/replaced, while the add_varmap is the list of
     functions that have stub versions (which are themselves in the
     upd_map).  To know if a function is going to be updated, then we
     just need to check the range of the upd_varmap.

   - the implementation code should never refer to the old version
     directly.  The stub code may refer to the old version, but it
     should do so directly (not with an extra level of indirection
     in the update case).

   - we define four sets of variables
     - those to look up immediately, includes 
       - UnresolvedSymbol (for default functions)
       - any of those that we are looking up that are also in the
         range of the update/add varmap (may just be the add varmap).
         Need to look them up before we replace the old value
         in the table.  This also implies that the entry should
         not be indirected through the table.
     - those to update/add
       - on entry (flags.upd_varmap), this list is those that
         will replace existing functions.  During transformation we
         also add variables that are to be exported in addition.
     - those to add
       - these are ones that stub existed for.  Therefore, the 
         stub was in the upd list, while the actual function
         is in this list.  This way, old code will see the stub,
         and future linked code will see the actual function.
     - the remainder of the ones to lookup
         - these are the normal externs (i.e. not in the old
           implementation that we may be updating).

     When we are not dynamically updating a file, then the first
     and third set will be empty (except for UnresolvedSymbol).
*)

open Popsyntax
module Id = Identifier
module PT = Poptype 

(*--------------------------------------------------------------------*)
(* flags *)

let use_unresolved_sym_exn = ref false
let translate_fn_ptrs = ref false
type local_var_convert_type = NONE | SIMPLE | MD5
let rename_local_vars = ref SIMPLE
let count_references = ref false
let export_init = ref false

(*--------------------------------------------------------------------*)
(* helper functions *)

(* Look through the list of declarations, returning the one with
   the requested name.  Raises Not_found if no such declaration exists. *)
let find_decl n is_val_decl decls =
  let rec aux ds =
    match ds with
      (d,loc)::t ->
	(let check is_val_decl' n' = 
	  if n = n' && is_val_decl' = is_val_decl then (d,loc) else aux t in
	match d with
	  FunDecl fd -> check true fd.fn_name
	| StructDecl sd -> check false sd.st_name
	| UnionDecl ud -> check false ud.un_name
	| AbsDecl ad -> check false ad.abs_name
	| ExceptionDecl (v,_,_) -> check true v
	| ExternType (v,_,_) -> check false v
	| ExternVal (v,_) -> check true v
	| GlobalDecl (_,v,_,_) -> check true v
	| PrefixDecl _ ->
	    failwith "prefixdecls should have been eliminated"
	| OpenDecl _ ->
	    failwith "opendecls should have been eliminated")
    | [] -> raise Not_found in
  aux decls

let bogus_loc = (Gcdfec.seg_of_abs 0 0)

let make_exp e = 
  { raw_exp = e; exp_typ = None; exp_loc = bogus_loc;
    exp_un_after = mt_varset; }

let make_stmt rs = 
  { raw_stmt = rs; stmt_loc = bogus_loc;
    un_after = mt_varset; un_before = mt_varset }

let make_termrep t = make_exp(TypInst (make_exp(RepTerm),[t]))

let indirect cap t = TupleType (cap,[t])

let indirect_global cap t =
  match t with
    FnType _ -> t 
  | _ -> indirect cap t

let indirect_term e = Primop(AddrOf,[e])

let is_fn_type t = 
  match t with
    FnType _ -> true
  | _ -> false

let new_id =
  let count = ref 0 in 
  (function x -> let c = !count in incr count; x^"__"^(string_of_int c))

let gen_argvars ts =
  let rec aux i ts =
    match ts with
      [] -> []
    | h::t -> (Char.escaped (Char.chr i),h)::(aux (i+1) t) in
  aux (Char.code 'a') ts

(* convert a long name into a single variable name for being in the GOT.
   Changes each ? to a __ *)
let var2field v =
  let len = String.length v in
  let i = ref 0 in
  let questions = 
    let count = ref 0 in
    while !i < len do
      if v.[!i] = '?' then incr count;
      incr i;
    done;
    !count
  in
  let w = String.create (len+questions) in
  let j = ref 0 in
  i:=0;
  while !i < len do
    if v.[!i]='?' 
    then begin w.[!j] <- '_'; w.[!j+1] <- '_'; incr j; end
    else w.[!j] <- v.[!i];
    incr j;
    incr i;
  done;
  w

(* combine a list of statements into a single Seq *)
let combine_stmts sl body =
  let rec aux l =
    match l with
      [] -> body
    | [x] -> 
	if body.raw_stmt = Skip then x 
	else make_stmt(Seq(x,body))
    | (x::xs) -> make_stmt(Seq(x,aux xs)) in
  aux sl

(* combines a list of decl statements (where the body of the decl is
   assumed to be Skip), and nests them into a single statement
   where the innermost decl has the given body. *)
let combine_decls dl body =
  let rec aux dl stms =
    match dl with
      [] -> 
	(match stms with
	  None -> body
	| Some stms -> make_stmt(Seq(stms,body)))
    | (s::tl) ->
	(match s.raw_stmt with
	  Decl(v,t,eor,s) ->
	    let stms = 
	      if s.raw_stmt != Skip then
		(match stms with
		  None -> Some s
		| Some stms -> Some (make_stmt(Seq(stms,s))))
	      else
		stms in
	    let s' = aux tl stms in { s with raw_stmt = Decl(v,t,eor,s') }
	| _ -> failwith "not a Decl") in
  aux dl None

(* creates a large && expression arranging the given expressions arguments
   leftwise *)
let and_exp es =
  let rec aux andexp es =
    match es with
      e::t -> 
	aux (make_exp(Conditional (andexp,e,make_exp(Const(Bool false))))) t
    | [] -> andexp in
  match es with
    [e] -> e
  | e1::e2::t -> 
      aux (make_exp(Conditional (e1,e2,make_exp(Const(Bool false))))) t
  | [] -> failwith "popdyntrans: [] not allowed for and_exp"

let add_to_set s sl =
  if not (List.mem s !sl) then
    sl := s::(!sl)
  else
    ()

(* debugging *)
let list_to_string l toString =
  let rec aux = function
      [] -> ""
    | [x] -> toString x
    | (h::t) ->
        let s = toString h in
        (Printf.sprintf "%s, " s) ^ (aux t) in
  aux l

(* ENVIRONMENTS *)

type trans_scope =
    Func of string
  | Def of string
  | Other

type action = 
    LookupEarly | LookupLate | Update | Add

type var_info =
    { actual_varname: string;
      dyn_varname: string;
      var_type: typ;
      dyn_action: action }

type init_info =
    { has_init: bool;
      no_refs: bool; }

type env = { local_env: string list;             (* needed to see if globals
					            are shadowed *)
	     curr_def: trans_scope;              (* def being processed *)
	     externs:                            (* list of externs *)
	       (string, var_info) Hashtbl.t;
	     globals: PT.global_env;             (* global env generated
					            during typechecking *)
             decls: (stmt list) ref;             (* extra local declarations 
						    needed for accessing 
						    exncons through the GOT *)
	     topdecls: (top_decl list) ref;      (* extra toplevel decls
						    needed by inits *)
	     inits: (stmt list) ref;             (* extra statements needed
						    to initialize toplevel
						    variables *)
	     exports:                            (* items to be exported *)
	       (string, var_info) Hashtbl.t;
	     exports_in_GOT: (string list) ref;  (* exports to add to GOT *)
	     export_deps:
	       (string, string list) Hashtbl.t;  (* maps the name of an
						    exported function to
						    the imported vals it
						    requires *)
	     user_init: init_info ref;           (* there is a user-defined
						    init function *)
	     user_reinit: bool ref;              (* there is a user-defined
						    re_init function *)
	     notice_updates: bool;		 (* true if this file is
						    updateable *)
	     static_exports: bool;               (* true if exports should
						    not be made static *)
	     export_local_vars: bool;            (* true if we should
						    register local vars *)
	     export_add_varmap:                  (* export add item names *)
	       (string * string) list;
	     export_upd_varmap:                  (* export upd item names *)
	       (string * string) list;
	     mod_name: string;                   (* module's name *)
	     file_name: string;                  (* module's filename *)
	     (* for performance testing *)
	     static_refs: int ref;		 (* #accesses to non-extern
						    global variables *)
	     dynamic_refs: int ref;		 (* #accesses to external
						    global variables *)
	   }

let pr_varinfo_tab tab =
  let action2string a =
    match a with
      LookupEarly -> "L-early"
    | LookupLate -> "L-late"
    | Update -> "U"
    | Add -> "A" in
  Hashtbl.iter 
    (fun _ 
	{ actual_varname = v;
	  dyn_varname = v';
	  var_type = t;
	  dyn_action = a } ->
      Printf.eprintf "(%s:%s) -> %s | %s\n"
	v (typ2string t) v' (action2string a))
    tab

(* indicates the binding of a local variable shadows a global one *)
let bind env v =
  { env with local_env = v::env.local_env }

(* returns true if we need to add an extra indirection *)
let do_indirect env (v,t) =
  match t with
    FnType _ -> 
      (* if it's a non-global, it must be a function pointer *)
      if List.mem v env.local_env then true
      else (* otherwise, check the global environment *)
	not (Popcompenv.is_fun_decl (Popcompenv.env_empty env.globals) v)
  | ExnconType _ -> 
      if Dict.member env.globals.PT.exceptions v then false else true
  | _ -> true

(* The next few functions traverse over a type expression and add an
   extra level of indirection to all function types *)

let rec indirect_fn_typ may_upd do_expopt env t =
  let indirect_fn_typ' = indirect_fn_typ may_upd do_expopt env in
  let indirect_fn_typs' = indirect_fn_typs may_upd do_expopt env in
  let indirect_fn_typopt' = indirect_fn_typopt may_upd do_expopt env in
  match t with
    Evar (vc,tor) -> Evar (vc,ref (indirect_fn_typopt' !tor))
  | ArrayType (t,eopt) -> 
      let t' = indirect_fn_typ' t in
      ArrayType (t', do_expopt env eopt)
  | FnType (c,vs,t,tl) -> 
      let t' = FnType (c,vs,indirect_fn_typ' t,indirect_fn_typs' tl) in
      if may_upd && !translate_fn_ptrs then indirect ReadOnly t' else t'
  | TupleType (c,tl) -> TupleType (c,indirect_fn_typs' tl)
  | NamedType (nr, tl) -> NamedType (nr, indirect_fn_typs' tl)
  | MutableTyp tr -> MutableTyp (ref (indirect_fn_typ' !tr))
  | UnresolvedTyId (n,tl) -> UnresolvedTyId (n, indirect_fn_typs' tl)
  | RepType t -> RepType (indirect_fn_typ' t)
  | ExnconType t -> ExnconType (indirect_fn_typ' t)
  | _ -> t

and indirect_fn_typs may_upd do_expopt env ts =
  List.map (indirect_fn_typ may_upd do_expopt env) ts
and indirect_fn_typopt may_upd do_expopt env topt =
  match topt with 
    None -> None 
  | Some t -> Some (indirect_fn_typ may_upd do_expopt env t)
and indirect_fn_typsopt may_upd do_expopt env tlo =
  match tlo with 
    None -> None 
  | Some ts -> Some (indirect_fn_typs may_upd do_expopt env ts)

(* if is an external, returns its type, and whether it's a variable
   (otherwise it's a declaration) *)
let lookup_external env v =
  if List.mem v env.local_env then
    raise Not_found (* bound locally *)
  else
    let { var_type = t } = 
      Hashtbl.find env.externs v in
    (t,do_indirect env (v,t))

(* same as lookup_external, but for exports *)
let lookup_export env v =
  if List.mem v env.local_env then
    raise Not_found (* bound locally *)
  else
    let { var_type = t } = Hashtbl.find env.exports v in
    (t,do_indirect env (v,t))

let in_varmap v varmap = List.mem_assoc v varmap
let in_varmap_range v varmap =
  List.fold_left 
    (fun res (_,x) -> res || (x = v))
    false varmap

let in_varmaps f v vml =
  List.fold_left
    (fun res varmap -> res || (f v varmap))
    false vml

let rename_local v modname filename =
  match !rename_local_vars with
    NONE -> v
  | SIMPLE -> modname ^ "?Local?" ^ v 
  | MD5 ->
      let idhash = (Digest.file filename) in
      (* make it just identifier-friendly characters *)
      (* ... first convert it to base64 representation *)
      let idstr' = 
	Base64.base64string_of_string idhash 0 
	  (String.length idhash) in
      (* ... then replace /, =, and + with _A, _B, and _C *)
      let rec aux s cur_idx lst_idx acc len =
	if (cur_idx = len) then acc
	else 
	  let (new_idx,new_acc) =
	    let cstr =
	      let c = s.[cur_idx] in
	      if c = '/' then "_A"
	      else if c = '=' then "_B"
	      else if c = '+' then "_C" 
	      else "" in
	    if cstr <> "" then
	      (cur_idx+1,acc ^ (String.sub s lst_idx 
				  (cur_idx-lst_idx)) ^ cstr)
	    else
	      (lst_idx,acc) in
	  aux s (cur_idx+1) new_idx new_acc len in
      ((aux idstr' 0 0 "I" (String.length idstr'))^v)

(* keep track of the values defined by this file; used to generate
   update statements to the dynamic symbol table within init *)
let note_export env s v t =
  if (s = Public) || (s = Static && env.export_local_vars) then
    (* first figure out the dynamic symbol name *)
    (let v' =
      let v'' =
	try
	  List.assoc v env.export_upd_varmap
	with Not_found ->
	  (try
	    List.assoc v env.export_add_varmap
	  with Not_found -> v) in
      if s = Static then rename_local v'' env.mod_name env.file_name
      else v'' in
    (* don't export init or reinit, by default *)
    let do_add =
      if v = "init" then
	if s <> Static || (env.export_local_vars && !export_init) then 
	  ((env.user_init) := { has_init = !(env.user_init).has_init;
			       no_refs = false };
	   true)
	else false
      else if v = "re_init" then
	(s <> Static || (env.export_local_vars && !export_init))
      else true in
    if do_add then 
      Hashtbl.add env.exports v
	{ actual_varname = v;
	  dyn_varname = v';
	  var_type = t;
	  dyn_action = 
	    if in_varmap v env.export_add_varmap then Add else Update }
    else ())
  else ()

(* provides the lookup name for any local variables to be looked up
   dynamically *)
let rename_extern_local_var v =
  let get_prefix n =
    let nLen = String.length n in
    let rec aux i =
      if i=nLen then None
      else
	if n.[i] = '?' then
	  Some(String.sub n 0 i,String.sub n (i+1) (nLen - i - 1))
	else aux (i+1) in
    aux 0 in
  let has_prefix n p = 
    let pLen = String.length p in
    if String.length n <= pLen then None
    else 
      let rec aux i = 
	if i=pLen then Some (String.sub n (i+1) (String.length n - pLen - 1))
	else if n.[i] <> p.[i] then None else aux (i+1)
      in
      aux 0 in
  match (has_prefix v "Local") with
    Some v' ->
      (match (get_prefix v') with
	Some (p,v'') ->
	  (match !rename_local_vars with
	    NONE -> v''
	  | SIMPLE -> p ^ "?Local?" ^ v''
	  | MD5 -> (Digest.file (p^".pop")) ^ v'')
      | None -> failwith 
	    (Printf.sprintf "Local variable but no filename: %s" v'))
  | None -> v

(*--------------------------------------------------------------------*)
(*                            the GOT                                 *)
(*--------------------------------------------------------------------*)

let got_name = new_id "GOT"

(* Find all of the external values used by the file and store them
   in the provided hashtable.  Returns the declarations that don't
   refer to external values/types *)

let externs decls may_upd do_late_lookup dyn_var tab =
  let indirect_fn_typ' t = indirect_fn_typ may_upd (fun _ e -> e) () t in
  let indirect_fn_typs' ts = indirect_fn_typs may_upd (fun _ e -> e) () ts in
  let create_varinfo v t =
    let dyn_v = dyn_var v in
    let dyn_a = 
      if may_upd then
        if do_late_lookup v then LookupLate else LookupEarly 
      else LookupLate in
    { actual_varname = v;
      dyn_varname = dyn_v;
      var_type = t;
      dyn_action = dyn_a } in
  let rec aux decls ds =
    match ds with
      [] -> decls
    | (ExternVal (v,t),_) :: tl ->
	let t =
	  (match t with 
	    FnType (c,vs,t,ts) -> FnType (c,vs,indirect_fn_typ' t,
					indirect_fn_typs' ts)
	  | _ -> t) in
	let t' = indirect_global ReadWrite t in
	Hashtbl.add tab v (create_varinfo v t');
	aux decls tl
    | ((ExceptionDecl (v,s,t),_) as d)::tl ->
	(match s with
	  Extern -> 
	    let t = indirect_fn_typ' t in
	    Hashtbl.add tab v (create_varinfo v (ExnconType t));
	    aux decls tl
	| _ -> aux (d::decls) tl)
    | d::tl -> aux (d::decls) tl in
  let decls = aux [] decls in
  List.rev decls

(* Creates the structure declaration for the GOT.  If this file is
   updateable, we must add an extra indirection for each extern. *)

(* externs:(var * typ) list -> structdecl *)
let got_decl (xtab:(string, var_info) Hashtbl.t) =
  let fields = 
    let fs = ref [] in
    Hashtbl.iter (fun _ { actual_varname = v;
			  var_type = t } -> 
      fs := (v,ReadWrite,t)::(!fs))
      xtab;
    !fs in
  let type_name = new_id "GOT_t" in
  { st_scope = Static;
    st_name = type_name;
    st_tyvars = [];
    st_possibly_null = false;
    st_fields = fields }

(* default values for GOT *)

(* Thrown in default functions *)
exception AddDefExn
let have_unresolved_sym_excon_decl = ref false
let unresolved_sym_exncon = "UnresolvedSymbol"
let unresolved_sym_excon_decl = 
  (ExceptionDecl(unresolved_sym_exncon, Extern, StringType))

(* for hash-consing default exncon values *)
let def_exncons = ref (Dict.empty compare)
let def_fns = ref (Dict.empty compare)
let def_fn_exn = ref None
let clear_cached_default_values () =
  def_exncons := (Dict.empty compare);
  def_fn_exn := None

(* for dealing with abstract types *)
exception NonNullAbstractType
let have_opt_decl = ref false
let opt_decl =
  (StructDecl({ st_scope = Static;
		st_name = "__Opt";
		st_tyvars = ["a"];
		st_possibly_null = true;
		st_fields = [("v",ReadWrite,VarType "a")]}))

(* Returns an expression, and an optional list of declarations.  If we
   need to use the UnresolvedSymbol exception and it has not yet been
   declared, throw an exception and restart.  Similarly, if we
   are asked to create an initializer for a non-null abstract type,
   throw an exception so the caller can "correct" itself by
   changing the type to a nullable one. *)

let rec default_initializer sym got global_env t un =
  let def_init t = default_initializer sym got global_env t un in
  let abort s = failwith ("popdyntrans.default_init: "^s)  in
  let comp_t = compress t in
  match comp_t with
    IntType(true,_) -> make_exp(Const (Int Numtypes.i32_0)), []
  | IntType(false,_) -> 
      make_exp(Cast(t,make_exp(Const (Int Numtypes.i32_0)))), []
  | BooleanType     -> make_exp(Const (Bool false)), []
  | StringType      -> make_exp(Const (String "")), []
  | CharType        -> make_exp(Const (Char '\000')), []
  | FloatType       -> make_exp(Const (Float Numtypes.zero_f32)), []
  | DoubleType      -> make_exp(Const (Double Numtypes.zero_f64)), []
  | ArrayType(t,None) ->
      make_exp(ConstArray([],Some t)), []
  | ArrayType(t,Some e) ->
      let const_array i =
	let rec nlist idx acc_e acc_ds =
	  if idx <= 0 then (acc_e,acc_ds)
	  else 
	    let e,ds = def_init t in
	    nlist (idx-1) (e::acc_e) (ds@acc_ds) in
	let arr_exp, ds = nlist (Numtypes.int32_to_int i) [] [] in
	make_exp(ConstArray(arr_exp,Some t)), ds in
      let fail () = (* non-constant array size *)
	failwith "can't generate default for non-constant array" in

      (match e.raw_exp with
	(* integer constant *)
	Const(Int i) -> const_array i

	(* binop -- see if we can constant-fold it *)
      | Primop(p,([e1;e2] as es)) ->
	  let re = Poptype.optimize_binop p e1 e2 in
	  (match re with
	    Const(Int i) -> e.raw_exp <- re; const_array i
	  | _ -> fail ())

	(* unop -- see if we can constant-fold it *)
      | Primop(p,[e1]) ->
	  let re = Poptype.optimize_unop p e1 in
	  (match re with
	    Const(Int i) -> e.raw_exp <- re; const_array i
	  | _ -> fail ())
	    
      (* no good ... *)
      | _ -> fail ())
  | TupleType (c,ts)    -> 
      let es,ds = List.split (List.map def_init ts) in
      let e = make_exp(NewTuple es) in
      let e = 
	if c = ReadOnly then 
	  make_exp(Cast(TupleType(ReadOnly,ts),e))
	else e in
      e, (List.flatten ds)

  | NamedType (n,ts) ->
      (* is n (which should be fully named) nullable *)
      let possibly_null n =
	(try (Dict.lookup global_env.PT.structs n).st_possibly_null with
	  Dict.Absent ->
	    (try snd (Dict.lookup global_env.PT.abstracts n) with
	      Dict.Absent -> false)) in

      (* we can assume n is already complete, since we typechecked already *)
      let name = !n in
      if (possibly_null name) then 
	make_exp(Const(Null)), []
      else 
	begin try 
	  (* First see if it's a structure *)
	  let sd = Dict.lookup global_env.PT.structs name in
	  let init = List.combine sd.st_tyvars ts in
	  let proc_field (f,c,t) =
	    let e,d = def_init (PT.type_subst init t) in
	    d, (None, e)  in
	  let ds, es = List.split (List.map proc_field sd.st_fields) in
	  make_exp (NewStruct (name,ref (Some ts),es)), (List.flatten ds)
	with Dict.Absent ->
	  (* Now check if it's a union *)
	  begin try 
	    let ud = Dict.lookup global_env.PT.unions name in
	    let (field,e_opt), d = 
	      match ud.un_fields with 
		[] -> abort "Union with no cases"
	      | (f,t)::tl -> 
		  if t=VoidType then (f,None), []
		  else 	      
		    let init = List.combine ud.un_tyvars ts in
		    let e,d = def_init (PT.type_subst init t) in
		    (f,Some e), d
	    in
	    make_exp (NewUnion (name,ref (Some ts),field,e_opt)), d
	  with Dict.Absent -> 
	    (* Otherwise an abstract type -- no good *)
	    raise NonNullAbstractType
	  end
	end
  | FnType (c, vs, t, ts) ->
      let (name,decls) = 
	if (Dict.member !def_fns comp_t) then
	  let n = Dict.lookup !def_fns comp_t in
	  (n,[])
	else
	  (let body, decls = 
	    if !use_unresolved_sym_exn then
	      if not !have_unresolved_sym_excon_decl then
		(clear_cached_default_values ();
		 raise AddDefExn)
	      else
		(let exn_exp = 
		  make_exp(StructMember(make_exp(Var got),
					unresolved_sym_exncon)) in
		let exnname = new_id "exn" in
		let body = 
		  make_stmt 
		    (Decl
		       (exnname,ExnconType StringType,
			ref (Some(exn_exp)),
			make_stmt
			  (Exp(make_exp
			     (Raise(make_exp
				(NewExn(exnname,
				   Some (make_exp
				     (Const(String sym))))))))))) in
		body, [])
	    else 
	      (let exn_exp, decls =
		(match !def_fn_exn with
		  None -> 
		    let decl_exn_exp, exncon_decl = def_init ExnType in
		    let exnname = new_id "exn" in
		    let decl = GlobalDecl (Static, exnname,
					   ExnType, ref (Some decl_exn_exp)) in
		    let exn_exp = (make_exp(Var exnname)) in
		def_fn_exn := Some (exn_exp);
		    exn_exp, (decl, bogus_loc)::exncon_decl
		| Some exn_exp ->
		    exn_exp, []) in
	      let body = make_stmt (Exp(make_exp (Raise(exn_exp)))) in
	      body, decls) in
	  let name = new_id "fn" in
	  let fndecl = 
	    { fn_static = false;
	      fn_convention = c;
	      fn_name = name;
	      fn_tyvars = vs;
	      fn_ret_type = t;
	      fn_args = gen_argvars ts;
	      fn_body = body } in
	  def_fns := Dict.insert !def_fns comp_t name; 
	  (name, [(FunDecl fndecl,bogus_loc)]@decls)) in
      (make_exp(Var name), decls)
  | ExnconType t ->
      let (name,decls) = 
	if not (Dict.member !def_exncons t) then
	  (let n = new_id "exncon" in
	  def_exncons := Dict.insert !def_exncons t n; 
	  (n, [(ExceptionDecl(n,Static,t),bogus_loc)]))
	else
	  let n = Dict.lookup !def_exncons t in
	  (n,[]) in
      (make_exp(Var name), decls)
  | ExnType ->
      let exncon_name, exncon_decl =
	let exp, decls = def_init (ExnconType VoidType) in
	(match (exp.raw_exp) with
	  Var name -> name, decls
	| _ -> failwith "def_init did not return Var for exncontype") in
      
      (make_exp(NewExn (exncon_name,None)),exncon_decl)
  | _ -> abort ("No default init for type "^(typ2string t))

(* If this is a statically linked file that is nonetheless using a
   GOT, then we make the default values in the GOT point directly to
   the externs. *)

let extern_as_default v t =
  (make_exp (indirect_term (make_exp (Var v)))), []

(* Given the externs for the file and its global environment, returns back 
   the needed declarations for the GOT, its type, and initialization.  
   If the static_exports flag is set, then the initial values of the
   GOT come from the external definitions rather than the default values. *)

(* (var * typ) list * global_env -> topdecl list *)
let rec got (xtab:(string, var_info) Hashtbl.t) may_upd do_late_lookup static_exports global_env =
  (* need to clear from previously processed file *)
  clear_cached_default_values ();

  let field_values ts =
    let decls = ref [] in
    let rec aux ts =
      match ts with
	[] -> []
      | (v,_,t)::tl ->
	  let (e,ds) = 
	    if not static_exports then
	      default_initializer 
		v got_name global_env t mt_varset 
	    else 
	      extern_as_default v t in
	  decls := (ds @ !decls);
	  (Some (var2field v),e)::(aux tl) in
    let fields = aux ts in
    (fields,!decls) in

  let decl,(fvals,decls) = 
    try
      let decl = got_decl xtab in
      { decl with st_fields = 
	List.map (fun (v,s,t) -> (var2field v,s,t)) decl.st_fields }, 
      (field_values decl.st_fields)
    with AddDefExn ->
      (have_unresolved_sym_excon_decl := true;
       let _ = externs [(unresolved_sym_excon_decl,bogus_loc)] 
	   may_upd do_late_lookup rename_extern_local_var xtab in
       let decl = got_decl xtab in
       { decl with st_fields = 
	 List.map (fun (v,s,t) -> (var2field v,s,t)) decl.st_fields }, 
       (field_values decl.st_fields))
    | NonNullAbstractType ->
	(* XXX need to do a few things here and elsewhere:
	   alter the GOT to have the abstract type in question
	   be an "opt_decl" instead, so that it can be null.  Then
	   we have to change the places where the value is referenced
	   to be an extra level of indirection.  *)
	failwith 
	  "popdyntrans: extern to non-null abstract type not implemented" in
  
  let init_exp =
    NewStruct(decl.st_name, ref None, fvals) in
  
  (decls,(StructDecl decl,bogus_loc),
   (GlobalDecl(Static, got_name,
	       NamedType ((ref decl.st_name),[]),
	       ref (Some (make_exp init_exp))),bogus_loc))

(* given a GOT type and value declaration, this adds the given
   globals to both *)
let add_globals_to_got may_upd got_sdecl got_decl gs =
  let fields = 
    List.map (fun (v,t) -> (var2field v,ReadWrite,t)) gs in
  let field_vals =
    List.map 
      (fun (v,t) -> let e,_ = extern_as_default v t in 
      (Some (var2field v), e)) gs in
  match got_sdecl, got_decl with
    (StructDecl sd,loc1), ((GlobalDecl(s,v,t,eor),_) as gd) ->
      (match !eor with
	Some { raw_exp = NewStruct (n,tor,fvals) } ->
	  eor := Some(make_exp(NewStruct (n,tor,fvals@field_vals)));
	  (StructDecl { sd with st_fields = sd.st_fields @ fields },loc1), gd
      |	_ -> failwith "add_globals_to_got: no or improper init exp")
  | _ -> failwith "add_globals_to_got: didn't get GOT structdecl"
      

(*--------------------------------------------------------------------*)
(*                       references to the GOT                        *)
(*--------------------------------------------------------------------*)

(* note --- we assume that typechecking has been run, so that the
   UnionSwitch statement returned by the parser has been appropriately
   translated into an ExnSwitch, NewStruct into NewExn, etc. *)

(* used to keep track of which functions depend on which imported 
   symbols.  This information is currently not used. *)

let note_dependence env v =
  if env.static_exports then ()
  else
    match env.curr_def with
      Func f ->
	let old_deps = 
	  try
	    let l = Hashtbl.find env.export_deps f in
	    Hashtbl.remove env.export_deps f;
	    l
	  with Not_found ->
	    [] in
	let new_deps =
	  if List.mem v old_deps then old_deps
	  else v::old_deps in
	Hashtbl.add env.export_deps f new_deps
    | _ -> ()

(* don't make pop_main static if it exists *)

let pop_main = "pop_main"

(* VARIABLES *)

let dyntrans_toplevel_init lv var var_e t env =
  (* Since it will now be non-constant, it needs to be
     added to the init function; we'll use a dummy initialization
     function at first, to make the typechecker happy. *)
  let (def_e,tds) = 
    default_initializer var got_name env.globals 
      (if env.notice_updates && !translate_fn_ptrs && is_fn_type t then 
	indirect ReadOnly t
      else t) mt_varset in
    (* generate the dyninit initialization statement *)
    (* XXX this won't work if the statement is nested---that is
       *(int (int)) foo = ^(bar)
       will end up with an initialization statement of
	 foo = GOT.bar
       when it should really be
	 foo.1 = GOT bar 
       Seems like we need to track the expression somehow
       so that we know how to properly generate this statement. *)
  let var_e = 
    if env.notice_updates && !translate_fn_ptrs && is_fn_type t then 
      indirect_term (make_exp var_e)
    else var_e in
  let init_stmt = 
    make_stmt 
      (Exp (make_exp(AssignOp(make_exp(Var lv),None,make_exp(var_e))))) in
  (env.inits) := init_stmt :: (!(env.inits));
  (env.topdecls) := tds @ (!(env.topdecls));
  def_e.raw_exp

(* redirects a variable to go through the GOT; the check as to whether
   to perform this translation should occur in the caller *)
let dyntrans_var env v t needs_indirect fnptr_indirect =
  (* change the expression to go through the GOT *)
  let e = (StructMember(make_exp(Var got_name),(var2field v))) in
  let e = if needs_indirect then TupleMember(make_exp e,1) else e in

  (* note the dependence on the current definition *)
  note_dependence env v;

  (* fix-up, if not occuring within a function *)
  match env.curr_def with
    Func _ -> 
      if fnptr_indirect then 
	let e = indirect_term (make_exp e) in
	let t = indirect ReadOnly t in
	Cast (t,make_exp(e))
      else e
  | Def lv -> dyntrans_toplevel_init lv v e t env
  | _ -> failwith "in dyntrans_var when not in fun or toplevel init"

(* VARIABLE REFERENCE COUNTING *)

let dynamic_var = "dynamic_calls"
let static_var = "static_calls"
exception No_stm
let insert_count =
  (* will fill in the sexp and dexp parts after the first call *)
  let realfun sexp dexp env before stm nums numd =
    let incr_stm exp n =
      if n > 0 then
	make_stmt 
	  (Exp (make_exp
		  (AssignOp(exp,
			    Some Plus,
			    make_exp(Const(Int(Numtypes.int_to_int32 n)))))))
      else raise No_stm in
    try
      let new_stm =
	if nums <= 0 then
	  incr_stm dexp numd
	else if numd <= 0 then
	  incr_stm sexp nums
	else
	  make_stmt(Seq(incr_stm dexp numd,
			incr_stm sexp nums)) in
      if before then 
	make_stmt(Seq(new_stm,stm))
      else
	make_stmt(Seq(stm,new_stm))
    with No_stm ->
      stm in
  let rec deffun env before stm nums numd =
    let sexp =
      try 
	let t,needs_indirect = lookup_external env static_var in
	dyntrans_var env static_var t needs_indirect false
      with Not_found -> (* must be dlpop itself *)
	Var static_var in
    let dexp = 
      try
	let t,needs_indirect = lookup_external env dynamic_var in
	dyntrans_var env dynamic_var t needs_indirect false
      with Not_found -> 
	Var dynamic_var in
    f := (realfun (make_exp sexp) (make_exp dexp));
    (!f) env before stm nums numd
  and f = ref deffun in
  (fun env before stm nums numd -> !f env before stm nums numd)

(* TYPES *)

let rec dyntrans_typ env t = 
  indirect_fn_typ env.notice_updates dyntrans_expopt env t
and dyntrans_typs env ts = 
  indirect_fn_typs env.notice_updates dyntrans_expopt env ts
and dyntrans_typopt env topt = 
  indirect_fn_typopt env.notice_updates dyntrans_expopt env topt
and dyntrans_typsopt env tsopt = 
  indirect_fn_typsopt env.notice_updates dyntrans_expopt env tsopt

(* EXPRESSIONS *)

and dyntrans_exp env exp =
  let dyntrans_exp' = dyntrans_exp env in
  let dyntrans_exps' = dyntrans_exps env in
  let dyntrans_expopt' = dyntrans_expopt env in
  let dyntrans_stmt' = dyntrans_stmt env in
  let dyntrans_typ' = dyntrans_typ env in
  let dyntrans_typs' = dyntrans_typs env in
  let dyntrans_typopt' = dyntrans_typopt env in
  let dyntrans_typsopt' = dyntrans_typsopt env in
  let pr_typsopt tlo =
    match tlo with
      Some ts -> 
	Printf.eprintf "<";
	List.iter (function t ->
	  let s = typ2string t in
	  Printf.eprintf "%s " s) ts;
	Printf.eprintf ">\n";
    | None -> () in
  let exp_typ exp = 
    (match exp.exp_typ with
      None -> failwith ("can't get type for "^(exp2string exp))
    | Some t -> t) in
  let dyntrans_funcall avoid_indirect fexp tlor args =
    let fexp' = 
      let e = dyntrans_exp' fexp in
      if env.notice_updates && !translate_fn_ptrs && (not avoid_indirect) then
	make_exp(TupleMember(e,1))
      else e in
    FunCall (fexp', ref (dyntrans_typsopt' !tlor), dyntrans_exps' args) in
  let e' =
    match exp.raw_exp with
      Const c -> exp.raw_exp
    | ConstArray (el, topt) -> 
	ConstArray (dyntrans_exps' el, dyntrans_typopt' topt)
    | Var v -> 
	(* see if we need to indirect this reference through the GOT *)
	(let requires_extra_indirect t = 
	  env.notice_updates && 
	  !translate_fn_ptrs && 
	  (not (do_indirect env (v,t))) in
	try
	  let t,needs_indirect = lookup_external env v in
          (* note that a dynamic ref occuring here *)
	  incr (env.dynamic_refs); 
	  dyntrans_var env v t needs_indirect (requires_extra_indirect t)
	with Not_found ->
	  (* non-external variable *)
	  (* XXX not sure if this stuff is correct for translating fn ptrs *)
	  (if env.notice_updates && !translate_fn_ptrs then
	    try
	      let t,needs_indirect = lookup_export env v in
	      if requires_extra_indirect t then
		(add_to_set v env.exports_in_GOT;
                 (* note that a dynamic ref occuring here *)
		 incr (env.dynamic_refs); 
		 dyntrans_var env v t needs_indirect true)
	      else
		Var v
	    with Not_found ->
	      (* must be a local *)
	      (* let _ = Printf.eprintf "dyntrans_var: local var %s\n" v in *)
	      let t = exp_typ exp in
	      if requires_extra_indirect t then
		let t = dyntrans_typ' t in
		Cast (t, make_exp(NewTuple [make_exp (Var v)]))
	      else
		Var v
	  else
	    (* static variables access; don't do the count unless
	       a non-local *)
	    (if not (List.mem v env.local_env) then
	      (incr (env.static_refs);
	       if v = "init" then
		 env.user_init := { has_init = !(env.user_init).has_init;
				    no_refs = false });
	     Var v)))
    | Primop (op, el) -> Primop (op, dyntrans_exps' el)
    | Conditional (be,ife,elsee) -> 
	Conditional (dyntrans_exp' be, dyntrans_exp' ife, dyntrans_exp' elsee)
    | AssignOp (lhs,opopt,rhs) ->
	AssignOp (dyntrans_exp' lhs,opopt,dyntrans_exp' rhs)

    (* deal with function expression cases specially *)
    (* XXX add cases for codegen, anon funs, ... *)
    | FunCall ({ raw_exp = Var v } as fexp, tlor, args) ->
	(try
	  (* need to indirect this reference through the GOT *)
	  let t,needs_indirect = lookup_external env v in
	  dyntrans_funcall false fexp tlor args
	with Not_found -> (* non-external variable *)
	  if do_indirect env (v,exp_typ fexp) then (* function pointer *)
	    dyntrans_funcall false fexp tlor args
	  else (* global function def *)
	    (if v = "init" then
	      env.user_init := { has_init = !(env.user_init).has_init;
				 no_refs = false };
	     FunCall (fexp, ref (dyntrans_typsopt' !tlor), 
		      dyntrans_exps' args)))
    | FunCall (fexp, tlor, args) -> dyntrans_funcall false fexp tlor args
    | TypInst (e,ts) -> TypInst (dyntrans_exp' e,dyntrans_typs' ts)
    | NewStruct (n, tlor ,fnoptelist) ->
	let dyntrans_field (fopt, e) = (fopt, dyntrans_exp' e) in
	NewStruct (n, ref (dyntrans_typsopt' !tlor), 
		   List.map dyntrans_field fnoptelist)
    | StructMember (e',n) -> StructMember (dyntrans_exp' e',n)
    | NewUnion (n, tlor, nf, eopt) ->
	NewUnion (n, ref (dyntrans_typsopt' !tlor), nf, dyntrans_expopt' eopt)
    | UnionMember (e',n) -> UnionMember (dyntrans_exp' e',n)
    | NewTuple el -> NewTuple (dyntrans_exps' el)
    | TupleMember (e',x) -> TupleMember (dyntrans_exp' e',x)
    | NewAbstype (n, tlor, tlor2, e) ->
	NewAbstype (n, ref (dyntrans_typsopt' !tlor), 
		    ref (dyntrans_typsopt' !tlor2), 
		    dyntrans_exp' e)
    | Subscript (e',ie) -> Subscript (dyntrans_exp' e', dyntrans_exp' ie)
    | Codegen fd -> Codegen (dyntrans_fundecl env fd)
    | Fill e -> Fill (dyntrans_exp' e)
    | NewExn (v,eopt) -> 
	let v' = dyntrans_exncon env v in
	NewExn(v', dyntrans_expopt' eopt)
    | Raise e -> Raise (dyntrans_exp' e)
    | SeqExp el -> SeqExp (dyntrans_exps' el)
    | Nop -> exp.raw_exp
    | Cast (t,e') -> Cast (dyntrans_typ' t,dyntrans_exp' e')
    | Fun fd -> Fun (dyntrans_fundecl env fd)
    | RepTerm -> exp.raw_exp
  in
  { exp with raw_exp = e' }

and dyntrans_exps env el = List.map (dyntrans_exp env) el
and dyntrans_expopt env eopt =
  match eopt with None -> None | Some e -> Some (dyntrans_exp env e)

(* if the exception name is an external, then create a variable declaration
   to hold its indirection through the GOT and return that variable. *)
and dyntrans_exncon env v =
  try
    let t,needs_indirect = lookup_external env v in
    let e = StructMember (make_exp(Var got_name),(var2field v)) in
    (* let e = if needs_indirect then TupleMember(make_exp e,1) else e in *)

    (* note the dependence on the current definition *)
    note_dependence env v;

    (* fix-up, if not a function *)
    match env.curr_def with
      Func _ -> 
	(* declare a variable in the function to hold the exception
	   constructor *)
	let v' = new_id "exnconv" in
	let bodystm = 
	  if !count_references then
	    insert_count env false (make_stmt(Skip)) 0 1 
	  else make_stmt(Skip) in
	let s = make_stmt(Decl (v', t, ref (Some (make_exp e)), bodystm)) in
	env.decls := (s::!(env.decls));
	v'
    | Def lv -> 
	failwith "references to externally bound exceptions in"^
	" toplevel initializations not implemented.";
	let e' = dyntrans_toplevel_init lv v e t env in
	(match e' with
	  Var v' -> v'
	| _ -> failwith "in dyntrans_exncon: dyntrans_toplevel_init"^
	    " returned non-variable")
    | _ -> failwith "in dyntrans_exncon when not in fun or toplevel init"

  with Not_found -> 
    (incr (env.static_refs); v)

(* STATEMENTS *)

and dyntrans_stmt env stmt =
  (* make new environment for counting static and dynamic calls *)
  let env' = { env with static_refs = ref 0; 
	                dynamic_refs = ref 0 } in
  let dyntrans_exp' = dyntrans_exp env' in
  let dyntrans_exps' = dyntrans_exps env' in
  let dyntrans_expopt' = dyntrans_expopt env' in
  let dyntrans_typ' = dyntrans_typ env' in
  let dyntrans_typopt' = dyntrans_typopt env' in
  let dyntrans_stmt' = dyntrans_stmt env' in
  let dyntrans_stmtopt' = dyntrans_stmtopt env' in
  let dyntrans_arm (x,s) = (x,dyntrans_stmt' s) in
  let dyntrans_sarm sa = 
    (* figure out all of the variables bound in the context of the arm *)
    let arm_vars p =
      let do_prim_pat p =
	match p with
	  Var_pat (v,_) -> [v]
	| _ -> [] in
      match p with
      	No_pat -> []
      |	Prim_pat p -> do_prim_pat p
      |	Tuple_pat pl ->
	  List.flatten (List.map do_prim_pat pl) in
    let env'' =
      List.fold_left 
	(fun env'' v -> bind env'' v) env' (arm_vars sa.arm_pat) in
    (* figure out if the field name is an external exception *)
    let v' = (* assumes no name clashes with structs, abstypes, etc. *)
      dyntrans_exncon env'' sa.arm_field in
    { sa with arm_field = v'; arm_body = dyntrans_stmt env'' sa.arm_body } in
  let bind v = bind env' v in
  let clear_refs () = 	  
    (env'.dynamic_refs) := 0;
    (env'.static_refs) := 0 in
  (*-------------------------------------------------------------------------*)
  let s' = 
    match stmt.raw_stmt with
      Skip -> stmt.raw_stmt
    | Exp e -> Exp (dyntrans_exp' e)
    | Seq (s,s2) -> 
	let s = dyntrans_stmt' s in
	let s2 = dyntrans_stmt' s2 in
	Seq (s,s2)
    | Return eopt ->
	let eopt = dyntrans_expopt' eopt in
	if !count_references then	
	  if !(env'.static_refs) <> 0 ||
	     !(env'.dynamic_refs) <> 0 then
	    (let s = make_stmt(Return eopt) in
	    let s' = insert_count env' true s 
		!(env'.static_refs) !(env'.dynamic_refs) in
	    clear_refs ();
	    s'.raw_stmt)
	  else
	    Return eopt
	else
	  Return eopt
    | IfThenElse (e,s1,s2) ->
	let e = dyntrans_exp' e in
	let s1 = dyntrans_stmt' s1 in
	let s2 = dyntrans_stmt' s2 in
	if !count_references then
	  (let s = 
	   IfThenElse(e,
		      insert_count env' true s1 
			!(env'.static_refs) !(env'.dynamic_refs),
		      insert_count env' true s2 
			!(env'.static_refs) !(env'.dynamic_refs)) in
	  clear_refs ();
	  s)
	else
	  IfThenElse (e,s1,s2)
    | While (e,s) -> 
	let e = dyntrans_exp' e in
	let s = dyntrans_stmt' s in
	if !count_references then
	  (let s = 
	    While (e,insert_count env' true s 
		       !(env'.static_refs) !(env'.dynamic_refs)) in
	  (* don't clear refs -- need a count outside the loop as well *)
	  s)
	else
	  While (e,s)
    | Break vopt -> stmt.raw_stmt
    | Continue vopt -> stmt.raw_stmt
    | For (e1,e2,e3,s) ->
	let e1 = dyntrans_exp' e1 in
	let inits = !(env'.static_refs) in
	let initd = !(env'.dynamic_refs) in
	clear_refs ();
	let e2 = dyntrans_exp' e2 in
	let tests = !(env'.static_refs) in
	let testd = !(env'.dynamic_refs) in
	clear_refs ();
	let e3 = dyntrans_exp' e3 in
	let s = dyntrans_stmt' s in
	let bodys = !(env'.static_refs) in
	let bodyd = !(env'.dynamic_refs) in
	(env'.static_refs) := tests;
	(env'.dynamic_refs) := testd;
	if !count_references then
	  (let s = 
	    let bodystm =
	      (* additions for test *)
	      let s' = insert_count env' true s tests testd in
	      (* additions for increment and body *)
	      insert_count env' false s' bodys bodyd in
	    For (e1,e2,e3,bodystm) in
	  (* see if we need to count for the init part *)
	  if inits <> 0 || initd <> 0 then
	    let s = make_stmt(s) in
	    (insert_count env' true s inits initd).raw_stmt
	  else s)
	else For(e1,e2,e3,s)
    | IntSwitch (e, isl, s) ->
	(* XXX for all of the switch cases, I simplify by incrementing
	   before the actual expression.  This is not correct in
	   the case that the expression contains a raise, but oh well *)
	let e = dyntrans_exp' e in
	let exps = !(env'.static_refs) in
	let expd = !(env'.dynamic_refs) in
	clear_refs ();
	let isl = List.map dyntrans_arm isl in
	let s = dyntrans_stmt' s in
	let stm = IntSwitch (e, isl, s) in
	if !count_references then
	  if exps <> 0 || expd <> 0 then
	    let s = make_stmt(stm) in
	    (insert_count env' true s exps expd).raw_stmt
	  else stm
	else
	  stm
    | CharSwitch (e,csl,s) ->
	let e = dyntrans_exp' e in
	let exps = !(env'.static_refs) in
	let expd = !(env'.dynamic_refs) in
	clear_refs ();
	let csl = List.map dyntrans_arm csl in
	let s = dyntrans_stmt' s in
	let stm = CharSwitch (e, csl, s) in
	if !count_references then
	  if exps <> 0 || expd <> 0 then
	    let s = make_stmt(stm) in
	    (insert_count env' true s exps expd).raw_stmt
	  else stm
	else
	  stm
    | UnionSwitch (e,sal,sopt) ->
	let e = dyntrans_exp' e in
	let exps = !(env'.static_refs) in
	let expd = !(env'.dynamic_refs) in
	clear_refs ();
	let sal = List.map dyntrans_sarm sal in
	let sopt = dyntrans_stmtopt' sopt in
	let stm = UnionSwitch (e, sal, sopt) in
	if !count_references then
	  if exps <> 0 || expd <> 0 then
	    let s = make_stmt(stm) in
	    (insert_count env' true s exps expd).raw_stmt
	  else stm
	else
	  stm
    | ExnSwitch (e,sal,sopt) ->
	let e = dyntrans_exp' e in
	let exps = !(env'.static_refs) in
	let expd = !(env'.dynamic_refs) in
	clear_refs ();
	let sal = List.map dyntrans_sarm sal in
	let sopt = dyntrans_stmtopt' sopt in	
	let stm = ExnSwitch (e,sal,sopt) in
	if !count_references then
	  if exps <> 0 || expd <> 0 then
	    let s = make_stmt(stm) in
	    (insert_count env' true s exps expd).raw_stmt
	  else stm
	else
	  stm 
    | Decl (v,t,eor,s) -> 
	let eor = ref (dyntrans_expopt' !eor) in
	let s = dyntrans_stmt (bind v) s in
	if !count_references then
	  (let s = 
	    Decl(v,dyntrans_typ' t,eor,
		 insert_count env' true s 
		   !(env'.static_refs) !(env'.dynamic_refs)) in
	  clear_refs ();
	  s)
	else
	  Decl (v,dyntrans_typ' t,eor,s)
    | Label (v,s) -> Label (v,dyntrans_stmt' s)
    | Do (s,e) -> 
	let s = dyntrans_stmt' s in
	let e = dyntrans_exp' e in
	if !count_references then
	  (let s = 
	    Do (insert_count env' false s 
		  !(env'.static_refs) !(env'.dynamic_refs),
		e) in
	  clear_refs ();
	  s)
	else
	  Do (s,e)
    | TryHandle (s1,v,s2) -> 
	let s1 = dyntrans_stmt' s1 in
	let s2 = dyntrans_stmt (bind v) s2 in
	TryHandle (s1, v, s2)
    | TryCatchFinally (s,sal,defopt,finopt) ->
	let s = dyntrans_stmt' s in
	let sal = List.map dyntrans_sarm sal in
	let defopt = dyntrans_stmtopt' defopt in
	let finopt = dyntrans_stmtopt' finopt in
	TryCatchFinally (s, sal, defopt, finopt)
    | Cut s -> Cut (dyntrans_stmt' s)
    | Splice s -> Splice (dyntrans_stmt' s)
    | With (v,tor,vs,e,s) -> 
	let e = dyntrans_exp' e in 
	let s = dyntrans_stmt (bind v) s in
	if !count_references then
	  (let s = 
	    With (v, ref (dyntrans_typopt' !tor), vs, e, 
		  insert_count env' true s 
		    !(env'.static_refs) !(env'.dynamic_refs)) in
	  clear_refs ();
	  s)
	else
	  With (v,ref (dyntrans_typopt' !tor), vs, e, s)
    | Rdtsc (e1,e2) ->
	let e1' = dyntrans_exp' e1 in 
	let e2' = dyntrans_exp' e2 in
	Rdtsc (e1',e2')
  in
  (* add counting statement if necessary *)
  if !count_references then
    let s = { stmt with raw_stmt = s' } in
    insert_count env' false s !(env'.static_refs) !(env'.dynamic_refs)
  else
    { stmt with raw_stmt = s' }
  
and dyntrans_stmtopt env eopt =
  match eopt with None -> None | Some e -> Some (dyntrans_stmt env e)

(* DECLARATIONS *)

and dyntrans_topdecl env ((rawdecl,loc) as decl) =
  let dyntrans_expopt' = dyntrans_expopt env in
  let dyntrans_typ' = dyntrans_typ env in
  let dyntrans_typs' = dyntrans_typs env in
  let d' =
    match rawdecl with
      FunDecl fd -> 
	let d = 
	  FunDecl (dyntrans_fundecl 
		     { env with curr_def = Func fd.fn_name } fd) in
	if fd.fn_name = "init" then 
	  ((env.user_init) := { has_init = true;
				no_refs = (not fd.fn_static) && 
			                  !(env.user_init).no_refs })
	else if fd.fn_name = "re_init" then 
	  ((env.user_reinit) := true);
	d
    | StructDecl sd ->
	StructDecl
	  { sd with st_fields =
	    List.map 
	      (function (fn,c,t) -> (fn,c,dyntrans_typ' t)) sd.st_fields }
    | UnionDecl ud ->
	UnionDecl
	  { ud with un_fields =
	    List.map 
	      (function (fn,t) -> (fn,dyntrans_typ' t)) ud.un_fields }
    | ExceptionDecl (v,s,t) -> 
	let t = dyntrans_typ' t in
	(* note_export env s v (ExnconType t); *)
	let s =
	  if s = Public then
	    if not env.static_exports then Static else s
	  else s in
	ExceptionDecl (v,s,t)
    | ExternVal (v,t) -> 
	if env.notice_updates && !translate_fn_ptrs then
	  let t = 
	    (match t with 
	      FnType (c,vs,t,ts) -> FnType (c,vs,dyntrans_typ' t,dyntrans_typs' ts)
	    | _ -> t) in
	  ExternVal (v,t)
	else
	  rawdecl
    | AbsDecl ad -> 
	if env.notice_updates && !translate_fn_ptrs then
	  AbsDecl { ad with abs_defn = dyntrans_typ' ad.abs_defn }
	else
	  rawdecl
    | ExternType (_, _, _) -> rawdecl
    | GlobalDecl (s,v,t,eor) ->
	let t = dyntrans_typ' t in
	(* note_export env s v t; *)
	let s =
	  if s = Public then 
	    if not env.static_exports then Static else s
	  else s in
	(* FMS: Should not translate if v is a special nullary constant. *)
	(match is_special v with
        | Some(0,_) -> rawdecl 
	| _ ->
	    GlobalDecl (s,v,t,
			ref (dyntrans_expopt { env with curr_def = Def v } 
			       !eor)))
    | PrefixDecl (v,tds) -> 
	PrefixDecl (v, List.map (dyntrans_topdecl env) tds)
    | OpenDecl (v,tds) -> 
	OpenDecl (v, List.map (dyntrans_topdecl env) tds)
  in
  (d',loc)

and dyntrans_fundecl env fd =
  let dyntrans_typ' = dyntrans_typ env in
  let dyntrans_typs' = dyntrans_typs env in

  (* translate the body *)
  let env' =
    List.fold_left (fun env' v -> bind env' v) env (List.map fst fd.fn_args) in
  let body' = (dyntrans_stmt env' fd.fn_body) in
  (* now add in all of the declarations for exn switches *)
  let body'' = combine_decls !(env.decls) body' in
  (env.decls) := []; (* reset for the next function decl *)

  (* translate the declaration (may need to indirect some fn ptrs) *)
  let ret_type = dyntrans_typ' fd.fn_ret_type in
  let args = List.map (function (v,t) -> (v,dyntrans_typ' t)) fd.fn_args in
  let t = FnType (fd.fn_convention,fd.fn_tyvars, ret_type, List.map snd args) in

  (* note the export and return the transformed decl *)
(*
  note_export env (if fd.fn_static then Public else Static) fd.fn_name t;
*)
  let s = 
    if fd.fn_static && fd.fn_name <> pop_main && not env.static_exports then
      false
    else
      fd.fn_static in
  { fd with 
    fn_static = s; fn_ret_type = ret_type; fn_args = args; fn_body = body'' }

(*--------------------------------------------------------------------*)
(*                         the init function                          *)
(*--------------------------------------------------------------------*)

(* gathers all of the exported toplevel variables *)

let note_exports env ((rawdecl,loc) as decl) =
  let do_exp _ e = e in
  match rawdecl with
    FunDecl fd -> 
      let ret_type = 
	indirect_fn_typ env.notice_updates do_exp env fd.fn_ret_type in
      let args = List.map 
	  (function (v,t) -> 
	    (v,indirect_fn_typ env.notice_updates do_exp env t)) fd.fn_args in
      let t = FnType (fd.fn_convention,
		      fd.fn_tyvars, ret_type, List.map snd args) in
      let s = if fd.fn_static then Public else Static in
      note_export env s fd.fn_name t
  | ExceptionDecl (v,s,t) -> 
      let t' = indirect_fn_typ env.notice_updates do_exp env t in
      note_export env s v (ExnconType t')
  | GlobalDecl (s,v,t,eor) ->
      let t' = indirect_fn_typ env.notice_updates do_exp env t in
      note_export env s v t'
  | _ -> ()

(* assumes that the exports portion of the environment has been 
   filled in by the previous stage. *)
    
let gen_init env do_export do_lookups got_decl_opt init_id =

  (********************)
  (* helper functions *)
  (********************)

  (* names lookup and update functions *)
  let lookup_id = new_id "lookup" in 
  let lookup_closure_id = new_id "lookup_closure" in
  let lookup_old_flag = new_id "looked_up_old" in
  let lookup_flag = new_id "looked_up" in
  let update_id = new_id "update" in 
  let update_closure_id = new_id "update_closure" in
  let upd_flag =  new_id "is_updated" in
  let no_init_id = new_id "no_init" in 
  let init_flag = new_id "done_init" in

  (* name for flag used by mutually recursive version *)
  let vflag_id v = (var2field v)^"_flag" in
  let bool_exp v b =
    make_exp(AssignOp(make_exp(Var v),
		      None,
		      make_exp (Const(Bool b)))) in
  let bool_stmt v b =
    make_stmt(Exp(bool_exp v b)) in

  let gen_flag_decls ds b =
    let rec aux decls ds =
      match ds with
	(v,_)::tl ->
	  let vflag = vflag_id v in
	  let d = 
	    GlobalDecl (Static,vflag,BooleanType,
			ref (Some (make_exp(Const(Bool b))))) in
	  aux ((d,bogus_loc)::decls) tl
      |	[] -> decls
    in
    aux [] ds in

  let wrap_stmts_with_flag stms flg do_set elsestm_opt =
    let upd_stmt = 
      if do_set then make_stmt(Exp (bool_exp flg true))
      else make_stmt(Skip) in
    let if_body = combine_stmts (upd_stmt::stms) (make_stmt(Skip)) in
    let if_stmt =
      make_stmt(IfThenElse(make_exp
			     (Primop(Not,
				     [make_exp(Var(flg))])),
			   if_body,
			   (match elsestm_opt with
			     Some stm -> stm
			   | None -> make_stmt(Skip)))) in
    if_stmt in

  (* given a list of variables, generates a linking statement
     for each one by calling gen_stm *)
  let gen_linking_stms gen_stm xtab do_gen =
    let stms = ref [] in
    Hashtbl.iter
      (fun _ ({ actual_varname = act_v;
		dyn_varname = dyn_v;
		var_type = t } as vi) ->
        if (do_gen vi) then
	  stms := (gen_stm act_v dyn_v t)::(!stms)
	else
	  ())
      xtab;
    !stms in
  
  (* Generates a list of statements to lookup each import and fill
     its value in the GOT *)
  let gen_imports xtab do_gen =
    let gen_import fun_v sym_v t =
      (* FMS: Don't import built-in nullary operators. *)
      (match is_special sym_v with
      |	Some(0,_) -> make_stmt(Skip)
      |	_ -> 
	  let termrep = make_termrep t in
	  let lookup_funcall = 
	    make_exp(FunCall
		       (make_exp(Var lookup_id),
			ref None,
			[ make_exp(Var lookup_closure_id);
			  make_exp(Const (String sym_v));
			  termrep ])) in
	  let update_got =
	    make_exp(AssignOp
		       (make_exp(StructMember
				   (make_exp(Var got_name),
				    (var2field fun_v))),
			None,
			lookup_funcall)) in
	  (make_stmt(Exp update_got))) in
    gen_linking_stms gen_import xtab do_gen in

  (* Generates a list of statements to register each export.  We need
     to add an extra indirection to be able to look up the address
     of a symbol *)

  let gen_exports () =
    let gen_export fun_v sym_v t =
      try
	(* FMS: Don't process special nullary operators. (added match) *)
	(match is_special sym_v with
	| Some(0,_) -> make_stmt(Skip)
	| _ ->
	    let needs_indirect = do_indirect env (fun_v,t) in
	    let t = if needs_indirect then indirect ReadWrite t else t in
	    let termrep = make_termrep t in
	    let funcall = 
	      make_exp(FunCall
			 (make_exp(Var update_id),
			  ref None,
			  [ make_exp(Var update_closure_id);
			    make_exp(Const (String sym_v));
			    termrep;
			    if needs_indirect then 
			      make_exp(indirect_term (make_exp(Var fun_v)))
			    else 
			      (make_exp(Var fun_v)) ])) in
	    make_stmt(Exp funcall))
      with Not_found ->
	failwith ("error -- could not find external "^fun_v) in

    let stms = 
      (gen_linking_stms gen_export
	 env.exports (fun { dyn_action = a } -> a = Update)) @
      (gen_linking_stms gen_export
	 env.exports (fun { dyn_action = a } -> a = Add)) in
    stms
  in

  (* Generates the call to the user-defined init function, if
     it exists.  In addition, it voids out entries in the GOT only
     referred to by init, so that old code is not needlessly kept
     live *)

  let gen_user_init_call no_refs has_init has_reinit =
    let make_call_stm id =
      make_stmt(Exp(make_exp(FunCall(make_exp(Var id),
				     ref None,
				     [])))) in
    (* generate calls for init() function *)
    let init_stms =
      if has_init then
        (* generate calls to zero out GOT entries after used by user init *)
	let uninit_stms =
	  if not no_refs then [] else
	  (match got_decl_opt with
	    Some got_decl ->
	      (* find all external refs in init, excluding refs made 
		 by other parts of this file *)
	      let filter_calls =
		try
                  (* get all references made by init *)
		  let initcalls = ref (Hashtbl.find env.export_deps "init") in
                  (* now filter to those not referenced elsewhere *)
		  Hashtbl.iter
		    (fun k d ->
		      if k <> "init" then
			initcalls := 
			  List.filter 
			    (function x -> not (List.mem x d)) !initcalls
		      else ())
		    env.export_deps;
		  !initcalls
		with Not_found -> 
                  (* no init, or no calls made by it are external *)
		  [] in
	      (* get the default inits for the GOT *)
	      let got_v, got_field_inits =
		match got_decl with
		  GlobalDecl(_,got_v,_,eor),_ ->
		    (match !eor with
		      Some { raw_exp = NewStruct (_,_,fvals) } -> got_v, fvals
		    | _ -> failwith "got_decl missing exp gen_user_init_call")
		| _ -> failwith "got_decl malformed in gen_user_init_call" in
	      (* for each call, restore the default init to the GOT *)
	      List.fold_left
		(fun stms v ->
		  let v = var2field v in
		  let init_exp = 
		    try List.assoc (Some v) got_field_inits with Not_found -> 
		      failwith 
			(Printf.sprintf 
			   "couldn't find var %s in GOT field inits" v) in
		  let init_stmt = 
		    make_stmt 
		      (Exp (make_exp
			      (AssignOp
				 (make_exp(StructMember
					     (make_exp(Var got_v),v)),
				  None,
				  init_exp)))) in
		  init_stmt::stms)
		[] filter_calls
	  | None -> []) in
	let init_call_stm = make_call_stm "init" in
	init_call_stm::uninit_stms
      else
	[] in

    [wrap_stmts_with_flag 
       init_stms (vflag_id init_flag) true
       (if has_reinit then Some (make_call_stm "re_init") else None)] in

  (************************)
  (* The dyninit function *)
  (************************)

  (* The makeup of the init function is as follows:
     - initial lookup statements
       These are needed to get old versions of functions about to be
       replaced
     - export statements
       Registers the symbols to be exported by this file
     - lookup statements
       Most of the lookups for imported symbols occur here
     - special initialization statements
       These initialize toplevel variables whose initialization
       statements were constant before the transformation but now
       are not
     - call to user-defined init function (if it exists)
  *)

  (* generate import statements *)
  let (start_import_stms, finish_import_stms) = 
    if do_lookups then 
      (gen_imports env.externs (fun { dyn_action = a } -> a = LookupEarly),
       gen_imports env.externs (fun { dyn_action = a } -> a = LookupLate))
    else
      ([],[]) in

  (* Add wrapper for early import statements *)
  let start_import_stms = 
    start_import_stms@
     [ make_stmt(IfThenElse(make_exp(Var(no_init_id)),
			    make_stmt(Return(None)),
			    make_stmt(Skip))) ] in

  (* Add globally exported functions to GOT for function pointers *)
  let finish_import_stms =
    if env.notice_updates && !translate_fn_ptrs then
      let export_import_stms =
	let global_env = Popcompenv.env_empty env.globals in
	gen_imports env.exports
 	  (fun { actual_varname = v } -> 
	    List.mem v !(env.exports_in_GOT) &&
	    Popcompenv.is_fun_decl global_env v) in
      export_import_stms @ finish_import_stms
    else finish_import_stms in

  (* generate export statements *)
  let export_stms = if do_export then gen_exports () else [] in

  (* generate call to user-defined init function *)
  let init_stm, flags = 
    if !(env.user_init).has_init ||
       !(env.user_reinit) then
      (wrap_stmts_with_flag 
	 (gen_user_init_call !(env.user_init).no_refs
	   !(env.user_init).has_init !(env.user_reinit))
	 no_init_id false None,
       [(init_flag,())])
    else
      (make_stmt(Skip), []) in

  (* generate the imports and exports in the function body *)
  let start_import_stms, flags =
    if start_import_stms != [] then
      ([wrap_stmts_with_flag 
	  start_import_stms (vflag_id lookup_old_flag) true None],
       (lookup_old_flag,())::flags)
    else
      (start_import_stms,flags) in
  let export_stms, flags = 
    if export_stms != [] then
      ([wrap_stmts_with_flag export_stms (vflag_id upd_flag) true None],
       (upd_flag,())::flags)
    else 
      (export_stms, flags) in

  let finish_import_stms, flags = 
    (* prevent relinking for non-updateable files *)
    if (finish_import_stms != []) && (not env.notice_updates) then
      (let set_flag_stm =
	make_stmt(IfThenElse(make_exp
			       (Primop(Not,
				       [make_exp(Var(no_init_id))])),
			     make_stmt(Exp (bool_exp 
					      (vflag_id lookup_flag) true)),
			     make_stmt(Skip))) in
      let stms = 
	[wrap_stmts_with_flag 
	   (set_flag_stm::finish_import_stms) 
	   (vflag_id lookup_flag) false None] in
      stms, (lookup_flag,())::flags)
    else 
      (finish_import_stms, flags) in

  (* combine together to form the total function body *)
  let body =
    (combine_stmts 
       (start_import_stms@export_stms@finish_import_stms@ (!(env.inits)))
       init_stm) in

  (* generate the declaration for the dyninit function *)
  let fd =
    { fn_static = true;
      fn_convention = Cdecl;
      fn_name = init_id;
      fn_tyvars = ["b";"c"];
      fn_ret_type = VoidType;
      fn_args = [ (lookup_id, FnType(Cdecl, ["a"],
				     VarType "a",
				     [ VarType "b";
				       StringType;
				       RepType (VarType "a")]));
		  (lookup_closure_id, VarType "b");
		  (update_id, FnType(Cdecl, ["a"],
				     VoidType,
				     [ VarType "c";
				       StringType;
				       RepType (VarType "a");
				       VarType "a"]));
		  (update_closure_id, VarType "c");
		  (no_init_id, BooleanType) ];
      fn_body = body;
    }
  in
  let decls = [FunDecl fd,bogus_loc] in
  let decls = (gen_flag_decls flags false)@decls in
  decls
  
(*--------------------------------------------------------------------*)
(* Takes as arguments the list of declarations that make up the program.
   Performs typechecking, and then returns a new decl list (which
   must be typechecked by the caller) representing the dynamically
   loadable code. *)

type dyntrans_flags =
    { do_import: bool;
      do_export: bool;
      do_notice_updates: bool;
      do_static_exports: bool;
      do_export_local_vars: bool;
      add_varmap: (string * string) list;
      upd_varmap: (string * string) list;
      lookup_varmap: (string * string) list;
    }

let dyntrans ifc_only decls flags filename modname =
  Printf.printf "Transforming file: %s\n" modname; flush stdout;
  let init_id = "dyninit_"^modname in
  let final_got_decl = ref None in
  (* first type check *)
  let (decls, global_env) = Poptype.type_check ifc_only decls in
  (* add references to reference counters if we're using them *)
  let decls = 
    if !count_references then
      (* see if this file already references the static and/or
	 dynamic variables *)
      let add_extern v decls = 
	try
	  let d = find_decl v true decls in
	  decls
	with Not_found -> (* not there, add it *)
	  (ExternVal (v, IntType (true,B4)),bogus_loc)::decls in
      add_extern static_var (add_extern dynamic_var decls)
    else
      decls in
  let env, decls =
    if flags.do_import then
      (let xtab = Hashtbl.create 59 in
      let do_late_lookup =
	if flags.do_notice_updates then
	  (function v -> 
	    (not ((in_varmap_range v flags.upd_varmap) ||
	          (in_varmap v flags.lookup_varmap))))
	else
	  (function _ -> false) in

      (* filter out the externs *)
      let dyn_v v =
	rename_extern_local_var 
	  (try List.assoc v flags.lookup_varmap with Not_found -> v) in
      let decls' = 
  	externs 
	  decls 
	  flags.do_notice_updates 
	  do_late_lookup
	  dyn_v
	  xtab in

      (* add the GOT stuff *)
      let (init_decls, got_sdecl, got_decl) =
	got
	  xtab
	  flags.do_notice_updates 
	  do_late_lookup
	  flags.do_static_exports 
	  global_env in
(*
      Printf.eprintf "externs are:\n"; pr_varinfo_tab xtab;
*)
      (* change references to be through the GOT *)
      let env = { local_env = [];
		  curr_def = Other;
		  externs = xtab;
		  globals = global_env;
		  decls = ref [];
		  inits = ref [];
		  topdecls = ref [];
		  exports = Hashtbl.create 59;
		  exports_in_GOT = ref [];
		  export_deps = Hashtbl.create 59;
		  user_init = ref 
		    { has_init = false; no_refs = true };
		  user_reinit = ref false;
		  notice_updates = flags.do_notice_updates;
		  static_exports = flags.do_static_exports;
		  export_local_vars = flags.do_export_local_vars;
		  export_add_varmap = flags.add_varmap;
		  export_upd_varmap = flags.upd_varmap;
		  mod_name = modname;
		  file_name = filename;
		  static_refs = ref 0;
		  dynamic_refs = ref 0 } in
      (* note all of the exports before doing the translation *)
      List.iter (fun d -> note_exports env d; ()) decls;

      (* now do the translation *)
      let decls = 
	List.map (dyntrans_topdecl env) 
	  (if flags.do_static_exports then decls else decls') in
      
      (* add global function vars to GOT *)
      let got_sdecl, got_decl = 
	if (env.notice_updates && !translate_fn_ptrs) then
	  let gs =
	    let global_env = Popcompenv.env_empty env.globals in
	    let s = ref [] in
	    Hashtbl.iter (fun _ { actual_varname = v; var_type = t } ->
	      if List.mem v !(env.exports_in_GOT) &&
		 (Popcompenv.is_fun_decl global_env v) then
		s := (v,t)::(!s)
	      else ()) env.exports;
	    !s in
	  add_globals_to_got env.notice_updates got_sdecl got_decl gs
	else
	  got_sdecl, got_decl in

      final_got_decl := (Some got_decl);
      let got_decls = init_decls @ [ got_sdecl; got_decl ] in
      env, got_decls@decls@(!(env.topdecls)))
    else (* not flags.do_import *)
      (let env = { local_env = [];
		   curr_def = Other;                  (* not used *)
		   externs = Hashtbl.create 11;       (* not used *)
		   globals = global_env;
		   decls = ref [];                    (* not used *)
		   inits = ref [];                    (* not used *)
		   topdecls = ref [];                 (* not used *)
		   exports = Hashtbl.create 59;
		   exports_in_GOT = ref [];           (* not used *)
		   export_deps = Hashtbl.create 11;   (* not used *)
		   user_init = ref 
		     { has_init = false; no_refs = true };
		   user_reinit = ref false;
		   notice_updates = flags.do_notice_updates;
		   static_exports = flags.do_static_exports;
		   export_local_vars = flags.do_export_local_vars;
		   export_add_varmap = flags.add_varmap;
		   export_upd_varmap = flags.upd_varmap;
		   mod_name = modname;
		   file_name = filename;
		   static_refs = ref 0;
		   dynamic_refs = ref 0 } in
      (* generate export list only *)
      List.iter (fun d -> note_exports env d; ()) decls;
      env, decls) in

  (* add initialization function *)
  let initdecls = 
    gen_init 
      env flags.do_export 
      (* even if we have static exports (and thus use the static
	 linker to fill in the values in the table), we want to
	 do the lookups if we notice updates, so that we get the
	 new version during relinking *)
      ((not flags.do_static_exports) || flags.do_notice_updates)
      !final_got_decl
      init_id 
  in
(*
  let print_init_info outc inf =
    Printf.fprintf outc 
      "has init fun: %b\nno_refs: %b\n" inf.has_init inf.no_refs in
  print_init_info Pervasives.stderr !(env.user_init);
*)
  decls@initdecls
