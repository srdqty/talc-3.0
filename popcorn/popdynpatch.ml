(**********************************************************************)
(* (c) Michael Hicks                                                  *)
(*     May 2000, all rights reserved.                               *)
(**********************************************************************)

(* This code is used to "compile" patch files for dynamic updates.
   The basic algorithm is (MWH---a little out of date):

  - parse the patch file, acquiring 
    - the name of the implementation file
    - the name of the interface code file (optional)
    - the shared variables names
    - the variables that should be renamed
  - get the implementation file
    - parse it
    - for each of the shared vars v 
      - change the definition in the parsed implementation to an 
        extern.
    - for each defined v not in the shared variables
      - change the name to new__v, both in the use and the definition.
      - note a mapping XXX -> new__XXX
  - get the interface code file, if present
    - parse it
    - for each defined stub, named stub__XXX
      - if a mapping for XXX already exists (i.e. XXX -> new__XXX),
        then add to it, resulting in (XXX -> stub__XXX, new__XXX).
      - otherwise, add mapping XXX -> stub__XXX
    - append it to the modified implementation file
*)

open Popsyntax

(*************)
(* UTILITIES *)
(*************)

(* for converting :: varnames to ? ones *)
let internal_varname s =
  let rec aux news ofs =
    try
      let idx = String.index_from s ofs ':' in
      if s.[idx+1] = ':' then
	(let prefix = String.sub s ofs (idx+1-ofs) in
	prefix.[idx-ofs] <- '?';
	aux (news ^ prefix) (idx+2))
      else
	invalid_arg ("internal_varname: "^s^" is not a legal identifier")
    with Not_found ->
      news ^ (String.sub s ofs ((String.length s) - ofs)) in
  aux "" 0

(* splits the string at the first ? *)
let get_prefix n =
  let nLen = String.length n in
  let rec aux i =
    if i=nLen then None
    else
      if n.[i] = '?' then
	Some(String.sub n 0 i,String.sub n (i+1) (nLen - i - 1))
      else aux (i+1) in
  aux 0

(* Substring matching; if the second is a substring of the first,
   this returns the rest *)
let has_prefix n p = 
  let pLen = String.length p in
  if String.length n <= pLen then None
  else 
    let rec aux i = 
      if i=pLen then Some (String.sub n (i+1) (String.length n - pLen - 1))
      else if n.[i] <> p.[i] then None else aux (i+1)
    in
    aux 0

(******************)
(* THE PATCH FILE *)
(******************)

type lex_tokens =
    Implementation
  | Interface
  | Sharing
  | Renaming
  | Token of string

let token2str t =
  match t with
    Implementation -> "implementation:"
  | Interface -> "interface:"
  | Sharing -> "sharing:"
  | Renaming -> "renaming:"
  | Token s -> s

let str2token s =
  match s with
    "implementation:" -> Implementation
  | "interface:" -> Interface
  | "sharing:" -> Sharing
  | "renaming:" -> Renaming
  | _ -> Token s

(*----------------------------------------------------------------------*)
(* lexer---gets all of the tokens ahead of time *)
let string_to_list s =
  let rec skip_whitespace s i l =
    if i = l then i
    else
      match s.[i] with 
	' ' | '\n' | '\r' | '\t' -> skip_whitespace s (i+1) l
      |	_ -> i in
  let rec get_token s ofs i l =
    if i = l then
      (String.sub s ofs (i-ofs), i)
    else
      match s.[i] with
	' ' | '\n' | '\r' | '\t' ->
	  let token = String.sub s ofs (i-ofs) in
	  (token, skip_whitespace s (i+1) l)
      | _ -> get_token s ofs (i+1) l in
  let rec get_tokens s i l =
    if i = l then []
    else
      let (token,i) = get_token s i i l in
      token::(get_tokens s i l) in
  let slen = (String.length s) in
  let i = skip_whitespace s 0 slen in
  get_tokens s i slen

let tokenize s =
  let ss = string_to_list s in
  let ts = List.map str2token ss in
  Stream.of_list ts

(*----------------------------------------------------------------------*)
(* parser *)
let parse ts =
  (* parsing helper functions *)
  let error msg t =
    let s =
      (match t with
	Some t -> (token2str t)
      |	None -> "EOF") in
    failwith ("parse error in patch file: "^msg^"; got "^s) in
  let get_token () = 
    let t = Stream.next ts in
    (* Printf.printf "read token %s\n" (token2str t); *)
    t in
  let peek () = 
    let t = Stream.peek ts in
    (* (match t with
      Some t -> Printf.printf "peeked token %s\n" (token2str t)
    | None -> Printf.printf "peeked at end of stream\n"); *)
    t in
  let rec split_string s c =
    let rec getidx s n len =
      if n = len then raise Not_found 
      else
	if s.[n] = c then n else (getidx s (n+1) len) in
    let rec getelems s =
      try
	let len = String.length s in
	let idx = (getidx s 0 len) in
	(String.sub s 0 idx)::
       	getelems (String.sub s (idx+1) (len-idx-1)) 
      with Not_found ->
	[s]
    in
    getelems s in

  (* data to obtain *)
  let impl_fname = ref None in
  let ifc_fname = ref None in
  let svars = ref [] in
  let rnamevars = ref [] in

  (* toplevel parsing function *)
  let rec parse () =
    try
      (match (get_token ()) with
	Implementation -> 
	  (match (peek ()) with
	    Some (Token s) ->
	      let _ = get_token () in
	      impl_fname := Some s;
	  | t -> error "expecting implementation contents" t)
      |	Interface ->
	  (match (peek ()) with
	    Some (Token s) ->
	      let _ = get_token () in
	      ifc_fname := Some s;
	  | t -> error ("expecting interface file contents") t)	  
      |	Sharing ->
	  let rec aux () =
	    (match (peek ()) with
	      Some (Token s) -> 
		let _ = get_token () in
		svars := s::(!svars); aux ()
	    | _ -> svars := (List.rev !svars)) in
	  aux ()
      |	Renaming ->
	  let rec aux () =
	    (match (peek ()) with
	      Some (Token s) -> 
		let _ = get_token () in
		(* assumes the token is of form x=y; split at the = *)
		let ts = split_string s '=' in
		(match ts with
		  [origvar; newvar] ->
		    rnamevars := (origvar,newvar)::(!rnamevars); 
		    aux ()
		| _ -> error "illegal rename var spec" (Some (Token s)))
	    | _ -> rnamevars := (List.rev !rnamevars)) in
	  aux ()
      |	Token s -> error ("expecting keyword") (Some (Token s)));
      parse ()             (* go for next one *)
    with Stream.Failure -> (* finished *)
      () in
  parse ();
  (!impl_fname, !ifc_fname, !svars, !rnamevars)

(*----------------------------------------------------------------------*)
(* Reads the entire contents of the given file and stores them in a string *)
let get_string_from_file filename =
  try
    let fin = open_in_bin filename in
    let len = in_channel_length fin in
    let buf = String.create len in
    let res = input fin buf 0 len in
    if res <> len then
      (let ebuf = Printf.sprintf "Error reading file %s; len=%d, ret=%d"
          filename len res in
      prerr_endline ebuf; raise Exit);
    close_in fin;
    buf
  with
    Sys_error e ->
      let ebuf = Printf.sprintf "Couldn't open file %s" filename in
      failwith ebuf
  | Invalid_argument e ->
      failwith "Bad string length on input"

(*----------------------------------------------------------------------*)
(* opens the patch file and parses it, returning the implementation
   file name, the interface code filename, and the shared variables *)
let get_patch_file fname =
  (* XXX deal with failure to open the file *)
  let filestr = get_string_from_file fname in
  let ts = tokenize filestr in
  parse ts

(***************************)
(* THE IMPLEMENTATION FILE *)
(***************************)

(* All of the code below is used to translate the names of variables in the
   new implementation file to be different from the names of shared
   variables *)

let new_var v = "New?"^v

type env = { local_env: string list;  (* needed to see if globals
					 are shadowed *)
	     global_env:              (* the impl file global type env *)
	       Poptype.global_env;
	     vars: string list;       (* list of variables to update *)
	     type_var_map:            (* changes to apply to typenames *)
	       (string * string) list;
	     add_new: bool;	      (* indicate whether to prepend
					 type vars to rename with New? *)
	   }

(* for completing the names of variables within an open declaration *)
(*
let complete_val_name env n =
  let defined n = List.mem n env.local_env in
  try
    if defined n then n 
    else Dict.lookup env.global_env.Poptype.open_vals n
  with Dict.Absent -> n
*)

(* for completing the names of typenames within an open declaration *)
(*
let complete_typ_name env n =
  let defined name =
    (Dict.member env.global_env.Poptype.structs   name ||
     Dict.member env.global_env.Poptype.unions    name ||
     Dict.member env.global_env.Poptype.abstypes  name ||
     Dict.member env.global_env.Poptype.abstracts name)
  in
  try
    if defined n then n
    else Dict.lookup env.global_env.Poptype.open_typs n
  with Dict.Absent -> n
*)

(* indicates the binding of a local variable shadows a global one *)
let bind env v =
  { env with local_env = v::env.local_env }
let binds env vs =
  List.fold_left (fun env v -> bind env v) env vs

(* !!! During all of the translation code that follows, we assume 
   typechecking has been done, meaning that NewStruct really means
   NewStruct, as opposed to either NewExn or NewUnion as well. *)

(*----------------------------------------------------------------------*)
let dyntrans_var env v =
  (* let v = complete_val_name env v in *)
  if List.mem v env.vars &&
     not (List.mem v env.local_env) then
    new_var v
  else
    v

(*----------------------------------------------------------------------*)
let rec dyntrans_typename env v =
  (* let v = complete_typ_name env n in *)
  if env.add_new then (* first pass---see if the variable, when New?
			 added, is in the rename map; add New? if so *)
    let v' = new_var v in
    if List.mem_assoc v' env.type_var_map then v' else v
  else (* second pass---apply the map to variables as they are *)
    try
      List.assoc v env.type_var_map 
    with Not_found -> v

(*----------------------------------------------------------------------*)
let rec dyntrans_typ env t =
  let dyntrans_expopt' = dyntrans_expopt env in
  let dyntrans_typ' = dyntrans_typ env in
  let dyntrans_typs' = dyntrans_typs env in
  let dyntrans_typopt' = dyntrans_typopt env in
  match t with
    Evar (vc,tor) -> Evar (vc,ref (dyntrans_typopt' !tor))
  | ArrayType (t,eopt) -> ArrayType (dyntrans_typ' t, dyntrans_expopt' eopt)
  | FnType (c,vs,t,tl) -> FnType (c,vs,dyntrans_typ' t,dyntrans_typs' tl)
  | TupleType (c,tl) -> TupleType (c,dyntrans_typs' tl)
  | NamedType (nr, tl) -> NamedType (ref (dyntrans_typename env !nr), 
				     dyntrans_typs' tl)
  | MutableTyp tr -> MutableTyp (ref (dyntrans_typ' !tr))
  | UnresolvedTyId (n,tl) -> 
      UnresolvedTyId (dyntrans_typename env n, dyntrans_typs' tl)
  | RepType t -> RepType (dyntrans_typ' t)
  | ExnconType t -> ExnconType (dyntrans_typ' t)
  | _ -> t

and dyntrans_typs env ts = List.map (dyntrans_typ env) ts
and dyntrans_typopt env topt =
  match topt with None -> None | Some t -> Some (dyntrans_typ env t)
and dyntrans_typsopt env tlo =
  match tlo with None -> None | Some ts -> Some (dyntrans_typs env ts)

(*----------------------------------------------------------------------*)
and dyntrans_exp env exp =
  let dyntrans_exp' = dyntrans_exp env in
  let dyntrans_exps' = dyntrans_exps env in
  let dyntrans_expopt' = dyntrans_expopt env in
  let dyntrans_stmt' = dyntrans_stmt env in
  let dyntrans_typ' = dyntrans_typ env in
  let dyntrans_typs' = dyntrans_typs env in
  let dyntrans_typopt' = dyntrans_typopt env in
  let dyntrans_typsopt' = dyntrans_typsopt env in
  let e' =
    match exp.raw_exp with
      ConstArray (el, topt) -> 
	ConstArray (dyntrans_exps' el, dyntrans_typopt' topt)
    | Var v -> Var (dyntrans_var env v)
    | Primop (op, el) -> Primop (op, dyntrans_exps' el)
    | Conditional (be,ife,elsee) -> 
	Conditional (dyntrans_exp' be, dyntrans_exp' ife, dyntrans_exp' elsee)
    | AssignOp (lhs,opopt,rhs) ->
	AssignOp (dyntrans_exp' lhs,opopt,dyntrans_exp' rhs)
    | FunCall (fexp, tlor, args) ->
	FunCall (dyntrans_exp' fexp, ref (dyntrans_typsopt' !tlor), 
		 dyntrans_exps' args)
    | TypInst (e,ts) -> TypInst (dyntrans_exp' e, dyntrans_typs' ts)
    | NewStruct (n, tlor ,fnoptelist) ->
	let dyntrans_field (fopt, e) = (fopt, dyntrans_exp' e) in
	NewStruct (dyntrans_typename env n, 
		   ref (dyntrans_typsopt' !tlor), 
		   List.map dyntrans_field fnoptelist)
    | StructMember (e',n) -> StructMember (dyntrans_exp' e',n)
    | NewUnion (n, tlor, nf, eopt) ->
	NewUnion (dyntrans_typename env n, 
		  ref (dyntrans_typsopt' !tlor), 
		  nf, dyntrans_expopt' eopt)
    | UnionMember (e',n) -> UnionMember (dyntrans_exp' e',n)
    | NewTuple el -> NewTuple (dyntrans_exps' el)
    | TupleMember (e',x) -> TupleMember (dyntrans_exp' e',x)
    | NewAbstype (n, tlor, tlor2, e) ->
	NewAbstype (dyntrans_typename env n, 
		    ref (dyntrans_typsopt' !tlor), 
		    ref (dyntrans_typsopt' !tlor2), 
		    dyntrans_exp' e)
    | Subscript (e',ie) -> Subscript (dyntrans_exp' e', dyntrans_exp' ie)
    | Codegen fd -> Codegen (dyntrans_fundecl env fd)
    | Fill e -> Fill (dyntrans_exp' e)
    | NewExn (v,eopt) -> NewExn (dyntrans_var env v, 
				 dyntrans_expopt' eopt)
    | Raise e -> Raise (dyntrans_exp' e)
    | SeqExp el -> SeqExp (dyntrans_exps' el)
    | Cast (t,e') -> Cast (dyntrans_typ' t,dyntrans_exp' e')
    | Fun fd -> Fun (dyntrans_fundecl env fd)
    | _ -> exp.raw_exp
  in
  { exp with raw_exp = e' }

and dyntrans_exps env el = List.map (dyntrans_exp env) el
and dyntrans_expopt env eopt =
  match eopt with None -> None | Some e -> Some (dyntrans_exp env e)

(*----------------------------------------------------------------------*)
and dyntrans_stmt env stmt =
  let dyntrans_exp' = dyntrans_exp env in
  let dyntrans_exps' = dyntrans_exps env in
  let dyntrans_expopt' = dyntrans_expopt env in
  let dyntrans_stmt' = dyntrans_stmt env in
  let dyntrans_stmtopt' = dyntrans_stmtopt env in
  let dyntrans_typopt' = dyntrans_typopt env in
  let dyntrans_typ' = dyntrans_typ env in
  let dyntrans_arm (x,s) = (x,dyntrans_stmt' s) in
  let dyntrans_sarm sa = 
    (* figure out all of the variables bound in the context of the arm *)
    (* XXX do we need to do translations here to properly match paterns?
       If so, we should treat ExnSwitch and UnionSwitch differently *)
    let arm_vars p =
      let do_prim_pat p =
	match p with
	  Var_pat (v,_) -> [v]
	| _ -> [] in
      match p with
      	No_pat -> []
      |	Prim_pat p -> do_prim_pat p
      |	Tuple_pat ps ->
	  List.flatten (List.map do_prim_pat ps) in
    let env' =
      List.fold_left (fun env' v -> bind env' v) env (arm_vars sa.arm_pat) in
    (* figure out if the field name is exception name to change *)
    let v' = dyntrans_var env' sa.arm_field in
    { sa with arm_field = v'; arm_body = dyntrans_stmt env' sa.arm_body } in

  let s' = 
    match stmt.raw_stmt with
      Exp e -> Exp (dyntrans_exp' e)
    | Seq (s,s2) -> Seq (dyntrans_stmt' s, dyntrans_stmt' s2)
    | Return eopt -> Return (dyntrans_expopt' eopt)
    | IfThenElse (e,s1,s2) ->
	IfThenElse (dyntrans_exp' e,dyntrans_stmt' s1,dyntrans_stmt' s2)
    | While (e,s) -> While (dyntrans_exp' e,dyntrans_stmt' s)
    | For (e1,e2,e3,s) ->
	For (dyntrans_exp' e1,dyntrans_exp' e2,dyntrans_exp' e3, 
	     dyntrans_stmt' s)
    | IntSwitch (e, isl, s) ->
	IntSwitch (dyntrans_exp' e, List.map dyntrans_arm isl, 
		   dyntrans_stmt' s)
    | CharSwitch (e,csl,s) ->
	CharSwitch (dyntrans_exp' e, List.map dyntrans_arm csl, 
		    dyntrans_stmt' s)
    | UnionSwitch (e,sal,sopt) ->
	UnionSwitch (dyntrans_exp' e, List.map dyntrans_sarm sal, 
		     dyntrans_stmtopt' sopt)
    | ExnSwitch (e,sal,sopt) ->
	ExnSwitch (dyntrans_exp' e,List.map dyntrans_sarm sal,
		   dyntrans_stmtopt' sopt)
    | Decl (v,t,eor,s) -> Decl (v,dyntrans_typ' t,
				ref (dyntrans_expopt' !eor),
				dyntrans_stmt (bind env v) s)
    | Label (v,s) -> Label (v,dyntrans_stmt' s)
    | Do (s,e) -> Do (dyntrans_stmt' s,dyntrans_exp' e)
    | TryHandle (s1,v,s2) -> TryHandle (dyntrans_stmt' s1,v,
					dyntrans_stmt (bind env v) s2)
    | TryCatchFinally (s,sal,defopt,finopt) ->
	TryCatchFinally (dyntrans_stmt' s,List.map dyntrans_sarm sal,
			 dyntrans_stmtopt' defopt,dyntrans_stmtopt' finopt)
    | Cut s -> Cut (dyntrans_stmt' s)
    | Splice s -> Splice (dyntrans_stmt' s)
    | With (v,tor,vs,e,s) -> 
	With (v,ref (dyntrans_typopt' !tor),
	      vs, dyntrans_exp' e, dyntrans_stmt (bind env v) s)
    | _ -> stmt.raw_stmt
  in
  { stmt with raw_stmt = s' }
  
and dyntrans_stmtopt env eopt =
  match eopt with None -> None | Some e -> Some (dyntrans_stmt env e)

(*----------------------------------------------------------------------*)
and dyntrans_topdecl env ((rawdecl,loc) as decl) =
  let dyntrans_expopt' = dyntrans_expopt env in
  let dyntrans_typ' = dyntrans_typ env in
  let d' =
    match rawdecl with
      FunDecl fd -> FunDecl (dyntrans_fundecl env fd)
    | StructDecl sd ->
	let dyntrans_sfield (fn,cap,t) = (fn,cap,dyntrans_typ env t) in
	StructDecl 
	  { sd with 
	    st_name = dyntrans_typename env sd.st_name;
	    st_fields = List.map dyntrans_sfield sd.st_fields }
    | UnionDecl ud ->
	  let dyntrans_sfield (fn,t) = (fn,dyntrans_typ env t) in
	  UnionDecl
	    { ud with 
	      un_name = dyntrans_typename env ud.un_name;
	      un_fields = List.map dyntrans_sfield ud.un_fields }
    | AbsDecl ad -> 
	AbsDecl { ad with 
		  abs_name = dyntrans_typename env ad.abs_name;
		  abs_defn = dyntrans_typ env ad.abs_defn }
    | ExceptionDecl (v,s,t) -> 
	ExceptionDecl (dyntrans_var env v,s,dyntrans_typ' t)
    | GlobalDecl (s,v,t,eor) ->
	GlobalDecl (s,dyntrans_var env v,
		    dyntrans_typ' t, ref (dyntrans_expopt' !eor))
    | PrefixDecl (v,tds) -> 
	failwith "dyntrans_topdecl: prefixes should have been eliminated"
    | OpenDecl (v,tds) -> 
	failwith "dyntrans_topdecl: opens should have been eliminated"
(*
	OpenDecl 
	  (v, List.map 
	       (dyntrans_topdecl 
		  { env with global_env = 
                    Poptype.open_prefix env.global_env v })
	       tds)
*)
    | ExternType (n,vs,b) ->
	ExternType (dyntrans_typename env n, vs, b)
    | ExternVal (v,t) -> ExternVal (v, dyntrans_typ' t) in
  (d',loc)

(*----------------------------------------------------------------------*)
and dyntrans_fundecl env fd =
  let env' = env in
  (* List.fold_left (fun env v -> (bind env v)) env fd.fn_tyvars in *)
  let env'' =
    List.fold_left (fun env (v,_) -> (bind env v)) env fd.fn_args in
  let body' = (dyntrans_stmt env'' fd.fn_body) in
  let args' = List.map (function (v,t) -> (v,dyntrans_typ env t)) fd.fn_args in
  let ret_type' = dyntrans_typ env fd.fn_ret_type in
  { fd with fn_name = dyntrans_var env fd.fn_name;
            fn_body = body'; 
            fn_args = args'; 
            fn_ret_type = ret_type' }

    (* Translate type variables to using the alternate names.
       Temporarily convert the variable to be of the "new" kind
       before doing the mapping. *)
(*
    let alt_name v =
      try 
	List.assoc (new_var v) rvars
      with Not_found -> v in
*)

(*----------------------------------------------------------------------*)
(* For each topdecl, if its var is in the shared variable list, then
   its definition is made extern.  Otherwise, the old defintion is
   used but its name is changed to the new implementation name and is
   noted for later translation of the expression parts. *)

type vartype =
    IsShared 
  | IsDefined
  | IsExtern

(* translate the name of the given declaration, returning a pair
   of a variable map (describing the variable change that was
   made) and the new declaration *)
let rec make_def_extern ((rawdecl,loc) as decl) get_vartype old_var =
  (* returns a pair consisting of a variable mapping and
     a new declaration, based on the sort of variable given. *)
  let do_trans curr_name new_name curr_scope extern_decl =
    match (get_vartype curr_name curr_scope) with
      IsShared | IsExtern -> ([],[extern_decl,loc])
    | IsDefined -> ([new_name,curr_name],[decl]) 
  in
  match rawdecl with
    FunDecl fd ->
      do_trans fd.fn_name (new_var fd.fn_name)
	(if fd.fn_static then Public else Static)
	(ExternVal (old_var fd.fn_name, FnType (fd.fn_convention, 
						fd.fn_tyvars, fd.fn_ret_type,
						(List.map snd fd.fn_args))))
  | StructDecl sd ->
      ([],snd(do_trans sd.st_name sd.st_name sd.st_scope 
		(StructDecl { sd with st_scope = Extern; 
                                      st_name = old_var sd.st_name })))
  | UnionDecl ud ->
      ([],snd(do_trans ud.un_name ud.un_name ud.un_scope 
		(UnionDecl { ud with un_scope = Extern;
                                     un_name = old_var ud.un_name })))
  | AbsDecl ad -> 
      ([],snd(do_trans ad.abs_name ad.abs_name ad.abs_scope 
		(AbsDecl { ad with abs_scope = Extern; 
                                   abs_name = old_var ad.abs_name })))
  | ExceptionDecl (v,s,t) ->
      do_trans v (new_var v) s
	(ExceptionDecl (old_var v,Extern,t))
  | GlobalDecl (s,v,t,eor) ->
      let t = 
	(match t with
	  ArrayType (t',_) -> ArrayType (t',None)
	| _ -> t) in
      do_trans v (new_var v) s 
	(GlobalDecl (Extern,old_var v,t,(* eor *) ref None))
  | OpenDecl (v,tds) -> 
      failwith "make_def_extern: opens should have been eliminated"
(*
   let (vars,decls) = make_defs_extern tds in
   (vars, [OpenDecl (v, decls),loc])
*)
  | PrefixDecl (v,tds) -> 
      failwith "make_def_extern: prefixes should have been eliminated"
  | _ -> ([],[decl])
	
and make_defs_extern decls get_vartype =
  List.fold_right
    (fun decl (vars,decls) -> 
      let (vs,ds) = make_def_extern decl get_vartype (function x -> x) in
      (vs@vars,ds@decls))
    decls ([],[])

let translate_decls impl_decls ifc_decls svars rvars =

  (* returns the "type" of the variable *)
  let get_vartype v scope =
    if (List.mem v svars) then
      if scope = Abstract then
	failwith (Printf.sprintf "shared type definition %s is abstract" v)
      else
	IsShared
    else if scope <> Extern then IsDefined
    else IsExtern in

  let translate_global_env global_env vars =
    let trans_var v = if (List.mem v vars) then (new_var v) else v in
    let trans_dict d =
      Dict.fold_dict 
	(fun k v d -> Dict.insert d (trans_var k) v) 
	d (Dict.empty compare) in
    { global_env with
        Poptype.globals = trans_dict global_env.Poptype.globals;
        Poptype.exceptions = trans_dict global_env.Poptype.exceptions;
        Poptype.functions = trans_dict global_env.Poptype.functions; } in

  (* First typecheck the implementation file without changes *)
  let (impl_decls,global_env) = Poptype.type_check false impl_decls in

  (* Note all variables defined by this file that are not in the
     shared variable list, and make extern those definitions that are *)
  let (varmap,impl_decls) = make_defs_extern impl_decls get_vartype in
  let (_,vars) = List.split varmap in
(*
  print_endline "variable list is:";
  List.iter print_endline vars;
*)
  (* Now translate the names in the global environment (which used
     the original file for typechecking, before the names were
     changed).  Use the variable list to do this. *)
  let global_env = translate_global_env global_env vars in

  (* Change variables in the program to those in the variable map
     (just calculated) and the type variable map. *)
  let env = { local_env = [];
	      global_env = global_env;
	      vars = vars;
	      type_var_map = rvars;
	      add_new = true } in
  let impl_decls = List.map (dyntrans_topdecl env) impl_decls in

  (* Now run through the whole thing, including the ifc decls, and
     rewrite the typenames to the user-specified names. *)
  let (decls,global_env) = Poptype.type_check false (ifc_decls@impl_decls) in
  let env = { local_env = [];
	      global_env = global_env;
	      vars = []; (* no need for this -- already translated *)
	      type_var_map = rvars;
	      add_new = false } in
  let decls = List.map (dyntrans_topdecl env) decls in  

  (varmap,decls)

(*----------------------------------------------------------------------*)
(* XXX consider making all non-stub defs in ifc code file static *)
let note_stubs ifc_decls impl_varmap =

  (* routines to help deal with the stub variables *)
  let stub_var v = "Stub?"^v in
  let is_stub v =
    try
      (String.sub v 0 5) = "Stub?"
    with e -> false in
  let stub_root v =
    if is_stub v then
      String.sub v 5 ((String.length v) - 5)
    else
      failwith 
	(Printf.sprintf 
	   "internal_error: called stub_root with non-stub |%s|" v) in

  (* adds mapping to the variable.  If mapping already present, adds
     to the domain; otherwise adds a new mapping *)
  let add_mapping varmap k v = (v,k)::varmap in
  let remove_mapping varmap k =
    let (mapping,varmap') =
      List.fold_right
	(fun (v',k') (mapping,varmap) ->
	  if mapping <> None then (mapping,(v',k')::varmap)
	  else
	    if k = k' then (Some (v',k'),varmap)
	    else (None, (v',k')::varmap))
	varmap (None,[]) in
    match mapping with
      None -> failwith 
	  (Printf.sprintf "could not remove mapping for %s from upd_varmap"
	     k)
    | Some (v,k) -> (v,k,varmap') in

  (* if this is a stub declaration, add the mapping the variable map *)
  let rec proc_stub_decls upd_varmap add_varmap tds =
    match tds with
      [] -> (upd_varmap, add_varmap)
    | (rawdecl,_)::rest ->
	let (upd_varmap, add_varmap) = 
	  (match rawdecl with
	    FunDecl { fn_name = n } ->
	      if (is_stub n) then
		(* move existing mapping from upd_varmap to add_varmap *)
		let root_name = (stub_root n) in
		let (v,k,upd_varmap) = remove_mapping upd_varmap root_name in
		(add_mapping upd_varmap root_name n,
		 add_mapping add_varmap k v)
	      else
		(upd_varmap, add_varmap)
	  | PrefixDecl (v,tds) -> 
	      failwith "proc_stub_decls: prefix found---should be none"
	  | OpenDecl (v,tds) ->
	      failwith "proc_stub_decls: open found---should be none"
	      (* proc_stub_decls upd_varmap add_varmap tds *)
	  | _ -> (upd_varmap, add_varmap)) in
	proc_stub_decls upd_varmap add_varmap rest in

  (* note all of the stub names in the variable map *)
(*
  print_endline "impl-variable map is:";
  List.iter (function (k,v) -> Printf.printf "%s=%s\n" k v) impl_varmap;
*)
  proc_stub_decls impl_varmap [] ifc_decls


(*----------------------------------------------------------------------*)
(* finds all externs with prefix Old or in the mod::Local::x format,
   and adds them to the lookup-varmap *) 
let note_old_and_local_defs ifc_decls =

  (* routines to help deal with the old & variables *)
  let is_old v =
    try
      (String.sub v 0 4) = "Old?"
    with e -> false in
  let old_root v =
    if is_old v then
      String.sub v 4 ((String.length v) - 4)
    else
      failwith 
	(Printf.sprintf 
	   "internal_error: called old_root with non-old |%s|" v) in
  let is_local v =
    match (get_prefix v) with
      Some (p,rest) ->
	(match (has_prefix rest "Local?") with
	  Some _ -> true
	| None -> false)
    | None -> false in

  (* adds mapping to the variable.  If mapping already present, adds
     to the domain; otherwise adds a new mapping *)
  let add_mapping varmap k v = (v,k)::varmap in

  (* if this is a stub declaration, add the mapping the variable map *)
  let rec proc_decls lookup_varmap tds =
    match tds with
      [] -> lookup_varmap
    | (rawdecl,_)::rest ->
	let lookup_varmap = 
	  (match rawdecl with
	    ExternVal (v,t) ->
	      if (is_old v) then 
		let root_name = (old_root v) in
		add_mapping lookup_varmap root_name v
	      else if (is_local v) then
		add_mapping lookup_varmap v v
	      else
		lookup_varmap 
	  | ExceptionDecl (v,s,t) ->
	      if (s = Extern) && (is_old v) then 
		let root_name = (old_root v) in
		add_mapping lookup_varmap root_name v
	      else if (is_local v) then
		add_mapping lookup_varmap v v
	      else
		lookup_varmap 
	  | _ -> lookup_varmap) in
	proc_decls lookup_varmap rest in

  (* note all of the old names in the variable map *)
  proc_decls [] ifc_decls
  
(*----------------------------------------------------------------------*)
(* entry point *)
let patchtrans impl_decls ifc_decls svars rvars =

  (* Convert patchfile shared variable names to internal names *)
  let svars = List.map internal_varname svars in
  let rvars = List.map 
      (function (k,v) -> 
	internal_varname k, internal_varname v) rvars in
  (* A little checking---no variable in svars should appear in
     the domain of rvars *)
  List.iter (fun v ->
    if (List.mem_assoc (new_var v) rvars) then
      failwith 
	(Printf.sprintf "shared variable %s is in the rename list: %s=%s\n"
	   v (new_var v) (List.assoc (new_var v) rvars))
    else ()) svars;
(*
  print_endline "shared variable list is:";
  List.iter print_endline svars;

  print_endline "rename-variable map is:";
  List.iter (function (k,v) -> Printf.printf "%s=%s\n" k v) rvars;
*)
  (* Now perform the translation *)
  let (impl_varmap, decls) = 
    translate_decls impl_decls ifc_decls svars rvars in
(*
  Printf.printf "popdynpatch: after translation of impl file:\n";
  pr_popprogram Format.std_formatter std_opts decls;
*)
  let (upd_varmap, add_varmap) = note_stubs decls impl_varmap in
  let lookup_varmap = note_old_and_local_defs decls in
(*
  print_endline "final add variable map is:";
  List.iter (function (k,v) -> Printf.printf "%s -> %s\n" k v) add_varmap;

  print_endline "final update variable map is:";
  List.iter (function (k,v) -> Printf.printf "%s -> %s\n" k v) upd_varmap;

  print_endline "final lookup variable map is:";
  List.iter (function (k,v) -> Printf.printf "%s -> %s\n" k v) lookup_varmap;
*)
  (add_varmap,upd_varmap,lookup_varmap,decls)
