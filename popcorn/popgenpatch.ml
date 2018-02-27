(**********************************************************************)
(* (c) Michael Hicks                                                  *)
(*     May 2000, all rights reserved.                               *)
(**********************************************************************)
(*
   This code compares two Popcorn sourcefiles and determines
   the elements of the file that have changed.  From this information,
   it generates a patch file and an interface code file
*)

(* TODO:

1) need to deal with scope better.  What happens if: 
   - especially have to deal with types---don't even do this for
   non-automatic patches/updates.

   - a formerly static declaration becomes non-static but is otherwise
   the same?  In the case that we don't normally store static vars in
   the sym table, it may be treated as if it were "new" declaration.

   - the opposite happens?  In this case, we have to remove the
   declaration from the global type interface if it was a type, or
   from the symbol table if it's a symbol.

2) read from multiple typename mapping files?  I could imagine having
   one for libraries, one for the executable, etc.

3) auto-generation for abstract types that change names?  We could use
   the definition (in the case that this is the file that's defined
   the type), and then rely on the users of the type to get the name
   from the rename-vars file.

4) what happens if the old file has an extern version of a definition
   in the new file?  Do we consider it the same function?  This could
   happen if we move functions around among files.  Seems like we do
   want to pretend as if it were defined in the old file.

   It may also be that there is no extern, but a function that appears
   to be "added" has just been moved from another file.  A more global
   analysis would detect this.

5) in eq_type could deal with type variables differently so that
   superficial name changes would not indicate a new type ...
*)

open Popsyntax

(************)
(* UTILTIES *)
(************)
let debug = false
let dprint s = 
  if debug then
    (Printf.eprintf "%s" s; flush Pervasives.stderr)
  else ()

let internal_error file msg =
  failwith (Printf.sprintf "internal_error: %s: %s\n" file msg)

let remove_duplicates l =
  let rec aux x l =
    match l with
      [] -> []
    | (h::t) -> 
	if x = h then (aux x t) 
	else h::(aux x t) in
  let rec aux2 l =
    match l with
      [] -> []
    | (h::t) ->
	let t' = aux h t in
	h::(aux2 t') in
  aux2 l

let merge_unique l1 l2 =
  List.fold_left
    (fun l x -> if (List.mem x l) then l else x::l)
    l2 l1

let print_list l toString print_string =
  let print_spc =
    let noreturn = ref true in
    function () ->
      if !noreturn then
	(print_string "  "; noreturn := false)
      else
	print_string "\n  " in
  let rec aux = function
      [] -> ()
    | [x] ->
        let s = toString x in
	if s <> "" then
          (print_spc (); print_string s)
    | (h::t) ->
        let s = toString h in
	if s <> "" then
          (print_spc (); print_string s);
        aux t in
  aux l; print_string "\n"

let new_var v = "New?"^v
let is_new v = try (String.sub v 0 4) = "New?" with e -> false
let kill_new v = String.sub v 4 ((String.length v)-4)

let stub_var v = "Stub?"^v

let bogus_loc = (Gcdfec.seg_of_abs 0 0)

let make_exp e = 
  { raw_exp = e; exp_typ = None; exp_loc = bogus_loc;
    exp_un_after = mt_varset; }

let new_id =
  let count = ref 0 in 
  (function x -> let c = !count in incr count; x^"__"^(string_of_int c))

let make_stmt rs = 
  { raw_stmt = rs; stmt_loc = bogus_loc;
    un_after = mt_varset; un_before = mt_varset }

let fd_of_fntype t nm scope exp =
  match t with
    FnType (c, vs,rett,argts) ->
      let n = ref 0 in
      let freshv () = 
	let nm = "v"^(string_of_int !n) in
	incr n;
	nm in
      let rec doargs ts =
	(match ts with
	  [] -> []
	| (t::ts) -> (freshv (),t)::(doargs ts)) in
      { fn_static = scope;
	fn_convention = c;
	fn_name = nm;
	fn_tyvars = vs;
	fn_ret_type = rett;
	fn_args = doargs argts;
	fn_body = exp }
  | _ -> internal_error "fd_of_fntype" 
	(Printf.sprintf "called with non function-type %s"
	   (typ2string t))

(* returns name, whether it is a value def?, and the scope *)
let get_info d =
  match d with
    FunDecl fd -> (fd.fn_name,true,if fd.fn_static then Public else Static)
  | StructDecl sd -> (sd.st_name,false,sd.st_scope)
  | UnionDecl ud -> (ud.un_name,false,ud.un_scope)
  | AbsDecl ad -> (ad.abs_name,false,ad.abs_scope)
  | ExceptionDecl (v,s,_) -> (v,true,s)
  | ExternType (v,_,_) -> (v,false,Extern)
  | ExternVal (v,_) -> (v,true,Extern)
  | GlobalDecl (s,v,_,_) -> (v,true,s)
  | _ -> internal_error "get_info" "found open or prefix decl"

type typedecl =
    Struct of structdecl
  | Union of uniondecl
  | Abstype of absdecl
  | AbstractD of (var list * bool)

(* Generate a new name for a type based on the contents of its 
   implementation *)
let xform_typevar n td =
  (* create a string name for the type definition *)
  let bogus_loc = (Gcdfec.seg_of_abs 0 0) in
  let dcl =
    match td with
      Struct sd -> (StructDecl sd,bogus_loc)
    | Union ud -> (UnionDecl ud,bogus_loc)
    | Abstype ad -> (AbsDecl ad,bogus_loc)
    | AbstractD (vs,null) -> failwith "no support for renaming abstract types"
  in
  let tdstr = topdecl2string dcl in
  (* create the digest *)
  let tdhash = Digest.string tdstr in
  (* make it just identifier-friendly characters *)
  (* ... first convert it to base64 representation *)
  let idstr' = 
    Base64.base64string_of_string tdhash 0 (String.length tdhash) in
  (* ... then replace /, =, and + characters with _A, _B, and _C *)
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
	  (cur_idx+1,acc ^ (String.sub s lst_idx (cur_idx-lst_idx)) ^ cstr)
	else
	  (lst_idx,acc) in
      aux s (cur_idx+1) new_idx new_acc len in
  let idstr = aux idstr' 0 0 "" (String.length idstr') in
  idstr

type valtype =
    Ptr of typ
  | Name of typ

let typeof_valtype vt =
  match vt with 
    Ptr t -> t
  | Name t -> t

let valtype_is_fun vt =
  match vt with
    Name (FnType _) -> true
  | Ptr (FnType _) -> failwith "found fun pointer!"
  | _ -> false

let valtype_is_exndecl vt =
  match vt with
    Name (ExnconType _) -> true
  | _ -> false

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

(* makes filename have an absolute path, based on the given working
   directory *)
let absolute_dir d wd = 
  (* runs through the directory string and removes all occurrences of . *)
  let ditch_dot s =
    let rec aux curridx lastidx len res =
      if (len - curridx <= 1) then 
	res ^ (String.sub s lastidx (len-lastidx))
      else
	if s.[curridx] = '.' then
	  if s.[curridx+1] = '/' then
	    aux (curridx+2) (curridx+2) 
	      len (res^ (String.sub s lastidx (curridx-lastidx)))
	  else if s.[curridx+1] = '.' then
	    aux (curridx+2) lastidx len res
	  else
	    aux (curridx+1) lastidx len res
	else
	  aux (curridx+1) lastidx len res in
    aux 0 0 (String.length s) "" in
  (* compresses strings of multiple /'s into a single / *)
  let ditch_slashslash s =
    let rec aux curridx lastidx len res =
      if (len - curridx <= 1) then 
	res ^ (String.sub s lastidx (len-lastidx))
      else
	if s.[curridx] = '/' &&
	  s.[curridx+1] = '/' then
	  aux (curridx+1) (curridx+1) 
	    len (res^ (String.sub s lastidx (curridx-lastidx)))
	else
	  aux (curridx+1) lastidx len res in
    aux 0 0 (String.length s) "" in
  (* runs through the directory and eliminates all occurrences of ..
     by eliminating previous elements of the directory.  This will
     only work properly (for sure) on absolute paths *)
  let ditch_dotdot s =
    let res = String.create (String.length s) in
    let rec aux sidx slen residx resdiridxs =
      if sidx = slen then 
	String.sub res 0 residx
      else
	if (slen - sidx) >= 3 &&
	   s.[sidx] = '/' &&
	   s.[sidx+1] = '.' &&
	   s.[sidx+2] = '.' then
          (* found a ..; blow away last directory component *)
	  if resdiridxs = [] then
	    failwith "found a .. without a corresponding directory component"
	  else
	    aux (sidx+3) slen (List.hd resdiridxs) (List.tl resdiridxs)
	else
	  (res.[residx] <- s.[sidx];
	   let resdiridxs = 
	     if s.[sidx] = '/' then residx::resdiridxs else resdiridxs in
	   aux (sidx+1) slen (residx+1) resdiridxs) in
    aux 0 (String.length s) 0 [] in
  (*--------------------------------------------------*)
  (* if filename is relative, then add the working directory to the front *)
  let d = if Filename.is_relative d then Filename.concat wd d else d in
  (* remove extra /'s *)
  let d = ditch_slashslash d in
  (* make sure it ends in a / *)
  let d = if d.[(String.length d)-1] <> '/' then (d^"/") else d in
  (* remove all of the .'s *)
  let d = ditch_dot d in
  (* remove all of the ..'s *)
  let d = ditch_dotdot d in
  (* result is the absolute path of the directory *)
  d

let absolute_path f =
  let cwd = Sys.getcwd () in
  let dir = Filename.dirname f in
  let file = Filename.basename f in
  let abs_dir = absolute_dir dir cwd in
  Filename.concat abs_dir file

(* naming for automatically generated conversion functions *)
let old_name_of n =
  n ^ "__old2new"
let new_name_of n =
  n ^ "__new2old"
let typename_from n =
  try
    let l = String.length n in
    let s = String.sub n (l-9) 9 in
    if (s = "__old2new") || (s = "__new2old") then
      String.sub n 0 (l-9)
    else
      failwith "is not a proper conversion function name"
  with _ ->
    failwith "not a proper conversion function name"

(*----------------------------------------------------------------------*)
(* Flags *)
let overwrite_patch_files = ref false
let export_locals = ref false
let verbose = ref false
let add_sametype_stubs = ref false
let value_sharing = ref false

(* Global data *)
(* used to generate the patch file and interface code file *)
let rename_vars = ref []
let sharing = ref []
let datas = ref []
let same_datas = ref []
let stubs = ref []
let same_stubs = ref []

(* used in the convert file *)
let convert_decls = ref []
let convert_vars = ref []

(* used temporarily during the translation *)
let processed = ref []
let deleted = ref []
let checked_types = ref []
let checked_vals = ref []
let old_filename = ref ""
let old_modname = ref ""
let output_directory = ref (Sys.getcwd ())

let reset_global_data () =
  rename_vars := [];
  sharing := [];
  processed := [];
  deleted := [];
  stubs := [];
  same_stubs := [];
  datas := [];
  same_datas := [];
  checked_types := [];
  checked_vals := [];
  old_filename := "";
  old_modname := "";
  convert_decls := [];
  convert_vars := []

let print_global_data () =
  let valtype2str vt =
    match vt with 
      Ptr t -> "(Ptr "^(typ2string t)^")"
    | Name t -> "(Name "^(typ2string t)^")" in
  let scope2str s =
    match s with
      Static -> "static"
    | Extern -> "extern"
    | Public -> ""
    | Abstract -> "abstract" in
  print_endline "type_equalities:";
  print_list !checked_types (fun (v,b) -> 
    Printf.sprintf "(%s,%s)" v (if b then "true" else "false"))
    print_string;
  print_endline "\nvalue_equalities:";
  print_list !checked_vals (fun (v,b) -> 
    Printf.sprintf "(%s,%s)" v (if b then "true" else "false"))
    print_string;
  print_endline "\nrename_vars:";
  print_list !rename_vars (fun (ov,nv) -> Printf.sprintf "(%s,%s)" ov nv)
    print_string;
  print_endline "\nsharing:";
  print_list !sharing (fun v -> v) print_string;
  print_endline "\ndatas:";
  print_list !datas (fun (n,t1,s1,t2,s2) -> 
    Printf.sprintf "(%s,%s %s,%s %s)" 
      n (scope2str s1) (typ2string t1) (scope2str s2) (typ2string t2))
    print_string;
  print_endline "\ndatas (same type):";
  print_list !same_datas (fun (n,t1,s1,t2,s2) -> 
    Printf.sprintf "(%s,%s %s,%s %s)" 
      n (scope2str s1) (typ2string t1) (scope2str s2) (typ2string t2))
    print_string;
  print_endline "\nstubs:";
  print_list !stubs (fun (n,t1,s1,t2,s2) -> 
    Printf.sprintf "(%s,%s %s,%s %s)" 
      n (scope2str s1) (typ2string t1) (scope2str s2) (typ2string t2))
    print_string;
  print_endline "\nstubs (same type):";
  print_list !same_stubs (fun (n,t1,s1,t2,s2) -> 
    Printf.sprintf "(%s,%s %s,%s %s)" 
      n (scope2str s1) (typ2string t1) (scope2str s2) (typ2string t2))
    print_string;
  print_endline "\nconversion variables:";
  print_list !convert_vars (function x -> x) print_string;
  print_endline "\ndeleted definitions:";
  print_list !deleted (function x -> x) print_string

let add_to_rename_vars oldv newv = 
  try 
    let curr_newv = List.assoc oldv !rename_vars in 
    if curr_newv <> newv then 
      internal_error "add_to_rename_vars" 
	(Printf.sprintf 
	   "tried to add (%s,%s) to rename_vars, but already maps to %s\n" 
	   oldv newv curr_newv) 
    else ()
  with Not_found -> 
    rename_vars := (oldv,newv)::(!rename_vars)

(* given the type as it appears in the program, we use the generated
   global information to change the type to how it would appear in
   the interface code file *)
let new_type t =
  let rec aux t =
    match t with
      VoidType -> t
    | Evar(c,r) ->
	(match !r with
	  None -> t
	| Some t' -> Evar (c, ref (Some (aux t'))))
    | VarType _ -> t
    | IntType(_,_) -> t
    | BooleanType -> t
    | StringType -> t
    | CharType -> t
    | FloatType -> t
    | DoubleType -> t
    | ArrayType(t',eopt) -> ArrayType (aux t',eopt)
    | FnType (c,vs,t',ts) -> FnType (c,vs,aux t',auxl ts)
    | TupleType (c,ts) -> TupleType (c,auxl ts)
    | NamedType (tn,ts) -> 
	(* convert the type name:
	   if the the new version of the name is in the rename list,
	   then use it *)
	let ts' = auxl ts in
	let tn' = 
	  if (List.mem_assoc (new_var !tn) !rename_vars) then ref (new_var !tn)
	  else tn in
	NamedType (tn',ts')
    | ExnType -> t
    | MutableTyp tr -> MutableTyp (ref (aux !tr))
    | UnresolvedTyId(_,_) -> 
	internal_error "new_type" "found UnresolvedTyId"
    | RepType t' -> RepType (aux t')
    | ExnconType t' -> ExnconType (aux t')
  and auxl ts =
    List.map aux ts in
  aux t
	  
(* returns true if the typename n is equal in both files, false if
   not equal.  Raises Not_found if we haven't processed it yet, and
   adds it to the list of checked types as true *)
let compared_X xlist n =
  try
    List.assoc n !xlist
  with Not_found ->
    xlist := (n,true)::!xlist;
    raise Not_found

let compared_types = compared_X checked_types 
let compared_vals = compared_X checked_vals
  
(* called when we determine that a type is not equal; types are
   assumed to be equal in general, and added to the list by
   compared_types *)
let note_unequal_X xlist n =
  xlist := (n,false)::(List.remove_assoc n !xlist)

let note_unequal_type = note_unequal_X checked_types
let note_unequal_val = note_unequal_X checked_vals

(* warnings ... *)

let default_error_level = 1
let verbose_error_level = 2
let error_level = ref default_error_level
let emit_warning_level s l =
  if l <= !error_level then
    Printf.eprintf "Warning (file %s): %s\n" !old_modname s
  else ()
let emit_warning s = emit_warning_level s default_error_level

(*----------------------------------------------------------------------*)
type env = { global_env: Poptype.global_env;
	     decls: top_decl list }

(*----------------------------------------------------------------------*)
(* look up the defintion of the given type name in the given 
   global environment. *)
let lookup_typedecl n global_env =
  try 
    Struct (Dict.lookup global_env.Poptype.structs n)
  with Dict.Absent ->
    (try 
      Union (Dict.lookup global_env.Poptype.unions n)
    with Dict.Absent -> 
      (try
	AbstractD (Dict.lookup global_env.Poptype.abstracts n)
      with Dict.Absent ->
	(try
	  Abstype(Dict.lookup global_env.Poptype.abstypes n)
	with Dict.Absent ->
	  raise Not_found)))

let lookup_valdecl n global_env =
  try 
    let t = Dict.lookup global_env.Poptype.exceptions n in 
    Name (ExnconType t)
  with Dict.Absent -> 
    (try
      Name (Dict.lookup global_env.Poptype.functions n)
    with Dict.Absent ->
      (try 
	let (b,t) = Dict.lookup global_env.Poptype.globals n in 
	Ptr t
      with Dict.Absent ->
	raise Not_found))

(**********************)
(* SYNTACTIC EQUALITY *)
(**********************)

(* All of the routines that follow test whether various elements of
   Popcorn syntax are equal *)

let eq_list f l l' = 
  try
    (List.fold_left2 
       (fun res a a' -> if res then f a a' else false)
       true l l')
  with Invalid_argument _ -> false

let eq_opt f x x' =
  match x, x' with
    None, None -> true
  | Some x, Some x' -> f x x'
  | _, _ -> false

(* recursively check if the two type definitions are equal.  The thing
   is, we want to use the environment for the new file for t1 and the
   environment for the old file for t2.  This is needed to make sure
   that the namedtypes in each type have the same definition (obvious
   in the poptype case, not here).  How to deal with abstract types?
   Punt for now.  It's also not clear how we want to deal with evars;
   don't permit them for now (since typechecking should take care of
   them) and see how it goes.

   If the types are not equal, we should add New::t1=... to the rename
   var list, indicating that the type differs in the new version and
   should be renamed.  One way to determine ... is to calculate the
   MD5 hash of the definition of the type (if available). *)
exception Unify
let rec eq_type t1 env1 t2 env2 =
  let eq_typeopt' t t' = eq_typeopt t env1 t' env2 in
  let rec un t1 t2 =
    (* compress the types to get rid of any indirection *)
    let t1 = compress t1 in
    let t2 = compress t2 in
    (* don't check for structural equality because the NamedTypes
       present in each type may not have the same definitions, and
       thus the types are not equal *)
    match t1,t2 with
      Evar (vc1,tor1), Evar (vc2,tor2) ->
	if (vc1 = vc2) && (eq_typeopt' !tor1 !tor2) then () else
	raise Unify
    | MutableTyp _, _ -> 
	Printf.eprintf 
	  "eq_type found MutableTyp in (%s,%s)" 
	  (typ2string t1) (typ2string t2); 
	raise Unify
    | _, MutableTyp _ -> un t2 t1
    | UnresolvedTyId _, _ -> 
	Printf.eprintf 
	  "eq_type found UnresolvedTyId in (%s,%s)" 
	  (typ2string t1) (typ2string t2); 
	raise Unify
    | _, UnresolvedTyId _ -> un t2 t1
    | VoidType,VoidType -> ()
    | IntType (b1,s1), IntType (b2,s2) -> 
	if (b1 = b2) && (s1 = s2) then () else raise Unify
    | BooleanType, BooleanType -> ()
    | StringType, StringType -> ()
    | CharType, CharType -> ()
    | FloatType, FloatType -> ()
    | DoubleType, DoubleType -> ()
    | ExnType, ExnType -> ()
    | ArrayType(t1,_),ArrayType(t2,_) -> 
	(* XXX deal with expression part? *) un t1 t2
    | FnType(c1,[],t1,ts1),FnType(c2,[],t2,ts2) -> 
	if c1 = c2 then	uns (t1::ts1) (t2::ts2)
	else raise Unify
    | FnType(c1,vs1,t1,ts1),FnType(c2,vs2,t2,ts2) ->
	let inst = 
	  try List.combine vs1 (List.map (fun v -> VarType v) vs2) 
	with Invalid_argument _-> raise Unify in
	if c1 = c2 then	uns (t1::ts1) (Poptype.type_substs inst (t2::ts2))
	else raise Unify
    | TupleType(c1,ts1),TupleType(c2,ts2) -> 
	if c1 = c2 then uns ts1 ts2 else raise Unify
    | NamedType(tn1,ts1),NamedType(tn2,ts2) ->
	(* check if the names are the same *)
	if (!tn1 <> !tn2) then raise Unify
	else 
	  let is_equal =
            (* check if the declarations are the same *)
	    (try
	      (* see if we've already compared them *)
	      compared_types !tn1
	    with Not_found ->
	      let d1 = 
		try
		  lookup_typedecl !tn1 env1.global_env 
		with Not_found ->
		  internal_error "eq_type" (!tn1^" not found in new env") in
	      let d2 = 
		try 
		  lookup_typedecl !tn2 env2.global_env 
		with Not_found -> 
		  internal_error "eq_type" (!tn2^" not found in old env") in
	      if not (eq_typedecl d1 env1 d2 env2) then
		(note_unequal_type !tn1; false)
	      else true) in
	  if is_equal then () else raise Unify
    | VarType(x),VarType(y) -> if x = y then () else raise Unify
    | RepType(t1),RepType(t2) -> un t1 t2
    | ExnconType(t1),ExnconType(t2) -> un t1 t2
    | _,_ -> raise Unify
  and uns ts1 ts2 =
    match ts1, ts2 with
      [],[] -> ()
    | t1::ts1,t2::ts2 -> un t1 t2; uns ts1 ts2
    | _,_ -> raise Unify
  in try un t1 t2; true with Unify -> false

and eq_typelist ts1 env1 ts2 env2 =
  let eq_type' t1 t2 = eq_type t1 env1 t2 env2 in
  eq_list eq_type' ts1 ts2

and eq_typeopt to1 env1 to2 env2 =
  let eq_type' t1 t2 = eq_type t1 env1 t2 env2 in
  eq_opt eq_type' to1 to2

and eq_struct sd1 env1 sd2 env2 =
  let eq_fields fs1 fs2 =
    let eq_field (f1, c1, t1) (f2, c2, t2) =
      (f1 = f2) && (c1 = c2) && (eq_type t1 env1 t2 env2) in
    eq_list eq_field fs1 fs2 in
  (sd1.st_scope = sd2.st_scope) &&
  (sd1.st_tyvars = sd2.st_tyvars) &&
  (sd1.st_possibly_null = sd2.st_possibly_null) &&
  eq_fields sd1.st_fields sd2.st_fields

and eq_union ud1 env1 ud2 env2 =
  let eq_fields fs1 fs2 =
    let eq_field (f1, t1) (f2, t2) =
      (f1 = f2) && (eq_type t1 env1 t2 env2) in
    eq_list eq_field fs1 fs2 in
  (ud1.un_scope = ud2.un_scope) &&
  (ud1.un_tyvars = ud2.un_tyvars) &&
  (ud1.un_possibly_null = ud2.un_possibly_null) &&
  eq_fields ud1.un_fields ud2.un_fields

and eq_abstype ad1 env1 ad2 env2 =
  (ad1.abs_scope == ad2.abs_scope) &&
  (ad1.abs_all_tyvars == ad2.abs_all_tyvars) &&
  (ad1.abs_exist_tyvars == ad2.abs_exist_tyvars) &&
  (eq_type ad1.abs_defn env1 ad2.abs_defn env2)

(* check if two type definitions are equal.  Doesn't bother to compare the
   names of the types---assumes this was done by the caller *)
and eq_typedecl td1 env1 td2 env2 =
  match td1, td2 with
    Struct sd1, Struct sd2 -> eq_struct sd1 env1 sd2 env2
  | Union ud1, Union ud2 -> eq_union ud1 env1 ud2 env2
  | Abstype ad1, Abstype ad2 -> eq_abstype ad1 env1 ad2 env2
  | AbstractD (vs1, may_be_null1), AbstractD (vs2, may_be_null2) ->
      (vs1 = vs2) && (may_be_null1 = may_be_null2)
  | _,_ -> false

and eq_valtype vt1 env1 vt2 env2 =
  match vt1, vt2 with
    Ptr t1, Ptr t2 -> eq_type t1 env1 t2 env2
  | Name t1, Name t2 -> eq_type t1 env1 t2 env2
  | _, _ -> false

and eq_valdecl d1 env1 d2 env2 =
  let eq_expopt' eopt1 eopt2 = 
    eq_expopt eopt1 env1 eopt2 env2 [] in
  (* ... these helper functions don't check name or type
     equality here, as it's assumed from previous checks *)
  let eq_fndecl' fd fd' = eq_fndecl fd env1 fd' env2 in
  let eq_exndecl (_,s1,t1) (_,s2,t2) = (s1 = s2) in
  let eq_glbldecl (s1,_,_,eor1) (s2,_,_,eor2) =
    (s1 = s2) && (eq_expopt' !eor1 !eor2) in
  let eq_decls d1 d2 =
    match d1,d2 with
      FunDecl fd1, FunDecl fd2 -> eq_fndecl' fd1 fd2
    | ExceptionDecl ed1, ExceptionDecl ed2 -> eq_exndecl ed1 ed2
    | GlobalDecl gd1, GlobalDecl gd2 -> eq_glbldecl gd1 gd2
    | ExternVal (v1,t1), ExternVal (v2,t2) -> true
	(* assumes name and type were checked before calling this fun *)
    | _, ExternVal (v,t) -> false
    | ExternVal (v,t), _ -> false
    | _,_ -> invalid_arg "eq_valdecl" in
  eq_decls d1 d2

(* checks if two statements are syntactically equal *)
and eq_stmt s1 env1 s2 env2 local_env =
  let eq_exp' e1 e2 = eq_exp e1 env1 e2 env2 local_env in
  let eq_stmt' s1 s2 = eq_stmt s1 env1 s2 env2 local_env in
  let eq_type' t1 t2 = eq_type t1 env1 t2 env2 in
  let eq_typeopt' t t' = eq_typeopt t env1 t' env2 in
  let eq_expopt' eopt1 eopt2 = 
    eq_expopt eopt1 env1 eopt2 env2 local_env in
  let eq_stmtopt' s1 s2 = 
    eq_stmtopt s1 env1 s2 env2 local_env in
  (* XXX add vars to local env *)
  let eq_sarm sa sa' = 
    let eq_prim_pat pp pp' =
      match pp,pp' with
	Wild_pat tr, Wild_pat tr' -> eq_type' !tr !tr'
      | Var_pat (v,tr), Var_pat (v',tr') ->
	  (v = v') && (eq_type' !tr !tr')
      | _,_ -> false in
    let eq_pat p p' =
      match p,p' with
	No_pat, No_pat -> true
      |	Prim_pat pp, Prim_pat pp' -> eq_prim_pat pp pp'
      |	Tuple_pat pl, Tuple_pat pl' ->
	  List.fold_left2
	    (fun res pp pp' -> res && (eq_prim_pat pp pp'))
	    true pl pl'
      |	_,_ -> false in
    let bound_arm_vars p = (* figures out all of the bound locals *)
      let do_prim_pat p =
	match p with
	  Var_pat (v,_) -> [v]
	| _ -> [] in
      match p with
      	No_pat -> []
      |	Prim_pat p -> do_prim_pat p
      |	Tuple_pat pl ->
	  List.flatten (List.map do_prim_pat pl) in
    match sa,sa' with
      { arm_field = f; arm_pat = p; arm_body = s },
      { arm_field = f'; arm_pat = p'; arm_body = s' } ->
	(f = f') && (eq_pat p p') && 
	(eq_stmt s env1 s' env2 ((bound_arm_vars p)@local_env)) in
  (*------------------------------------------------------------*)
  let s1 = s1.raw_stmt in
  let s2 = s2.raw_stmt in
  match s1,s2 with
  | Skip, Skip -> true
  | Exp e, Exp e' -> eq_exp' e e'
  | Seq (s,s2), Seq (s',s2') -> (eq_stmt' s s') && (eq_stmt' s2 s2')
  | Return eopt, Return eopt2 -> eq_expopt' eopt eopt2
  | IfThenElse (e,s1,s2), IfThenElse (e',s1',s2') ->
      (eq_exp' e e') && (eq_stmt' s1 s1') && (eq_stmt' s2 s2')
  | While (e,s), While (e',s') -> (eq_exp' e e') && (eq_stmt' s s')
  | Break vopt, Break vopt2 -> vopt = vopt2
  | Continue vopt, Continue vopt2 -> vopt = vopt2
  | For (e1,e2,e3,s), For (e1',e2',e3',s') ->
      (eq_exp' e1 e1') && (eq_exp' e2 e2') && (eq_exp' e3 e3') &&
      (eq_stmt' s s')
  | IntSwitch (e, isl, s), IntSwitch (e', isl', s') ->
      (eq_exp' e e') && 
      (eq_list (fun (i,s) (i',s') -> (i = i') && (eq_stmt' s s')) isl isl') &&
      (eq_stmt' s s')
  | CharSwitch (e,csl,s), CharSwitch (e',csl',s') ->
      (eq_exp' e e') && 
      (eq_list (fun (c,s) (c',s') -> (c = c') && (eq_stmt' s s')) csl csl') &&
      (eq_stmt' s s')
  | UnionSwitch (e,sal,sopt), UnionSwitch (e',sal',sopt') ->
      (eq_exp' e e') && (eq_list eq_sarm sal sal') && 
      (eq_stmtopt' sopt sopt')
  | ExnSwitch (e,sal,sopt), ExnSwitch (e',sal',sopt') ->
      (eq_exp' e e') && (eq_list eq_sarm sal sal') && 
      (eq_stmtopt' sopt sopt')
  | Decl (v,t,eor,s), Decl (v',t',eor',s') ->
      (v = v') && (eq_type' t t') && (eq_expopt' !eor !eor') &&
      (eq_stmt s env1 s' env2 (v::local_env))
  | Label (v,s), Label (v',s') ->
      (v = v') && (eq_stmt' s s')
  | Do (s,e), Do (s',e') -> (eq_stmt' s s') && (eq_exp' e e')
  | TryHandle (s1,v,s2), TryHandle (s1',v',s2') ->
      (eq_stmt' s1 s1') && (v = v') && 
      (eq_stmt s2 env1 s2' env2 (v::local_env))
  | TryCatchFinally (s,sal,defopt,finopt), 
    TryCatchFinally (s',sal',defopt',finopt') ->
      (eq_stmt' s s') && (eq_list eq_sarm sal sal') &&
      (eq_stmtopt' defopt defopt') &&
      (eq_stmtopt' finopt finopt')
  | Cut s, Cut s' -> eq_stmt' s s'
  | Splice s, Splice s' -> eq_stmt' s s'
  | With (v,tor,vl,e,s), With (v',tor',vl',e',s') ->
      (v = v') && (eq_typeopt' !tor !tor') &&
      (vl = vl') && (eq_exp' e e') && 
      (eq_stmt s env1 s' env2 (v::local_env))
  | _,_ -> false

and eq_stmtlist sl1 env1 sl2 env2 local_env =
  let eq_stmt' s1 s2 = eq_stmt s1 env1 s2 env2 local_env in
  eq_list eq_stmt' sl1 sl2

and eq_stmtopt so1 env1 so2 env2 local_env =
  let eq_stmt' s1 s2 = eq_stmt s1 env1 s2 env2 local_env in
  eq_opt eq_stmt' so1 so2

and eq_fndecl fd1 env1 fd2 env2 =
  let eq_type' t1 t2 = eq_type t1 env1 t2 env2 in
  (fd1.fn_static = fd2.fn_static) &&
  (fd1.fn_name = fd2.fn_name) &&
  (fd1.fn_tyvars = fd2.fn_tyvars) &&
  (eq_type' fd1.fn_ret_type fd2.fn_ret_type) &&
  (eq_list (fun (v,t) (v',t') -> (v = v') && (eq_type' t t'))
     fd1.fn_args fd2.fn_args) &&
  (let (vs,_) = List.split fd1.fn_args in
  eq_stmt fd1.fn_body env1 fd2.fn_body env2 vs)

and eq_exp e1 env1 e2 env2 local_env =
  let eq_exp' e1 e2 = eq_exp e1 env1 e2 env2 local_env in
  let eq_explist' el el' = 
    eq_explist el env1 el' env2 local_env in
  let eq_expopt' eopt eopt' = 
    eq_expopt eopt env1 eopt' env2 local_env in
  let eq_fndecl' fd fd' = eq_fndecl fd env1 fd' env2 in
  let eq_type' t1 t2 = eq_type t1 env1 t2 env2 in
  let eq_typeopt' t t' = eq_typeopt t env1 t' env2 in
  let eq_typelist' ts ts' = eq_typelist ts env1 ts' env2 in
  let eq_stmt' s1 s2 = eq_stmt s1 env1 s2 env2 local_env in
  let is_local v = List.mem v local_env in
  let def_not_found_err n t =
    internal_error "eq_exp"
      (Printf.sprintf "%s has type %s in file %s, but no declaration"
	 n (typ2string t) !old_filename) in
  let eq_var v v' =
    if (v = v') then
      if not (is_local v) then
	(try
	  compared_vals v
	with Not_found -> (* haven't compared these yet *)
	  (try
	    (let vt = lookup_valdecl v env1.global_env in
	    let vt' = lookup_valdecl v' env2.global_env in
	    (* first check their types *)
	    if not (eq_valtype vt env1 vt' env2) then
	      (note_unequal_val v; false)
	    (* types are the same, check if the decls themselves
	       have changed.  If so, then this is a change for the 
	       current function if these functions ARE LOCAL TO THIS
	       FILE.  XXX This is because all calls to
	       functions declared in the same file are not
	       indirected through the GOT.  In the future, we might
	       indirect exported ones through the GOT, in which
	       case, it won't matter if the function changed. *)
	    else 
	      (let (d1,_) = 
		try 
		  find_decl v true env1.decls
		with Not_found -> 
		  (let s = program2string env1.decls in
		  Printf.printf "New program\n==============\n%s" s;
		  def_not_found_err v (typeof_valtype vt)) in
	      let (d2,_) =
		try 
		  find_decl v true env2.decls
		with Not_found -> 
		  (let s = program2string env2.decls in
		  Printf.printf "Old program\n==============\n%s" s;
		  def_not_found_err v (typeof_valtype vt)) in
	      try
	          (* compare syntactically *)
		if not (eq_valdecl d1 env1 d2 env2) then
		  (note_unequal_val v; false)
		else true
	      with Invalid_argument _ ->
		(note_unequal_val v; false)))
	  with Not_found ->
	    internal_error "eq_exp"
	      ("could not find type of variable "^v)))
      else true (* XXX is this the right behavior for local vars? *)
    else false in
  let e1 = e1.raw_exp in
  let e2 = e2.raw_exp in
  match e1, e2 with
  | Const c, Const c' -> c = c'
  | ConstArray (el, topt), ConstArray (el', topt') ->
      (eq_explist' el el') && (eq_typeopt' topt topt')
  | Var v, Var v' -> eq_var v v'
  | Primop (op, el), Primop (op', el') ->
      (op = op') && (eq_explist' el el')
  | Conditional (be,ife,elsee), Conditional (be',ife',elsee') ->
      (eq_exp' be be') && (eq_exp' ife ife') && (eq_exp' elsee elsee')
  | AssignOp (lhs,opopt,rhs), AssignOp (lhs',opopt',rhs') ->
      (eq_exp' lhs lhs') && (opopt = opopt') && (eq_exp' rhs rhs')
  | FunCall (fexp, tlor, args), FunCall (fexp', tlor', args') ->
      (eq_exp' fexp fexp') && 
      (eq_opt (fun x x' -> eq_list eq_type' x x') !tlor !tlor') &&
      (eq_explist' args args')
  | TypInst (e,ts), TypInst (e',ts') ->
      (eq_exp' e e') && (eq_typelist' ts ts')
  | NewStruct (n,tlor,fnoptelist), NewStruct (n',tlor',fnoptelist') ->
      (n = n') &&
      (eq_opt (fun x x' -> eq_list eq_type' x x') !tlor !tlor') &&
      (eq_list 
	 (fun (fopt, e) (fopt', e') -> 
	   (eq_opt (=) fopt fopt') && (eq_exp' e e'))
	 fnoptelist fnoptelist')
  | StructMember (e,n), StructMember (e',n') -> (eq_exp' e e') && (n = n')
  | NewUnion (n, tlor, nf, eopt), NewUnion (n', tlor', nf', eopt') ->
      (n = n') &&
      (eq_opt (fun x x' -> eq_list eq_type' x x') !tlor !tlor') &&
      (nf = nf') &&
      (eq_expopt' eopt eopt')
  | UnionMember (e,n), UnionMember (e',n') -> (eq_exp' e e') && (n = n')
  | NewTuple el, NewTuple el' -> eq_explist' el el'
  | TupleMember (e,x), TupleMember (e',x') -> (eq_exp' e e') && (x = x')
  | NewAbstype (n, tlor, tlor2, e), NewAbstype (n', tlor', tlor2', e') ->
      (n = n') &&
      (eq_opt (fun x x' -> eq_list eq_type' x x') !tlor !tlor') &&
      (eq_opt (fun x x' -> eq_list eq_type' x x') !tlor !tlor') &&
      (eq_exp' e e')
  | Subscript (e,ie), Subscript (e',ie') -> (eq_exp' e e') && (eq_exp' ie ie')
  | Codegen fd, Codegen fd' -> eq_fndecl' fd fd'
  | Fill e, Fill e' -> eq_exp' e e'
  | NewExn (v,eopt), NewExn (v',eopt') -> 
      (eq_var v v') && (eq_expopt' eopt eopt')
  | Raise e, Raise e' -> eq_exp' e e'
  | SeqExp el, SeqExp el' -> eq_explist' el el'
  | Nop, Nop -> true
  | Cast (t,e), Cast (t',e') -> (eq_type' t t') && (eq_exp' e e')
  | Fun fd, Fun fd' -> eq_fndecl' fd fd'
  | RepTerm, RepTerm -> true
  | _,_ -> false

and eq_explist el1 env1 el2 env2 local_env =
  let eq_exp' e1 e2 = eq_exp e1 env1 e2 env2 local_env in
  eq_list eq_exp' el1 el2

and eq_expopt eo1 env1 eo2 env2 local_env =
  let eq_exp' e1 e2 = eq_exp e1 env1 e2 env2 local_env in
  eq_opt eq_exp' eo1 eo2

(********)
(* MAIN *)
(********)

(* Process declarations from the new implementation.  If an old
   version of the declaration exists, then we modify the generated
   state to be able to generate the appropriate parts in the patch spec
   and patch code. This returns whether the new declaration is different
   from the equivalent old one, and whether it is stat*)
and process_decl env1 env2 ((d1,loc) as newdecl) =
  (* look up the type of the given identifiers in the two
     environments, returning as a pair.  Raises Not_found if
     either is not found *)
  let get_valtypes n =
    (lookup_valdecl n env1.global_env, 
     lookup_valdecl n env2.global_env) in
  let get_typedecls n =
    (lookup_typedecl n env1.global_env, 
     lookup_typedecl n env2.global_env) in
  let eq_type' t1 t2 = eq_type t1 env1 t2 env2 in
  let eq_stmt' s1 s2 = eq_stmt s1 env1 s2 env2 in
  let eq_expopt' s1 s2 = eq_expopt s1 env1 s2 env2 [] in
  let eq_fndecl' fd1 fd2 = eq_fndecl fd1 env1 fd2 env2 in
  (* error conditions *)
  let same_type_err n t =
    internal_error "process_decl"
      (Printf.sprintf "%s has same type %s, but unlike declaration" 
	 n (typ2string t)) in
  let def_not_found_err n t =
    internal_error "process_decl"
      (Printf.sprintf "%s has type %s in file %s, but no declaration"
	 n (typ2string t) !old_filename) in
  (*-----------------------------------------------------------------*)
  try
    (let (n,is_val,scope1) = get_info d1 in
    (* if this is a value definition ... *)
    if (is_val) && (scope1 <> Extern) then
      (processed := n::(!processed);
      let (vt1,vt2) = get_valtypes n in
      (* find the old declaration *)
      let d2 = 
	try 
	  let (d2,_) = find_decl n true env2.decls in d2	
	with Not_found -> 
	  (let s = program2string env2.decls in
	  Printf.printf "Old program\n==============\n%s" s;
	  def_not_found_err n (typeof_valtype vt1)) in
      (* if the types match ... *)
      if (eq_valtype vt1 env1 vt2 env2) then
	((* see if the declarations match *)
	 let decls_equal =
	   try 
	     (* see if we've already compared them *)
	     compared_vals n
	   with Not_found ->
	     (try
	       (* compare syntactically *)
	       if not (eq_valdecl d1 env1 d2 env2) then
		 (note_unequal_val n; false)
	       else true
	     with Invalid_argument _ ->
	       same_type_err n (typeof_valtype vt1)) in
	 let is_fun1, is_exndecl1 = 
	   valtype_is_fun vt1, valtype_is_exndecl vt1 in
	 let is_fun2, is_exndecl2 = 
	   valtype_is_fun vt2, valtype_is_exndecl vt2 in
	 if decls_equal then
	   (* declarations are the same; note name in the sharing list ... *)
	   (processed := List.tl !processed;
	    if !value_sharing then 
	      (if scope1 = Public then
		sharing := n::(!sharing))
	    (* ... unless we're not sharing; then copy the data instead *)
	    else
	      (match is_fun1, is_fun2 with
		true, true -> ()
	      |	false, false ->
		  (match is_exndecl1, is_exndecl2 with
		    false, false ->
		      let (_,_,scope2) = get_info d2 in
		      same_datas := (n, (typeof_valtype vt2), scope2, 
				     (typeof_valtype vt1), scope1)::!same_datas
		  | true, true ->
		      emit_warning 
			(Printf.sprintf "Cannot propagate exception %s" n)
		  | _ -> 
		      internal_error "process_decl"
			(n^" has the same type in both but not of the"^
                           " same kind!" ))
	      |	_ ->
		  internal_error "process_decl"
		    (n^" has the same type in both but not of the same"^
                       " kind!" )))
	 else (* not the same, even if they have the same type.*)
	   (match is_fun1, is_fun2 with
           (* if both function types, make a stub *)
	     true, true ->
	       let (_,_,scope2) = get_info d2 in
	       same_stubs := (n, typeof_valtype vt2, scope2, 
			      typeof_valtype vt1, scope1)::!same_stubs

           (* both non-function types: copy the data & signal a warning *)
	   | false, false ->
	       (* can't do the copy if exception decls *)
	       (match is_exndecl1, is_exndecl2 with
		 false, false ->
		   if not !value_sharing then
		     (let (_,_,scope2) = get_info d2 in
(*
		     same_datas := (n, (typeof_valtype vt2), scope2, 
				    (typeof_valtype vt1),scope1)::!same_datas);
*)
		     (* add to the "change type" list, since the
			declaration changed---this implies that 
			there is an initializer so that the data must
			copied piecewise rather than inherited *)
		     (* XXX want to piecewise copy when there's an
			initializer and the data *hasn't* changed, too *)
		     datas := (n, (typeof_valtype vt2), scope2, 
			       (typeof_valtype vt1), scope1)::!datas);
		   emit_warning
		     (Printf.sprintf "%s:%s has a new declaration"
			n (typ2string (typeof_valtype vt1)))
	       | true, true ->
		   emit_warning 
			(Printf.sprintf "Cannot propagate exception %s" n)
	       | _ -> 
		   internal_error "process_decl"
		     (n^" has the same type in both but not of the"^
                      " same kind!" ))
	   | _ ->
	     internal_error "process_decl"
	       (n^" has the same type in both but not of the same kind!")))
      (* the types don't match *)
      else
	(note_unequal_val n;
        let is_fun1 = valtype_is_fun vt1 in
	let is_fun2 = valtype_is_fun vt2 in
	let (_,_,scope2) = get_info d2 in
	(* if both function types, make a stub *)
	if is_fun1 && is_fun2 then
	  stubs := (n, (typeof_valtype vt2), scope2, 
		    (typeof_valtype vt1), scope1)::!stubs
	(* if both non-function types, make a note *)
	else if (not is_fun1) && (not is_fun2) then
	  (datas := (n, (typeof_valtype vt2), scope2, 
		    (typeof_valtype vt1), scope1)::!datas;
	   (* if not static data, issue a warning that all
	      clients must be updated at the same time as this file *)
	   if (scope2 <> Static) then
	     emit_warning 
	       (Printf.sprintf "non-static data %s changed type" n))
	else
	  emit_warning (Printf.sprintf "overriding %s:%s with type %s"
	    n (typ2string (typeof_valtype vt1)) 
	    (typ2string (typeof_valtype vt2)))))

    (* this is a type definition (either local or extern) ... *)
    else if (not is_val) then
      (if (scope1 <> Extern) then
	processed := n::(!processed);
      (* first see if we already know the answer; this will also
         find types that don't have a declaration in the old file
         but are mentioned in the renamed-types file *)
      if List.mem_assoc n !checked_types then 
	(if (List.assoc n !checked_types) && (scope1 <> Extern) then
	  (if scope1 = Public then sharing := n::(!sharing);
	   processed := List.tl !processed)
	else ())
      else
	(* don't know; we have to check the hard way *)
	(let (td1,td2) = get_typedecls n in
        (* if the types match add to the sharing list *)
	if (eq_typedecl td1 env1 td2 env2) then
	  (* they match, add to the sharing list if not extern *)
	  (if (scope1 <> Extern) then
	    (if scope1 = Public then sharing := n::(!sharing);
	     processed := List.tl !processed)
	  else ())
	else
	  (note_unequal_type n;
	   add_to_rename_vars (new_var n) (xform_typevar n td1))))
    (* an extern function, just ignore it *)
    else ())
  with Not_found ->
    (* raised by get_valtypes and get_typedecls---means that that the 
       given declaration is new to the file, so we don't need to do 
       anything *)
    ()

(*----------------------------------------------------------------------*)
let gen_patch_data old_decls new_decls =

  (* First typecheck the two files.  This will generate the global
     environment, and convert the syntax appropriately. *)
  dprint "typechecking old decls\n";
  let (old_decls,global_env_old) = Poptype.type_check true old_decls in
  if debug then
    dprint (Poptype.global_env2string global_env_old);
  dprint "typechecking new decls\n";
  let (new_decls,global_env_new) = Poptype.type_check true new_decls in
  if debug then
    dprint (Poptype.global_env2string global_env_new);
  let old_env = { global_env = global_env_old;
		  decls = old_decls } in
  let new_env = { global_env = global_env_new;
		  decls = new_decls; } in

  (* Process each declaration in the new file *)
  dprint "processing new_decls\n";
  List.iter (process_decl new_env old_env) new_decls;
  (* Check for any deleted definitions *)
  dprint "looking for deleted decls\n";
  List.iter 
    (function (d,_) -> 
      let (n,is_val,scope) = get_info d in
      if scope <> Extern then 
	try
	  find_decl n is_val new_env.decls; ()
	with Not_found ->
	  deleted := n::(!deleted)
      else ()) old_decls;
  (* Print the results *)
  if !verbose then
    print_global_data ();
  (old_env,new_env)
	
(*-------------------------------------------------------------------------*)
(* determines all of the type declarations required for a given set of
   type declarations, tdecls.  Returns a superset of exist_decls;
   declarations are retrieved from alldecls.  This procedure acts
   recursively, *)
let pre = ref ""
let rec externs_of_typdecls tdecls exist_decls alldecls =
  (* returns a list of NamedType names that appear in the given 
     structure or union declaration *)
  let extern_names_for_decl td =
    let get_all f l =
      List.flatten (List.map f l) in
    let rec get_named_type t =
      match (Popsyntax.compress t) with
	NamedType (tn,ts) ->
	  let ts = get_all (get_named_type) ts in
	  ts@[!tn]
      |	ArrayType (t,_) -> get_named_type t
      |	FnType (_,_,t',ts) -> 
	  let t' = get_named_type t' in
	  t'@(get_all (get_named_type) ts)
      |	RepType t -> get_named_type t
      |	ExnconType t -> get_named_type t
      | _ -> [] in
    match td with
      (StructDecl { st_fields = fields },_) ->
	get_all (function (n,_,t) -> get_named_type t) fields
    | (UnionDecl { un_fields = fields },_) ->
	get_all (function (n,t) -> get_named_type t) fields
    | _ -> [] in
  (* a list of all of the names in the given definitions (including
     the definition names themselves) *)
  let extern_names =
    (* names of the given type decls *)
    let tdecl_names = 
      let name_of_decl td =
	match td with
	  (StructDecl { st_name = name },_) -> name
	| (UnionDecl { un_name = name },_) -> name
	| (AbsDecl { abs_name = name },_) -> name
	| (ExternType (name,_,_),_) -> name 
	| _ -> failwith "not a typedecl" in
      List.map name_of_decl tdecls in
    dprint (Printf.sprintf "%sexterns_of_typdecls:\n" !pre);
    List.iter (function n -> dprint (Printf.sprintf "  %s\n" n)) tdecl_names;
    (* combine the given type decl names and the ones they depend on *)
    remove_duplicates
      (List.fold_left 
	 (fun es td ->
	   let es' = extern_names_for_decl td in
	   es' @ es)
      tdecl_names tdecls) in
  dprint (Printf.sprintf "%sexterns_of_those_typdecls:\n" !pre);
  List.iter (function n -> dprint (Printf.sprintf "  %s\n" n)) extern_names;
  dprint "  --generating new decls\n";
  let new_decls =
    let need_second_pass = ref false in
    let rec aux tynames =
    match tynames with
      [] -> exist_decls 
    | (n::t) ->
	try
	  let d = find_decl n false (exist_decls@tdecls) in
	  aux t (* do nothing -- we already have it *)
	with Not_found ->
	  (* grab it from the program *)
	  (try
	    let d = find_decl n false alldecls in
	    let (_,eds) = 
	      Popdynpatch.make_def_extern d
		(fun _ _ -> Popdynpatch.IsExtern)
		(function x -> x) in
	    (* will need to recursively find any externs this extern has *)
	    need_second_pass := true;
(*
	    let old_pre = !pre in
	    pre := ("=="^(!pre));
	    let exist_decls = externs_of_typdecls eds exist_decls alldecls in
	    pre := old_pre;
	    aux t (eds@exist_decls)
*)
	    eds@(aux t)
	  with Not_found ->
	    failwith ("externs_of_typdecls: could not find decl "^n)) in
    let extdecls = aux extern_names in
    if !need_second_pass then
      externs_of_typdecls extdecls extdecls alldecls
    else extdecls in
  new_decls

(*-------------------------------------------------------------------------*)
(* given a list of variable maps (string * bool pairs), finds an external
   declaration *)
let gen_typdecls decls vars =
  let rec aux ts =
    match ts with
      [] -> []
    | ((tn,unchanged)::ts) ->
	if not unchanged then
	  (try
	    let d = find_decl tn false decls in
	    let (_,eds) = 
	      Popdynpatch.make_def_extern d
		(fun _ _ -> Popdynpatch.IsExtern)
		(function x -> x) in
	    eds@(aux ts)
	  with Not_found ->
	    (* this must have been read in from the rename_vars_file *)
	    aux ts)
	else (aux ts) in
 aux vars

(*-------------------------------------------------------------------------*)
(* takes a pair of values and their types and generates code that converts
   from one type to the other.  If no obvious code can be generated,
   a "comment" string is returned instead.  Result is a statement. *)
exception No_trans (* can't perform the translation --- bail out *)
let rec gen_state_trans v_from t_from env_from v_to t_to env_to conv_fn =
  (* keep track of the conversion functions used and at what type,
     then return that stuff at the end so we can automatically generate
     the externs *)
  let conv_funs = ref [] in
  let rec aux from_exp t_from to_exp t_to =
    (* couldn't translate the state *)
    let no_trans s =
      emit_warning_level
	(Printf.sprintf "could not convert state: found %s in (%s,%s)" 
	   s (typ2string t_from) (typ2string t_to))
	verbose_error_level;
      raise No_trans in
    (* copies the contents of the two variables exactly *)
    let copy () =
      make_stmt(Exp(make_exp(AssignOp(to_exp,None,from_exp)))) in
    (*-----------------------------------------------------------------*)
    let t_from = compress t_from in
    let t_to = compress t_to in
    match t_from,t_to with
      Evar (vc1,tor1), Evar (vc2,tor2) -> no_trans "Evar"
    | MutableTyp _, _ -> no_trans "MutableTyp"
    | _, MutableTyp _ -> no_trans "MutableTyp"
    | UnresolvedTyId _, _ -> no_trans "UnresolvedTyId"
    | _, UnresolvedTyId _ -> no_trans "UnresolvedTyId"
    | VoidType,VoidType -> no_trans "VoidType"
    | IntType (b1,s1), IntType (b2,s2) -> 
	(* XXX add casts for small to large types *)
	if (b1 = b2) && (s1 = s2) then copy ()
	else no_trans "differnt IntType qualifiers"
    | BooleanType, BooleanType -> copy ()
    | StringType, StringType -> copy ()
    | CharType, CharType -> copy ()
    | FloatType, FloatType -> copy ()
    | DoubleType, DoubleType -> copy ()
    | ExnType, ExnType -> copy ()
    | ArrayType(t_from,_),ArrayType(t_to,_) -> 
        (* generate a loop and copy each element piecewise. *)
        (*--------------------------------------------------*)
	let loop_var = new_id "idx" in
	let body_from_exp = 
	  make_exp(Subscript(from_exp,make_exp(Var(loop_var)))) in
	let to_exp = 
	  make_exp(Subscript(to_exp,make_exp(Var(loop_var)))) in
        (* generate the loop body *)
	let body_stm = aux body_from_exp t_from to_exp t_to in
        (* generate the loop *)
	let for_stm =
	  make_stmt
	    (For(make_exp
		   (AssignOp(make_exp(Var(loop_var)),
			     None,make_exp(Const(Int Numtypes.i32_0)))),
		 make_exp
		   (Primop(Lt,[ make_exp(Var(loop_var));
				make_exp(FunCall(make_exp(Var "size"),
						ref None,
						[from_exp]))])),
		 make_exp
		   (AssignOp(make_exp(Var(loop_var)),
			     Some Plus,make_exp(Const(Int Numtypes.i32_1)))),
		 body_stm)) in
	let loop_stm = make_stmt(Decl(loop_var,IntType (true,B4),
				      ref None,for_stm)) in
	loop_stm
    | FnType _,FnType _ ->
        (* check if the same type, else fail *)
	if (eq_type t_from env_from t_to env_to) then copy () 
	else no_trans "different fun types"
    | TupleType(c1,ts1),TupleType(c2,ts2) -> 
        (* piecewise copy each element of the tuple *)
        (*------------------------------------------*)
	let combine_stmts sl =
	  match sl with
	    [] -> 
	      internal_error 
		"gen_state_trans/combine_stmts"
		"called with empty list"
	  | [s] -> s
	  | s::sl ->
	      let sl = List.rev sl in
	      List.fold_right (fun s res -> make_stmt(Seq(s,res))) sl s in
	(try
	  let idx = ref 0 in
	  let stms =
	    List.fold_left2
	      (fun stms t_from t_to ->
		incr idx;
		let from_exp = 
		  make_exp(TupleMember(from_exp,!idx)) in
		let to_exp =
		  make_exp(TupleMember(to_exp,!idx)) in
		(aux from_exp t_from to_exp t_to)::stms)
	      [] ts1 ts2 in
	  combine_stmts stms
	with Invalid_argument _ -> (* lists different lengths *)
	  no_trans "different length tuples")
    | NamedType(tn1,ts1),NamedType(tn2,ts2) ->
        (* check if the names are the same *)
	if (!tn1 <> !tn2) then no_trans "different typenames"
	else if (eq_type t_from env_from t_to env_to) then
	  copy ()
	else 
          (* invoke the translation function *)
	  let conv_fn_name = conv_fn !tn1 in
	  conv_funs :=
	    (if List.mem_assoc conv_fn_name !conv_funs then
	      !conv_funs
	    else (conv_fn_name,!tn1)::(!conv_funs));
	  let from_exp =
	    make_exp(FunCall(make_exp(Var conv_fn_name), ref None,
			     [from_exp])) in
	  make_stmt(Exp(make_exp(AssignOp(to_exp,None,from_exp))))	  
    | VarType(x),VarType(y) -> no_trans "vartypes"
    | RepType(t_from),RepType(t_to) ->
        (* check if the same type, else fail *)
	if (eq_type t_from env_from t_to env_to) then copy () 
	else no_trans "different reptypes"
    | ExnconType(t_from),ExnconType(t_to) ->
        (* check if the same type, else fail *)
	if (eq_type t_from env_from t_to env_to) then copy () 
	else no_trans "different excontypes"
    (* XXX could do more here ... e.g. coercions from/to ints and floats *)
    | _,_ -> no_trans "unlike types" in
  try
    let stm = aux (make_exp(Var v_from)) t_from (make_exp(Var v_to)) t_to in
    stm, !conv_funs
  with No_trans (* couldn't do it; make a "comment" string *) ->
    let str = Printf.sprintf "%s:%s -> %s:%s"
	v_from (typ2string t_from) v_to (typ2string t_to) in
    let stm = make_stmt(Exp(make_exp(Const(String str)))) in
    stm, []

(*-------------------------------------------------------------------------*)
(* After generating the relevant data, this automatically creates
   the patch file, and the stub file if necessary. *)
let gen_interface_code_file old_env new_env filename have_conv =
  dprint "generating interface code file\n";  
  let old_decls = old_env.decls in

  (* generate the stub functions in the interface code file *)
  let gen_stubs () =
    let exp_of nm t = 
      make_exp
	(Raise(make_exp
		 (NewExn("Core?InvalidArg",
			 Some (make_exp
				 (Const(String ((stub_var nm)^" ("^
						(typ2string t)^")")))))))) in
    let rec gen_funs l stubs =
      match l with
	[] -> stubs
      | ((v,t,s,tn,sn)::ss) ->
          (* only want to generate stubs for previously exported
	     functions---wouldn't make sense to do so for local
	     functions because the stub would never get called (i.e.,
	     locals should only be called directly by code within the
	     module, and so the stub won't be noticed) *)
	  if s <> Static then (* XXX what about Extern? *)
	    let tn = new_type tn in
	    (* if the new function is static, it will get renamed
	       in popdyntrans ... *)
	    (* generate the stub declaration *)
	    let fd = fd_of_fntype t v true 
		(make_stmt(Exp (exp_of v tn))) in
	    gen_funs ss ((FunDecl fd, bogus_loc)::stubs)
	  else 
	    (gen_funs ss stubs) in
    let funs = 
      if !add_sametype_stubs then
	gen_funs (!same_stubs @ !stubs) []
      else 
	gen_funs !stubs [] in
    if funs <> [] then
      [PrefixDecl ("Stub",funs),bogus_loc]
    else funs in

  (* make a skeleton of the interface code file's initialization
     function.  This will either mention (as no-effect strings) all of
     the data that has changed type or actually generate code for the
     conversion.  In the case that we are not generating stubs for
     functions whose types are unchanged, we add "comments" here for
     that as well. *)
  let gen_init () =
    let combine stm stms =
      make_stmt(Seq(stm, stms)) in
    (* creates a constant string as a statement---serves as a "comment" *)
    let comment comm stms =
      combine (make_stmt(Exp(make_exp(Const(String comm))))) stms in
    (* notes functions that have changed but have same type *)
    let rec gen_comment_funs l last_stm =
      match l with
	[] -> last_stm
      | ((v,_,_,t,s)::ss) ->
	  if s <> Static then
	    let str = Printf.sprintf "function %s:%s changed"
		v (typ2string t) in
	    let stms = gen_comment_funs ss last_stm in
	    comment str stms
	  else 
	    (gen_comment_funs ss last_stm) in
    (* notes or converts data that has changed type *)
    let rec gen_convert_stms l last_stm =
      match l with
	[] -> [], last_stm
      | ((v,tn,dn,t,s)::ds) ->
	  let oldv = 
	    if (s = Static) then
	      Popdyntrans.rename_local v !old_modname !old_filename
	    else v in
	  let newv = new_var v in
	  let stm, conv_funs = 
	    gen_state_trans oldv tn old_env newv t new_env old_name_of in
	  let conv_funs', stms = gen_convert_stms ds last_stm in
	  let stm = combine stm stms in
	  (conv_funs@conv_funs'), stm in
    (* intializes new versions of data that have the same type as the old *)
    let rec gen_init_stms l last_stm = 
      match l with
	[] -> last_stm 
      | ((v,_,_,_,s)::ds) ->
	  let oldv = 
	    if (s = Static) then
	      Popdyntrans.rename_local v !old_modname !old_filename
	    else v in
	  let newv = new_var v in
	  let e = make_exp(AssignOp(make_exp(Var newv),None,
				    make_exp(Var oldv))) in
	  make_stmt(Seq(make_stmt(Exp e),
			gen_init_stms ds last_stm)) in
    (* ------------------------------------------------------------- *)
    (* generate data changes *)
    dprint "  -- generating convert statements\n";
    let conv_funs, stm = gen_convert_stms !datas (make_stmt(Skip)) in
    dprint "  -- generating extdecls\n";
    let extdecls =
      List.map
	(fun (fn_name,tyname) ->
	  (ExternVal 
	     (fn_name, 
	      FnType (default_convention, [],
		      NamedType (ref (new_var tyname),[]),
		      [NamedType (ref tyname, [])]))), bogus_loc)
	(remove_duplicates conv_funs) in
    (* generate comments about function changes with unchanged type *)
    dprint "  -- generating \"comments\"\n";
    let stm = 
      if !add_sametype_stubs then stm 
      else gen_comment_funs !same_stubs stm in
    (* if not value sharing, initialize new versions of global vars *)
    dprint "  -- generating init statements\n";
    let stm =
      if !value_sharing then stm
      else gen_init_stms !same_datas stm in
    (* create the init function *)
    if (stm.raw_stmt = Skip) then []
    else
      extdecls @
      [ FunDecl
	  { fn_static = false;
	    fn_convention = default_convention;
	    fn_name = "init";
	    fn_tyvars = [];
	    fn_ret_type = VoidType;
	    fn_args = [];
	    fn_body = stm }, bogus_loc ] in
  
  (* generate externs to type declarations from the old implementation;
     will probably want these in writing the real stub and init functions *)
  let gen_extdecls () =
    let rec gen_valdecls vs =
      match vs with
	[] -> []
      | ((v,tn,dn,t,s)::ds) ->
	  try
	    let d = find_decl v true old_decls in
	    let (_,eds) = 
	      Popdynpatch.make_def_extern d 
		(fun _ _ -> Popdynpatch.IsExtern) 
	    	(function v ->
		  if (s = Static) then
		    Popdyntrans.rename_local v !old_modname !old_filename
		  else v) in
	    eds@(gen_valdecls ds)
	  with Not_found ->
	    (* this must have been read in from the rename_vars_file *)
	    failwith ("could not find decl for "^v) in
    dprint " --generating typedecls\n";
    let old_typdecls = gen_typdecls old_decls !checked_types in
    dprint " --generating old typedecls\n";
    let old_exttypdecls = 
      externs_of_typdecls old_typdecls [] old_decls in
    dprint " --generating valdecls\n";
    old_typdecls@old_exttypdecls@(gen_valdecls !datas)@
    (gen_valdecls !same_datas) in

  (* create and write the interface code file *)
  let decls = 
    let ds = dprint "-- generating init fun\n"; gen_init () in
    let ds2 = dprint "-- generating stubs\n"; gen_stubs () in
    ds@ds2 in
  if decls = [] || !processed = [] then
    false (* no need to generate a file *)
  else
    (let decls = 
      let ds = dprint "-- generating externs\n"; gen_extdecls () in
      ds@decls in
    if not !overwrite_patch_files then
      if (Sys.file_exists filename) then
	raise (Sys_error (filename ^ " exists"));
    let outc = open_out filename in 
    Printf.fprintf outc "#include \"core.h\"\n";
    pr_popprogram (Format.formatter_of_out_channel outc) std_opts decls;
    close_out outc;
    dprint "-- done\n";
    true)
	    
(*----------------------------------------------------------------------*)
(* creates the patch file based on the generated data. *)
(* XXX how to deal with filenames here?  Use absolute ones for now ... *)
let gen_patch_file force_patch patch_filename filename interface_filename =
  (* first see if we need to create one: if the processed list is
     empty then nothing changed. *)
  if !processed = [] && !deleted = [] && (not force_patch) then
    emit_warning "unchanged; not creating a patch file"
  else
    (dprint "generating patch file\n";
(*
     Printf.printf "%s changed:\n" filename;
     print_endline "sharing:";
     print_list !sharing (fun v -> v) print_string;
     print_endline "\nprocessed:";
     print_list !processed (fun v -> v) print_string;
*)
     if not !overwrite_patch_files then
       if (Sys.file_exists patch_filename) then
	 raise (Sys_error (patch_filename ^ " exists"));
     let outc = open_out patch_filename in 
     Printf.fprintf outc "implementation: %s\n" filename;
     if interface_filename <> "" then
       Printf.fprintf outc "interface: %s\n" interface_filename;
     if !sharing <> [] then
       (Printf.fprintf outc "sharing:\n";
	print_list !sharing (function s -> (var2string s)) 
	  (Printf.fprintf outc "%s"));
     if !rename_vars <> [] then
       (Printf.fprintf outc "renaming:\n";
	print_list !rename_vars 
	  (function (s,s2) -> ((var2string s)^"="^(var2string s2))) 
	  (Printf.fprintf outc "%s"));
     close_out outc)

(*----------------------------------------------------------------------*)
(* writes all of the typename mappings to a file.  This will serve
   to help translations of other files identify which types have
   changed that may not exist in the old file but do in the new one.
   XXX have multiple rename files?  This would make it easier to
   deal with various repositories of files that comprise a single
   executable, i.e. libraries, source files, etc. *)
let gen_rename_tynames_file filename =
  (* Overwrite the types file with the expanded information; only
     include "new" variables in this file (i.e. not ones read in
     from older rename-vars files) *)
  if !rename_vars <> [] && not (!processed = [] && !deleted = []) then
    (dprint "generating TYPEMAP file\n";
     let outc = open_out filename in 
    print_list !rename_vars 
      (function (s,s2) -> 
	(* new mapping---add it *)
	if (is_new s) then
	  ((var2string s)^"="^(var2string s2))
	(* only write old mappings to the file that are not
	   overridden by current mappings *)
	else
	  (let news = new_var s in
	  if not (List.mem_assoc news !rename_vars) then
	    ((var2string s)^"="^(var2string s2))
	  else ""))
      (Printf.fprintf outc "%s");  
    close_out outc)

(*----------------------------------------------------------------------*)
(* for all of the types that have changed their definitions (as indicated 
   by rename_vars), create some code that converts from the old type
   to the new and vice versa.  This gets written to a separate file, to
   be used by the generated patch files in doing state translation or
   for the stubs. *)
let convert_type name env1 env2 =
  let old_var_name, new_var_name = "from", "to" in
  let make_fndecl body from_name to_name fn_name =
    { fn_static = true;
      fn_convention = default_convention;
      fn_name = fn_name;
      fn_tyvars = [];
      fn_ret_type = NamedType (ref to_name, []);
      fn_args = [old_var_name, NamedType (ref from_name, [])];
      fn_body = body } in
  
  (* Each conversion function has the following formula:
     Copy all of the like fields.  Add default values for
     new fields.  Ignore any deleted fields.  For fields with
     different types, if it is named type, call the conversion
     function for it; otherwise abort. *)
     
  let convert_sd old2new name sd1 env1 sd2 env2 =
    let from_type_name, to_type_name, fn_name = 
      if old2new then (name, new_var name, old_name_of name)
      else (new_var name, name, new_name_of name) in
    (* look in the old structure def to find the given field, 
       and make sure it has the given type. *)
    let missing = 0 in
    let exists = 1 in
    let changed = 2 in
    let field_type n t =
      let status = ref missing in
      List.exists
	(function (n2,_,t2) -> 
	  if (n = n2) then
	    (if (eq_type t env2 t2 env1) then 
	      status := exists
	    else 
	      status := changed;
	     true)
	  else false)
	sd1.st_fields;
      !status in
    (* create the new struct's initialization fields *)
    let (samefields, newfields, changedfields) =
      List.fold_left 
	(fun (samefields, newfields, changedfields) ((n,_,t) as field) -> 
	  let status = field_type n t in
	  (* field is missing in old version; initialize with default value *)
	  if status = missing then
	    let e = 
	      Poptype.default_initializer 
		env2.global_env t bogus_loc mt_varset in
	    (samefields, (Some n,e)::newfields, changedfields)
	  (* exists at same type in old version; copy it over *)
	  else if status = exists then
	    let e = make_exp(StructMember(make_exp(Var(old_var_name)),n)) in
	    ((Some n,e)::samefields, newfields, changedfields)
	  (* exists at different type; if a named type, call the conversion
	     function, otherwise leave a "comment" *)
	  else
	    (* XXX could improve this by calling the state_trans code
	       that we've already written.  Then could deal with arrays
	       of namedtypes, for example.  Would have to create local
	       var placeholders since we can't do this inline. *)
	    let t' = compress t in
	    let e =
	      (match t' with
		NamedType (tnr,_) -> (* call conversion function *)
		  let conv_fn = 
		    if old2new then old_name_of !tnr 
		    else new_name_of !tnr in
		  let e = 
		    make_exp(StructMember(make_exp(Var(old_var_name)),n)) in
		  make_exp(FunCall(make_exp(Var conv_fn),
				   ref None,[e]))
	      |	_ -> make_exp(Const(String "XXX FILL"))) in
	    (samefields, newfields, (Some n,e)::changedfields))
	([],[],[]) sd2.st_fields in
    (* generate the conversion function body *)
    let body = 
      make_stmt(Decl(new_var_name,
		     NamedType (ref to_type_name, []),
		     ref (Some
			    (make_exp
			       (NewStruct(to_type_name,
					  ref None,
					  samefields @ newfields @
					  changedfields)))),
		     make_stmt(Return(Some (make_exp
					      (Var new_var_name)))))) in
    (* add null check if necessary *)
    let body =
      if sd1.st_possibly_null then
	if sd2.st_possibly_null then
	  (* both old and new versions may be null *)
	  make_stmt(IfThenElse
		      (make_exp
			 (Primop(Eq,
				 [ make_exp(Var old_var_name);
				   make_exp(Const(Null)) ])),
		       make_stmt(Return(Some(make_exp
					       (Const(Null))))),
		       body))
	else
	  (* old version can be null but new version may not *)
	  (* XXX could generate if statement to throw exception
	     if element to convert is null.  Have user provide
	     what exception that should be. *)
	  failwith 
	    (Printf.sprintf
	       "convert_type: old version of %s may be null but not new"
	       name)
      else
	body (* new version may or may not be null; same either way *) in
	
    make_fndecl body from_type_name to_type_name fn_name in
  
  let convert_ud old2new name ud1 env1 ud2 env2 =
    let from_type_name, to_type_name, fn_name = 
      if old2new then (name, new_var name, old_name_of name)
      else (new_var name, name, new_name_of name) in
    (* look in the old structure def to find the given field, 
       and make sure it has the given type. *)
    let missing = 0 in
    let exists = 1 in
    let changed = 2 in
    let field_type n t =
      let status = ref missing in
      List.exists
	(function (n2,t2) -> 
	  if (n = n2) then
	    (if (eq_type t env2 t2 env1) then 
	      status := exists
	    else 
	      status := changed;
	     true)
	  else false)
	ud2.un_fields;
      !status in
    (* create the union's initialization fields *)
    let (samefields, missingfields, changedfields) =
      List.fold_left 
	(fun (samefields, missingfields, changedfields) ((n,t) as field) -> 
	  let status = field_type n t in
	  let opt_exp t fe =
	    if t = VoidType then None
	    else Some (fe ()) in
	  if status = missing then
	    (samefields, (n,t,None)::missingfields, changedfields)
	  else if status = exists then
	    let e = 
	      opt_exp t
		(function () -> 
		  make_exp(UnionMember(make_exp(Var(old_var_name)),n))) in
	    ((n,t,e)::samefields, missingfields, changedfields)
	  else
	    (let t' = compress t in
	    let e =
	      (match t' with
		NamedType (tnr,_) -> (* call conversion function *)
		  let conv_fn = 
		    if old2new then old_name_of !tnr 
		    else new_name_of !tnr in
		  let e = 
		    make_exp(StructMember(make_exp(Var(old_var_name)),n)) in
		  make_exp(FunCall(make_exp(Var conv_fn),
				   ref None,[e]))
	      |	_ -> make_exp(Const(String "XXX FILL"))) in
	    (samefields, missingfields, (n,t,Some e)::changedfields)))
	([],[],[]) ud1.un_fields in
    (* generate the conversion function body *)
    let body = 
      let pat_of t =
	if t = VoidType then No_pat
	else Prim_pat (Var_pat ("x", ref t)) in
      let arm_list =
	List.map (function (n,t,e) ->
	  { arm_field = n; 
	    arm_pat = pat_of t;
	    arm_body = 
	    make_stmt(Exp(make_exp
			    (AssignOp(make_exp(Var new_var_name),None,
				      make_exp(NewUnion(to_type_name, ref None,
							n, e)))))) })
	  (samefields @ changedfields) in
      (* if missingfields is non-empty we add a default-case that
	 does something user-specific *)
      let default_stmt () =
	if missingfields <> [] then
	  let str =
	    List.fold_left 
	      (fun s (n,t,_) -> 
		Printf.sprintf "%s %s:%s" s n (typ2string t))
	      "XXX" missingfields in
	  Some(make_stmt(Exp(make_exp(Const(String str)))))
	else None in
      let body =
	make_stmt(Decl(new_var_name,
		       NamedType (ref to_type_name, []),
		       ref None,
		       make_stmt
			 (Seq
			    (make_stmt(UnionSwitch
					 (make_exp(Var(old_var_name)),
					  arm_list,
					  default_stmt ())),
			     make_stmt(Return 
					 (Some(make_exp
						 (Var(new_var_name))))))))) in
      if ud1.un_possibly_null then
	if ud2.un_possibly_null then
	  (* both old and new versions may be null *)
	  make_stmt(IfThenElse
		      (make_exp
			 (Primop(Eq,
				 [ make_exp(Var old_var_name);
				   make_exp(Const(Null)) ])),
		       make_stmt(Return(Some(make_exp
					       (Const(Null))))),
		       body))
	else
	  (* old version can be null but new version may not *)
	  failwith 
	    (Printf.sprintf
	       "convert_type: old version of %s may be null but not new"
	       name)
      else
	body (* new version may or may not be null; same either way *)
    in
    make_fndecl body from_type_name to_type_name fn_name in

  let old_td, new_td = 
    lookup_typedecl name env1.global_env,
    lookup_typedecl name env2.global_env
  in
  match old_td, new_td with
    Struct sd1, Struct sd2 ->
      [FunDecl (convert_sd true name sd1 env1 sd2 env2), bogus_loc;
       FunDecl (convert_sd false name sd2 env2 sd1 env1), bogus_loc]

  | Union ud1, Union ud2 ->
      [FunDecl (convert_ud true name ud1 env1 ud2 env2), bogus_loc;
       FunDecl (convert_ud false name ud2 env2 ud1 env1), bogus_loc]

  | Abstype ad1, Abstype ad2 -> [] (* XXX not implemented *)
  | AbstractD (vs1,n1), AbstractD (vs2,n2) -> [] (* XXX not implemented *)
  | _, _ -> 
      failwith "convert_type: unlike decls not implemented"
      (* raise Not_found (* i.e. not implemented *) *)

let gen_type_convert_file env1 env2 filename =
  (* for each type in checked_types that is non-equal, 
     generate the conversion functions for the type *)
  dprint "Generating type conversion file\n";
  let unequal_typenames =
    List.filter (function (_, eq) -> not eq) !checked_types in
  (* filter these with the conversion functions we already have *)
  let unequal_typenames =
    List.filter (function (x, _) -> not (List.mem x !convert_vars))
      unequal_typenames in
  let decls =
    if unequal_typenames <> [] then
      let convert_funs, vars =
	let rec aux tns =
	  match tns with
	    [] -> []
	  | (name,_)::tns ->
	      let new_name = new_var name in
	      let no_good () =
	        (* definition not found for some reason *)
		emit_warning
		  (Printf.sprintf 
		     "can't create convert function for %s" name) in
	      try
		let decls = convert_type name env1 env2 in
		if decls <> [] then 
		  ((name, false), decls)::(aux tns)
		else 
		  (no_good (); aux tns)
	      with Not_found ->
		(no_good (); aux tns) in
	let vs, cfs = List.split (aux unequal_typenames) in
	List.flatten cfs, vs in
      (* generate extern's for the old type declarations *)
      (* Printf.eprintf "generating old typextdecls\n"; *)
      let old_typdecls = gen_typdecls env1.decls vars in
      let old_exttypdecls = 
	externs_of_typdecls old_typdecls (!convert_decls) env1.decls in
      (* generate extern's for the new type declarations. *)
      (* Printf.eprintf "generating new typextdecls\n"; *)
      let new_typdecls = gen_typdecls env2.decls vars in
      let new_exttypdecls = 
	externs_of_typdecls 
	  new_typdecls (old_exttypdecls@(!convert_decls)) env2.decls in
      (* -- *)
      (* let old_typdecls = old_typdecls@old_exttypdecls in *)
      let new_typdecls = new_typdecls@new_exttypdecls in
      (* prepend decl names with New:: *)
      let new_typdecls = 
	List.map (function d ->
	  Popdynpatch.dyntrans_topdecl 
	    { Popdynpatch.local_env = [];
	      Popdynpatch.global_env = env2.global_env;
	      Popdynpatch.vars = [];
	      Popdynpatch.type_var_map = !rename_vars;
	      Popdynpatch.add_new = true; }
	    d) new_typdecls in
      old_typdecls@new_typdecls@convert_funs
    else 
      [] in
(*
  (* combine these declarations with the ones we read in at the start *)
  let decls = (!convert_decls)@decls in
*)
  (* write it to the file *)
  if decls <> [] then
    (* first write the code part of the convert file *)
    (let outc = open_out (filename^"_patch.pop") in
    pr_popprogram (Format.formatter_of_out_channel outc) std_opts decls;
    close_out outc;
    (* now generate the patch file that points to the code *)
    let outc = open_out (filename^".patch") in
    Printf.fprintf outc "interface: %s\n" 
      (Filename.basename (filename^"_patch.pop"));
    if !sharing <> [] then
      (Printf.fprintf outc "sharing:\n";
       print_list !sharing (function s -> (var2string s)) 
	 (Printf.fprintf outc "%s"));
    if !rename_vars <> [] then
      (Printf.fprintf outc "renaming:\n";
       print_list !rename_vars 
	 (function (s,s2) -> ((var2string s)^"="^(var2string s2))) 
	 (Printf.fprintf outc "%s"));
    close_out outc;
    true)
  else false

let read_type_convert_file filename =
  (* since we automatically generate this file; we don't
     bother to preprocess it *)
  let decls = Gcdfe.fe Poplex.token Popparse.top filename in
  convert_decls := (List.rev decls);
  (* pull out the names of the types that we convert here *)
  let without_externs =
    let get_all f l =
      List.flatten (List.map f l) in
    let filter_extern ((d,_) as decl) =
      let get_x f =
	if f then [] 
	else 
	  let s = topdecl2string decl in
	  failwith ("typedecl not extern "^s) in
      match d with
	StructDecl sd ->
	  get_x (sd.st_scope = Extern)
      |	UnionDecl ud ->
	  get_x (ud.un_scope = Extern)
      | AbsDecl ad -> 
	  get_x (ad.abs_scope = Extern)
      |	ExternType _ -> []
      |	_ -> [decl] in
    get_all filter_extern decls in
  (* now find the names of the conversion functions *)
  let type_names =
    let get_name ((d,_) as decl) =
      match d with 
	FunDecl fd -> typename_from fd.fn_name
      |	_ -> 
	  let s = topdecl2string decl in failwith ("non-function "^s) in
    remove_duplicates 
      (List.map get_name without_externs) in
  convert_vars := type_names

(***********)
(* PARSING *)
(***********)

let read_rename_tynames_file filename is_old_file =
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
  let rec aux ts =
    match ts with
      [] -> ()
    | (s::t) ->
	let error () =
	  internal_error "read_rename_tynames_file"
	    (Printf.sprintf "illegal rename var spec |%s|" s) in
	(* assumes the token is of form x=y; split at the = *)
	let ts = split_string s '=' in
	(match ts with
	  [origvar; newvar] ->
	    let origvar = Popdynpatch.internal_varname origvar in
	    (* renaming inherited from old typename file *)
	    if not (is_new origvar) then 
	      add_to_rename_vars origvar newvar
	    (* renaming from current file *)
	    else
	      (if is_old_file then 
		add_to_rename_vars (kill_new origvar) newvar
	      else
		(add_to_rename_vars origvar newvar;
		 note_unequal_type (kill_new origvar)));
	    aux t
	| _ -> error ()) in
  (*------------------------------------------------------------*)
  let s = Popdynpatch.get_string_from_file filename in
  let ts = Popdynpatch.string_to_list s in
  aux ts (* ;
  print_global_data () *)

(*-------------------------------------------------------------------------*)
let includes = ref [];;
let add_include p = 
  if not (List.mem p (!includes))
  then includes := p :: !includes
  else ()

let defines = ref []
let add_define p = 
  if not (List.mem p (!defines))
  then defines := p :: !defines
  else ()

(*-------------------------------------------------------------------------*)
(* Adds the directory of the popcorn library to the list of includes *)
let include_poplibdir () =
  let poplibdir = 
    try Sys.getenv "POPCORNLIB"
    with Not_found -> 
      try Filename.concat 
	  (Sys.getenv "TALCLIB")
	  (Filename.concat ".." (Filename.concat "popcorn" "lib"))
      with Not_found -> Filename.current_dir_name in
  add_include poplibdir

let abort_on_error () = 
  if Gcdfec.error_p () then raise Gcdfec.Exit

(*-------------------------------------------------------------------------*)
(* preprocesses the given file, returns the name of the resulting file *)
let preprocess_file filename basename modname =
  include_poplibdir ();
  let preprocfile  = modname ^ ".i" in 
  let cwd = Filename.dirname filename in
  let cmd = 
    (if Sys.os_type = "Unix" 
    then 
      let inc_string = 
	List.fold_right (fun d r -> "-I " ^d ^ " " ^ r) !includes "" in
      let def_string =
	List.fold_left (fun r d -> "-D " ^d ^ " " ^ r) "" !defines in
      Printf.sprintf "gcc -x c -E %s %s %s > %s" 
	inc_string def_string basename preprocfile
    else 
      let inc_string = 
	List.fold_left (fun r d -> "/I" ^d ^ " " ^ r) "" !includes in
      let def_string = 
	List.fold_left (fun r d -> "/D" ^d ^ " " ^ r) "" !defines in
      Printf.sprintf "cl /nologo %s %s /P /TC %s" 
	inc_string def_string basename) in
  let cmd = "cd "^cwd^"; "^cmd in
  (* Printf.printf "preproc command = |%s|\n" cmd; *)
  flush Pervasives.stdout;
  Sys.command cmd;
  Filename.concat cwd preprocfile

(*-------------------------------------------------------------------------*)
(* Given a file (and its alternate names), performs pre-processing,
   and returns back the AST *)
let parse_pop_file filename basename modname =
  let preprocfile = preprocess_file filename basename modname in
  let rm_ppop = 
    let ppop_removed = ref false in
    (function () ->
      if not !ppop_removed then Sys.remove preprocfile; 
      ppop_removed := true) in
  try
    let decls = Gcdfe.fe Poplex.token Popparse.top preprocfile in
    abort_on_error ();
    (* rm_ppop (); *)
    (decls, rm_ppop)
  with e -> 
    rm_ppop (); raise e

(*-------------------------------------------------------------------------*)
(* Creates the AST's of the two files to compare *)
let gen_ast old_file new_file cleanup =
  let basename = Filename.basename old_file in
  let modname = Filename.chop_extension basename in
  let (old_ast,rm_ppop1) = 
    parse_pop_file old_file basename modname in
  cleanup := rm_ppop1;
  let basename = Filename.basename new_file in
  let (new_ast,rm_ppop2) = 
    parse_pop_file new_file basename (Filename.chop_extension basename) in
  cleanup := (fun () -> rm_ppop1 (); rm_ppop2 ());
  old_filename := old_file;
  old_modname := modname;
  (old_ast,new_ast)

(*-------------------------------------------------------------------------*)
let rename_vars_file = "TYPENAME_MAP"
let type_convert_file = "convert"

let set_usage_string name =
  Printf.sprintf "usage : %s [options] new_file [old_file]\n" name

let proc_args argv =
  let usage_msg = set_usage_string argv.(0) in

  (* specify here how many non-flagged args we expect *)
  let required_args_done = ref 0 in
  let required_args_expected = 1 in

  let old_file = ref "" in
  let new_file = ref "" in
  let new_rename_vars_file = ref "" in
  let old_rename_vars_file = ref "" in
  let convert_file = ref "" in
  let arg_specs =
    [("--curr-tymap", 
      Arg.String(function s -> new_rename_vars_file := s),
      "Specify file containing typename mappings for the new version");
     ("--old-tymap", 
      Arg.String(function s -> old_rename_vars_file := s),
      "Specify file containing typename mappings for the old version");
     ("--convert-file", 
      Arg.String(function s -> convert_file := s),
      "Specify file containing type conversion functions");
     ("--overwrite-patch",
      Arg.Set overwrite_patch_files,
      "If patch files exist, overwrite them");
     ("--export-locals",
      Arg.Set export_locals,
      "File to be updated has exported local vars");
     ("--outdir",
      Arg.String(function s -> output_directory := 
	 (absolute_dir s (Sys.getcwd ()))),
      "Directory to write patch files");
     ("--sametype-stubs",
      Arg.Set add_sametype_stubs,
      "Add stub if two functions differ but have the same type");
     ("--value-sharing",
      Arg.Set value_sharing,
      "If value decls are unchanged, add to sharing list");
     ("--verbose",
      Arg.Unit (function _ -> 
	verbose := true; error_level := verbose_error_level),
      "Print more warnings and global state after generating patch");
     ("--quiet",
      Arg.Unit (function _ -> error_level := 0),
      "Suppress warnings")] in
  let proc_other_arg s = 
    let assign_file var =
      if Sys.file_exists s then var := s 
      else raise (Arg.Bad("File "^s^" does not exist")) in
    ((match (!required_args_done) with
      0 -> assign_file new_file
    | 1 -> assign_file old_file
    | _ -> raise (Arg.Bad("unexpected argument: " ^ s)));
     required_args_done := !required_args_done + 1) in

  Arg.parse arg_specs proc_other_arg usage_msg;
  if (!required_args_done) < required_args_expected then
    (Printf.printf "%s: missing argument.\n" argv.(0);
     Arg.usage arg_specs usage_msg; exit 1);

  (* if no typename map files specified, use default ones if they exist *)
  (* --- new typemap --- *)
  if !new_rename_vars_file = "" then
    (let new_wd = Filename.dirname !new_file in
    let new_tmfile = Filename.concat new_wd rename_vars_file in
    if Sys.file_exists new_tmfile then
      new_rename_vars_file := new_tmfile)
  else
    (if Filename.is_relative !new_rename_vars_file then
      if (!new_rename_vars_file).[0] <> '.' then
	new_rename_vars_file := 
	  Filename.concat Filename.current_dir_name (!new_rename_vars_file));
  (* --- type conversion file --- *)
  if !convert_file = "" then
    (let new_wd = !output_directory in
    let convertfile = Filename.concat new_wd type_convert_file in
    if Sys.file_exists (convertfile^"_patch.pop") then
      convert_file := convertfile)
  else
    (if Filename.is_relative !convert_file then
      if (!convert_file).[0] <> '.' then
	convert_file := 
	  Filename.concat Filename.current_dir_name (!convert_file));
  (* --- old typemap --- *)
  if !old_rename_vars_file = "" then
    (let old_wd = Filename.dirname !old_file in
    let old_tmfile = Filename.concat old_wd rename_vars_file in
    if Sys.file_exists old_tmfile then
      if old_tmfile <> !new_rename_vars_file then
	old_rename_vars_file := old_tmfile);	
(*  
  Printf.printf "curr_map=%s, old_map=%s\n" 
    !new_rename_vars_file !old_rename_vars_file;
*)
  (!old_file,!new_file,
   !old_rename_vars_file,!new_rename_vars_file,
   !convert_file)

(*-------------------------------------------------------------------------*)
let main () =
  let cleanup = ref (fun () -> ()) in
  let (old_file,new_file,
       old_rename_vars_file,new_rename_vars_file,
       convert_file) =
    proc_args Sys.argv in  
  (try
    (* set up the global data *)
    reset_global_data ();
    (* read in the type mapping files if they exist *)
    if old_rename_vars_file <> "" then
      read_rename_tynames_file old_rename_vars_file true;
    if new_rename_vars_file <> "" then
      read_rename_tynames_file new_rename_vars_file false;
    if convert_file <> "" then
      read_type_convert_file (convert_file^"_patch.pop");
    let new_file = absolute_path new_file in
    let have_ifc_file, interface_filename, force_patch =
      (* Will do the comparison *)
      if (old_file <> "") then
        (* parse the files *)
	let (old_ast, new_ast) = gen_ast old_file new_file cleanup in
    (* generate the global data *)
	let (old_env, new_env) = gen_patch_data old_ast new_ast in
	let (old_ast, new_ast) = old_env.decls, new_env.decls in
    (* generate conversion file *)
	let have_conv = 
	  gen_type_convert_file old_env new_env 
	    (if convert_file <> "" then convert_file 
	    else Filename.concat !output_directory type_convert_file) in
    (* generate the interface code file *)
	let interface_filename = 
	  Filename.concat !output_directory 
	    (Filename.basename 
	       ((Filename.chop_extension new_file)^"_patch.pop")) in
	let have_ifc_file =
	  gen_interface_code_file old_env new_env interface_filename
	    have_conv in
	(have_ifc_file,interface_filename,false)
      (* This is a new file---just generate a patch *)
      else 
	((false,"",true)) in
    (* generate the patch file *)
    let patch_filename = 
      Filename.concat !output_directory 
	(Filename.basename 
	   ((Filename.chop_extension new_file)^".patch")) in
    gen_patch_file force_patch patch_filename new_file 
      (if have_ifc_file then 
	(Filename.basename interface_filename) else "");
    (* generate the rename vars file *)
    gen_rename_tynames_file 
      (if new_rename_vars_file <> "" then
	new_rename_vars_file else rename_vars_file)
  with e -> 
    Printf.eprintf "Uncaught exception: %s\n" (Printexc.to_string e));
  !cleanup ()
;;

main ();;
