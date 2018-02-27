(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Chris Hawblitzel,Frederick Smith    *)
(*     Dan Grossman                                                   *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* Prior to Release 1.7, this was part of Popcompile; now it is opened by
   that module.*)

open Numtypes
module T    = Tal
module P    = Popsyntax 
module PT   = Poptype 
module Id   = Identifier
module Peep = Poppeep
module X    = Poperr

type typ   = Popsyntax.typ
type con   = Tal.con
type int32 = Numtypes.int32
type id    = Id.identifier

type ret_class =
    StdReturn of con
  | VoidReturn 
  | FloatReturn

(**************************** Exceptions ****************)
exception Unimplemented of string
exception Impossible    of string
exception Void_Type (* Void_Types have no TAL constructor *)

let unimpl s = raise (Unimplemented ("popcomptypes: "^s))
let impos  s = raise (Impossible    ("popcomptypes: "^s))
let deSome x = match x with Some(v) -> v | None -> impos "deSome"

let to_i32      = int_to_int32 
let dc  = T.defcon

(*********************** Convert to TAL Identifiers ************)
let tid_val  p = Id.id_of_string ("_" ^ p)
let tid_fun convention arg_size p = 
  (match convention with
    P.Cdecl   -> Id.id_of_string ("_" ^ p)
  | P.Stdcall -> Id.id_of_string ("_"^p^"@"^(string_of_int arg_size)))

let tid_mem  p = Id.id_of_string (p   ^ "?mem")
let tid_type p = Id.id_of_string (p   ^ "?")
let tid_tyv  p = Id.id_of_string ("v" ^ p)
let tid_exn  p = Id.id_of_string (p   ^ "?exn")
let tid_name p = Id.id_of_string (p ^ "?nm")
let tid_internal p = Id.id_of_string ("_?" ^ p)

(********************** Type Sizes / Floating Point Utilities *)

(* floating point stack 1 element high *)
let fpstack1 = T.fpstack_create 1

(* Number of 32-bit words in values of tal type c *)
(* Assumes all type variables are 4 bytes wide *)
let rec words_in_con c = 
  match c.T.rcon with
    T.Cprim T.PCfloat64 -> 2
  | T.Cfield (c,_) -> words_in_con c
  | T.Cprod cs -> List.fold_left (fun sum c -> sum + words_in_con c) 0 cs
  | _ -> 1

(********************** Type Variable Utilities ***************)
let tyvars_to_cons tyvars = List.map (fun v -> T.cvar (tid_tyv v)) tyvars
let tyvars_to_lam tyvars con =  
  List.fold_right (fun v -> T.clam (tid_tyv v) T.k4byte) tyvars con
let ids_to_lam ids con = 
  List.fold_right (fun v -> T.clam v T.k4byte) ids con
let tyvars_to_exists tyvars con = 
  List.fold_right (fun v -> T.cexist (tid_tyv v) T.k4byte) tyvars con
let rec tyvars_to_kind ts k =
  List.fold_right (fun t k -> T.karrow T.k4byte k) ts k

(********************** Cons and Abbrevs **********************)
let stack1_v              = Id.id_of_string "s1" 
let stack2_v              = Id.id_of_string "s2" 
let array_size_var        = Id.id_of_string "?sz"
let array_abbrev_var      = Id.id_of_string "?arr"
let string_abbrev_var     = Id.id_of_string "?str"
let handle_abbrev_var     = Id.id_of_string "?H"
let handler_abbrev_var    = Id.id_of_string "?Ha"
let exn_stack_abbrev_var  = Id.id_of_string "?E"
let exn_var               = Id.id_of_string "?exn"
let exnname_var           = Id.id_of_string "?exnname"
let exnname_arg_var       = Id.id_new       "c"

let gen_stack_abbrev_var ret_class convention =
  (match convention,ret_class with
    P.Cdecl  , StdReturn _ -> Id.id_of_string "?S"
  | P.Cdecl  , VoidReturn  -> Id.id_of_string "?Sv"
  | P.Cdecl  , FloatReturn -> Id.id_of_string "?Sf"
  | P.Stdcall, StdReturn _ -> Id.id_of_string "?Ss"
  | P.Stdcall, VoidReturn  -> Id.id_of_string "?Ssv"
  | P.Stdcall, FloatReturn -> Id.id_of_string "?Ssf")

let bogus_option_var      = Id.id_new       "bogus_option"

let cap1_v                = Id.id_of_string "e1"
let cap2_v                = Id.id_of_string "e2"
let cap1_c                = T.cvar cap1_v
let cap2_c                = T.cvar cap2_v

let stack1_c = T.cvar stack1_v
let stack2_c = T.cvar stack2_v

let exn_con           = T.cvar exn_var
let exnname_arg_con v = T.cvar v
let exnname_con'      = T.cvar exnname_var
let exnname_con    c  = T.capp exnname_con' c

(* given an exnname_con, extracts the argument constructor *)
let exnname_arg_con_of = function
    { T.rcon = T.Capp(n,f) } ->
      if n = exnname_con' then 
	(match f with
	  { T.rcon = T.Cfield (c,v) } -> c
	| _ -> impos "exnname_arg_con_of: not an exn constructor")
      else impos "exnname_arg_con_of: not an exn constructor"
  | _ -> impos "exnname_arg_con_of: not an exn constructor"

let int_con           = T.cbyte4
let bool_con          = T.chptr [i32_0;i32_1] None None
let char_con          = int_con
let string_con        = T.cvar string_abbrev_var
let float_con         = T.pcfloat32
let double_con        = T.pcfloat64
let opt_con        c  = T.chptr [i32_0] (Some c) None
let array_con      c  = T.capp (T.cvar array_abbrev_var) c
(* LR *)
let rep_con        c  = T.chptr [] (Some (T.cprod [T.cr (T.RCon c)])) None
(* end LR *)
let exn_stack_con  s e =
    T.capp (T.capp (T.cvar exn_stack_abbrev_var) s) e
let handle_con     s e =
  T.capp (T.capp (T.cvar handle_abbrev_var) s) e
let array_real_con c  = 
  let cv = T.cvar array_size_var in 
  let ce = T.cfield c T.ReadWrite in
  T.cexist array_size_var T.kint 
    (T.cprod_b [ T.cfield (T.csing cv) T.Read; 
		 T.cfield (T.cprod_b [T.carray cv ce]) T.Read])
;;

let handler_con s1 s2 e1 e2 =
  (T.capp (T.capp (T.capp (T.capp (T.cvar handler_abbrev_var) s1) s2) e1) e2) 
let bogus_option_con  = T.chptr [i32_0] None None

let array_abbrev =
  let c = Id.id_of_string "c" in
  T.clam c T.ktype (array_real_con (T.cvar c))

let string_abbrev = array_real_con (T.pcbytes T.Byte1)

let exnname_abbrev = 
  T.clam exnname_arg_var T.kmem
    (T.chptr [] (Some (T.cprod [T.cfield T.cbyte4 T.ReadWrite]))
       (Some (exnname_arg_con exnname_arg_var,T.ReadWrite)))

let exn_body v =
  let exnname_arg_con = exnname_arg_con v in
  T.cprod_b [ T.cfield (exnname_con exnname_arg_con) T.Read; 
	      T.cfield string_con T.Read;  (* FMS: Location information. *)
	      exnname_arg_con
	    ]

let exn_abbrev = T.cexist exnname_arg_var T.kmem (exn_body exnname_arg_var)

(* Type of the handler as seen in function types. *)
let handle_abbrev = 
  let sv  = Id.id_of_string "s" in let svc = T.cvar sv in
  let ev  = Id.id_of_string "e" in let evc = T.cvar ev in
  T.clam sv T.kstack 
    (T.clam ev T.kcap
       (T.ccode_l_cap [T.Eax,exn_con; T.Esp,T.csptr svc]
	  evc ))

(* Abbreviation for shape of stack pointer in EBP *)
let exn_stack_abbrev = 
  let sv  = Id.id_of_string "s" in let svc = T.cvar sv in
  let ev  = Id.id_of_string "e" in let evc = T.cvar ev in
  T.clam sv T.kstack
    (T.clam ev T.kcap
       (T.ccons (handle_con svc evc) svc))

(* Abbreviate the stack shape common to all labels.
   We need two variables because void functions return nothing in EAX. *)

(*let (stack_abbrev, stack_abbrev_void, stack_abbrev_float) =
  let rv    = Id.id_of_string "?ret" in let rvc  = T.cvar rv    in
  let s1v   = Id.id_of_string "?s1"  in let s1vc = T.cvar s1v   in
  let s2v   = Id.id_of_string "?s2"  in let s2vc = T.cvar s2v   in
  let e1v   = Id.id_of_string "?e1"  in let e1vc = T.cvar e1v   in
  let e2v   = Id.id_of_string "?e2"  in let e2vc = T.cvar e2v   in
  let (abbrev,abbrev_void,abbrev_float) = 
    let (ret_state,ret_state_void,ret_state_float) =
      let ret_ctxt = 
	let ebp_stk = exn_stack_con s2vc e2vc in
	[T.Esp, T.csptr (T.cappend s1vc ebp_stk);
	 T.Ebp, T.csptr ebp_stk]
      in
      (T.ccode_l_cap ((T.Eax, rvc):: ret_ctxt)
         (T.cjoin [e1vc;e2vc]),
       T.ccode_l_cap ret_ctxt
	 (T.cjoin [e1vc;e2vc]),
       T.ccode_l_cap_fps ret_ctxt
	 (T.cjoin [e1vc;e2vc]) fpstack1)
    in
    let aux r =
      T.clam s1v T.kstack
        (T.clam s2v T.kstack
	   (T.clam e1v T.kcap
	      (T.clam e2v T.kcap
		 (T.ccons r s1vc)))) in
    (T.clam rv T.k4byte (aux ret_state), 
     aux ret_state_void,
     aux ret_state_float) in
  (abbrev, abbrev_void, abbrev_float)
*)

let gen_stack_abbrev ret_class convention =
  let rv    = Id.id_of_string "?ret" in let rvc  = T.cvar rv    in
  let spv   = Id.id_of_string "?sp"  in let spvc = T.cvar spv   in
  let s1v   = Id.id_of_string "?s1"  in let s1vc = T.cvar s1v   in
  let s2v   = Id.id_of_string "?s2"  in let s2vc = T.cvar s2v   in
  let e1v   = Id.id_of_string "?e1"  in let e1vc = T.cvar e1v   in
  let e2v   = Id.id_of_string "?e2"  in let e2vc = T.cvar e2v   in
  let ret_state = 
      let ret_ctxt = 
	let ebp_stk = exn_stack_con s2vc e2vc in
	let s1r = 
	  match convention with 
	    P.Stdcall -> s1vc 
	  | P.Cdecl -> (T.cappend spvc s1vc) in
	[T.Esp, T.csptr (T.cappend s1r ebp_stk);
	 T.Ebp, T.csptr ebp_stk]
      in
    match ret_class with
    | StdReturn _ -> 
	T.ccode_l_cap ((T.Eax, rvc):: ret_ctxt) (T.cjoin [e1vc;e2vc])
    | VoidReturn ->        
	T.ccode_l_cap ret_ctxt (T.cjoin [e1vc;e2vc])
    | FloatReturn ->
       T.ccode_l_cap_fps ret_ctxt (T.cjoin [e1vc;e2vc]) fpstack1
  in
  let aux r =
    T.clam spv T.kstack
      (T.clam s1v T.kstack
	 (T.clam s2v T.kstack
	    (T.clam e1v T.kcap
	       (T.clam e2v T.kcap
		  (T.ccons r (T.cappend spvc s1vc)))))) in
  (match ret_class with
    StdReturn _ -> T.clam rv T.k4byte (aux ret_state)
  | _           ->                     aux ret_state)

let stack_abbrev = gen_stack_abbrev (StdReturn T.cbyte4) P.Cdecl
let stack_abbrev_void = gen_stack_abbrev VoidReturn P.Cdecl
let stack_abbrev_float = gen_stack_abbrev FloatReturn P.Cdecl

(* stack has sp:: s1 @ h(s2) @ s2 , return type has capabilities &[e1,e2] *)
(* sp is the type of the parameters on the stack. *)
let stack_con ret_class convention sp s1 s2 e1 e2 =
  let sa_v = gen_stack_abbrev_var ret_class convention in
  match ret_class with
    StdReturn ret_con -> 
      (T.capp (T.capp  (T.capp (T.capp (T.capp (T.capp 
						  (T.cvar sa_v) ret_con) sp) s1) s2) e1) e2)
  | _ -> 
      (T.capp (T.capp (T.capp (T.capp (T.capp (T.cvar sa_v) sp) s1) s2) e1) e2)

let handler_abbrev =
  let s1v = Id.id_of_string "?s1" in let s1vc = T.cvar s1v in
  let s2v = Id.id_of_string "?s2" in let s2vc = T.cvar s2v in
  let e1v = Id.id_of_string "?e1" in let e1vc = T.cvar e1v in
  let e2v = Id.id_of_string "?e2" in let e2vc = T.cvar e2v in
  let esp_stk = T.ccons (T.csptr (exn_stack_con s2vc e2vc ))
                        (T.cappend s1vc (exn_stack_con s2vc e2vc)) in
  T.clam s1v T.kstack 
    (T.clam s2v  T.kstack 
       (T.clam e1v T.kcap
	  (T.clam e2v T.kcap
	     esp_stk )))

let std_abbrevs =
  let a = [array_abbrev_var,       array_abbrev;
	    string_abbrev_var,     string_abbrev; 
	    exnname_var,           exnname_abbrev;
	    exn_var,               exn_abbrev;
	    handle_abbrev_var,     handle_abbrev; 
	    exn_stack_abbrev_var,  exn_stack_abbrev;
	    handler_abbrev_var,    handler_abbrev] in
  let stack_as = 
    let aux r c = (gen_stack_abbrev_var r c, gen_stack_abbrev r c) in
    let r1= StdReturn T.cbyte4 in let r2= VoidReturn in let r3= FloatReturn in
    let c1 = P.Cdecl in let c2= P.Stdcall in 
    [ aux r1 c1; aux r2 c1; aux r3 c1;
      aux r1 c2; aux r2 c2; aux r3 c2] in
  let a = a @ stack_as in
   (* Sanity check. *)
   (List.fold_left (fun ctxt (v, c) -> 
      let k,c = Talcon.check ctxt c in 
      Talctxt.add_abbrev ctxt v c) Talctxt.empty_ctxt a);
  a
;;

(************************ Con utilities *********************)
let app_cons     c cs = List.fold_left T.capp c cs
let raw_name_con n cs = app_cons     (T.clab   n) cs
let name_con     n cs = raw_name_con (tid_type n) cs
let mem_name_con n cs = raw_name_con (tid_mem  n) cs

let close_code names tyvars code = 
  (* Given type, attach forall names,tyvars,s1,s2,cg *)
  List.fold_right (fun v c -> T.cforall v T.kname c) names
  (List.fold_right (fun v c -> T.cforall (tid_tyv v) T.k4byte c) tyvars
     (T.cforall stack1_v T.kstack
	(T.cforall stack2_v T.kstack
	   (T.cforall cap1_v T.kcap    
	      (T.cforall cap2_v T.kcap
			   code)))))

(************************ Con predicates *********************)
(* better way to do this? *)
let is_exnname_con c =
  (match c with
    { T.rcon = T.Capp(c1,c2) } ->
      (match c1 with 
	{ T.rcon = T.Cvar v } -> 
	  if v = exnname_var then true else false
      |	_ -> false)
  | _ -> false)

(************************** Type Translation ******************)

let cap2var cap =
  match cap with
    P.ReadOnly  -> T.Read
  | P.ReadWrite -> T.ReadWrite

let rec typ2con t = (* Translate popcorn type to Tal type (i.e. con) *)
  match t with 
    P.VoidType -> raise Void_Type
  | P.Evar(c,r)   ->
	(* an unconstrained evar can be instantiated with any 4-byte type,
         * we choose int to be simple *)
      begin match !r with
	Some t -> typ2con t
      | None   -> 
	  begin match c with
	    P.Any | P.Byte4 -> (r := Some(P.IntType(true,P.B4)); int_con)
	  | P.Option -> T.clab bogus_option_var
	  end
      end
  | P.VarType v    -> T.cvar (tid_tyv v)
  | P.IntType _    -> int_con
  | P.BooleanType  -> bool_con
  | P.StringType   -> string_con
  | P.CharType     -> char_con
  | P.FloatType    -> float_con
  | P.DoubleType   -> double_con
  | P.ExnType      -> exn_con
  | P.ArrayType(t',_) -> array_con (typ2con t')
(* LR *)
  | P.RepType(t)   -> rep_con (typ2con t)
(* end LR *)
  | P.ExnconType(typ)  -> 
    let con       = try typ2con typ with Void_Type -> T.cbyte4   in
    let con'      = T.cfield con T.Read                          in
    exnname_con con'
  | P.FnType    (c,vs,t',tl) -> let (res,_,_) = fun_con c vs t' tl in res
  | P.NamedType (n,ts)     -> name_con !n (types2cons ts)
  | P.TupleType (c,ts) -> 
      T.cprod_b (List.map (fun t -> 
	T.cfield (typ2con t) (cap2var c)) ts)
  | P.MutableTyp tr -> typ2con (!tr)
  | P.UnresolvedTyId _ -> failwith "Popcomptypes: got an UnresolvedTyId"

(* This function was added to allow the generation of TAL types
   that include Rep information; the old fun_con calls this one after
   translating the parameters *)

and fun_con' conv vs ret_typ p_cons = (* vs are type args *)
  let stack_p     = List.fold_right T.ccons p_cons T.cempty  in
  let ret_class = 
    try
      (match ret_typ with
	P.FloatType | P.DoubleType -> FloatReturn
      |	_ -> StdReturn (typ2con ret_typ))
    with Void_Type -> VoidReturn in
  let stack = stack_con ret_class conv stack_p stack1_c stack2_c cap1_c cap2_c    in
  let exn_stack   = exn_stack_con stack2_c cap2_c in
  let fun_state   =
    T.ccode_l_cap [ (T.Esp,T.csptr (T.cappend stack exn_stack));
		    (T.Ebp,T.csptr exn_stack)]
      (T.cjoin [cap1_c; cap2_c])
  in
  let lab_con = close_code [] vs fun_state in
  (lab_con,stack,stack2_c)

and fun_con c vs ret_typ params = (* vs are type arguments. *)
    (* Calling convention pushes args from right to left, return value in EAX *)
    let rec map params a =
      match params with
	[]     -> a 
      | hd::tl ->
	  (try map tl ((typ2con hd) :: a) with Void_Type -> (map tl a))in
    let p_cons      = List.rev (map params [])                          in
    fun_con' c vs ret_typ p_cons
and types2cons ts = List.map typ2con ts


let mallocarg2con m =
  let rec aux m =
    match m with
      T.Mprod  mas -> T.cprod (List.map aux mas)
    | T.Mbytes s -> T.cfield (T.pcbytes s) T.ReadWrite
    | T.Mbytearray (scale,size) ->
 	T.carray (T.pcint size) (T.cfield (T.pcbytes scale) T.ReadWrite) in
  aux m

let con2field c = T.cfield c T.Read

let typ2mallocfield t = 
  match P.compress t with
    P.DoubleType -> T.Mbytes T.Byte8
  | _ -> T.Mbytes T.Byte4

let bogus_option_con_block = (bogus_option_var,T.k4byte,bogus_option_con)

 (* To turn off annotation hack, use this instead of following and
    modify popcompile.ml popcomptypes.mli as directed there *)
(* let fun_coercion stack1 stack2 cg cons =
   cg::(T.Tapp stack2)::(T.Tapp stack1)::
   (List.rev (List.map (fun c -> T.Tapp c) cons)) *)
let fun_coercion i1 i2 bottom cap1 cap2 cons =
  (T.Tapp (T.Con cap2))
  ::(T.Tapp (T.Con cap1))
  ::(T.Tapp (T.StackTail(T.Ebp,1)))
  ::(T.Tapp (T.StackSlice(T.Esp, i1, i2, bottom)))
  ::(List.rev (List.map (fun c -> T.Tapp (T.Con c)) cons))

(* Functions are already pointers so we don't need indirection to get the 
 * actual value.  In all other cases we do. *)
let needs_indirect t = 
  match t with (P.FnType _ | P.ExnconType _) -> false | _ -> true

let rec get_name t = (* get the name of a named type *)
  let aux r = 
    match r with
      None   -> impos "get_name: uninstantiated evar"
    | Some t -> get_name t
  in
  match t with
    P.NamedType  (n,ts) -> !n
  | P.Evar       (_,r)  -> aux !r 
  | P.MutableTyp tr-> get_name (!tr)
  | _                   -> impos ("get_name: unnamed type " ^ (P.typ2string t))
      
(**************************** Struct Information *************)
type struct_info =
    { struct_null:     bool;
      struct_con:      T.con;
      struct_mem_con:  T.con;
      struct_mem_kind: T.kind;
      struct_kind:     T.kind;
      sfield_infos:    (P.field_name * int * T.con * T.variance) list
    } 

let info_struct scope name tyvars null fields get_field_size get_field_infos =

  let tycons = tyvars_to_cons tyvars          in
  let tylam  = tyvars_to_lam  tyvars          in    
  let kind   = tyvars_to_kind tyvars T.k4byte in

  let mem_kind = 
    let sz = 4 * 
      	(List.fold_left (fun sum f -> sum + get_field_size f) 0 fields) 
    in let base_kind = T.kmemi (to_i32 sz) in
    tyvars_to_kind tyvars base_kind
  in

  let field_infos = get_field_infos fields                   in
  let aux (_,_,con,cap) = T.cfield con cap                   in
  let struct_mem_con'   = T.cprod (List.map aux field_infos) in

  let struct_con = tylam (T.chptr 
			    (if null then [i32_0] else []) 
			    (Some (mem_name_con name tycons))
			    None) in
  let struct_mem_con = tylam struct_mem_con' in
  { sfield_infos    = field_infos;
    struct_null     = null;
    struct_con      = struct_con;
    struct_mem_con  = struct_mem_con;
    struct_kind     = kind;
    struct_mem_kind = mem_kind
  }

let get_struct_field_infos fields =
  let offset = ref 0 in
  let post_incr  i k = (let j = !i in i:=j+k;j) in
  let info_field sf =
    let (n,cap,t) = sf        in
    let con       = typ2con t in
(*
    Printf.printf "processing struct con\n";
    Talpp.print_con Format.std_formatter Talpp.std_options con;
    Format.pp_print_newline Format.std_formatter ();
*)
    let t_cap = cap2var cap in
    n,post_incr offset (words_in_con con),con,t_cap in
  List.map info_field fields

let info_structdecl st = (* : P.structdecl -> struct_info *)
  let scope,name,tyvars,null,fields =
    st.P.st_scope,st.P.st_name,st.P.st_tyvars,st.P.st_possibly_null,
    st.P.st_fields in
  let get_field_size (_,_,typ) = P.words_in_typ typ in
  info_struct scope name tyvars null fields get_field_size get_struct_field_infos

let get_con_field_infos imports =
  let offset = ref 0 in
  let post_incr  i k = (let j = !i in i:=j+k;j) in
  let info_field (n,con) = 
(*
    (Printf.printf "processing import con\n");
     Talpp.print_con Format.std_formatter Talpp.std_options con;
     Format.pp_print_newline Format.std_formatter ();
*)
    (Id.id_to_string n),post_incr offset (words_in_con con),con,T.ReadWrite
  in
  List.map info_field imports
    

(* create struct for imports from dynamic linking *)
let import_struct_info (name : string) (tyvars : string list) 
  (imports : (Identifier.identifier * Tal.con) list) =
  let get_field_size (_,c) = words_in_con c in
  info_struct P.Static name tyvars false imports get_field_size get_con_field_infos
  
(****** end dynamic linking **********)


let struct_null  s_info = s_info.struct_null 
let struct_t     s_info = s_info.struct_con,     s_info.struct_kind
let struct_mem_t s_info = s_info.struct_mem_con, s_info.struct_mem_kind

let struct_field_offset s_info f =
  let rec aux fields =
    match fields with
      [] -> impos ("struct_field_offset: No such field "^f)
    |	((n,o,_,_)::tl) -> if n=f then o else (aux tl) in
  aux s_info.sfield_infos

let struct_field_offsets s_info =
  List.map (fun (_,i,_,_) -> i) s_info.sfield_infos

let struct_field_cons s_info =
  List.map (fun (_,_,c,_) -> c) s_info.sfield_infos

let roll_struct s_info name cs = (* Coerce a value to a struct name. *)
  let coerce_to_mem = T.Roll      (app_cons s_info.struct_con cs ) in
  let coerce_to_sum = T.RollTosum (name_con name cs)               in
  let coerce        = T.Roll      (name_con name cs)               in
  if struct_null s_info
  then [coerce_to_sum;coerce_to_mem]
  else [coerce;coerce_to_mem]

(******************** Union Information *****************)
type union_info = 
    { union_tyvars: id list;  
      union_null:   bool;
      union_con:    T.con;
      union_kind:   T.kind;
      void_infos:   (P.field_name * int32)           list;
      value_infos:  (P.field_name * (int32 * T.con)) list
    } 

let info_uniondecl ud = (* : P.uniondecl -> union_info *)  
  if ud.P.un_possibly_null
  then unimpl "info_uniondecl : option unions";
  let scope,name,tyvars,union_fields = 
    ud.P.un_scope, ud.P.un_name, ud.P.un_tyvars, ud.P.un_fields in

  let tylam = tyvars_to_lam  tyvars          in
  let kind  = tyvars_to_kind tyvars T.k4byte in

  let rec sort_fields fields void_fields value_fields =
    match fields with
      [] -> (List.rev void_fields, List.rev value_fields)
    | (f,P.VoidType)::tl -> sort_fields tl (f::void_fields) value_fields
    | (f,t)::tl          -> sort_fields tl void_fields ((f,t)::value_fields) in
  let void_fields, value_fields = sort_fields union_fields [] [] in
  let value_field_con (f,t)     = (f,typ2con t)                  in
  let value_field_cons = List.map value_field_con value_fields   in

  let i = ref 1 in
  let post_incr i = (let j = !i in  i:=j+1;to_i32 j)                in
  let void_infos  = List.map (fun f -> (f,post_incr i)) void_fields in
  let value_info (f,con) = let j = post_incr i in (f,(j,con))       in
  let value_infos =
    try
      List.map value_info value_field_cons
    with Void_Type -> impos "info_uniondecl: void_type" in
  let tags = List.map snd void_infos                    in
  let sum  = 
    match value_infos with
      [] -> None
    | _  ->
	let aux (n,(tag,con)) =
	  T.cprod [ T.cfield (T.csing (T.pcint tag)) T.Read; con2field con ] in
	let vars = List.map aux value_infos in
	Some (T.csum vars) in
  { union_tyvars = List.map tid_tyv tyvars; 
    void_infos   = void_infos;
    value_infos  = value_infos;
    union_con    = tylam (T.chptr tags sum None);
    union_kind   = kind;
    union_null   = ud.P.un_possibly_null
  } 	
let union_t              u_info        = u_info.union_con, u_info.union_kind
let union_void_tag_assoc u_info fn     = List.assoc fn u_info.void_infos
let union_val_tag_assoc  u_info fn     = List.assoc fn u_info.value_infos
let union_num_voids      u_info        = List.length u_info.void_infos
let union_instantiate    u_info con ts = (* would be nice not to need Talcon *)
  Talcon.substs 
      (Dict.empty Id.id_compare,Dict.inserts (Dict.empty Id.id_compare)
	    (List.combine u_info.union_tyvars ts))
    con
(************************** abstypes **************)
type abstype_info = 
    { abstype_con: T.con;
      abstype_kind: T.kind;
      abstype_coercions: T.con list -> T.con list -> T.coercion list
    } 

let abstype_t ai = (ai.abstype_con, ai.abstype_kind)
  
let info_abstype ad = 
  let scope,name,all_tyvars,exist_tyvars,defn = 
    ad.P.abs_scope,ad.P.abs_name,ad.P.abs_all_tyvars,ad.P.abs_exist_tyvars,
    ad.P.abs_defn in
  let all_tycons = tyvars_to_cons all_tyvars in
  let exist_tycons = tyvars_to_cons exist_tyvars in
  let tylam = tyvars_to_lam all_tyvars in
  let tyexist = tyvars_to_exists (List.rev exist_tyvars) in
  let kind = tyvars_to_kind all_tyvars T.k4byte in
  let body = typ2con defn in
  let con = tylam (tyexist body) in
  (* suppose the definition is:
   *   abstype <a1,...,an>T[e1,...,em] = tau.  
   * for convenience, let's rewrite the body as a lambda:
   *   T_defn = \a1,...,an.\e1,...,em.tau
   * now suppose that a value v at the TAL level has type:
   *   T_defn u1 ... un t1 ... tm 
   * and we want to coerce it to a <u1,..,un>T type.  To
   * do so, we must coerce the value as follows:  First, define
   *  body = T_defn u1 ... un
   * then these are the coercions:
   *  vm = pack v  hiding tm as Exists em.(body t1 t2...em )
   *  ...
   *  v2 = pack v3 hiding t2 as Exists e2,...,em.(body t1 e2...em)
   *  v1 = pack v2 hiding t1 as Exists e1,e2,...,em.(body e1 e2...em)
   * and finally, we must Roll to the label type T u1 ... un.
   *)
  let coercions all_ts exist_ts = 
    let evars = List.map tid_tyv exist_tyvars in
    let doexists = List.fold_right (fun v c -> T.cexist v T.k4byte c) in
    let rec loop evars types used_evars = 
      match evars,types with
	[],[] -> []
      |	v::vs,t::ts -> 
	  let used_evars = v::used_evars in
	  let f1 = doexists used_evars body in
	  let f2 = tyvars_to_lam all_tyvars (ids_to_lam vs f1) in
	  let c = T.Pack(t,app_cons f2 (all_ts @ ts)) in
	  c::(loop vs ts used_evars) 
      |	_,_ -> impos "mismatch in exists coercion calculation" in
    (T.Roll(name_con name all_ts)):: (List.rev (loop evars exist_ts [])) in 

    
    (* I'm too tired to generalize this so for now, I'm assuming that
     * there's at most one existentially quantified variable -- I'll
     * laugh when this breaks.  JGM 
      match evars,exist_ts with
	[v],[t] -> 
	  [T.Roll(name_con name all_ts);
	   T.Pack(t,app_cons (tylam (T.cexist v T.k4byte body)) all_ts)]
      |	_,_ -> impos "multiple abstract variables unimplemented...sorry." in
*)
  { abstype_con = con; 
    abstype_kind = kind;
    abstype_coercions = coercions }  
;;    

let abs_pack_coercions ai all_ts exist_ts = 
  ai.abstype_coercions (types2cons all_ts) (types2cons exist_ts)
;;

(************************** Other **************)
let bool b = (T.Immed (if b then i32_1 else i32_0),[T.Tosum bool_con])

let rec no_effect e =
  (* what we really mean is no effect on other variables and doesn't touch
     register ESI and doesn't branch.  So no nested calls to New are allowed.
     conservative is false *)
  match e.P.raw_exp with
    P.Const        _      -> true
  | P.Var          _      -> true
(* LR *)
  | P.RepTerm             -> true
(* end LR *)
  | P.Primop     (p,es)   -> no_effect_l es
  | P.TypInst(e,t)        -> no_effect e
  | P.StructMember (e',_) -> no_effect e'
  | P.NewUnion(nt,_,f,eo) -> (match eo with None -> true |Some e -> false)
  | P.UnionMember(e,f)    -> no_effect e
  | P.TupleMember  (e',i) -> no_effect e'
  | P.Subscript (e1,e2)   -> no_effect e1 && no_effect e2
  | P.NewExn (v,eo)       -> (match eo with None -> true |Some e -> false)
  | P.Raise e             -> no_effect e
  | P.SeqExp es           -> no_effect_l es
  | P.Nop                 -> true
  | P.Cast (t,e)          -> no_effect e
  | P.Fun f               -> true
  
  | _ -> false

and no_effect_l es =
  List.for_all no_effect es

let exp2typ e = P.compress (deSome e.P.exp_typ)
let exp2con e = typ2con (exp2typ e)

