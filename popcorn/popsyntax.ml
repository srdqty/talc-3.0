(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Chris Hawblitzel, Frederick Smith   *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

open Numtypes;;

type location = Gcdfec.seg
type var = string
type type_name = string
type field_name = string

module Varset = Smallset.Make(struct type t = var let compare = compare end)

type varset = Varset.t

type scope = Static | Public | Extern | Abstract
type capability = ReadOnly | ReadWrite
type size = B1 | B2 | B4
type convention = Stdcall | Cdecl

type var_class = Any | Option | Byte4

type const = 
    Int of int32
  | Bool of bool
  | String of string
  | Char of char
  | Float of f32
  | Double of f64
  | Null

type primop = 
    Plus | Times | TimesU | Minus | Div | DivU | Mod | ModU |
    Eq | Neq | Gt | GtU | Lt | LtU | Gte | GteU | Lte | LteU | Not |
    Bitnot | Bitand | Bitor | Bitxor | Bitlshift | Bitlrshift | Bitarshift |
    Size | Ord | Chr |
    (* floating point operations *)
    PlusF | MinusF | TimesF | DivF |
    EqF | NeqF | GtF | GteF | LtF | LteF |
    (* More floating point operations. *)
    PiF | Log2_eF | Log2_10F | Log10_2F | Loge_2F |
    CosF | SinF | TanF | SqrtF | F2xm1F | FabsF | FroundF | (* Unary ops. *)
    AtanF | FremF | Fyl2xF | Fyl2xp1F | (* Binary operators. *)
    (* address operator *)
    AddrOf

type typ =
    VoidType
  | Evar of  var_class * (typ option ref)  (* used for unification only *)
  | VarType of var
  | IntType of bool * size (* true corresponds to signed. *)
  | BooleanType
  | StringType
  | CharType
  | FloatType
  | DoubleType
  | ArrayType of typ * exp option (* optional size *)
  | FnType of convention * var list * typ * (typ list)
  | TupleType of capability * (typ list)
  | NamedType of type_name ref * (typ list)
  | ExnType
  | MutableTyp of typ ref
  | UnresolvedTyId of type_name * (typ list)
(* LR *)
  | RepType of typ
(* end LR *)
  | ExnconType of typ

and raw_exp =
    Const of const
  | ConstArray of exp list * typ option
  | Var of var
  | Primop of primop * (exp list)
  | Conditional of exp * exp * exp
  | AssignOp of exp * (primop option) * exp  (* e.g., x+=1, x[i] *= 3 *)
  | FunCall of exp * ((typ list) option ref) * (exp list)
  | TypInst of exp * typ list
  | NewStruct of 
      type_name * ((typ list) option ref) * ((field_name option * exp) list)
  | StructMember of exp * field_name
  | NewUnion of type_name * ((typ list) option ref) * field_name * (exp option)
  | UnionMember of exp * field_name
  | NewTuple of exp list
  | TupleMember of exp * int
  | NewAbstype of type_name * ((typ list) option ref) * ((typ list) option ref) * exp
  | Subscript of exp * exp
(*  | NewArray of exp * exp *)
(* Cyclone *)
  | Codegen of fndecl
  | Fill of exp
(* End Cyclone *)
  | NewExn of var * (exp option)
  | Raise of exp
  | SeqExp of exp list
  | Nop
  | Cast of typ * exp
  | Fun of fndecl  (* nested function definition *)
(* LR *)
  | RepTerm (* should only occur within TypInst *)
(* end LR *)

and exp = 
    { mutable exp_typ: typ option;
      mutable raw_exp: raw_exp;
      mutable exp_un_after: varset;
      exp_loc: location
    } 

and raw_stmt =
    Skip
  | Exp of exp
  | Seq of stmt * stmt
  | Return of exp option
  | IfThenElse of exp * stmt * stmt
  | While of exp * stmt
  | Break of var option
  | Continue of var option
  | For of exp * exp * exp * stmt
  | IntSwitch of exp * (int32 * stmt) list * stmt
  | CharSwitch of exp * (char * stmt) list * stmt
  | UnionSwitch of exp * switch_arm list * (stmt option)
  | ExnSwitch of exp * switch_arm list * (stmt option)
  | Decl of var * typ * (exp option) ref * stmt
  | Label of var * stmt
(* Cyclone *)
  | Cut of stmt
  | Splice of stmt
(* End Cyclone *)
  | Do of stmt * exp
  | TryHandle of stmt * var * stmt
  | TryCatchFinally of 
      stmt *             (* try this statement *)
      switch_arm list *  (* switch on the exception *)
      (stmt option) *    (* optional default for exception *)
      (stmt option)      (* finally clause *)
  | With of var * typ option ref * (var list) * exp * stmt
  | Rdtsc of exp * exp  (* hack for low overhead timer. *)
and prim_pattern = Wild_pat of typ ref | Var_pat of var * (typ ref) 
and pattern = 
    No_pat 
  | Prim_pat of prim_pattern 
  | Tuple_pat of prim_pattern list
and switch_arm = { arm_field: field_name;
		   arm_pat:   pattern;
		   arm_body:  stmt
		 } 
and stmt = { mutable raw_stmt:  raw_stmt; 
	     mutable un_before: varset; (* uninit before stmt, set in tcStmt *)
	     mutable un_after:  varset; (* uninit before stmt, set in tcStmt *)
	     stmt_loc: location 
	   }
and fndecl = { fn_static: bool;
	       fn_convention: convention;
	       fn_name: var;
	       fn_tyvars: var list;
	       fn_ret_type: typ;
	       fn_args: (var * typ) list;
	       fn_body: stmt
	      } 

type struct_field = (field_name * capability * typ)
type union_field = (field_name * typ)


type structdecl = { st_scope: scope;
		    st_name: type_name;
		    st_tyvars: var list;
		    st_possibly_null: bool;
		    st_fields: struct_field list
		  }	
type uniondecl = { un_scope: scope;
		   un_name: type_name;
		   un_tyvars: var list;
		   un_possibly_null: bool;
		   un_fields: union_field list
		 }	
type absdecl = { abs_scope: scope;
		 abs_name:  type_name;
		 abs_all_tyvars: var list;
		 abs_exist_tyvars: var list;
		 abs_defn: typ
	       } 

type raw_top_decl =
    FunDecl of fndecl
  | StructDecl of structdecl
  | UnionDecl of uniondecl
  | AbsDecl of absdecl
  | ExceptionDecl of (var * scope * typ)
  | ExternType of type_name * (var list) * bool (* bool indicates option *)
  | ExternVal of var * typ
  | GlobalDecl of (scope * var * typ * (exp option) ref)
  | PrefixDecl of var * top_decl list
  | OpenDecl of var * top_decl list
and top_decl = raw_top_decl * location

(**************************************************************************)

(* Built-in values *)

let default_convention = Cdecl;;

let null_pointer_exn = "NullPointer";;
let union_variant_exn = "UnionVariant";;
let array_bounds_exn = "ArrayBounds";;


let cstk_type   = "_?cstk_type"  (* Type of call-stack *)
let cstk_type_def = { st_scope = Extern;
		      st_name = cstk_type;
		      st_tyvars = [];
		      st_possibly_null = true;
		      st_fields = [ ("hd",ReadWrite,StringType);
				    ("tl",ReadWrite,
				     NamedType(ref cstk_type,[]))]
				  } 

let global_cstk = "__zzzz_global_cstk" (* Global call-stack *)
let active_cstk = "__zzzz_active_cstk" (* Call-stack of active exception. *)
let active_exn  = "__zzzz_active_exn"  (* Active exn *)

let stack_traces = ref false (* flag *)

                            (*******************)
                            (* PRETTY PRINTING *)
                            (*******************)
(**************************************************************************)

(* PRINTING UTILITIES *)

module F = Format
let pr = F.pp_print_string
let ps = F.pp_print_space
let psori f () = F.pp_print_break f 1 2
let pn = F.pp_force_newline

type popprint_opts = { flat: bool; elide_evar: bool; print_mods: bool }
let std_opts = { flat = false; elide_evar = true; print_mods = true }

let is_infix op =
  match op with
  | Not | Bitnot | Size | Ord | Chr | AddrOf
  | PiF | Log2_eF | Log2_10F | Log10_2F | Loge_2F
  | CosF | SinF | TanF | AtanF | FremF | SqrtF | F2xm1F | FabsF | FroundF 
  | Fyl2xF | Fyl2xp1F -> false
  | _ -> true

let is_func op =
  match op with
  | PiF | Log2_eF | Log2_10F | Log10_2F | Loge_2F
  | CosF | SinF | TanF | AtanF | FremF | SqrtF | F2xm1F | FabsF | FroundF 
  | Fyl2xF | Fyl2xp1F -> true
  | _ -> false

(* returns a pair, where the first elem is an int describing the operator's
   precedence (1 being the highest), and the second elem is a boolean
   indicating whether or not the operator is associative *)
(* XXX verify all of these are correct *)
type assoc = Left | Right
let primop_weight op =
  match op with
    (Plus | PlusF) -> (4,Left)
  | (Times | TimesU | TimesF) -> (3,Left)
  | (Minus | MinusF) -> (4, Left)
  | (Div | DivU | DivF) -> (3, Left)
  | (Mod | ModU) -> (3, Left)
  | (Eq | EqF) -> (7, Left)
  | (Neq | NeqF) -> (7, Left)
  | (Gt | GtU | GtF) -> (6, Left)
  | (Lt | LtU | LtF) -> (6, Left)
  | (Gte | GteU | GteF) -> (6, Left)
  | (Lte | LteU | LteF) -> (6, Left)
  | Not -> (2,Right)
  | Bitnot -> (2,Right)
  | Bitand -> (8, Left)
  | Bitor -> (10, Left)
  | Bitxor -> (9, Left)
  | Bitlshift -> (5, Left)
  | Bitlrshift -> (5, Left)
  | Bitarshift -> (5,Left)
  | Size -> (2,Right)
  | Ord -> (2,Right)
  | Chr -> (2,Right)
  | AddrOf -> (2,Right)
(* New floating point operations. *)
  | PiF     -> (0,Left)
  | Log2_eF -> (0,Left)
  | Log2_10F -> (0,Left)
  | Log10_2F -> (0,Left)
  | Loge_2F -> (0,Left)
  | CosF -> (0,Left)
  | SinF -> (0,Left)
  | TanF -> (0,Left)
  | AtanF -> (0,Left)
  | FremF -> (0,Left)
  | SqrtF -> (0,Left)
  | F2xm1F -> (0,Left)
  | FabsF -> (0,Left)
  | FroundF -> (0,Left)
  | Fyl2xF -> (0,Left)
  | Fyl2xp1F -> (0,Left)

(* XXX check these *)
let rec rawexp_weight exp =
  match exp with
    Const _ -> (0,Left)
  | ConstArray _ -> (0,Left)
  | Var _ -> (0,Left)
  | Primop (op,_) -> primop_weight op
  | Conditional (_,_,_) -> (13,Right)
  | AssignOp (_,_,_) -> (14,Right)
  | FunCall (_,_,_) -> (0,Left)
  | TypInst (_,_) -> (0,Left)
  | NewStruct (_,_,_) -> (1,Left)
  | StructMember (_,_) -> (0,Left)
  | NewUnion (_,_,_,_) -> (1,Left)
  | UnionMember (_,_) -> (0,Left)
  | NewTuple _ -> (1,Left)
  | TupleMember (_,_) -> (0,Left)
  | NewAbstype (_,_,_,_) -> (1,Left)
  | Subscript (_,_) -> (0,Left)
  | Codegen _ -> (0,Left)
  | Fill _ -> (0,Left)
  | NewExn (_,_) -> (1,Left)
  | Raise _ -> (0,Left)
  | SeqExp _ -> (15,Left)
  | Nop -> (1,Left)
  | Cast (_,_) -> (1,Right)
  | Fun _ -> (0,Left)
  | RepTerm -> (0,Left)

(* indicates whether the given statement is a single or compound
   statement, and whether or not it needs a trailing semi *)
type stmt_type = Single of bool | Compound of bool

let pr_type t =
  match t with
    Single b -> Printf.eprintf "(SINGLE %b)\n" b
  | Compound b -> Printf.eprintf "(COMPOUND %b)\n" b

let rec get_stmt_type { raw_stmt = s } =
  let defer_to_child s =
    let t = get_stmt_type s in
    match t with
      Compound _ -> Single false 
    | _ -> t in
  let compound s =
    Compound
      (match get_stmt_type s with
	Compound b -> b
      | Single b -> b) in
  match s with
    Skip -> Single true
  | Exp e -> Single true
  | Seq (s,s2) ->
      if s.raw_stmt <> Skip && s2.raw_stmt <> Skip then
	compound s2
      else Single true
  | Return eopt -> Single true
  | IfThenElse (e,s1,s2) -> 
      if s2.raw_stmt <> Skip then 
	defer_to_child s2
      else
	defer_to_child s1
  | While (e,s) -> defer_to_child s
  | Break vopt -> Single true
  | Continue vopt -> Single true
  | For (e1,e2,e3,s) -> defer_to_child s
  | IntSwitch (e, isl, s) -> Single false
  | CharSwitch (e,csl,s) -> Single false
  | UnionSwitch (e,sal,sopt) -> Single false
  | ExnSwitch (e,sal,sopt) -> Single false
  | Decl (v,t,eor,s) -> compound s
  | Label (v,s) -> get_stmt_type s
  | Do (s,e) -> Single true
  | TryHandle (s1,v,s2) -> defer_to_child s2
  | TryCatchFinally (s,sal,defopt,finopt) ->
      (match finopt with
	Some s -> defer_to_child s
      |	None -> Single false)
  | Cut s -> defer_to_child s
  | Splice s -> defer_to_child s
  | With (v,tor,vl,e,s) -> defer_to_child s
  | Rdtsc (_,_) -> Single true
(* for extracting modifiers from types *)
type type_modifier =
    ModArray of exp option
  | ModParams of convention * string list * typ list

(* returns the type without the modifiers and the list of modifiers *)
let rec split_type t =
  match t with
    VoidType -> t, []
  | Evar(c,r) ->
      (match c,!r with
	_,None -> t, []
      | _,Some t' -> split_type t')
  | VarType _ -> t, []
  | IntType(_,_) -> t, []
  | BooleanType -> t, []
  | StringType -> t, []
  | CharType -> t, []
  | FloatType -> t, []
  | DoubleType -> t, []
  | ArrayType(t',eopt) ->
      let t',ms = split_type t' in
      t', ModArray(eopt)::ms
  | FnType (c,vs,t',ts) -> 
      let t',ms = split_type t' in
      t', ModParams(c,vs,ts)::ms
  | TupleType _ -> t, []
  | NamedType (_,_) -> t, []
  | ExnType -> t, []
  | MutableTyp tr -> split_type !tr
  | UnresolvedTyId(_,_) -> t, []
  | RepType _ -> t, []
  | ExnconType _ -> t, []

(*-------------------------------------------------------------------------*)

(* CAPABILITIES *)

let cap2string c = 
  match c with
    ReadOnly -> "ReadOnly"
  | ReadWrite -> "ReadWrite"

(* VARIABLES *)

let var2string v =
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
    then begin w.[!j] <- ':'; w.[!j+1] <- ':'; incr j; end
    else w.[!j] <- v.[!i];
    incr j;
    incr i;
  done;
  w

(* LISTS (of types, expressions, etc.) *)

let pr_poplist f opt p prsep obox l r ts = 
  let rec aux = function 
      [] -> ()
    | [t] -> p t
    | t::ts -> p t; prsep f; aux ts in
  if l <> "" then pr f l;
  if not opt.flat then obox ();
  aux ts;
  if not opt.flat then F.pp_close_box f ();
  if r <> "" then pr f r

(* same as above, but print function takes an int arg to indicate
   place of t in the list; 0 is the first list element *)
let pr_poplistnum f opt p prsep obox l r ts = 
  let rec aux i = function 
      [] -> ()
    | [t] -> p i t
    | t::ts -> p i t; prsep f; aux (i+1) ts in
  if l <> "" then pr f l;
  if not opt.flat then obox ();
  aux 0 ts;
  if not opt.flat then F.pp_close_box f ();
  if r <> "" then pr f r

(* CONSTANTS and PRIMOPS *)

let const2string c =
  match c with
    Int i -> Numtypes.string_of_int32 i
  | Bool b -> if b then "true" else "false"
  | String s -> "\""^(String.escaped s)^"\"" (* XXX may need to be smarter *)
  | Char c -> "'"^(Char.escaped c)^"'"
	(* XXX should fix this to be re-parsable *)
  | Float f -> Numtypes.f32_to_dec f
  | Double d -> Numtypes.f64_to_dec d
  | Null -> "null"

let primop2string op =
  match op with
    (Plus | PlusF) -> "+"
  | (Times | TimesU | TimesF) -> "*"
  | (Minus | MinusF) -> "-"
  | (Div | DivU | DivF) -> "/"
  | (Mod | ModU) -> "%"
  | (Eq | EqF) -> "=="
  | (Neq | NeqF) -> "!="
  | (Gt | GtU | GtF) -> ">"
  | (Lt | LtU | LtF) -> "<"
  | (Gte | GteU | GteF) -> ">="
  | (Lte | LteU | LteF) -> "<="
  | Not -> "!"
  | Bitnot -> "~"
  | Bitand -> "&"
  | Bitor -> "|"
  | Bitxor -> "^"
  | Bitlshift -> "<<"
  | Bitlrshift -> ">>>"
  | Bitarshift ->">>"
  | Size -> "size"
  | Ord -> "ord"
  | Chr -> "chr"
  | AddrOf -> "&"
  (* floating point ops *)
  | PiF -> "Math::pi"
  | Log2_eF -> "Math::log2_e"
  | Log2_10F -> "Math::log2_10"
  | Log10_2F -> "Math::log10_2"
  | Loge_2F -> "Math::loge_2"
  | CosF -> "Math::cos"
  | SinF -> "Math::sin"
  | TanF -> "Math::tan"
  | AtanF -> "Math::atan"
  | FremF -> "Math::frem"
  | SqrtF -> "Math::sqrt"
  | F2xm1F -> "Math::f2xm1"
  | FabsF -> "Math::fabs"
  | FroundF -> "Math::fround"
  | Fyl2xF -> "Math::fyl2x"
  | Fyl2xp1F -> "Math::fyl2xp1"
 
(* TYPES *)

let rec pr_mods f opt ms =
  let hovbox () = F.pp_open_hovbox f 0 in
  let break() = if not opt.flat then F.pp_print_break f 0 0 in

  let pr_poptype' t = pr_poptype f opt None t; () in
  let pr_poptypes = 
    pr_poplist f opt pr_poptype' (fun f -> pr f ","; break ()) in
  let pr_popexp' = pr_popexp f opt in
  let pr_poptyvars = 
    pr_poplist f opt (pr f) (fun f -> pr f ",") in

  let rec aux ms =
    match ms with
      ModArray(eopt)::t -> 
	(match eopt with
	  Some e ->
	    pr_poplist f opt pr_popexp' (fun f -> ()) hovbox 
	      "[" "]" [e]
	| None -> pr f "[]");
	aux t
    | ModParams(c,tyvars,types)::t -> 
	(match c with 
	  Cdecl -> pr f " __cdecl "
	| _ -> ());
	if tyvars <> [] then 
	  pr_poptyvars hovbox "<" ">" tyvars;
	pr_poptypes hovbox "(" ")" types;
	aux t
    |	[] -> () in
  aux ms

and pr_poptype f opt vopt t =
  let obox0() = if not opt.flat then F.pp_open_hvbox f 0 in
  let obox() = if not opt.flat then F.pp_open_hvbox f 2 in
  let cbox() = if not opt.flat then F.pp_close_box f () in
  let break() = if not opt.flat then F.pp_print_break f 0 0 in
  let hovbox () = F.pp_open_hovbox f 0 in

  let pr_poptype' t = pr_poptype f { opt with print_mods = true } None t; () in
  let pr_poptypes = 
    pr_poplist f opt pr_poptype' (fun f -> pr f ","; break ()) in
  let pr_poptyvars = 
    pr_poplist f opt (pr f) (fun f -> pr f ",") in
  let pr_popexp' = pr_popexp f opt in

  let pr_var v =
    (match v with
      Some v -> ps f (); pr f (var2string v)
    | None -> ());
    None in (* not returning any mods *)

  (*------------------------------------------------------------------------*)
  match t with
    VoidType -> pr f "void"; pr_var vopt
  | Evar(c,r) ->
      if not opt.elide_evar then
	(match c,!r with
	  Option,None -> pr f "?%"
	| Byte4,None -> pr f "4%"
	| Any,None -> pr f "%"
	| _,Some t -> pr_poptype' t)
      else
	pr f "_";
      pr_var vopt
  | VarType v -> pr f v; pr_var vopt
  | IntType(sign,size) -> 
      let sz = (match size with B1 -> "byte" | B2 -> "short" | B4 -> "int") in
      if sign then pr f sz
      else F.fprintf f "unsigned@ %s" sz;
      pr_var vopt
  | BooleanType -> pr f "bool"; pr_var vopt
  | StringType -> pr f "string"; pr_var vopt
  | CharType -> pr f "char"; pr_var vopt
  | FloatType -> pr f "float"; pr_var vopt
  | DoubleType -> pr f "double"; pr_var vopt
  | ArrayType(_,_) ->
      let t', ms = split_type t in
      pr_poptype' t';
      (match vopt with
	Some v -> pr f (" "^(var2string v))
      |	None -> pr f " ");
      if opt.print_mods then
	(ps f (); pr_mods f opt ms);
      Some ms
  | FnType (_,_,_,_) -> 
      let t', ms = split_type t in
      pr_poptype' t';
      (match vopt with
	Some v -> pr f (" "^(var2string v))
      |	None -> ());
      if opt.print_mods then
	(ps f (); pr_mods f opt ms);
      Some ms
  | TupleType (c,ts) -> 
      let lb = if c = ReadOnly then "%(" else "*(" in
      pr_poptypes hovbox lb ")" ts; pr_var vopt
  | NamedType (n,ts) -> 
      let v = var2string !n in
      if ts <> [] then
	(pr_poptypes hovbox "<" ">" ts);
      pr f v; pr_var vopt
  | ExnType -> pr f "exn"; pr_var vopt
  | MutableTyp tr -> pr_poptype f opt vopt (!tr)
  | UnresolvedTyId(n,ts) -> 
      let v = var2string n in
      if ts <> [] then
	(pr_poptypes hovbox "<" ">" ts);
      pr f v; pr_var vopt
  | RepType t -> pr_poptypes hovbox "<" ">" [t]; pr f "rep"; pr_var vopt
  | ExnconType t -> pr_poptypes hovbox "<" ">" [t]; pr f "exncon"; pr_var vopt

(* EXPRESSIONS *)

and pr_poprawexp f opt e =
  let obox0() = if not opt.flat then F.pp_open_hvbox f 0 in
  let obox() = if not opt.flat then F.pp_open_hvbox f 2 in
  let cbox() = if not opt.flat then F.pp_close_box f () in
  let break() = F.pp_print_break f 0 0 in
  let ovbox () = if not opt.flat then F.pp_open_vbox f 0 in
  let hovbox () = if not opt.flat then F.pp_open_hovbox f 0 in

  let pr_poptype' t = pr_poptype f opt None t; () in
  let pr_poptypes = 
    pr_poplist f opt pr_poptype' (fun f -> pr f ","; break ()) in
  let pr_popexp' = pr_popexp f opt in
  let pr_popexps = pr_poplist f opt pr_popexp' (fun f -> pr f ","; ps f ()) in

  let needs_paren idx (outprec,outassoc) (inprec,inassoc) =
    if      (outprec < inprec) then true
    else if (outprec > inprec) then false
    else (* equal precedence implies equal associativity *)
      if (outassoc = Left) && (idx = 0) then false
      else if (outassoc = Right) && (idx <> 0) then false
      else true in
  let pr_lparen idx outw inw =
    if needs_paren idx outw inw then
      pr f "(" in
  let pr_rparen idx outw inw =
    if needs_paren idx outw inw then
      pr f ")" in

  let pr_primop_exp op el =
    let opstr = primop2string op in
    let outw = primop_weight op in
    let pr_popexp'' i ({ raw_exp = re } as e) =
      let inw = rawexp_weight re in
      if (needs_paren i outw inw) || (op = Size || op = Ord || op = Chr) then
	(pr_popexps obox0 "(" ")" [e])
      else
       (* (pr_popexps obox0 "[" "]" [e]) in *)
	pr_popexp' e in
    if is_infix op then
      pr_poplistnum f opt pr_popexp''
	(fun f -> pr f " "; pr f opstr; ps f ()) hovbox "" "" el
    else if is_func op then
      pr_poplistnum f opt pr_popexp'' (fun f -> pr f ","; ps f ())
	hovbox (opstr^"(") ")" el
    else
      pr_poplistnum f opt pr_popexp'' (fun f -> ps f ()) hovbox opstr "" el in

  let pr_assignop_exp e opopt lhs rhs =
    let outw = rawexp_weight e in
    let lhsw = rawexp_weight lhs.raw_exp in
    let rhsw = rawexp_weight rhs.raw_exp in
    obox ();
    (* lhs *)
    if needs_paren 0 outw lhsw then
      (pr_popexps hovbox "(" ")" [lhs])
    else
      pr_popexp' lhs;
    pr f " ";
    (* assign operator *)
    (match opopt with
      Some op ->
	pr f (primop2string op)
    | None -> ());
    pr f "="; ps f ();
    (* rhs *)
    if needs_paren 1 outw rhsw then
      (pr_popexps hovbox "(" ")" [rhs])
    else
      pr_popexp' rhs;
    cbox () in

  (*------------------------------------------------------------------------*)
  (* invariant: the caller will print the trailing space *)
  match e with
    Const c -> pr f (const2string c)
  | ConstArray (el, topt) ->
      if el = [] then
	(match topt with
	  Some t -> pr_poptypes obox0 "{:" "}" [t]
	| None -> failwith "pr_poprawexp: array const with no elems or type")
      else
	pr_popexps obox0 "{" "}" el
  | Var v -> pr f (var2string v)
  (* pattern for exp++ *)
  | Primop (Minus, ([ { raw_exp = AssignOp(e,Some Plus,
					  { raw_exp = Const(Int x) })};
		     { raw_exp = Const(Int y) } ] as el)) ->
      if (x = Numtypes.i32_1 && y = Numtypes.i32_1) then
	(let outw, inw = primop_weight Plus, rawexp_weight e.raw_exp in
	pr_lparen 0 outw inw; pr_popexp' e; pr_rparen 0 outw inw; pr f "++")
      else
	pr_primop_exp Minus el
  (* pattern for exp-- *)
  | Primop (Plus, ([ { raw_exp = AssignOp(e,Some Minus,
					  { raw_exp = Const(Int x) })};
		     { raw_exp = Const(Int y) } ] as el)) ->
      if (x = Numtypes.i32_1 && y = Numtypes.i32_1) then
	(let outw, inw = primop_weight Plus, rawexp_weight e.raw_exp in
	pr_lparen 0 outw inw; pr_popexp' e; pr_rparen 0 outw inw; pr f "--")
      else
	pr_primop_exp Minus el
  | Primop (op, el) ->
      pr_primop_exp op el
  (* pattern for e1 && e2 *)
  | Conditional (e1,e2,{ raw_exp = Const(Bool false) }) ->
      let outw = (11,Left) in
      let e1w = rawexp_weight e1.raw_exp in
      let e2w = rawexp_weight e2.raw_exp in

      obox ();
      obox0 ();
      if needs_paren 0 outw e1w then
	(pr_popexps hovbox "(" ")" [e1])
      else
	pr_popexp' e1;
      ps f (); pr f "&&";
      cbox (); ps f (); 

      obox0 ();
      if needs_paren 1 outw e2w then
	(pr_popexps hovbox "(" ")" [e2])
      else
	pr_popexp' e2;
      cbox ();
      cbox ()
  (* pattern for e1 || e2 *)
  | Conditional (e1,{ raw_exp = Const(Bool true) },e2) ->
      let outw = (12,Left) in
      let e1w = rawexp_weight e1.raw_exp in
      let e2w = rawexp_weight e2.raw_exp in

      obox ();
      obox0 ();
      if needs_paren 0 outw e1w then
	(pr_popexps hovbox "(" ")" [e1])
      else
	pr_popexp' e1;
      ps f (); pr f "||";
      cbox (); ps f (); 

      obox0 ();
      if needs_paren 1 outw e2w then
	(pr_popexps hovbox "(" ")" [e2])
      else
	pr_popexp' e2;
      cbox ();
      cbox ()
	
  | Conditional (be,ife,elsee) -> 
      let outw = rawexp_weight e in
      let bew = rawexp_weight be.raw_exp in
      let ifew = rawexp_weight ife.raw_exp in
      let elseew = rawexp_weight elsee.raw_exp in

      obox ();
      (* bool-expr *)
      obox0 ();
      if needs_paren 0 outw bew then
	(pr_popexps hovbox "(" ")" [be])
      else
	pr_popexp' be;
      ps f (); pr f "?";
      cbox (); ps f (); 
      (* true-case *)
      obox ();
      if needs_paren 1 outw ifew then
	(pr_popexps hovbox "(" ")" [ife]; ps f (); pr f "?")
      else
	pr_popexp' ife; 
      ps f (); pr f ":";
      cbox (); ps f (); 
      (* false-case *)
      obox ();
      if needs_paren 2 outw elseew then
	(pr_popexps hovbox "(" ")" [elsee])
      else
	pr_popexp' elsee;
      cbox ();
      cbox ()
  (* pattern for ++exp *)
  | AssignOp (lhs,Some Plus,({ raw_exp = Const(Int x) } as e')) ->
      if x = Numtypes.i32_1 then
	(let outw, inw = rawexp_weight e, rawexp_weight e'.raw_exp in
	pr f "++"; pr_lparen 0 outw inw; pr_popexp' lhs; pr_rparen 0 outw inw)
      else
	pr_assignop_exp e (Some Plus) lhs e'      
  (* pattern for --exp *)
  | AssignOp (lhs,Some Minus,({ raw_exp = Const(Int x) } as e')) ->
      if x = Numtypes.i32_1 then
	(let outw, inw = rawexp_weight e, rawexp_weight e'.raw_exp in
	pr f "--"; pr_lparen 0 outw inw; pr_popexp' lhs; pr_rparen 0 outw inw)
      else
	pr_assignop_exp e (Some Plus) lhs e'      
  | AssignOp (lhs,opopt,rhs) ->
      pr_assignop_exp e opopt lhs rhs
  | FunCall (fexp, _, args) ->      (* XXX ignoring types part ... *)
      pr_popexp' fexp;
      pr_popexps obox0 "(" ")" args
  | TypInst (e,ts) ->
      pr_popexp' e; 
      if ts <> [] then
	pr_poptypes hovbox "@<" ">" ts
  | NewStruct (n, _,fnoptelist) ->  (* XXX ignoring types part ... *)
      let pr_se e =
	match e with
	  (Some n,e) -> 
	    obox0 (); pr f (var2string n); pr f "="; pr_popexp' e; cbox ()
	| (None,e) -> pr_popexp' e in
      let is_labeled =
	match fnoptelist with
	  (Some n,e)::t -> true
	| (None,e)::t -> false
	| [] -> false in
      pr f "^"; pr f (var2string n);
      if not is_labeled then
	pr_poplist f opt pr_se 
	  (fun f -> pr f ","; ps f ()) obox0 "(" ")" fnoptelist
      else
	pr_poplist f opt pr_se 
	  (fun f -> pr f ","; ps f ()) obox0 "{" "}" fnoptelist
  | StructMember (e',n) ->
      let outw = rawexp_weight e in
      let inw = rawexp_weight e'.raw_exp in
      if needs_paren 0 outw inw then
	(pr_popexps hovbox "(" ")" [e'])
      else
	pr_popexp' e'; 
      pr f "."; pr f n
  | NewUnion (n, _, nf, eopt) ->    (* XXX ignoring types part ... *)
      pr f "^"; pr f (var2string n); pr f "."; pr f nf;
      (match eopt with
	Some e -> pr_popexps hovbox "(" ")" [e]
      |	None -> ())
  | UnionMember (e',n) ->
      let outw = rawexp_weight e in
      let inw = rawexp_weight e'.raw_exp in
      if needs_paren 0 outw inw then
	(pr_popexps hovbox "(" ")" [e'])
      else
	pr_popexp' e'; 
      pr f "."; pr f n
  | NewTuple el ->
      pr f "^"; pr_popexps hovbox "(" ")" el
  | TupleMember (e',x) ->
      let outw = rawexp_weight e in
      let inw = rawexp_weight e'.raw_exp in
      if needs_paren 0 outw inw then
	(pr_popexps hovbox "(" ")" [e'])
      else
	pr_popexp' e'; 
      pr f "."; F.pp_print_int f x
  | NewAbstype (n, _, _, e) ->
      pr f "^"; pr f (var2string n); pr_popexps hovbox "(" ")" [e];
  | Subscript (e',ie) ->
      let outw = rawexp_weight e in
      let inw = rawexp_weight e'.raw_exp in
      if needs_paren 0 outw inw then
	(pr_popexps hovbox "(" ")" [e'])
      else
	pr_popexp' e'; 
      pr_popexps hovbox "[" "]" [ie];
  | Codegen fd ->
      obox ();
      pr f "codegen"; ps f (); 
      pr_poplist f opt (fun fd -> pr_fundecl f opt true fd) 
	(fun _ -> ()) ovbox "(" ")" [fd];
      cbox ()
  | Fill e ->
      pr f "fill"; ps f (); 
      pr_popexps hovbox "(" ")" [e]
  | NewExn (v,eopt) ->
      pr f "^"; pr f (var2string v); 
      (match eopt with
	Some e -> pr_popexps hovbox "(" ")" [e]
      |	None -> pr f "()")
  | Raise e ->
      pr f "raise"; ps f (); pr_popexps hovbox "(" ")" [e]
  | SeqExp el ->
      pr_popexps obox0 "(" ")" el
  | Nop -> ()
  | Cast (t,e') ->
      let outw = rawexp_weight e in
      let inw = rawexp_weight e'.raw_exp in
      pr_poptypes hovbox "(:" ")" [t]; 
      if needs_paren 1 outw inw then
	(pr_popexps hovbox "(" ")" [e'])
      else
	pr_popexp' e'
  | Fun fd ->
      obox ();
      pr f "fun"; ps f (); 
      pr_fundecl f opt true fd;
      cbox ()      
  | RepTerm -> pr f "repterm"

and pr_popexp f opt exp = 
  (* I presume the type part is added during inference; therefore
     I won't print it here *)
  pr_poprawexp f opt exp.raw_exp

(* STATEMENTS *)

and pr_poprawstmt f opt s =
  let obox0() = if not opt.flat then F.pp_open_hvbox f 0 in
  let obox() = if not opt.flat then F.pp_open_hvbox f 2 in
  let ovbox() = if not opt.flat then F.pp_open_vbox f 0 in
  let cbox() = if not opt.flat then F.pp_close_box f () in
  let break() = if not opt.flat then F.pp_print_break f 0 0 in
  let hovbox () = if not opt.flat then F.pp_open_hovbox f 0 in
  let hbox () = if not opt.flat then F.pp_open_hbox f () in

  let pr_poptype' t = pr_poptype f opt None t; () in
  let pr_poptypes = 
    pr_poplist f opt pr_poptype' (fun f -> pr f ","; break ()) in
  let pr_poptyvars = 
    pr_poplist f opt (pr f) (fun f -> pr f ",") in
  let pr_popexp' = pr_popexp f opt in
  let pr_popexps = pr_poplist f opt pr_popexp' (fun f -> pr f ","; ps f ()) in
  let pr_popstmt' = pr_popstmt f opt in

  let pr_semi t =
    (match t with
      Single true | Compound true -> pr f ";"
    | _ -> ()) in
  let pr_closing_semi t = if t = (Compound true) then pr f ";" in
  let is_compound t =
    match t with
      Compound _ -> true
    | _ -> false in
  let pr_openbrace t = if is_compound t then pr f " {" in
  let pr_closebrace t = if is_compound t then (ps f (); pr f "}") in

  (* for printing switch statements *)
  let switch_preamble e =
    obox0 ();
    pr f "switch ";
    pr_popexps obox "(" ")" [e];
    pr f " {"; 
    cbox ();
    psori f () (* break *) in
  let switch_arms al def pc gets =
    let rec aux al =
      match al with
	arm::t ->
	  let s = (gets arm) in
	  obox0 ();
	  pr f "case "; pc f arm; pr f ":"; 
	  cbox (); psori f ();
	  obox0 ();
	  pr_popstmt' s;
	  pr_semi (get_stmt_type s);
	  cbox ();
	  if t <> [] then ps f (); (* break *)
	  aux t
      | [] -> (* default case *)
	  (match def with
	    Some def ->
	      ps f (); pr f "default:"; psori f ();
	      obox0 ();
	      pr_popstmt' def;
	      pr_semi (get_stmt_type def);
	      cbox ()
	  | None -> ()) in
    ovbox ();
    aux al;
    cbox () in
  let pr_switch_clause f { arm_field = fn; arm_pat = p } = 
    let pr_prim_pat p =
      match p with
	Wild_pat _ -> pr f "_"
      |	Var_pat (v,_) -> pr f v in
    let pr_pat p =
      match p with 
	No_pat -> ()
      |	Prim_pat p -> 	  
	  pr_poplist f opt pr_prim_pat (fun f -> pr f ","; ps f ()) 
	    obox0 " (" ")" [p]
      |	Tuple_pat pl ->
	  pr_poplist f opt pr_prim_pat (fun f -> pr f ","; ps f ()) 
	    obox0 " *(" ")" pl in
    pr f (var2string fn); pr_pat p in

  (* for statements *)
  let pr_for decl_opt e1 e2 e3 s =
    let t = get_stmt_type s in
      (* header *)
    ovbox ();
    obox0 ();
    pr f "for ";
    (match decl_opt with
      Some pr_d ->
	(* assumes that e1 is a Nop *)
	pr f "(";
	if not opt.flat then obox0 ();
	pr_d (); pr f ";"; ps f ();
	pr_popexp' e2; pr f ";"; ps f ();
	pr_popexp' e3;
	if not opt.flat then F.pp_close_box f ();
	pr f ")"
    | None -> 
	pr_poplist f opt pr_popexp' (fun f -> pr f ";"; ps f ()) obox0 "(" ")" 
      [e1; e2; e3]);
    pr_openbrace t;
    cbox ();
    psori f (); (* break *)
      (* body *)
    obox0 ();
    pr_popstmt' s; 
    pr_closing_semi t;
    cbox ();
    pr_closebrace t;
    cbox () in

  (*------------------------------------------------------------------------*)
  match s with
    Skip -> ()
  | Exp e -> obox (); pr_popexp' e; cbox ()
  | Seq (s,s2) ->
      if s.raw_stmt <> Skip then
	(pr_popstmt' s; pr_semi (get_stmt_type s); ps f ());
      if s2.raw_stmt <> Skip then
	pr_popstmt' s2;
  | Return eopt ->
      obox();
      pr f "return";
      (match eopt with
	Some e -> pr_popexps obox0 " (" ")" [e]
      |	None -> ());
      cbox ()
  | IfThenElse (e,s1,s2) ->
      let t1 = get_stmt_type s1 in
      let t2 = get_stmt_type s2 in
      (* if-case *)
      ovbox ();
      obox0 ();
      pr f "if ";
      pr_popexps obox "(" ")" [e];
      pr_openbrace t1;
      cbox ();
      psori f (); (* break *)
      obox0 ();
      pr_popstmt' s1; 
      if s2.raw_stmt <> Skip then
	pr_semi t1
      else
	pr_closing_semi t1;
      cbox ();
      pr_closebrace t1;
      (* else-case *)
      if s2.raw_stmt <> Skip then 
	(ps f ();
	 pr f "else"; 
	 pr_openbrace t2;
	 psori f (); (* break *)
	 obox0 ();
	 pr_popstmt' s2; 
	 pr_closing_semi t2;
	 cbox (); 
	 pr_closebrace t2);
      cbox ()
  | While (e,s) ->
      let t = get_stmt_type s in
      ovbox ();
      obox0 ();
      pr f "while ";
      pr_popexps obox "(" ")" [e];
      pr_openbrace t;
      cbox ();
      psori f (); (* break *)
      obox0 ();
      pr_popstmt' s; 
      pr_closing_semi t; 
      cbox ();
      pr_closebrace t;
      cbox ()
  | Break vopt ->
      (match vopt with
	Some v ->
	  obox (); pr f "break"; ps f (); pr f v
      |	None -> pr f "break")
  | Continue vopt ->
      (match vopt with
	Some v ->
	  obox (); pr f "continue"; ps f (); pr f v
      |	None -> pr f "continue")
  | For (e1,e2,e3,s) ->
      pr_for None e1 e2 e3 s
  | IntSwitch (e, isl, s) ->
      ovbox ();
      switch_preamble e;
      switch_arms isl (Some s)
	(fun f (i,_) -> let s = Numtypes.string_of_int32 i in pr f s)
	(fun (_,s) -> s);
      ps f (); pr f "}";
      cbox ()
  | CharSwitch (e,csl,s) ->
      ovbox ();
      switch_preamble e;
      switch_arms csl (Some s)
	(fun f (c,_) -> let s = Char.escaped c in pr f ("'"^s^"'"))
	(fun (_,s) -> s);
      ps f (); pr f "}";
      cbox ()
  | UnionSwitch (e,sal,sopt) ->
      ovbox ();
      switch_preamble e;
      switch_arms sal sopt
	pr_switch_clause
	(fun sa -> sa.arm_body);
      ps f (); pr f "}";
      cbox ()
  | ExnSwitch (e,sal,sopt) ->
      ovbox ();
      switch_preamble e;
      switch_arms sal sopt
	pr_switch_clause
	(fun sa -> sa.arm_body);
      ps f (); pr f "}";
      cbox ()
  | Decl (v,t,eor,s) ->
      let pr_decl () =
	let _ = pr_poptype f opt (Some v) t in
	(match !eor with
	  Some e -> pr f " ="; ps f (); pr_popexp' e
	| None -> ()) in
	
      (match s.raw_stmt with
	For(e1,e2,e3,s) -> 
	  if e1.raw_exp = Nop then
	    pr_for (Some pr_decl) e1 e2 e3 s
	  else
	    (obox (); pr_decl (); pr f ";"; cbox ())
      |	Skip -> obox (); pr_decl (); cbox ()
      |	_ -> obox (); pr_decl (); pr f ";"; cbox ());

      (match s.raw_stmt with
	Skip -> ()
      | For(e1,_,_,_) ->
	  if e1.raw_exp = Nop then () 
	  else (ps f (); pr_popstmt' s)
      |	_ -> ps f (); pr_popstmt' s)
  | Label (v,s) ->
      pr f v; pr f ":"; ps f ();
      pr_popstmt' s; 
  | Do (s,e) ->
      let t = get_stmt_type s in
      (* header *)
      ovbox ();
      pr f "do"; 
      pr_openbrace t;
      psori f (); (* break *)
      (* body *)
      obox0 ();
      pr_popstmt' s; 
      pr_semi t;
      cbox ();
      ps f (); 
      obox0 ();
      (match t with
	Compound _ -> pr f "}"; ps f ()
      |	_ -> ());
      pr f "while "; 
      pr_poplist f opt pr_popexp' (fun f -> pr f ";"; ps f ()) obox0 "(" ")" 
	[e];
      cbox ();
      cbox ()
  | TryHandle (s1,v,s2) -> 
      (* header *)
      ovbox ();
      pr f "try {"; psori f (); (* break *)
      (* body *)
      obox0 ();
      pr_popstmt' s1;
      pr_semi (get_stmt_type s1);
      cbox ();
      ps f (); 
      obox0 ();
      pr f "}"; ps f (); pr f "handle "; 
      pr f v; ps f (); pr f "{";
      cbox (); psori f (); (* break *)
      obox0 ();
      pr_popstmt' s2;
      pr_semi (get_stmt_type s2);
      cbox ();
      ps f (); pr f "}";
      cbox ()      
  | TryCatchFinally (s,sal,defopt,finopt) ->
      (* header *)
      ovbox ();
      pr f "try {"; psori f (); (* break *)
      (* body *)
      obox0 ();
      pr_popstmt' s;
      pr_semi (get_stmt_type s);
      cbox ();
      ps f (); pr f "}"; ps f (); 
      (* catch plus clauses *)
      if sal <> [] then 
	(pr f "catch {"; psori f ();
	 switch_arms sal defopt pr_switch_clause
	   (fun sa -> sa.arm_body));
      ps f (); pr f "}";
      (* finally XXX *)
      (match finopt with
	Some s ->
	  let t = get_stmt_type s in
	  ps f (); pr f "finally"; 
	  pr_openbrace t;
	  psori f (); (* break *)
	  obox0 ();
	  pr_popstmt' s;
	  pr_closing_semi t;
	  cbox ();
	  pr_closebrace t
      |	None -> ());
      cbox ()
  | Cut s ->
      let t = get_stmt_type s in
      hovbox ();
      pr f "cut";
      pr_openbrace t;
      psori f ();
      obox0 ();
      pr_popstmt' s; 
      pr_closing_semi t;
      cbox (); 
      pr_closebrace t;
      cbox ()      
  | Splice s ->
      let t = get_stmt_type s in
      hovbox ();
      pr f "splice";
      pr_openbrace t;
      psori f ();
      obox0 ();
      pr_popstmt' s; 
      pr_closing_semi t;
      cbox (); 
      pr_closebrace t;
      cbox ()      
  | With (v,tor,vs,e,s) ->
      let t = get_stmt_type s in
      ovbox ();
      obox0 ();
      pr f "with "; pr f v; pr f " ";
      pr_poptyvars hbox "[" "]" vs;
      pr f " ="; ps f ();
      pr_popexp' e;      
      pr f " do";
      pr_openbrace t;
      cbox ();
      psori f (); (* break *)
      obox0 ();
      pr_popstmt' s;
      pr_closing_semi t;
      cbox ();
      pr_closebrace t;
      cbox ()
  | Rdtsc (e1,e2) ->
      hovbox ();
      pr f "rdtsc ";
      pr_popexp' e1;
      pr f " : ";
      pr_popexp' e2;
      cbox ();
and pr_popstmt f opt s =
  pr_poprawstmt f opt s.raw_stmt

(* TOP-LEVEL DECLARATIONS *)

and pr_fundecl f opt elide_scope fd =
  let obox0() = if not opt.flat then F.pp_open_hvbox f 0 in
  let obox() = if not opt.flat then F.pp_open_hvbox f 2 in
  let cbox() = if not opt.flat then F.pp_close_box f () in
  let break() = if not opt.flat then F.pp_print_break f 0 0 in
  let ovbox () = if not opt.flat then F.pp_open_vbox f 0 in
  let hovbox () = if not opt.flat then F.pp_open_hovbox f 0 in

  let pr_poptyvars = 
    pr_poplist f opt (pr f) (fun f -> pr f ",") in
  let pr_poptype' t = pr_poptype f opt None t; () in
  let pr_popstmt' = pr_popstmt f opt in

  let pr_semi t = 
    match t with 
      Compound true | Single true -> pr f ";"
    | _ -> () in

  (*----------------------------------------------------------------------*)
  let pr_arg (v,t) = pr_poptype f opt (Some v) t; () in
  ovbox ();
  hovbox ();
  if not fd.fn_static && not elide_scope then
    (pr f "static"; ps f ());
  let ms = pr_poptype f { opt with print_mods = false } None fd.fn_ret_type in
  ps f ();
  (match fd.fn_convention with
  | Cdecl -> pr f "__cdecl"; ps f ();
  | _ -> ());
  pr f (var2string fd.fn_name);
  if fd.fn_tyvars <> [] then
    pr_poptyvars obox "<" ">" fd.fn_tyvars;
  pr f " ";
  pr_poplist f opt pr_arg (fun f -> pr f ","; ps f ()) 
    obox0 "(" ")" fd.fn_args;
  ps f (); 
  (match ms with
    Some ms -> pr_mods f opt ms; ps f () 
  | None -> ());
  pr f "{";
  cbox (); psori f (); (* break *)
  ovbox ();
  pr_popstmt' fd.fn_body;
  pr_semi (get_stmt_type fd.fn_body);
  cbox (); break ();
  pr f "}";
  cbox ()

and pr_poprawdecl f opt d =
  let obox0() = if not opt.flat then F.pp_open_hvbox f 0 in
  let obox() = if not opt.flat then F.pp_open_hvbox f 2 in
  let ovbox() = if not opt.flat then F.pp_open_vbox f 0 in
  let cbox() = if not opt.flat then F.pp_close_box f () in
  let break() = if not opt.flat then F.pp_print_break f 0 0 in

  let hbox () = F.pp_open_hbox f () in
  let hvbox () = F.pp_open_hvbox f 0 in
  let hovbox () = F.pp_open_hovbox f 0 in

  let pr_poptype' t = pr_poptype f opt None t; () in
  let pr_poptypes = 
    pr_poplist f opt pr_poptype' (fun f -> pr f ","; break ()) in
  let pr_poptyvars = 
    pr_poplist f opt (pr f) (fun f -> pr f ",") in
  let pr_popexp' = pr_popexp f opt in
  let pr_popexps = pr_poplist f opt pr_popexp' (fun f -> pr f ","; ps f ()) in
  let pr_popstmt' = pr_popstmt f opt in
  let pr_poptopdecls tds =
    let rec aux tds =
      match tds with
	td::t ->
	  pr_poptopdecl f opt td;
	  if t <> [] then ps f ();
	  aux t
      |	[] -> () in
    ovbox ();
    aux tds;
    cbox () in

  let pr_scope scope =
    (match scope with
      Static -> pr f "static"; ps f ();
    | Public -> ();
    | Extern -> pr f "extern"; ps f ();
    | Abstract -> pr f "abstract"; ps f ()) in

  let pr_struct_or_union_decl kw scope possnull tyvars name fields prfd =
    let pr_field_decls sfl pf =
      let rec aux sfl =
	match sfl with
	| [x] -> pf x
	| x::rest ->
	    pf x; ps f (); aux rest
	| [] -> () in
      ovbox ();	
      aux sfl;
      cbox ()
    in
    ovbox ();
    obox ();
    (* scope *)
    pr_scope scope;
    (* ? *)
    if possnull then
      pr f "?"; 
    pr f kw; pr f " ";
    (* tyvars *)
    if tyvars <> [] then
      pr_poplist f opt (pr f) (fun f -> pr f ","; ps f ()) obox "<" ">" 
	tyvars;
    (* name *)
    pr f (var2string name);
    ps f (); pr f "{";
    cbox (); psori f (); (* break *)
    (* field decls *)
    pr_field_decls fields prfd;
    break ();
    pr f "}";
    cbox () in

  (*------------------------------------------------------------------------*)
  (* assumes that the caller will print the necessary break between
     each declaration. *)
  match d with
    FunDecl fd -> 
      pr_fundecl f opt false fd;
      flush Pervasives.stderr
  | StructDecl sd ->
      let pr_field (fn,cap,t) =
	obox0 ();
	if cap = ReadOnly then
	  (pr f "const"; ps f ());
	let _ = pr_poptype f opt (Some fn) t in
	pr f ";";
	cbox () in 
      pr_struct_or_union_decl "struct" sd.st_scope sd.st_possibly_null
	sd.st_tyvars sd.st_name sd.st_fields pr_field
  | UnionDecl ud ->
      let pr_field (fn,t) =
	obox0 ();
	let _ = pr_poptype f opt (Some fn) t; pr f ";" in
	cbox () in 
      pr_struct_or_union_decl "union" ud.un_scope ud.un_possibly_null
	ud.un_tyvars ud.un_name ud.un_fields pr_field      
  | ExceptionDecl (v,s,t) ->
      obox0 ();
      pr_scope s;
      pr f "exception";
      ps f (); pr f (var2string v);
      if t != VoidType then
	pr_poptypes obox0 "(" ")" [t];
      pr f ";";
      cbox ();
  | ExternVal (v,t) ->
      obox0 ();
      pr f "extern ";
      let _ = pr_poptype f opt (Some v) t in
      pr f ";";
      cbox ();
  | AbsDecl ad ->
      obox0 ();
      pr_scope ad.abs_scope;
      pr f "abstype";
      ps f (); 
      if ad.abs_all_tyvars <> [] then
	pr_poptyvars hbox "<" ">" ad.abs_all_tyvars;
      pr f (var2string ad.abs_name);
      pr_poptyvars hbox "[" "]" ad.abs_exist_tyvars;
      pr f " ="; ps f ();
      obox0 ();
      pr_poptype' ad.abs_defn;
      cbox ();
      pr f ";";
      cbox ()      
  | ExternType (name, vs, is_opt) -> 
      obox0 ();
      pr f "extern ";
      pr f (var2string name);
      if is_opt then pr f "?";
      if vs <> [] then pr_poptyvars hbox "<" ">" vs;
      pr f ";";
      cbox ();      
  | GlobalDecl (s,v,t,eor) ->
      if not opt.flat then
	F.pp_open_hovbox f 2;
      pr_scope s;
      let _ = pr_poptype f opt (Some v) t in
      (match !eor with
	Some e -> pr f " ="; ps f (); pr_popexp' e
      |	None -> ());
      pr f ";";
      cbox ()
  | PrefixDecl (v,tds) ->
      obox0 ();
      pr f "prefix"; ps f ();
      pr f v; pr f " {";
      cbox ();
      psori f (); (* break *)
      pr_poptopdecls tds;
      ps f (); pr f "}"
  | OpenDecl (v,tds) ->
      obox0 ();
      pr f "open"; ps f ();
      pr f v; pr f " {";
      cbox ();
      psori f (); (* break *)
      pr_poptopdecls tds;
      ps f (); pr f "}"

and pr_poptopdecl f opt (d,l) =
  pr_poprawdecl f opt d

let pr_popprogram f opt decls =
  let rec aux =
    function
      	[d] -> pr_poptopdecl f opt d
      | (d::t) -> pr_poptopdecl f opt d; Format.pp_print_cut f (); aux t
      |	[] -> () in
  Format.pp_open_vbox f 0;
  aux decls;
  Format.pp_print_newline f ()

(* Convert a pretty-printing function to a 2string function *)

let x2string px opt obox x =
  let f = F.str_formatter in
  obox f;
  px f opt x;
  F.pp_close_box f ();
  F.pp_print_flush f ();
  F.flush_str_formatter ()

let typ2string = 
  let obox f = F.pp_open_hbox f () in
  x2string (fun f opt t -> pr_poptype f opt None t; ())
    { flat = true; elide_evar = false; print_mods = true } obox

let exp2string =
  let obox f = F.pp_open_hvbox f 0 in
  x2string pr_popexp std_opts obox

let stmt2string =
  let obox f = F.pp_open_hvbox f 0 in
  x2string pr_popstmt std_opts obox

let topdecl2string =
  let obox f = F.pp_open_hvbox f 0 in
  x2string pr_poptopdecl std_opts obox

let program2string =
  let obox f = F.pp_open_hvbox f 0 in
  x2string pr_popprogram std_opts obox

                             (*************)
                             (* UTILITIES *)
                             (*************)
(*************************************************************************)

let eqtype t1 t2 = (t1 = t2)

let add_prefix prefix v = (prefix ^ "?" ^ v)

let size_leq s1 s2 = 
  begin match s1,s2 with
    B1,B1 | B1,B2 | B1,B4 | B2,B2 | B2,B4 | B4,B4 -> true
  | _,_ -> false
  end

let mt_varset  = Varset.empty

(* Path compression on Evar's so they point directly to the defined type 
   if any; also crawls under any MutableTyp constructors *)
let rec compress t =
  match t with
    Evar (_,r) ->
      (match !r with
	None -> t
      | Some t' -> (let t'' = compress t' in r := Some t''; t''))
  | MutableTyp tr -> compress (!tr)
  | _ -> t

(* number of 32-bit words in a popcorn type 
 * assumes all type variables abstract types of size 32 bits
 *)

let words_in_typ t =
  match compress t with
    VoidType -> 0
  | DoubleType -> 2
  | Evar _ -> 1 (* FMS: Commented out. Unconstrained evars have size 1.
		   failwith "words_in_typ: use after type checking" *)
  | _ -> 1

(* sum of the sizes of the types *)
let sumtyps ts = 
  let plustyp i t = i + words_in_typ t in
  List.fold_left plustyp 0 ts

let specials = 
  let spec = Hashtbl.create 53 in
  let specials_list = ["pi"      , 0, PiF; 
		       "log2_ef" , 0, Log2_eF; 
		       "log2_10f", 0, Log2_10F;
		       "log10_2f", 0, Log10_2F;
		       "loge_2f" , 0, Loge_2F;
		       "cos"     , 1, CosF;
		       "sin"     , 1, SinF;
		       "tan"     , 1, TanF;
		       "atan"    , 2, AtanF;
		       "frem"    , 2, FremF;
		       "sqrt"    , 1, SqrtF;
		       "f2xm1"   , 1, F2xm1F;
		       "fabs"    , 1, FabsF;
		       "fround"  , 1, FroundF;
		       "fyl2x"   , 2, Fyl2xF;
		       "fyl2xp1" , 2, Fyl2xp1F] in
  let specials_insert (s,a,o) = 
    Hashtbl.add spec (add_prefix "Math" s) (a,o) in
  List.iter specials_insert specials_list;
  spec
;;

let is_special v = 
  if Hashtbl.mem specials v then (Some (Hashtbl.find specials v)) else None
