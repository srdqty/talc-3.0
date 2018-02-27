(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Chris Hawblitzel,                   *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* ---------------------------- Syntax -------------------------------*)

open Numtypes;;

type location = Gcdfec.seg
type var = string
type type_name = string
type field_name = string

module Varset : Smallset.S with type elt = var
type varset = Varset.t

type scope = Static | Public | Extern | Abstract
type capability = ReadOnly | ReadWrite
type size = B1 | B2 | B4
type convention = Stdcall | Cdecl (* Stdcall is the new default. *)

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
    PiF | Log2_eF | Log2_10F | Log10_2F | Loge_2F | (* Nullary ops. *)
    CosF | SinF | TanF | SqrtF | F2xm1F | FabsF | FroundF | (* Unary ops. *)
    AtanF | FremF | Fyl2xF | Fyl2xp1F | (* Binary ops. *)
    (* *)
    AddrOf

type typ =
    VoidType
  | Evar of var_class * (typ option ref)  (* used for unification of types *)
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
  | NamedType of type_name ref * (typ list) (* ref is needed to support open *)
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
  | Rdtsc of exp * exp
and prim_pattern = Wild_pat of typ ref | Var_pat of var * (typ ref) 
and pattern = 
    No_pat 
  | Prim_pat of prim_pattern 
  | Tuple_pat of (prim_pattern list)
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
  | ExternType of type_name * (var list) * bool  (* bool indicates option *)
  | ExternVal of var * typ
  | GlobalDecl of (scope * var * typ * (exp option) ref)
  | PrefixDecl of var * top_decl list
  | OpenDecl of var * top_decl list
and top_decl = raw_top_decl * location

(* ----------------------- Built-in Values --------------------------*)

val default_convention : convention

val null_pointer_exn  : string
val union_variant_exn : string
val array_bounds_exn  : string

val cstk_type_def : structdecl
val cstk_type   : string  (* Type of call-stack *)
val global_cstk : string  (* Global call-stack *)
val active_cstk : string  (* Call-stack of active exception if any *)
val active_exn  : string  (* Active exn *)

val stack_traces : bool ref

(* ----------------------- Pretty Printing --------------------------*)

(* Printers using the Format module *)

(* the options are:
     flat       : don't do any indenting
     elide_var  : always print evars as "_"
     print_mods : print type modifiers (i.e. for arrays and fntypes) *)
type popprint_opts = { flat: bool; elide_evar: bool; print_mods: bool }
val std_opts : popprint_opts

type type_modifier =
    ModArray of exp option
  | ModParams of convention * string list * typ list
val pr_mods : Format.formatter -> popprint_opts -> type_modifier list -> unit 

val pr_poptype : Format.formatter -> popprint_opts -> var option -> typ -> 
  type_modifier list option
val pr_popexp : Format.formatter -> popprint_opts -> exp -> unit
val pr_popstmt : Format.formatter -> popprint_opts -> stmt -> unit
val pr_poptopdecl : Format.formatter -> popprint_opts -> top_decl -> unit
val pr_popprogram : Format.formatter -> popprint_opts -> top_decl list -> unit

(* converting stuff to strings *)

val cap2string : capability -> string
val var2string : var -> string
val typ2string : typ -> string
val exp2string : exp -> string
val stmt2string : stmt -> string
val topdecl2string : top_decl -> string
val program2string : top_decl list -> string
val primop2string : primop -> string

(* -------------------------- Utilities ------------------------------ *)

val eqtype : typ -> typ -> bool
val add_prefix : string -> var -> var
val size_leq : size -> size -> bool

val mt_varset : varset
val compress  : typ -> typ

val words_in_typ : typ -> int (* number of 32-bit words in a type *)
val sumtyps : typ list -> int (* sum of the # of words in a list of types *)

(* is_special var 
   Takes a fully qualified variable name (all prefixes tacked on).  Returns
   Some (arity * primop) of the special if it is special and None otherwise. *)
val is_special : string -> (int * primop) option
