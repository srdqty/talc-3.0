
(* DEBUG 
val debug : bool ref
 END DEBUG *)

type 'a internal_global_env = 
    { structs   : (Popsyntax.type_name,Popsyntax.structdecl) Dict.dict;
      unions    : (Popsyntax.type_name,Popsyntax.uniondecl)  Dict.dict;
      abstypes  : (Popsyntax.type_name,Popsyntax.absdecl)   Dict.dict;
      abstracts : (Popsyntax.type_name,(Popsyntax.var list * bool)) Dict.dict;
      union_fs  : (Popsyntax.field_name,Popsyntax.uniondecl option) Dict.dict;
      (* the bool ref is set to true if the global is referenced *)
      globals   : (Popsyntax.var,(bool ref * Popsyntax.typ)) Dict.dict;
      exceptions: (Popsyntax.var,Popsyntax.typ) Dict.dict;
      functions : (Popsyntax.var,'a) Dict.dict;
      open_typs : (Popsyntax.type_name,Popsyntax.type_name) Dict.dict;
      open_vals : (Popsyntax.var,Popsyntax.var) Dict.dict;
   } 

type global_env = Popsyntax.typ internal_global_env

val pr_global_env : 
    Format.formatter -> Popsyntax.popprint_opts -> global_env -> unit
val global_env2string : global_env -> string

val type_check : 
    bool -> Popsyntax.top_decl list -> (Popsyntax.top_decl list) * global_env

val type_subst :
    (Popsyntax.var * Popsyntax.typ) list -> Popsyntax.typ -> Popsyntax.typ
val type_substs :
    (Popsyntax.var * Popsyntax.typ) list -> Popsyntax.typ list -> 
      Popsyntax.typ list

val optimize_binop :
    Popsyntax.primop -> Popsyntax.exp -> Popsyntax.exp -> Popsyntax.raw_exp
val optimize_unop :
    Popsyntax.primop -> Popsyntax.exp -> Popsyntax.raw_exp

(* Cyclone *)
val doesSpliceRet: Popsyntax.stmt -> bool
(* End Cyclone *)

val eliminate_prefixes : Popsyntax.top_decl list -> Popsyntax.top_decl list
(*   val open_prefix : global_env -> Popsyntax.var -> global_env *)

(* dynamic linker *)
val default_initializer : global_env -> Popsyntax.typ -> Popsyntax.location -> Popsyntax.varset -> Popsyntax.exp
(* dynamic linker *)
