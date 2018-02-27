(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Stephanie Weirich,                  *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

type tipe =
    D_t | Int_t | String_t | Char_t | Indesc_t | Outdesc_t | Pair_t 
  | Fn_t of int

type coercion = 
    Int2D | String2D | Char2D | Indesc2D | Outdesc2D | Pair2D | Fn2D of int 
  | D2Int | D2String | D2Char | D2Indesc | D2Outdesc | D2Pair | D2Fn of int

type var = Sast.var
type primop = Sast.primop

type uexp =
    Int of Numtypes.int32
  | String of string
  | Char of char
  | Nil
  | True
  | False
  | Stdin
  | Stdout
  | Stderr
  | Var of var
  | Set of var * exp
  | Lambda of lambda
  | App of exp * (exp list)
  | Op of primop * (exp list)
  | Let of (var * exp) list * exp
  | If of exp * exp * exp
  | Seq of exp list
  | Coerce of coercion * exp
and exp = uexp * tipe
and lambda = ((var * tipe) list) * exp

type prog = (var * exp) list

val max_args : int
val primop_type : Sast.primop -> (tipe list) * tipe
val xexp : Sast.exp -> exp
val xprog : Sast.prog -> prog
val bug : string -> 'a
val string_of_tipe : tipe -> string
