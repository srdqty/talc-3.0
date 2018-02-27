(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Stephanie Weirich, Dan Grossman     *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

open Numtypes;;

type var = string

let var_counter = ref 0

let newvar() =
  let x = !var_counter in begin var_counter := x + 1; "x"^(string_of_int x) end

(* types include:
 *    ints, bools/nil, pairs, functions, strings, chars, indesc, outdesc
 *)
type primop = 
      (* arithmatic ops *)
      Plus
    | Minus
    | Times
    | Div 
      (* comparisons *)
    | Inteq
    | Ptreq 
    | Structeq
    | Not 
    | Less
    | Greater
    | Lesseq 
    | Greatereq 
      (* type predicates *)
    | Isint
    | Isbool
    | Isnil
    | Ispair
    | Isfn 
    | Ischar
    | Isstring
    | Isindesc
    | Isoutdesc
      (* lists *)
    | Cons 
    | Car 
    | Cdr 
    | Setcar 
    | Setcdr 
      (* I/O *)
    | Openin
    | Openout
    | Closein
    | Closeout
    | Flushout
    | Getchar
    | Peekchar
    | Getstring
    | Putchar
    | Putstring
    | Fgetchar
    | Fpeekchar
    | Fgetstring
    | Fputchar
    | Fputstring
    | Print
    | Currentin
    | Currentout
    | Callwin
    | Callwout
    | Winfile
    | Woutfile
    | Iseof
      (* String ops *)
    | Newstring 
    | Sizes 
    | Subs 
    | Sets 
      (* Char ops *)
    | Chr
    | Ord

type exp = 
    Int of int32
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
  | Letrec of (var * lambda) list * exp
  | If of exp * exp * exp
  | And of exp * exp
  | Or of exp * exp
  | Seq of exp list
and lambda = (var list) * exp

type prog = (var * exp) list   (* defines *)



