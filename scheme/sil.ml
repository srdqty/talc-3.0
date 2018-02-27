(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Stephanie Weirich,                  *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

open Numtypes;;
open Sast;;

let string_of_op op = 
  begin
    match op with
      (* arithmatic ops *)
      Plus -> "+" 
    | Minus -> "-" 
    | Times -> "*" 
    | Div -> "/" 
      (* comparisons *)
    | Inteq -> "=" 
    | Ptreq -> "eq?"
    | Structeq -> "equal?"
    | Not -> "not" 
    | Less -> "<" 
    | Greater -> ">" 
    | Lesseq -> "<="
    | Greatereq -> ">=" 
      (* type predicates *)
    | Isint -> "int?" 
    | Isbool -> "bool?" 
    | Isnil -> "nil?"
    | Ispair -> "pair?" 
    | Isfn -> "procedure?" 
    | Ischar -> "char?"
    | Isstring -> "string?"
    | Isindesc -> "input-port?"
    | Isoutdesc -> "output-port?"
      (* lists *)
    | Cons -> "cons" 
    | Car -> "car"
    | Cdr -> "cdr" 
    | Setcar -> "set-car!" 
    | Setcdr -> "set-cdr!"
      (* I/O *)
    | Openin -> "open-input-file"
    | Openout -> "open-output-file"
    | Closein -> "close-input-port"
    | Closeout -> "close-output-port"
    | Flushout -> "flush-out"
    | Getchar -> "read-char"
    | Peekchar -> "peek-char"
    | Getstring -> "get-string"
    | Putchar -> "put-char"
    | Putstring -> "put-string"
    | Fgetchar -> "fget-char"
    | Fpeekchar -> "fpeek-char"
    | Fgetstring -> "fget-string"
    | Fputchar -> "fput-char"
    | Fputstring -> "fput-string"
    | Print -> "write"
    | Currentin -> "current-input-port"
    | Currentout -> "current-output-port"
    | Callwin -> "call-with-input-file"
    | Callwout -> "call-with-output-file"
    | Winfile -> "with-input-from-file"
    | Woutfile -> "with-output-to-file"
    | Iseof -> "eof-object?"
      (* String ops *)
    | Newstring -> "newstring" 
    | Sizes -> "string-length" 
    | Subs -> "string-ref"
    | Sets -> "string-set!"
      (* Char ops *)
    | Chr -> "integer->char"
    | Ord -> "char->integer"
  end;;

type tipe =
    D_t | Int_t | String_t | Char_t | Indesc_t | Outdesc_t | Pair_t 
  | Fn_t of int

type coercion = 
    Int2D | String2D | Char2D | Indesc2D | Outdesc2D | Pair2D | Fn2D of int 
  | D2Int | D2String | D2Char | D2Indesc | D2Outdesc | D2Pair | D2Fn of int

let coercion_tipe coercion = 
  match coercion with
    Int2D -> (Int_t,D_t)
  | String2D -> (String_t,D_t) 
  | Char2D -> (Char_t,D_t)
  | Indesc2D -> (Indesc_t,D_t) 
  | Outdesc2D -> (Outdesc_t,D_t)
  | Pair2D -> (Pair_t,D_t)
  | Fn2D i -> (Fn_t i,D_t)
  | D2Int -> (D_t,Int_t)
  | D2String -> (D_t,String_t)
  | D2Char -> (D_t,Char_t)
  | D2Indesc -> (D_t,Indesc_t)
  | D2Outdesc -> (D_t,Outdesc_t)
  | D2Pair -> (D_t,Pair_t)
  | D2Fn i -> (D_t,Fn_t i)

let cancel c1 c2 =
  let (d1,c1) = coercion_tipe c1
  and (d2,c2) = coercion_tipe c2 in
  d1=c2 & c1=d2
;;

let string_of_tipe t = 
  match t with
    D_t -> "D"
  | Int_t -> "int"
  | String_t -> "string"
  | Char_t -> "char"
  | Indesc_t -> "indesc"
  | Outdesc_t -> "outdesc"
  | Pair_t -> "pair"
  | Fn_t i -> "fn"^(string_of_int i)

type primop = Sast.primop
type var = Sast.var

let max_args = 6;;

exception CompilerBug of string;;
let bug s = raise (CompilerBug s);;

type uexp =
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
  | If of exp * exp * exp
  | Seq of exp list
  | Coerce of coercion * exp
and exp = uexp * tipe
and lambda = ((var * tipe) list) * exp

type prog = (var * exp) list

exception IdentityCoercion
let coercions t =
  match t with
    D_t -> raise IdentityCoercion
  | Int_t -> (D2Int,Int2D)
  | String_t -> (D2String,String2D)
  | Char_t -> (D2Char,Char2D)
  | Indesc_t -> (D2Indesc,Indesc2D)
  | Outdesc_t -> (D2Outdesc,Outdesc2D)
  | Pair_t -> (D2Pair,Pair2D)
  | Fn_t i -> (D2Fn i,Fn2D i)
let d2tipe t = fst(coercions t)
let tipe2d t = snd(coercions t)
let type_of (ue,t) = t;;
let uexp_of (ue,t) = ue;;

let primop_type p = 
  match p with
    Sast.Plus -> ([Int_t;Int_t],Int_t)
  | Sast.Minus -> ([Int_t;Int_t],Int_t)
  | Sast.Times -> ([Int_t;Int_t],Int_t)
  | Sast.Div -> ([Int_t;Int_t],Int_t)
  | Sast.Inteq -> ([Int_t;Int_t],D_t)
  | Sast.Ptreq -> ([D_t;D_t],D_t)
  | Sast.Structeq -> ([D_t;D_t],D_t)
  | Sast.Not -> ([D_t],D_t)
  | Sast.Less -> ([Int_t;Int_t],D_t)
  | Sast.Greater -> ([Int_t;Int_t],D_t)
  | Sast.Lesseq -> ([Int_t;Int_t],D_t)
  | Sast.Greatereq -> ([Int_t;Int_t],D_t)
  | Sast.Isint -> ([D_t],D_t)
  | Sast.Isbool -> ([D_t],D_t)
  | Sast.Isnil -> ([D_t],D_t)
  | Sast.Ispair -> ([D_t],D_t)
  | Sast.Isfn -> ([D_t],D_t)
  | Sast.Ischar -> ([D_t],D_t)
  | Sast.Isstring -> ([D_t],D_t)
  | Sast.Isindesc -> ([D_t],D_t)
  | Sast.Isoutdesc -> ([D_t],D_t)
  | Sast.Cons -> ([D_t;D_t],Pair_t)
  | Sast.Car -> ([Pair_t],D_t)
  | Sast.Cdr -> ([Pair_t],D_t)
  | Sast.Setcar -> ([Pair_t;D_t],D_t)
  | Sast.Setcdr -> ([Pair_t;D_t],D_t)
  | Sast.Openin -> ([String_t],Indesc_t)
  | Sast.Openout -> ([String_t],Outdesc_t)
  | Sast.Closein -> ([Indesc_t],D_t)
  | Sast.Closeout -> ([Outdesc_t],D_t)
  | Sast.Flushout -> ([Indesc_t],D_t)
  | Sast.Getchar -> ([],D_t)
  | Sast.Peekchar -> ([],D_t)
  | Sast.Getstring -> ([Int_t],D_t)
  | Sast.Putchar -> ([Char_t],D_t)
  | Sast.Putstring -> ([String_t],D_t)
  | Sast.Fgetchar -> ([Indesc_t],D_t)
  | Sast.Fpeekchar -> ([Indesc_t],D_t)
  | Sast.Fgetstring -> ([Indesc_t;Int_t],D_t)
  | Sast.Fputchar -> ([Outdesc_t;Char_t],D_t)
  | Sast.Fputstring -> ([Outdesc_t;String_t],D_t)
  | Sast.Print -> ([D_t],D_t)

  | Sast.Currentin -> ([], Indesc_t)
  | Sast.Currentout -> ([], Outdesc_t)
  | Sast.Callwin -> ([String_t; Fn_t 1], D_t)
  | Sast.Callwout -> ([String_t; Fn_t 1], D_t)
  | Sast.Winfile -> ([String_t; Fn_t 0], D_t)
  | Sast.Woutfile -> ([String_t; Fn_t 0], D_t)
  | Sast.Iseof -> ([D_t], D_t)

  | Sast.Newstring -> ([Int_t],String_t)
  | Sast.Sizes -> ([String_t],Int_t)
  | Sast.Subs -> ([String_t;Int_t],Char_t)
  | Sast.Sets -> ([String_t;Int_t;Char_t],D_t)
  | Sast.Chr -> ([Int_t],Char_t)
  | Sast.Ord -> ([Char_t],Int_t)


exception CoerceFailure
let rec coerce coercion e = 
  let (cdom_t,ccodom_t) = coercion_tipe coercion in
  if cdom_t = type_of e then
    match e with
      (Coerce (coercion',e'),_) when cancel coercion' coercion ->
	e'
    | _ -> (Coerce (coercion,e),ccodom_t)
  else
    bug ("bad coercion: "^(string_of_tipe cdom_t)^"->"^
	 (string_of_tipe ccodom_t)^" does not match "^
	 (string_of_tipe (type_of e)))
;;

let coerce_to_d t e = 
  if type_of e = t then
  (try coerce (tipe2d t) e with IdentityCoercion -> e)
  else bug "coerce_to_d"

let coerce_from_d t e =
  if type_of e = D_t then
    (try coerce (d2tipe t) e with IdentityCoercion -> e)
  else bug "coerce_from_d"

let rec split count args =
  if count = 0 then ([],args) else
  match args with
    [] -> bug "split in Lambda"
  | (hd::tl) -> 
      let (a1,a2) = split (count - 1) args
      in (hd::a1,a2)

let rec xexp e =
  match e with
    Sast.Int(i) -> coerce Int2D (Int i,Int_t)
  | Sast.String(s) -> coerce String2D (String s,String_t)
  | Sast.Char(c) -> coerce Char2D (Char c,Char_t)
  | Sast.Nil -> (Nil,D_t)
  | Sast.True -> (True,D_t)
  | Sast.False -> (False,D_t)
  | Sast.Stdin -> coerce Indesc2D (Stdin,Indesc_t)
  | Sast.Stdout -> coerce Outdesc2D (Stdout,Outdesc_t)
  | Sast.Stderr -> coerce Outdesc2D (Stderr,Outdesc_t)
  | Sast.Var(v) -> (Var v,D_t)
  | Sast.Set(v,e) -> (Set(v,xexp e),D_t)
  | Sast.Lambda(vs,e) -> 
      let i = List.length vs in
      if i > max_args then
	let (vs1,vs2) = split max_args vs in
	xexp (Sast.Lambda(vs1,Sast.Lambda(vs2,e)))
      else coerce (Fn2D i) 
	  (Lambda(List.map (fun v -> (v,D_t)) vs,xexp e),Fn_t i)
  | Sast.App(e,es) -> 
      let i = List.length es in
      if i > max_args then
	let (es1,es2) = split max_args es in
	xexp(Sast.App(Sast.App(e,es1),es2))
      else 
      	(App(coerce (D2Fn i) (xexp e),List.map xexp es),D_t)
  | Sast.Op(p,es) -> 
      let (dom_ts,codom_t) = primop_type p in
      let es =
	 try
	    List.map2 (fun e t -> coerce_from_d t (xexp e)) es dom_ts 
	 with _ -> 
	    bug ("incorrect number of arguments to " ^ (string_of_op p) )
      in
      coerce_to_d codom_t (Op(p,es),codom_t)
  | Sast.Let(bs,e) -> 
      let (_,t) as e = xexp e in
      (Let(List.map (fun (v,e) -> (v,xexp e)) bs,e),t)
  | Sast.Letrec(fs,e) ->
      let setfs = List.map (fun (v,lam) -> (Sast.Set(v,Sast.Lambda lam))) fs in
      xexp(Sast.Let(List.map (fun (v,_) -> (v,Sast.Nil)) fs,
		    Sast.Seq(setfs @ [e])))
  | Sast.If(e1,e2,e3) -> 
      let (_,t) as e2 = xexp e2 in
      (If(xexp e1, e2, xexp e3),t)
  | Sast.And(e1,e2) -> xexp(Sast.If(e1,e2,Sast.True))
  | Sast.Or(e1,e2) -> xexp(Sast.If(e1,Sast.True,e2))
  | Sast.Seq(es) -> 
      begin
      	let es = List.map xexp es in
      	match (List.rev es) with
	  [] -> xexp (Sast.Nil)
      	| ((_,t)::_) -> (Seq es,t)
      end

let xprog ds = List.map (fun (x,e) -> (x,xexp e)) ds






