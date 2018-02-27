(**********************************************************************)
(* (c) David Walker                                                   *)
(*     July 1999, all rights reserved.                                *)
(**********************************************************************)

(* tallogic.ml
 *
 * Arithmetic and logical rules
 *
 *)

open Identifier;;
open Tal;;
open Talctxt;;
open Numtypes;;

type rule = con list -> con list * con list;;
type rules = (identifier, rule) Dict.dict;;

exception Bad_rule of string;;
let bad_rule name = raise (Bad_rule (id_to_string name));;

(* Structural Rules *)

let empty_name = id_of_string "empty";;
let only_name = id_of_string "only";;

(* Logical Rules *)

(* Arithmetic Rules *)

let pc0 = pcint i32_0;;
let pc1 = pcint i32_1;;

(* reflexivity of inequalities: a ineq a *)
let reflexltu_name = id_of_string "reflexltu";;
let reflexlteu_name = id_of_string "reflexlteu";;
let reflexlts_name = id_of_string "reflexlts";;
let reflexltes_name = id_of_string "reflexltes";;

let reflex_rule ineq name cs =
  match cs with
    [a] -> ([],[clog ineq [a; a]])
  | _ -> bad_rule name

let reflexltu_rule  = reflex_rule Cltu  reflexltu_name
let reflexlteu_rule = reflex_rule Clteu reflexlteu_name
let reflexlts_rule  = reflex_rule Clts  reflexlts_name
let reflexltes_rule = reflex_rule Cltes reflexltes_name

(* transitivity of inequalities: a ineq b  &&  b ineq c  implies  a ineq c *)
let transltu_name  = id_of_string "transltu";;
let translteu_name = id_of_string "translteu";;
let translts_name  = id_of_string "translts";;
let transltes_name = id_of_string "transltes";;

let trans_rule ineq name cs =
  match cs with
    [a;b;c] -> ([clog ineq [a; b]; clog ineq [b; c]], [clog ineq [a; c]])
  | _ -> bad_rule name;;

let transltu_rule  = trans_rule Cltu  transltu_name
let translteu_rule = trans_rule Clteu translteu_name
let translts_rule  = trans_rule Clts  translts_name
let transltes_rule = trans_rule Cltes transltes_name

(* weaken less than to less than or equal:
 * a < b   implies  a <= b
 * a <# b  implies  a <=# b
 *)
let ltu_to_lteu_name = id_of_string "ltu_to_lteu";;
let lts_to_ltes_name = id_of_string "lts_to_ltes";;

let ltu_to_lteu_rule cs =
  match cs with
    [a;b] -> ([cltu a b],[clteu a b])
  | _ -> bad_rule ltu_to_lteu_name

let lts_to_ltes_rule cs =
  match cs with
    [a;b] -> ([clts a b],[cltes a b])
  | _ -> bad_rule lts_to_ltes_name

(* Converts signed array bounds into unsigned array bounds
 * 0 <=s i   &&   i <s n   implies   i <u n
 *)
let array1_name = id_of_string "array1";;
let array1_rule cs =
  match cs with
    [i;n] -> ([cltes pc0 i; clts i n], [cltu i n])
  | _ -> bad_rule array1_name;;

(* For loops descending by 1
 * 0 <=s i   &&   i <s n   implies   i-1 <s n
 *)  
let array2_name = id_of_string "array2";;
let array2_rule cs =
  match cs with
    [i;n] -> 
      ([cltes pc0 i; clts i n], [clts (csub i pc1) n])
  | _ -> bad_rule array2_name;;

(* For loops ascending by 1
 * 0 <=s i   &&   i <s n   implies   0 <=s i++1
 *)  
let array3_name = id_of_string "array3";;
let array3_rule cs =
  match cs with
    [i;n] -> 
      let iplus = cadd [i;pc1] in
      ([cltes pc0 i; clts i n], [cltes pc0 iplus])
  | _ -> bad_rule array2_name;;

(*
 * i <= i ++ j   &&   i ++ j < k   implies   j < k 
 *)
let array4_name = id_of_string "array4";;
let array4_rule cs =
  match cs with
    [i;j;k] -> 
      let sum = cadd [i; j] in
      ([clteu i sum; cltu sum k],[cltu j k])
  | _ -> bad_rule array4_name;;  

(*  
 * 0 <=s stride*i  &&  stride*i <s stride*n  implies  stride*(i-1) <s stride*n
 *)
let arraydown_name = id_of_string "arraydown";;
let arraydown_rule cs =
  match cs with
    [stride;i;n] -> 
      let stridei      = (clog Cmuls [stride;i])        in
      let strideiminus = (clog Cmuls [stride;csub i pc1]) in
      let striden      = (clog Cmuls [stride;n])        in
      ([cltes pc0 stridei; clts stridei striden],[clts strideiminus striden])
  | _ -> bad_rule arraydown_name;;

(*
 * 0 <=s stride*i  &&  stride*i <s stride*n  implies  0 <= stride*(i+1)
 *)  
let arrayup_name = id_of_string "arrayup";;
let arrayup_rule cs =
  match cs with
    [stride;i;n] ->
      let stridei     = (clog Cmuls [stride;i])         in
      let strideiplus = (clog Cmuls [stride;cadd [pc1; i]]) in
      let striden     = (clog Cmuls [stride;n])         in
      ([cltes pc0 stridei; clts stridei striden],[clts pc0 strideiplus])
  | _ -> bad_rule arraydown_name;;

(* subtract from both sides of the inequality.
 * if c is constant between 0 and the largest signed integer (max) then:
 * a <=u max - c  && b <=u max - c  &&  a ++ c ineq b ++ c  
 *    implies  a ineq b
 *
 * the first two conditions are sufficient to show that a ++ c or
 * b ++ c do not overflow and that therefore a and b share the
 * same relationship as a ++ c and b ++ c
 *)
let minusltu_name = id_of_string "minusltu";;
let minuslteu_name = id_of_string "minuslteu";;
let minuslts_name = id_of_string "minuslts";;
let minusltes_name = id_of_string "minusltes";;

let minus_rule ineq name cs = 
  match cs with
    [a;b;c] -> 
      (match c.rcon with
	Cprim PCint i -> 
	  if i32_0 <$ i && i <$ max_int32 then
	    ([ clteu a (csub (pcint max_int32) c); 
	       clteu b (csub (pcint max_int32) c);
	       clog ineq [cadd [a;c]; cadd [b;c]]
	     ], 
	     [clog ineq [a;b]])
	  else
	    bad_rule name
      |	_ -> bad_rule name)
  | _ -> bad_rule name;;

let minusltu_rule  = minus_rule Cltu  minusltu_name;;
let minuslteu_rule = minus_rule Clteu minuslteu_name;;
let minuslts_rule  = minus_rule Clts  minuslts_name;;
let minusltes_rule = minus_rule Cltes minusltes_name;;

(* All non-structural rules *)
let rules =
  Dict.inserts (Dict.empty id_compare) 
    [

  reflexltu_name,reflexltu_rule;
  reflexlteu_name,reflexlteu_rule;
  reflexlts_name,reflexlts_rule;
  reflexltes_name,reflexltes_rule;

  transltu_name,transltu_rule;
  translteu_name,translteu_rule;
  translts_name,translts_rule;
  transltes_name,transltes_rule;

  array1_name,array1_rule;
  array2_name,array2_rule;
  array3_name,array3_rule;
  array4_name,array4_rule;
  arrayup_name,arrayup_rule;
  arraydown_name,arraydown_rule;

  ltu_to_lteu_name,ltu_to_lteu_rule;
  lts_to_ltes_name,lts_to_ltes_rule;

  minusltu_name,minusltu_rule;
  minuslteu_name,minuslteu_rule;
  minuslts_name,minuslts_rule;
  minusltes_name,minusltes_rule;

    ]
;;

let lookup rules name = 
  try Dict.lookup rules name
  with Dict.Absent -> raise Not_found
;;

(* test for structural rules first then apply logical or arithmetic rules *)
(* raises Not_found if the rule has not been defined *)
let apply ctxt prove rule_name cs =
  if (id_compare rule_name empty_name) = 0 then
    set_prop ctxt pctrue
  else if (id_compare rule_name only_name) = 0 then
    (prove ctxt cs; set_prop ctxt (clog Cand cs))
  else  
    begin
      let (premises,results) = (lookup rules rule_name) cs in
      prove ctxt premises;
      List.fold_left add_conjunct ctxt results
    end
;;

