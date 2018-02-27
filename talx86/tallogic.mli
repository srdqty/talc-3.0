(**********************************************************************)
(* (c) David Walker                                                   *)
(*     July 1999, all rights reserved.                                *)
(**********************************************************************)

(* tallogic.mli
 *
 * Arithmetic and logical rules
 *
 *)

open Identifier;;
open Tal;;
open Talctxt;;

exception Bad_rule of string;;

(* Apply an instance of a rule (id cs) in a particular context.  
 * Raise Bad_rule if (rule cs) is ill-formed (eg: rule has wrong arity).
 * The second argument proves its con list argument valid in the current 
 * context.
 *)
val apply  : 
   ctxt -> (ctxt -> con list -> unit) -> identifier -> con list -> ctxt;;
