(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew,                                     *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* Talbe
 * TAL backend functions.
 *
 *)

open Tal;;

(* Wrapped TAL frontend *)
val read_int : string -> tal_int;;
val read_pre_mod : string -> tal_pre_mod;;

(* Interface caching *)
val get_tali : int_ref -> tal_int;;

(*** Wrapped verifier ***)
val multiple_errors : bool ref;;
val silent_verify : bool ref;;
val print_interfaces : bool ref;;
val verify_internals : bool ref;;
val verify_flag : bool ref;;

(* verify talfilename module *)
val verify : string -> tal_pre_mod -> tal_int_type * tal_int_type;;

(*** Wrapped assembler: modulename module objectfilename ***)
val silent_asm : bool ref;;
(* asm talfilename module import/exports objectfilename *)
val asm : string -> tal_pre_mod -> tal_int_type * tal_int_type -> string -> bool;;

(*** Eventually a wrapped linker will go here ***)

(* EOF: talbe.mli *)
