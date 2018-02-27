(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew,                                     *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* tallinkchk.mli
 * TAL Link Verifier
 *
 * Checks various linking operations are correct.
 *)

open Tal;;
open Talverify;;

(* An import interface with an export interface, representing an object file *)
type imex = tal_int_type*tal_int_type

val verify_link : imex list -> imex;;
val verify_program : imex -> imex -> int_con list;;


(* tallinkchk.mli *)
