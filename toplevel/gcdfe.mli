(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew,                                     *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* Gcdfe - Generic Compiler Front End Driver
 * Provides some common code for doing parsing & lexing
 *)

(* Raises Gcdfec.Exit on error *)
val fe :
    (Lexing.lexbuf -> 'a) ->
      ((Lexing.lexbuf -> 'a) -> Lexing.lexbuf -> 'b) ->
	string ->
	  'b
;;

(* EOF: gcdfe.mli *)
