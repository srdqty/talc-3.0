(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Richard Samuels                     *)
(*     September 1998, all rights reserved.                           *)
(**********************************************************************)

(* Talasm
 * Generic TAL assembler, x86 stuff is in Talasmx86
 *)
   
open Tal;;
open Objfile;;

(* assemble imp (imports, exports) [.ot filename]
   Assembles a Tal implementation into an object file.
   Returns: The assembled object file. 
   Outputs binary annotations to .ot filename if present.
 *)

val assemble : tal_imp -> (tal_int_type * tal_int_type) -> objfile;;

(* EOF: talasm.mli *)

