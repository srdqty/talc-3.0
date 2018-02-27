(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, David Walker, Dan Grossman          *)
(*     August 1999, all rights reserved.                              *)
(**********************************************************************)
(* dasm.ml
 *
 * Disassembles an object file and associated .to file to produce a
 * TAL implementation.
 *)

(* disassemble [disassemble internals] [.to file name] [object file name] *)
module type dasm = sig
   val disassemble : bool -> string -> string -> Tal.tal_pre_mod
end 

(* opens files indicated by strings *)
(* disassemble [disassemble internals] [.to file name] [object file name] *)
module Chan : dasm

(* reads strings directly *)
(* disassemble [disassemble internals] [.to data] [object data ] *)
module Str : dasm	 
