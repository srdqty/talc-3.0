(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, David Walker, Dan Grossman          *)
(*     August 1999, all rights reserved.                              *)
(**********************************************************************)
(* disasmx86.mli
 * 
 * Disassembles an object file.  Provides most of the guts of instruction
 * decoding.  
 *)
val get_instr : Disobjfile.objfile -> Tal.instruction
val get_n_instrs : Disobjfile.objfile -> int -> (Tal.instruction list)

