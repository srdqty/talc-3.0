(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Richard Samuels                     *)
(*     September 1998, all rights reserved.                           *)
(**********************************************************************)

(* Talasmx86
 * x86 TAL assembler, generic stuff is in Talasm
 *)
(* Changes:
   RLS 3/3/99:
   - asm_env.a_symdict changed to asm_env.a_r_symdict, which is a symbol_dict
     ref instead of a symbol_dict, to allow Cyclone hole macros to function.
*)
   
open Tal;;
open Objfile;;

(***** asm_env *****)

(* NG: This really belongs somewhere else but the dependancies are all
 *     screwed up, so for now, it it here.
 *)

(* asm_env
 * A structure containing the variables needed to assemble instructions. 
 * This is used by asm.ml and SHOULD NOT CHANGE between implementations.
 *)

type asm_env = {
    a_schan : Stringchan.string_chan;  (* The output channel. *)
    a_r_symdict : symbol_dict ref;     (* The symbol dictionary. *)
    mutable a_hole_syms : symbol list; (* Hole symbols. *)
      a_secnum : int;		      (* The number of the section. *)
    mutable a_pos_offset : int;	      (* How real pos differs from expected.*)
    mutable a_r_reloclist : relocation list ref  (* Relocations are put here. *)
 }
;;

(***** x86 Specific Functions *****)

(* length_of_instr : instruction -> int
   Returns the instruction's maximum length. *)

val length_of_instr : instruction -> int;;

(* length_of_data_item 
   Returns the data item's length. *)

val length_of_data_item : data_item -> int;;

(* write_instr env instr
   Writes the entire instruction to the buffer. *)

val write_instr : asm_env -> instruction -> unit;;

(* write_data_item env ditem
   Writes the data item to the buffer. *)

val write_data_item : asm_env -> data_item -> unit;;

(* EOF: talasmx86.mli *)
