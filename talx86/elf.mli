(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Richard Samuels                     *)
(*     September 1998, all rights reserved.                           *)
(**********************************************************************)

(* Elf
 * Outputs elf format object files.
 *)
   
open Objfile;;

(* write_elf outfile srcfilename objfile
   Side effects: Creates an ELF file from the given objfile, 
   and writes it into an open file handle. *)

val write_elf : out_channel -> string -> objfile -> unit;;

(* create_elf srcfilename outfilename objfile
   Side effects: Creates and writes a new ELF file from the given objfile. *)

val create_elf : string -> string -> objfile -> unit;;

(* EOF: elf.mli *)
