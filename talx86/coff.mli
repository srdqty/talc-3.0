(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Richard Samuels                     *)
(*     September 1998, all rights reserved.                           *)
(**********************************************************************)

(* Coff
 * Outputs coff format object files.
 *)
   
open Objfile;;

type coff_file;;

(* make_coff outfilename objfile 
   Creates a COFF file object from the given objfile. *)
 
val make_coff : string -> objfile -> coff_file;;

(* write_coff outfile coff_file
   Writes the data from the coff_file structure to outfile. *)

val write_coff : out_channel -> coff_file -> unit;;

(* create_coff srcfilename outfilename objfile
   Opens a file with the given name, produces a COFF file structure from the
   object file, and writes it. *)

val create_coff : string -> string -> objfile -> unit;;

(* EOF: coff.mli *)
