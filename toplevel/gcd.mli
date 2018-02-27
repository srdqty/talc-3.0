(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew,                                     *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* Gcd - Generic Compiler Driver
 * Provides some common code for handling file types, compilation phases, etc.
 *)

(*** Compiling files ***)

(* compiler filename basename modulename -> success? *)
type compiler = string -> string -> string -> bool;;
(* suffix,compiler *)
type file_type = string * compiler;;

val set_file_types : file_type list -> unit;;
val std_file_types : file_type list;;

val compile_file : string -> unit;;

(*** Object File & Library & Output Name Management ***)

val add_object_file : string -> unit;;
val get_object_files : unit -> string list;;

val add_library : string -> unit;;
val get_libraries : unit -> string list;;

val set_output_name : string -> unit;;

(*** Compilation Phase Management ***)

val set_elab_only : unit -> unit;;
val set_generate_tal_only : unit -> unit;;
val set_assemble_only : unit -> unit;;
val set_do_link : unit -> unit;;

val code_generate_p : unit -> bool;;
val assemble_p : unit -> bool;;
val link_p : unit -> bool;;

(*** Printing IR at Compilation Points ***)

val print_at : string -> unit;;
val do_print : string -> (Format.formatter -> unit) -> unit;;

(*** Type Checking IR at Compilation Points ***)

val type_check_at : string -> unit;;
val do_type_check : string -> (Format.formatter -> unit) -> unit;;

(*** Options and Drivers ***)

val set_tooldesc : string -> unit;;
val short_options : (string * Arg.spec * string) list;;
val std_options : (string * Arg.spec * string) list;;

(* driver toolname options filetypes do_link
 * where do_link objectfiles libraries outputname
 *)
val driver :
  string ->
  (string * Arg.spec * string) list ->
  file_type list ->
  (unit -> bool) ->
  (string list -> string list -> string -> bool) -> unit
;;

(* EOF: gcd.mli *)
