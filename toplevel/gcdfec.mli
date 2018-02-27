(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew,                                     *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* Gcdfec - Generic Compiler Front End Driver Control
 * Provides some common code for location tracking and error posting.
 *)

exception Exit;;

(* reset_fe source *)
val reset_fe : string -> unit;;
(* Needed for printing error contexts other than during initial file processing
 *)
type ctxt;;
val get_ctxt : unit -> ctxt;;  
val set_ctxt : ctxt -> unit;;

(*** Location Tracking ***)

type loc;;
type seg;;

val string_of_loc : loc -> string;;
val string_of_seg : seg -> string;;

val new_line : Lexing.lexbuf -> unit;;

val loc_of_abs : int -> loc;;
val seg_of_abs : int -> int -> seg;;
val seg_symbol : unit -> seg;;
val seg_rhs : int -> seg;;
val seg_start : seg -> int;; (* needed for error messages *)

(*** Errors ***)

type error_kind = EKlex | EKparse | EKelab;;
type error = {
    err_source : string;
    err_seg : seg;
    err_kind : error_kind;
    err_desc : string
  }
;;

val mk_err_lex : seg -> string -> error;;
val mk_err_parse_symbol : string -> error;;
val mk_err_parse_rhs : int -> string -> error;;
val mk_err_elab : seg -> string -> error;;

(*** Error Posting ***)

exception Error of error;;
val print_context : bool ref;; (* NB: only works if ctxt is set properly *)
val post_error : error -> unit;;
val error_p : unit -> bool;;

(* EOF: gcdfec.mli *)
