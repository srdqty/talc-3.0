(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew,                                     *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* talpp.mli
 *
 * Pretty printer for x86 tal language.
 *
 * N.B. GAS not yet supported, movsx/movzx not yet supported
 *)

open Identifier;;
open Tal;;

type style = MASM | GAS
type detail = FullDetail | PartialDetail (* partial detail, used for errors *)

type options = { style          : style; 
		 kinds          : bool; 
		 cons           : bool; 
		 expand_abbrevs : bool;
		 expand_pr      : bool;
		 detail         : detail; }(* Changed by print_verify_error*)
;;
(* Control output of types in error messages! *)
val full_types : bool ref;;

val std_options : options;;

val print_scale : Format.formatter -> scale -> unit
val string_of_reg : reg -> string
val print_reg_part : Format.formatter -> options -> reg -> reg_part -> unit
val print_reg : Format.formatter -> options -> reg -> unit
val print_kind : Format.formatter -> options -> kind -> unit
val print_ckind : Format.formatter -> options -> kind -> unit
val print_primcon : Format.formatter -> primcon -> unit
val print_variance : Format.formatter -> variance -> unit
val print_machine_state :
  Format.formatter -> options -> machine_state -> unit
val print_fpstack : Format.formatter -> options -> fpstack -> unit
val print_con : Format.formatter -> options -> con -> unit
val print_ccon : Format.formatter -> options -> con -> unit
val print_coerce :
  Format.formatter -> (Format.formatter -> options -> 'a -> unit) ->
  options -> 'a coerce -> unit
val print_label_coerce :
  Format.formatter -> options -> identifier coerce -> unit
val print_reg_coerce : Format.formatter -> options -> reg coerce -> unit
val print_genop : Format.formatter -> options -> genop -> unit
val print_genop_coerce : Format.formatter -> options -> genop coerce -> unit
val print_unary_op : Format.formatter -> options -> genop -> unit
val print_binop : Format.formatter -> options -> genop -> genop -> unit
val print_cc : Format.formatter -> options -> condition -> unit
val print_mallocarg : Format.formatter -> options -> mallocarg -> unit
val print_instruction : Format.formatter -> options -> instruction -> unit
val print_code_block : Format.formatter -> options -> code_block -> unit
val print_data_block : Format.formatter -> options -> data_block -> unit
val print_tal_int : Format.formatter -> options -> string -> tal_int -> unit
val print_tal_int_type : Format.formatter -> options -> tal_int_type -> unit
val print_tal_pre_mod : Format.formatter -> options -> string -> tal_pre_mod -> unit

open Talctxt;;

val print_tal_loc : Format.formatter -> options -> tal_loc -> unit
val print_verify_error : Format.formatter -> options -> verify_error -> unit
val print_Talverify : Format.formatter -> options -> ctxt*verify_error -> unit
val print_ctxt : Format.formatter -> options -> ctxt -> unit

(* EOF: talpp.mli *)
