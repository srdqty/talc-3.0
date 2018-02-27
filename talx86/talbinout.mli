(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, David Walker, Dan Grossman          *)
(*     August 1999, all rights reserved.                              *)
(**********************************************************************)
(* talbinout:  provides output routines for binary version of
 * type information (i.e., ".to" file generation).
 *)

open Tal;;

val print_stats : bool ref;;

module type talbinout = sig
   type out_channel
   val emit_tal_pre_mod : out_channel -> tal_pre_mod -> unit;;

   (* emit a single con ... its inverse is Talbinin.read_in_con *)
   val emit_out_con : out_channel -> con -> unit;;
   val emit_out_kind : out_channel -> kind -> unit ;;
   val emit_out_label : out_channel -> Identifier.identifier -> unit;;
   val emit_out_psi : out_channel -> int_con list -> unit;;
   val emit_out_tal_int_type : out_channel -> tal_int_type -> unit;;
 end 

module Chan : talbinout with type out_channel = Pervasives.out_channel
module Buf :  talbinout with type out_channel = Buffer.t

