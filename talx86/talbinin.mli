(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, David Walker, Dan Grossman          *)
(*     August 1999, all rights reserved.                              *)
(**********************************************************************)
(* talbinin.ml 
 * 
 * Provides routines for reading in TAL constructs in a binary form.
 *)
open Utilities;;
open Numtypes;;
open Identifier;;
open Tal;;
open Talbin;;

type tal_info = 
    { ti_imports     : int_ref vector;
      ti_exports     : int_ref vector;
      ti_kindabbrevs : kind_abbrev vector; (* LX *)
      ti_abbrevs     : con_abbrev vector;
      ti_con_blocks  : con_block vector;
      ti_code_annots : (con option * (inst_annot list)) vector;
      ti_data_annots : (int32 * con option * ((data_annot list) coerce)) vector;
      ti_template_annots: (con * (con option * (inst_annot list)) vector) vector
    } 
(* read_tal_info read_internals ch *)

module type talbinin = sig
   type in_channel
   val read_tal_info : bool -> in_channel -> tal_info	 
   val read_in_con : in_channel -> con
   val read_in_kind : in_channel -> kind
   val read_in_label : in_channel -> identifier
   val read_in_psi : in_channel -> int_con list
   val read_in_tal_int_type : in_channel -> tal_int_type
end 

module TalbininFunc : functor (X : sig 
   type in_channel 
   val input_char : in_channel -> char
   val input_byte : in_channel -> int
   val pos_in : in_channel -> int
   val seek_in : in_channel -> int -> unit
   val close_in : in_channel -> unit
end ) -> talbinin with type in_channel = X.in_channel

module Chan : talbinin with type in_channel = Pervasives.in_channel
module Str : talbinin with type in_channel = Stringchan.string_chan

