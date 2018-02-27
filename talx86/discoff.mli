(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, David Walker, Dan Grossman          *)
(*     August 1999, all rights reserved.                              *)
(**********************************************************************)
(* discoff.mli
 * 
 * Provides a Disobjfile.objfile implementation for reading COFF object
 * files given a file name.
 *)

module type discoff = sig
   val coff_objfile : string -> Disobjfile.objfile
end 

module DiscoffFunc : functor ( X : sig 
   type in_channel
   val input_byte : in_channel -> int
   val seek_in : in_channel -> int -> unit
   val really_input : in_channel -> string -> int -> int -> unit
   val pos_in : in_channel -> int
   val open_in_bin : string -> in_channel
   val close_in : in_channel -> unit
end ) -> discoff


