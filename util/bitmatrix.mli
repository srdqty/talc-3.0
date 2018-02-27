(**********************************************************************)
(* (c) Greg Morrisett, Steve Zdancewic                                *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)


(* bitmatrix.mli
 *
 * The datatype for extensible bitmatrices.
 *)

open Identifier

type bit_matrix

val create : unit -> bit_matrix

(* Sets an entry to true *)
val set : bit_matrix -> identifier -> identifier -> unit

(* Sets an entry to false *)
val clear : bit_matrix -> identifier -> identifier -> unit

(* Gets the value of an entry.  Uninitialized default to false. *)
val get : bit_matrix -> identifier -> identifier -> bool

(* EOF: bitmatrix.mli *)
