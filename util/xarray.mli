(**********************************************************************)
(* (c) Greg Morrisett, Steve Zdancewic                                *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* xarray.mli
 *
 * A datatype for extensible arrays.  They work like normal arrays
 * except that there is an add function that appends a single element
 * to the end of the array, dynamically resizing if necessary.  All
 * operations behave as though the array is only as long as maximum
 * index of elements "initialized" by an add command.
 *)

type 'a xarray

val length : 'a xarray -> int
val get : 'a xarray -> int -> 'a
val set : 'a xarray -> int -> 'a -> unit
val add : 'a xarray -> 'a -> unit
val append : 'a xarray -> 'a xarray -> 'a xarray
val create : int -> 'a -> 'a xarray
val app : ('a -> 'b) -> 'a xarray -> unit
val map : ('a -> 'b) -> 'a xarray -> 'b xarray
val to_array : 'a xarray -> 'a array
val from_array : 'a array -> 'a xarray


(* EOF: xarray.mli *)
