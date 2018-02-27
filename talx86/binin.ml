(* binin
   Contains binary input routines for reading numbers from files. *)


open Utilities;;
open Numtypes;;


(* For little-endian machines only. *)

module type binin  = sig   
   type in_channel
   val get_byte : in_channel -> int
   val get_short : in_channel -> int
   val get_long : in_channel -> (int * int)
   val get_int : in_channel -> int
   val get_int32 : in_channel -> int32
   val get_num : in_channel -> int -> int32
end 

module BininFunc (X: sig
   type in_channel 
   val input_byte : in_channel -> int
   end ) : binin with type in_channel = X.in_channel = 
 struct 
type in_channel = X.in_channel
open X

let get_byte ch = input_byte ch;;

let get_2bytes ch = 
   let b_lo = get_byte ch in
   let b_hi = get_byte ch in
   (b_hi lsl 8) lor b_lo
;;

let get_long ch =			(* Returns (hi, lo) pair. *)
   let s_lo = get_2bytes ch in
   let s_hi = get_2bytes ch in
   (s_hi,s_lo)
;;

let get_4bytes ch =			(* Returns an int. *)
   let (s_hi, s_lo) = get_long ch in
   ((s_hi lsl 16) lor s_lo);;

let get_short ch = get_2bytes ch;;
let get_int ch = get_4bytes ch;;	(* Returns an int. *)
let get_int32 ch = int_to_int32 (get_int ch);;(* Returns an int32. *)

let get_num ch len =			(* Returns an int32. *)
   match len with 
    | 0 -> i32_0
    | 1 -> int_to_int32 (get_byte ch)
    | 2 -> int_to_int32 (get_short ch)
    | 4 -> (get_int32 ch)
    | x -> invalid_arg (Printf.sprintf "get_i32: bad length (%d)!" x)
;;
end 

module Chan = BininFunc (Pervasives) 

      
