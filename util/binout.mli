(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Richard Samuels                     *)
(*     September 1998, all rights reserved.                           *)
(**********************************************************************)

(* Binout
 * Contains functions for outputting bytes, shorts, longs, and strings.
 * And alignment functions.
 *)

type long = (int * int);;

val put_byte : out_channel -> int -> unit
val put_2bytes : out_channel -> int -> unit
val put_4bytes : out_channel -> int -> unit

val put_short : out_channel -> int -> unit
val put_int : out_channel -> int -> unit
val put_long : out_channel -> long -> unit

val put_hword : out_channel -> int -> unit
val put_word : out_channel -> int -> unit

val put_string_len : out_channel -> string -> int -> int -> unit
(* put_string_len chan str offset len
   Writes a string of the given length, from the given offset.
   While !outputtext = true, puts spaces in between the letters
   so each letter takes the same amount of space as a byte. *)

val put_string : out_channel -> string -> unit
(* put_string chan str *)

val put_extend : out_channel -> string -> int -> int -> unit
(* put_extend chan str offset len
   Outputs len bytes of the given string. If the string isn't long enough,
   it is extended with \000 bytes. *)

val assert_at : out_channel -> int -> string -> unit
(* assert_at chan where what *)

val align_4bytes : int -> int
val align_nbytes : int -> int -> int (* alignment -> offset ... *)
val pad_to_4bytes : out_channel -> unit 
val pad_to_nbytes : int -> out_channel -> unit 

(* EOF: binout.mli *)
