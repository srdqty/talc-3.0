(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Richard Samuels                     *)
(*     September 1998, all rights reserved.                           *)
(**********************************************************************)

(* Stringchan
 * The string_chan ADT, which manages writing to a string buffer.
 *)

type string_chan
(* A string channel, with a string and a marker where writing takes place. *)

(* Stringchan.create len *)
val create : int -> string_chan

(* Stringchan.from_string str *)
val from_string : string -> string_chan

(* get_mark sc *)
val get_mark : string_chan -> int

(* set_mark sc newmark *)
val set_mark : string_chan -> int -> unit

(* to_string sc *)
val to_string : string_chan -> string

(* put_char sc char *)
val put_char : string_chan -> char -> unit

(* put_byte sc byte *)
val put_byte : string_chan -> int -> unit

(* put_2bytes sc 2byteint *)
val put_2bytes : string_chan -> int -> unit

(* put_4bytes sc 4byteint *)
val put_4bytes : string_chan -> int -> unit

(* put_string_len sc str offset len 
   Puts len characters of str, starting from offset. *)
val put_string_len : string_chan -> string -> int -> int -> unit

(* put_string sc str *)
val put_string : string_chan -> string -> unit

(* put_extend sc str offset len
   Always puts len chars. If str is too short, adds '\000' chars. *)
val put_extend : string_chan -> string -> int -> int -> unit

(* peek_byte sc *)
val peek_byte : string_chan -> int

(* peek_2bytes sc *)
val peek_2bytes : string_chan -> int

(* peek_4bytes sc *)
val peek_4bytes : string_chan -> int

(* lets the string channel be used as an in_channel *)
type in_channel = string_chan
val input_char : in_channel -> char
val input_byte : in_channel -> int
val pos_in : in_channel -> int
val seek_in : in_channel -> int -> unit
val open_in_bin : string -> in_channel
val close_in : in_channel -> unit
val really_input : in_channel -> string -> int -> int -> unit

type out_channel = string_chan
val output_char : string_chan -> char -> unit
val output_string : string_chan -> string -> unit
val pos_out  : string_chan -> int -> unit
val output_buffer : string_chan -> Buffer.t -> unit

(* EOF: stringchan.mli *)
