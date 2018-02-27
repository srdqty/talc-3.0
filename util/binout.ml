(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Richard Samuels                     *)
(*     September 1998, all rights reserved.                           *)
(**********************************************************************)

(* Binout
 * Contains functions for outputting bytes, shorts, longs, and strings.
 * And alignment functions.
 *)

open Utilities;;

type long = (int * int);;

(* topbyte -- PRIVATE
   Returns the top byte of a 4-byte integer, preserving the sign bit. *)

let topbyte n =
   if (n < 0) then
      (((n asr 24) land 0xFF) lor 0x80)
   else
      ((n lsr 24) land 0xFF)
;;

(* Put functions : write to a channel, like the built-in output_byte. *)
let put_byte chan n =
   output_byte chan n
;;
   
let put_2bytes chan n =
   output_byte chan (n land 0xFF);
   output_byte chan ((n lsr 8) land 0xFF)
;;

let put_4bytes chan n =
   output_byte chan (n land 0xFF);
   output_byte chan ((n lsr 8) land 0xFF);
   output_byte chan ((n lsr 16) land 0xFF);
   output_byte chan (topbyte n)
;;

let put_short = put_2bytes;;
let put_int = put_4bytes;;
let put_long chan (s_hi, s_lo) = 
   put_short chan s_lo;
   put_short chan s_hi
;;

let put_hword = put_2bytes;;
let put_word = put_4bytes;;

let put_string_len chan str offset len = 
      (output chan str offset len)
;;

let put_string chan str = put_string_len chan str 0 (String.length str);;

(* Outputs len bytes of the given string. If the string isn't long enough,
   it is extended with \000 bytes. *)

let put_extend chan str offset len =
  let extended_string = (String.make (max len (String.length str)) '\000') in 
  begin
    (String.blit str 0 extended_string 0 (String.length str));
    (put_string_len chan extended_string offset len);
  end
;;

let assert_at chan where what =
  let pos = (pos_out chan) in
  if pos <> (pos_out stdout) then 
    if pos <> where then
      (print_string ("Wrong position for " ^ what ^ ": now at "
		     ^ (string_of_int pos) ^ ", should be at " 
		     ^ (string_of_int where)))
;;

(* align_4bytes
   Returns the next byte location from n that is divisible by 4. *)
let align_4bytes n = (n /+ 4) * 4;;
(* like align_4bytes but used for n instead. *)
let align_nbytes a n = (n /+ a) * a;;

(* pad_to_4bytes
   Writes pad bytes (\0) to the file until the next byte to be written will
   be aligned on a 4-byte boundary. *)
let pad_to_4bytes outfile = 
   while ((pos_out outfile) mod 4) != 0 do
      (output_byte outfile 0)
   done
;;

let pad_to_nbytes n outfile = 
   while ((pos_out outfile) mod n) != 0 do
      (output_byte outfile 0)
   done
;;

(* EOF: binout.ml *)




